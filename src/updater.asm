;*******************************************************************************
; UPDATER.ASM
; This file loads and validates an image in UltiMem RAM for the flasher.
; STANDALONE_FLASHER selects an update from disk. Monster's built-in flasher
; reopens the exact file selected and confirmed by flashcheck.asm.
;*******************************************************************************

.include "firmware.inc"
.include "flashio.inc"
.include "macros.inc"

;*******************************************************************************
; CONFIGURATION
; Stage the entire image before allowing the writer to erase the flash chip.
.ifdef STANDALONE_FLASHER
.define FLASH_CODE "CODE"
.define FLASH_RODATA "RODATA"
.define FLASH_BSS "BSS"
.else
.define FLASH_CODE "FLASH_CODE"
.define FLASH_RODATA "FLASH_RODATA"
.define FLASH_BSS "FLASH_BSS"
.endif
FLASH_BASE    = $6000
UPDATE_HEADER = $4000
UPDATE_BANK   = ULTIMEM_BLK2
.scope krn
	setnam = $ffbd
	clrchn = $ffcc
	close  = $ffc3
	chrout = $ffd2
	getin  = $ffe4
	plot   = $fff0
.endscope

;*******************************************************************************
ULTIMEM_BLKS  = $9ff2
ULTIMEM_BLK2  = $9ffa
ULTIMEM_BLK3  = $9ffc

.repeat 26, I
	.charmap $41+I, $41+I
.endrepeat

.segment FLASH_CODE

;*******************************************************************************
; PREPARE UPDATE
; Loads and validates the selected image in UltiMem RAM before flash erasure.
; OUT:
;  - .C: set if no usable update was found
.export __updater_prepare
.proc __updater_prepare
@ptr=rb
.ifdef STANDALONE_FLASHER
	ldx #15
@template:
	lda filename_template,x
	sta flashio::filename,x
	dex
	bpl @template

	ldx #4
@zero:	lda #'0'
	sta flashio::filename+7,x
	dex
	bpl @zero

	ldx #3
@magic:	lda FLASH_BASE+FIRMWARE_OFFSET,x
	cmp update_magic,x
	bne @directory
	dex
	bpl @magic

	ldx #4
@version:
	lda FLASH_BASE+FIRMWARE_VERSION,x
	cmp #'0'
	bcc @directory
	cmp #'9'+1
	bcs @directory
	dex
	bpl @version

	ldx #4
@copy_version:
	lda FLASH_BASE+FIRMWARE_VERSION,x
	sta flashio::filename+7,x
	dex
	bpl @copy_version

@directory:
	lda #3
	jsr flashio::screen
	lda #<msg_installed
	ldy #>msg_installed
	jsr flashio::message
	jsr print_version

	lda #$00
	sta found_update
	lda #<msg_searching
	ldy #>msg_searching
	jsr flashio::message

	lda #1
	ldxy #directory_name
	jsr krn::setnam
	jsr flashio::open_named
	bcc :+
	jmp @io_error
:
	; Directory stream: load address, then BASIC lines with link and size.
	; The first line is the volume label, never an update file.
	lda #1
	sta skip_entry
	jsr directory_byte
	jsr directory_byte

@line:	jsr directory_byte
	sta link_low
	jsr directory_byte
	ora link_low
	beq @end_directory
	jsr directory_byte
	jsr directory_byte
	lda #$00
	sta name_length
	sta quoted

@text:	jsr directory_byte
	bne :+
	lda #$00
	sta skip_entry
	jmp @line
:	cmp #'"'
	bne @character
	inc quoted
	lda quoted
	cmp #2
	bne @text
	lda skip_entry
	bne @text
	jsr consider_name
	jmp @text

@character:
	ldx quoted
	cpx #1
	bne @text
	ldx name_length
	cpx #17
	bcs @text
	sta candidate,x
	inc name_length
	jmp @text

@end_directory:
	jsr __updater_close
	lda found_update
	bne @stage
	lda #<msg_current
	ldy #>msg_current
	jsr flashio::status
	sec
	rts
.else
	; Monster already checked and confirmed this filename. Re-read only it.
	jmp @stage
.endif

@stage:	lda #5
	jsr flashio::screen
	lda #<msg_available
	ldy #>msg_available
	jsr flashio::message
	jsr print_version
	lda #<msg_loading
	ldy #>msg_loading
	jsr flashio::message
	jsr flashio::open
	bcc :+
	jmp @io_error
:
	; BLK2 = scratch RAM, BLK3 remains flash for the writer.
	lda ULTIMEM_BLKS
	and #$f3
	ora #$0c
	sta ULTIMEM_BLKS
	lda #$00
	sta UPDATE_BANK
	sta ULTIMEM_BLK2+1
	sta checksum
	sta checksum+1
	sta @ptr
	lda #$40
	sta @ptr+1

	; Stage and validate the first header before trusting its bank count.
@header:
	jsr stage_byte
	bcs @io_error
	inc @ptr
	lda @ptr
	cmp #FIRMWARE_OFFSET+FIRMWARE_SIZE
	bne @header

	jsr validate_header
	bcs @bad_image
	; Remember the next screen row so both counters update in place.
	sec
	jsr krn::plot
	stx loading_row
	jsr loading_progress

@data:	jsr stage_byte
	bcs @io_error
	inc @ptr
	bne @eof
	inc @ptr+1
@eof:	lda @ptr+1
	cmp #$60
	beq @bank_done
	lda flashio::eof
	bne @bad_image
	jmp @data

@bank_done:
	inc UPDATE_BANK
	jsr loading_progress
	lda UPDATE_BANK
	cmp flashio::blocks
	beq @loaded
	lda flashio::eof
	bne @bad_image
	lda #$40
	sta @ptr+1
	jmp @data

@loaded:
	; A trimmed image must end here.  Full 8 MiB emulator images aren't
	; accepted on disk, avoiding unchecked trailing data or wrong lengths.
	lda flashio::eof
	beq @bad_image
	lda checksum
	cmp expected_checksum
	bne @bad_image
	lda checksum+1
	cmp expected_checksum+1
	bne @bad_image
	jsr __updater_close
	lda #$00
	sta flashio::eof
	sta UPDATE_BANK
	clc
	rts

@bad_image:
	lda #<msg_invalid
	ldy #>msg_invalid
	bne @error
@io_error:
	lda #<msg_update_io
	ldy #>msg_update_io
@error:	jsr flashio::status
	sec
	rts
.endproc

;*******************************************************************************
; LOADING PROGRESS
; Displays the completed and remaining bank counts. Preserves the input
; channel, bank mapping, checksum, and staging pointer.
.proc loading_progress
	clc
	ldx loading_row
	ldy #$00
	jsr krn::plot
	lda #<msg_loaded
	ldy #>msg_loaded
	jsr flashio::message

	lda #$00
	sta flashio::decimal+1
	lda UPDATE_BANK
	sta flashio::decimal
	jsr flashio::print_dec4
	lda #'/'
	jsr krn::chrout
	lda #$00
	sta flashio::decimal+1
	lda flashio::blocks
	sta flashio::decimal
	jsr flashio::print_dec4

	lda #<msg_remaining
	ldy #>msg_remaining
	jsr flashio::message

	lda #$00
	sta flashio::decimal+1
	sec
	lda flashio::blocks
	sbc ULTIMEM_BLK2
	sta flashio::decimal
	jsr flashio::print_dec4
	lda #<msg_loading_blocks
	ldy #>msg_loading_blocks
	jmp flashio::message
.endproc

;*******************************************************************************
; DIRECTORY BYTE
; Reads one directory byte.
; OUT:
;  - .A: byte read on success
;  - .C: set on error
.proc directory_byte
	lda flashio::eof
	bne @error
	jsr flashio::read
	bcs @error
	lda flashio::byte
	rts

@error:	pla
	pla
	lda #<msg_update_io
	ldy #>msg_update_io
	jsr flashio::message
	sec
	rts
.endproc

;*******************************************************************************
; CONSIDER NAME
; Accepts the full .BIN name, which fits native CBM disks.
; Compare five decimal digits lexically; preserve the exact name for OPEN.
.proc consider_name
	lda name_length
	cmp #16
	bne @done
	tax
	dex
@syntax:
	lda candidate,x
	cpx #7
	bcc @literal
	cpx #12
	bcs @literal
	cmp #'0'
	bcc @done
	cmp #'9'+1
	bcs @done
	bcc @next
@literal:
	and #$7f
	cmp #$61
	bcc :+
	cmp #$7b
	bcs :+
	and #$df
:	cmp filename_template,x
	bne @done
@next:	dex
	bpl @syntax

	ldx #$07
@compare:
	lda candidate,x
	cmp flashio::filename,x
	bcc @done
	bne @newer
	inx
	cpx #12
	bne @compare
	rts

@newer:	lda name_length
	sta __updater_name_length
	ldx #$00
@copy:	lda candidate,x
	sta flashio::filename,x
	inx
	cpx __updater_name_length
	bne @copy
	lda #$01
	sta found_update
@done:	rts
.endproc

;*******************************************************************************
; STAGE BYTE
; Reads one byte into staging RAM and includes it in the checksum.
; IN:
;  - rb: staging address
; OUT:
;  - .C: set if the read fails or the previous byte was the last
.proc stage_byte
@ptr=rb
	; Never attempt to read beyond EOI, including during a short header.
	lda flashio::eof
	bne @error
	jsr flashio::read
	bcs @error

	ldy #$00
	lda flashio::byte
	sta (@ptr),y

	; exclude the checksum's own two bytes from the sum.
	ldx UPDATE_BANK
	bne @sum
	ldx @ptr+1
	cpx #$40
	bne @sum
	ldx @ptr
	cpx #FIRMWARE_CHECKSUM
	beq @ok
	cpx #FIRMWARE_CHECKSUM+1
	beq @ok
@sum:	clc
	adc checksum
	sta checksum
	bcc @ok
	inc checksum+1
@ok:	clc
	rts

@error:	sec
	rts
.endproc

;*******************************************************************************
; VALIDATE HEADER
; Checks the signature, version, and bank count, then reads the checksum.
; OUT:
;  - .C: set if the header is invalid
.proc validate_header
	ldx #$04
@signature:
	lda UPDATE_HEADER+4,x
	cmp cart_signature,x
	bne @bad
	dex
	bpl @signature

	ldx #$03
@magic:	lda UPDATE_HEADER+FIRMWARE_OFFSET,x
	cmp update_magic,x
	bne @bad
	dex
	bpl @magic

	ldx #$04
@version:
	lda UPDATE_HEADER+FIRMWARE_VERSION,x
	cmp flashio::filename+7,x
	bne @bad
	dex
	bpl @version

	lda UPDATE_HEADER+FIRMWARE_BLOCKS+1
	bne @bad
	lda UPDATE_HEADER+FIRMWARE_BLOCKS
	beq @bad
	cmp #129
	bcs @bad
	sta flashio::blocks
	sec
	sbc #$01
	sta flashio::last

	lda UPDATE_HEADER+FIRMWARE_CHECKSUM
	sta expected_checksum
	lda UPDATE_HEADER+FIRMWARE_CHECKSUM+1
	sta expected_checksum+1
	clc
	rts

@bad:	sec
	rts
.endproc

;*******************************************************************************
; STAGED BYTE
; Read the matching offset in staged RAM through BLK2.  Keep the header magic
; erased until every other byte has been programmed and verified.
.export __updater_staged_byte
.proc __updater_staged_byte
@ptr=rb
	lda flashio::block
	sta ULTIMEM_BLK2
	lda @ptr
	sta @load+1
	lda @ptr+1
	sec
	sbc #$20
	sta @load+2
@load:	lda $ffff
	sta flashio::byte

	lda flashio::block
	bne @done
	lda @ptr+1
	cmp #$60
	bne @done
	lda @ptr
	cmp #FIRMWARE_OFFSET
	bcc @done
	cmp #FIRMWARE_OFFSET+4
	bcs @done
	lda #$ff
	sta flashio::byte
@done:	rts
.endproc

;*******************************************************************************
; COMMIT UPDATE
; Programs the version marker after the rest of the image has been verified.
; OUT:
;  - .C: set on a programming error
.export __updater_commit
.proc __updater_commit
@ptr=rb
	lda #$00
	sta ULTIMEM_BLK3
	lda #$60
	sta @ptr+1
	lda #FIRMWARE_OFFSET
	sta @ptr

@byte:	ldx @ptr
	lda update_magic-FIRMWARE_OFFSET,x
	sta flashio::byte
	jsr flashio::program_byte
	bcs @done
	inc @ptr
	lda @ptr
	cmp #FIRMWARE_OFFSET+4
	bne @byte
	clc
@done:	rts
.endproc

;*******************************************************************************
; PRINT VERSION
.proc print_version
	jsr print_version_digits
	lda #$0d
	jmp krn::chrout
.endproc

;*******************************************************************************
; PRINT VERSION DIGITS
.proc print_version_digits
	ldx #$07
@digit:	lda flashio::filename,x
	jsr krn::chrout
	inx
	cpx #12
	bne @digit
	rts
.endproc

;*******************************************************************************
; CONFIRM UPDATE
; Ask only after the image is fully validated, before the first flash erase.
; Ignore buffered keys and enable the KERNAL IRQ to scan the keyboard.
.export __updater_confirm
.proc __updater_confirm
	lda #2
	jsr flashio::screen
	lda #<msg_confirm
	ldy #>msg_confirm
	jsr flashio::message

	jsr print_version_digits
	lda #<msg_yes_no
	ldy #>msg_yes_no
	jsr flashio::message

	lda #$00
	sta $c6				; KERNAL keyboard buffer count
	cli

@key:	jsr krn::getin
	cmp #$0d
	beq @cancel
	cmp #$03			; RUN/STOP
	beq @cancel
	and #$7f
	and #$df
	cmp #'N'
	beq @cancel
	cmp #'Y'
	bne @key

	sei
	jsr krn::chrout
	lda #$0d
	jsr krn::chrout
	clc
	rts

@cancel:
	sei
	lda #'N'
	jsr krn::chrout
	lda #$0d
	jsr krn::chrout
	lda #<msg_cancelled
	ldy #>msg_cancelled
	jsr flashio::status
	sec
	rts
.endproc

;*******************************************************************************
; CLOSE UPDATE FILE
; Closes the selected logical file and resets its read state.
.export __updater_close
.proc __updater_close
	jsr krn::clrchn
	lda #2
	jsr krn::close
	lda #$00
	sta flashio::file_open
	sta flashio::eof
	rts
.endproc

.segment FLASH_RODATA

;*******************************************************************************
; STRINGS
filename_template:	.byte "MONSTER00000.BIN"
cart_signature:		.byte "A0", $c3, $c2, $cd
update_magic:		.byte "MUP1"
directory_name:		.byte "$"
msg_installed:		.byte "   INSTALLED ", 0
msg_available:		.byte "   UPDATE    ", 0
msg_searching:		.byte " CHECKING FOR UPDATE", $0d, 0
msg_current:		.byte "   NO NEWER VERSION", $0d, 0
msg_loading:		.byte "  CHECKING IMAGE...", $0d, 0
msg_loaded:		.byte "   LOADED ", 0
msg_remaining:		.byte $0d, "  LEFT   ", 0
msg_loading_blocks:	.byte " BLOCKS", $0d, 0
msg_confirm:		.byte " FLASH ", 0
msg_yes_no:		.byte "? (Y/N) ", 0
msg_cancelled:		.byte "   UPDATE CANCELLED", $0d, 0
msg_invalid:		.byte " INVALID UPDATE IMAGE", $0d, 0
msg_update_io:		.byte "  UPDATE DISK ERROR", $0d, 0

.segment FLASH_BSS

;*******************************************************************************
; VARIABLES
.export __updater_saved_bank2
__updater_saved_bank2:	.word 0
loading_row:		.byte 0
found_update:		.byte 0
.export __updater_name_length
__updater_name_length:	.byte 0

;*******************************************************************************
link_low:		.byte 0
quoted:			.byte 0
skip_entry:		.byte 0
name_length:		.byte 0
candidate:		.res 17
checksum:		.word 0
expected_checksum:	.word 0
