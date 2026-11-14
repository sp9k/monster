;*******************************************************************************
; UPDATER.ASM
; This file loads and validates an image in UltiMem RAM for the flasher.
; Monster's built-in flasher reopens the file that flashcheck.asm selected and
; the user confirmed.
;*******************************************************************************

.include "../firmware.inc"
.include "../flashio.inc"
.include "../macros.inc"

;*******************************************************************************
; CONFIGURATION
; Stage the entire image before allowing the writer to erase the flash chip.
.define FLASH_CODE "FLASH_CODE"
.define FLASH_RODATA "FLASH_RODATA"
.define FLASH_BSS "FLASH_BSS"
UPDATE_HEADER = $4000
UPDATE_BANK   = ULTIMEM_BLK2
.import __kernal_clrchn, __kernal_close
.scope krn
	setnam = $ffbd
	clrchn = __kernal_clrchn
	close  = __kernal_close
	chrout = $ffd2
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
	lda #5
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
	ldx #$07
@digit:	lda flashio::filename,x
	jsr krn::chrout
	inx
	cpx #12
	bne @digit
	lda #$0d
	jmp krn::chrout
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
cart_signature:		.byte "A0", $c3, $c2, $cd
update_magic:		.byte "MUP1"
msg_available:		.byte "   UPDATE    ", 0
msg_loading:		.byte "  CHECKING IMAGE...", $0d, 0
msg_loaded:		.byte "   LOADED ", 0
msg_remaining:		.byte $0d, "  LEFT   ", 0
msg_loading_blocks:	.byte " BLOCKS", $0d, 0
msg_invalid:		.byte " INVALID UPDATE IMAGE", $0d, 0
msg_update_io:		.byte "  UPDATE DISK ERROR", $0d, 0

.segment FLASH_BSS

;*******************************************************************************
; VARIABLES
.export __updater_saved_bank2
__updater_saved_bank2:	.word 0
loading_row:		.byte 0
; Monster writes the confirmed name length before copying the writer to RAM.
.export __updater_name_length
__updater_name_length:	.byte 0

;*******************************************************************************
checksum:		.word 0
expected_checksum:	.word 0
