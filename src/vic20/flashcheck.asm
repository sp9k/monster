;*******************************************************************************
; FLASHCHECK.ASM
; This file checks the update disk before Monster asks for confirmation.
; It retains only the header so checking or cancelling preserves editor state.
;*******************************************************************************

.include "../firmware.inc"
.include "../kernal.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"
.include "ultimem/banks.inc"

.import __FLASH_CHECK_BSS_RUN__
.import __FLASH_CHECK_BSS_SIZE__

;*******************************************************************************
; CONFIGURATION
; Read the installed header through BLK2; use a private RAM workspace.
INSTALLED_ROM_BASE = $4000

; Update bank: RAM 95 at BLK1, boot ROM at BLK2, writer at BLK5.
; Never stage into the editor's RAM before confirmation.
.assert __FLASH_CHECK_BSS_SIZE__ <= 128, lderror, "update check initialization loop too small"
.assert SYMBOL_END_BANK <= FLASH_CHECK_RAM_BANK, error, "update check overlaps symbol RAM"

;*******************************************************************************
; WORKSPACE
.segment "FLASH_CHECK_BSS"
header:			.res 22
.export __flashcheck_filename
__flashcheck_filename:	.res 16
.export __flashcheck_device
__flashcheck_device:	.byte 0
check_file:		.byte 0
checked_blocks:		.byte 0
file_open:		.byte 0
eof_seen:		.byte 0
write_byte:		.byte 0
image_blocks:		.word 0
image_last:		.word 0

.segment "FLASH_LAUNCH"

;*******************************************************************************
; INIT
; Clears the workspace and reserves a free logical file number.
; Existing KERNAL files remain open.
.export __flashcheck_init
.proc __flashcheck_init
	lda #$00
	ldx #<(__FLASH_CHECK_BSS_SIZE__-1)
@clear:	sta __FLASH_CHECK_BSS_RUN__,x
	dex
	bpl @clear

	lda zp::device
	sta __flashcheck_device

	; Reserve a free logical file number without disturbing existing files.
	lda #127
	sta check_file
@next:	ldx zp::numfiles
@file:	dex
	bmi @done
	lda $0259,x
	cmp check_file
	bne @file
	dec check_file
	bne @next
@done:	rts
.endproc

;*******************************************************************************
; OPEN IMAGE
; Opens the selected filename and selects it as the input channel.
; OUT:
;  - .C: set if OPEN or CHKIN fails
.proc open_image
	lda __flashcheck_name_length
	ldxy #__flashcheck_filename
	jsr krn::setnam

	; fall through to open_named
.endproc

;*******************************************************************************
; OPEN NAMED
; Open the name already passed to SETNAM. OPEN_IMAGE falls through here.
.proc open_named
	lda #$00
	sta eof_seen

	lda check_file
	ldx __flashcheck_device
	ldy #$00
	jsr krn::setlfs

	; Even a failed OPEN may need CLOSE; this handle was unused on entry.
	lda #1
	sta file_open
	jsr krn::open
	bcs @error
	ldx check_file
	jsr krn::chkin
@error:	rts
.endproc

;*******************************************************************************
; READ IMAGE BYTE
; Reads one byte, treating EOI as a valid final byte.
; OUT:
;  - write_byte: byte read
;  - eof_seen: nonzero on the final byte
;  - .C: set on a disk error
.proc read_image_byte
	jsr krn::chrin
	sta write_byte
	jsr krn::readst
	beq @not_eof
	cmp #$40
	bne @error
	lda #1
	sta eof_seen
	clc
	rts

@not_eof:
	sta eof_seen
	clc
	rts

@error:	sec
	rts
.endproc

;*******************************************************************************
; PRINTZ
; Copies the status/error message to the shared linebuffer for the alert.
; IN:
;  - .AY: address of the zero-terminated message
; OUT:
;  - mem::linebuffer: message, without its trailing RETURN
.export __flashcheck_printz
.proc __flashcheck_printz
@src=r0
	sta @src
	sty @src+1
	ldy #$00
@copy:	lda (@src),y
	beq @end
	cmp #$0d
	beq @end
	sta mem::linebuffer,y
	iny
	cpy #28
	bcc @copy

@end:	lda #$00
	sta mem::linebuffer,y
	rts
.endproc

; Uppercase update messages use ordinary PETSCII.
.repeat 26, I
	.charmap $41+I, $41+I
.endrepeat

.segment "FLASH_LAUNCH"

;*******************************************************************************
; PREPARE UPDATE
; Selects the newest update and checks its entire contents without staging it.
; OUT:
;  - .C: set if no usable update was found
.export __flashcheck_prepare_update
.proc __flashcheck_prepare_update
@ptr=rb
	ldx #15
@template:
	lda filename_template,x
	sta __flashcheck_filename,x
	dex
	bpl @template

	ldx #4
@zero:	lda #'0'
	sta __flashcheck_filename+7,x
	dex
	bpl @zero

	ldx #3
@magic:	lda INSTALLED_ROM_BASE+FIRMWARE_OFFSET,x
	cmp update_magic,x
	bne @directory
	dex
	bpl @magic

	ldx #4
@version:
	lda INSTALLED_ROM_BASE+FIRMWARE_VERSION,x
	cmp #'0'
	bcc @directory
	cmp #'9'+1
	bcs @directory
	dex
	bpl @version

	ldx #4
@copy_version:
	lda INSTALLED_ROM_BASE+FIRMWARE_VERSION,x
	sta __flashcheck_filename+7,x
	dex
	bpl @copy_version

@directory:
	lda #<msg_installed
	ldy #>msg_installed
	jsr __flashcheck_printz

	lda #$00
	sta found_update
	lda #<msg_searching
	ldy #>msg_searching
	jsr __flashcheck_printz

	lda #1
	ldxy #directory_name
	jsr krn::setnam
	jsr open_named
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
	jsr __flashcheck_close_update_file
	lda found_update
	bne @image
	lda #<msg_current
	ldy #>msg_current
	jsr __flashcheck_printz
	sec
	rts

@image:	lda #<msg_available
	ldy #>msg_available
	jsr __flashcheck_printz
	lda #<msg_loading
	ldy #>msg_loading
	jsr __flashcheck_printz
	jsr open_image
	bcc :+
	jmp @io_error
:
	; Count the image banks without changing the cartridge mappings.
	lda #$00
	sta checked_blocks
	sta checksum
	sta checksum+1
	sta @ptr
	lda #$40
	sta @ptr+1

	; Read and validate the header before trusting its bank count.
@header:
	jsr check_byte
	bcs @io_error
	inc @ptr
	lda @ptr
	cmp #FIRMWARE_OFFSET+FIRMWARE_SIZE
	bne @header

	jsr validate_header
	bcs @bad_image

@data:	jsr check_byte
	bcs @io_error
	inc @ptr
	bne @eof
	inc @ptr+1
@eof:	lda @ptr+1
	cmp #$60
	beq @bank_done
	lda eof_seen
	bne @bad_image
	jmp @data

@bank_done:
	inc checked_blocks
	lda checked_blocks
	cmp image_blocks
	beq @loaded
	lda eof_seen
	bne @bad_image
	lda #$40
	sta @ptr+1
	jmp @data

@loaded:
	; A trimmed image must end here.  Full 8 MiB emulator images aren't
	; accepted on disk, avoiding unchecked trailing data or wrong lengths.
	lda eof_seen
	beq @bad_image
	lda checksum
	cmp expected_checksum
	bne @bad_image
	lda checksum+1
	cmp expected_checksum+1
	bne @bad_image
	jsr __flashcheck_close_update_file
	lda #$00
	sta eof_seen
	sta checked_blocks
	clc
	rts

@bad_image:
	lda #<msg_invalid
	ldy #>msg_invalid
	bne @error
@io_error:
	lda #<msg_update_io
	ldy #>msg_update_io
@error:	jsr __flashcheck_printz
	sec
	rts
.endproc

;*******************************************************************************
; DIRECTORY BYTE
; Reads one directory byte.
; OUT:
;  - .A: byte read on success
;  - .C: set on error
.proc directory_byte
	lda eof_seen
	bne @error
	jsr read_image_byte
	bcs @error
	lda write_byte
	rts

@error:	pla
	pla
	lda #<msg_update_io
	ldy #>msg_update_io
	jsr __flashcheck_printz
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
	cmp __flashcheck_filename,x
	bcc @done
	bne @newer
	inx
	cpx #12
	bne @compare
	rts

@newer:	lda name_length
	sta __flashcheck_name_length
	ldx #$00
@copy:	lda candidate,x
	sta __flashcheck_filename,x
	inx
	cpx __flashcheck_name_length
	bne @copy
	lda #$01
	sta found_update
@done:	rts
.endproc

;*******************************************************************************
; CHECK BYTE
; Reads and checksums one byte, retaining only the image header.
; IN:
;  - rb: position within the current image bank
; OUT:
;  - .C: set if the read fails or the previous byte was the last
.proc check_byte
@ptr=rb
	; Never attempt to read beyond EOI, including during a short header.
	lda eof_seen
	bne @error
	jsr read_image_byte
	bcs @error

	; Read every byte but retain only the header; Monster RAM stays intact.
	ldx checked_blocks
	bne @checksum
	ldx @ptr+1
	cpx #$40
	bne @checksum
	ldx @ptr
	cpx #FIRMWARE_OFFSET+FIRMWARE_SIZE
	bcs @checksum
	lda write_byte
	sta header,x
@checksum:
	lda write_byte

	; exclude the checksum's own two bytes from the sum.
	ldx checked_blocks
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
	lda header+4,x
	cmp cart_signature,x
	bne @bad
	dex
	bpl @signature

	ldx #$03
@magic:	lda header+FIRMWARE_OFFSET,x
	cmp update_magic,x
	bne @bad
	dex
	bpl @magic

	ldx #$04
@version:
	lda header+FIRMWARE_VERSION,x
	cmp __flashcheck_filename+7,x
	bne @bad
	dex
	bpl @version

	lda header+FIRMWARE_BLOCKS+1
	bne @bad
	lda header+FIRMWARE_BLOCKS
	beq @bad
	cmp #129
	bcs @bad
	sta image_blocks
	sec
	sbc #$01
	sta image_last

	lda header+FIRMWARE_CHECKSUM
	sta expected_checksum
	lda header+FIRMWARE_CHECKSUM+1
	sta expected_checksum+1
	clc
	rts

@bad:	sec
	rts
.endproc

;*******************************************************************************
; CLOSE UPDATE FILE
; Closes the selected logical file and resets its read state.
.export __flashcheck_close_update_file
.proc __flashcheck_close_update_file
	jsr krn::clrchn
	lda check_file
	jsr krn::close
	lda #$00
	sta file_open
	sta eof_seen
	rts
.endproc

.segment "FLASH_LAUNCH"

;*******************************************************************************
; STRINGS
filename_template:	.byte "MONSTER00000.BIN"
cart_signature:		.byte "A0", $c3, $c2, $cd
update_magic:		.byte "MUP1"
directory_name:		.byte "$"
msg_installed:		.byte "INSTALLED ", 0
msg_available:		.byte "UPDATE    ", 0
msg_searching:		.byte "CHECKING FOR UPDATE", $0d, 0
msg_current:		.byte "NO NEWER VERSION", $0d, 0
msg_loading:		.byte "CHECKING IMAGE...", $0d, 0
msg_loaded:		.byte "LOADED ", 0
msg_remaining:		.byte $0d, "LEFT   ", 0
msg_loading_blocks:	.byte " BLOCKS", $0d, 0
msg_confirm:		.byte "FLASH ", 0
msg_yes_no:		.byte "? (Y/N) ", 0
msg_cancelled:		.byte "UPDATE CANCELLED", $0d, 0
msg_invalid:		.byte "INVALID UPDATE IMAGE", $0d, 0
msg_update_io:		.byte "UPDATE DISK ERROR", $0d, 0

.segment "FLASH_CHECK_BSS"

;*******************************************************************************
; VARIABLES
found_update:		.byte 0
.export __flashcheck_name_length
__flashcheck_name_length:
	.byte 0

;*******************************************************************************
link_low:		.byte 0
quoted:			.byte 0
skip_entry:		.byte 0
name_length:		.byte 0
candidate:		.res 17
checksum:		.word 0
expected_checksum:	.word 0
