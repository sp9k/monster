;*******************************************************************************
; FLASHER.ASM
; This file contains the VIC-20 UltiMem flash writer.
; The standalone flasher streams MONSTER.BIN. The updater validates and
; stages a versioned image before using the same writer.
; Code runs in built-in RAM while BLK3 maps the destination flash bank.
;
; The flash command sequence follows the UltiMem reference flasher:
; https://github.com/ops/ultimem/blob/master/flash.s
;*******************************************************************************

.include "macros.inc"
.include "firmware.inc"

.ifdef UPDATER
.include "updater.inc"
.endif

.ifndef DEFAULT_DEVICE
DEFAULT_DEVICE = 10
.endif

; The cartridge links the same writer and updater at their RAM run addresses.
.ifndef STANDALONE_FLASHER
.define FLASH_CODE "FLASH_CODE"
.define FLASH_RODATA "FLASH_RODATA"
.define FLASH_BSS "FLASH_BSS"
.else
.define FLASH_CODE "CODE"
.define FLASH_RODATA "RODATA"
.define FLASH_BSS "BSS"
.endif

.ifndef IMAGE_BLOCKS
IMAGE_BLOCKS = 28
.endif

.assert IMAGE_BLOCKS > 0, error, "MONSTER image must contain at least one bank"
.assert IMAGE_BLOCKS <= 1024, error, "MONSTER image exceeds 8 MiB UltiMem"

; cl65's VIC-20 target maps source-code uppercase letters to the high PETSCII
; range.  CHROUT expects the ordinary $41-$5a codes for uppercase display.
.repeat 26, I
	.charmap $41+I, $41+I
.endrepeat

;*******************************************************************************
; KERNAL
; Direct ROM calls remain available while the cartridge is being erased.
.scope krn
	setlfs = $ffba
	setnam = $ffbd
	open   = $ffc0
	close  = $ffc3
	chkin  = $ffc6
	clrchn = $ffcc
	chrin  = $ffcf
	chrout = $ffd2
	readst = $ffb7
	getin  = $ffe4
	plot   = $fff0
.endscope

;*******************************************************************************
; ULTIMEM
ULTIMEM_CFG   = $9ff0
ULTIMEM_IORAM = $9ff1
ULTIMEM_BLKS  = $9ff2
ULTIMEM_ID    = $9ff3
ULTIMEM_BLK2  = $9ffa
ULTIMEM_BLK3  = $9ffc
ULTIMEM_BLK5  = $9ffe

ULTIMEM_RESET = $40
ULTIMEM_ID_8M = $11
RAM_BANKS     = 128			; 1 MiB RAM, in 8 KiB banks
RAM_TEST_BYTE = $6000			; first byte of the RAM bank mapped into BLK3
FLASH_BASE    = $6000
FLASH_END_HI  = $80
FLASH_TOGGLE  = $40
FLASH_TIMEOUT = $20

; One progress block is one UltiMem flash bank (8192 bytes).  The Makefile
; derives IMAGE_BLOCKS from the size of the bank-laid monster-cart.prg.
IMAGE_BLOCKS_LO = <IMAGE_BLOCKS
IMAGE_BLOCKS_HI = >IMAGE_BLOCKS
IMAGE_LAST_LO   = <(IMAGE_BLOCKS-1)
IMAGE_LAST_HI   = >(IMAGE_BLOCKS-1)

.ifdef STANDALONE_FLASHER
.segment "EXEHDR"
	.word @basic_end
	.word 10
	.byte $9e
	.byte "4109", 0			; SYS 4109 ($100d)
@basic_end:
	.word 0

.segment "CODE"
.assert * = $100d, error, "update the BASIC SYS address"
.endif
.segment FLASH_CODE

;*******************************************************************************
; START
; Checks the hardware and input, then erases, programs, and verifies the image.
; Restarts into the new ROM on success; returns with saved mappings on failure.
.export __flasher_start
.proc __flasher_start
@ptr=rb
	cld
	lda #<IMAGE_BLOCKS
	sta __flasher_image_blocks
	lda #>IMAGE_BLOCKS
	sta __flasher_image_blocks+1
	lda #<IMAGE_LAST_LO
	sta __flasher_image_last
	lda #IMAGE_LAST_HI
	sta __flasher_image_last+1

	lda #$00
	sta registers_saved
	sta flash_mapped
	sta __flasher_file_open

	lda #2
	jsr __flasher_screen

	; re-enable registers that may have been hidden by an earlier program.
	sei
	lda $9f55
	lda $9faa
	lda $9f01

.ifdef UPDATER
	lda ULTIMEM_BLK2
	sta updater::saved_bank2
	lda ULTIMEM_BLK2+1
	sta updater::saved_bank2+1
.endif
	lda ULTIMEM_CFG
	sta saved_cfg
	lda ULTIMEM_BLKS
	sta saved_blks
	lda ULTIMEM_BLK3
	sta saved_bank
	lda ULTIMEM_BLK3+1
	sta saved_bank+1
	lda #1
	sta registers_saved

	lda ULTIMEM_ID
	cmp #ULTIMEM_ID_8M
	beq :+
	jmp fail_no_ultimem
:	lda #<msg_testing_ram
	ldy #>msg_testing_ram
	jsr __flasher_printz
	jsr test_ram
	bcc :+
	jmp fail_ram
:
	; preserve every mapping except BLK3, which becomes flash ROM.
	lda ULTIMEM_BLKS
	and #$cf
	ora #$10
	sta ULTIMEM_BLKS
	lda #$00
	sta ULTIMEM_BLK3
	sta ULTIMEM_BLK3+1
	lda #1
	sta flash_mapped

	; select the S29GL064N autoselect mode and validate both ID bytes.
	ldy #$f0
	sty FLASH_BASE
	ldx #$90
	jsr flash_command
	lda FLASH_BASE
	cmp #$01
	beq :+
	jmp fail_manufacturer
:	lda FLASH_BASE+2
	cmp #$7e
	beq :+
	jmp fail_device
:	lda #$f0
	sta FLASH_BASE

.ifdef UPDATER
	jsr updater::prepare
	bcc :+
	jmp cleanup
:
.ifdef STANDALONE_FLASHER
	jsr updater::confirm
	bcc :+
	jmp cleanup
:
.endif
.else
	jsr __flasher_open_image
	bcc :+
	jmp fail_open
:
	; Keep the header for the version display, then replay it to the writer.
	lda #$00
	sta header_position
@header:
	jsr __flasher_read_image_byte
	bcc :+
	jmp fail_read
:	lda __flasher_eof_seen
	beq :+
	jmp fail_short
:	ldx header_position
	lda __flasher_write_byte
	sta image_header,x
	inc header_position
	lda header_position
	cmp #FIRMWARE_VERSION+5
	bne @header
	lda #$00
	sta header_position
.endif
	lda #<msg_erasing
	ldy #>msg_erasing
	jsr __flasher_status
	jsr led_on
	lda #$00
	sta __flasher_block_no
	sta __flasher_block_no+1

@erase_chip:
	lda #$00
	sta ULTIMEM_BLK3
	sta ULTIMEM_BLK3+1

	; AMD/S29GL chip erase: unlock, erase setup, unlock, chip erase.
	; This does not depend on the chip's sector layout.
	ldx #$80
	jsr flash_command
	ldx #$10
	jsr flash_command
	jsr poll_flash
	bcc :+
	jmp fail_erase_timeout
:	lda #$f0
	sta FLASH_BASE

@verify_erase:
	; verify every byte of the destination image before programming any of it.
	lda __flasher_block_no
	sta ULTIMEM_BLK3
	lda __flasher_block_no+1
	sta ULTIMEM_BLK3+1
	jsr verify_erased_bank
	bcc @erased
	jmp fail_erase
@erased:
	incw __flasher_block_no
	lda __flasher_block_no
	cmp __flasher_image_blocks
	bne @verify_erase
	lda __flasher_block_no+1
	cmp __flasher_image_blocks+1
	bne @verify_erase

	lda #$00
	sta __flasher_block_no
	sta __flasher_block_no+1
	sta blocks_done
	sta blocks_done+1
	jsr show_progress_screen

@next_block:
	lda __flasher_block_no
	sta ULTIMEM_BLK3
	lda __flasher_block_no+1
	sta ULTIMEM_BLK3+1
	lda #<FLASH_BASE
	sta @ptr
	lda #>FLASH_BASE
	sta @ptr+1

@next_byte:
.ifdef UPDATER
	jsr updater::staged_byte
.else
	jsr stream_byte
	bcc :+
	jmp fail_read
:
.endif
	jsr __flasher_program_byte
	bcc :+
	jmp fail_program
:	inc @ptr
	bne @check_eof
	inc @ptr+1

@check_eof:
	lda __flasher_eof_seen
	beq @not_eof

	; A trimmed input file may signal EOI on the final populated byte.
	lda @ptr+1
	cmp #FLASH_END_HI
	beq :+
	jmp fail_short
:	lda __flasher_block_no
	cmp __flasher_image_last
	beq :+
	jmp fail_short
:	lda __flasher_block_no+1
	cmp __flasher_image_last+1
	beq :+
	jmp fail_short
:	jmp @block_complete

@not_eof:
	lda @ptr+1
	cmp #FLASH_END_HI
	bne @next_byte

@block_complete:
	incw blocks_done
	jsr update_progress
	lda blocks_done
	cmp __flasher_image_blocks
	bne @more
	lda blocks_done+1
	cmp __flasher_image_blocks+1
	beq @all_blocks

@more:	lda __flasher_eof_seen
	beq :+
	jmp fail_short
:	inc __flasher_block_no
	bne @next_block
	inc __flasher_block_no+1
	jmp @next_block

@all_blocks:
.ifdef UPDATER
	jsr updater::commit
	bcc :+
	jmp fail_program
:
.endif
	jsr cleanup
	lda #4
	jsr __flasher_screen
	lda #$1e			; green text
	jsr krn::chrout
	lda #<msg_success
	ldy #>msg_success
	jsr __flasher_printz

	lda #' '
	jsr krn::chrout
	lda __flasher_image_blocks
	sta __flasher_decimal_value
	lda __flasher_image_blocks+1
	sta __flasher_decimal_value+1
	jsr __flasher_print_dec4
	lda #<msg_written
	ldy #>msg_written
	jsr __flasher_printz
	jmp restart
.endproc

;*******************************************************************************
; RESTART
; Software reset preserves UltiMem mappings. Expose the new cartridge header
; in BLK5 before pulsing RESET, with all other expansion memory detached.
; Does not return.
.proc restart
	sei
	lda #$00
	sta ULTIMEM_IORAM
	sta ULTIMEM_BLK5
	sta ULTIMEM_BLK5+1
	lda #$40			; BLK5 = flash bank 0, BLK1..3 disabled
	sta ULTIMEM_BLKS
	lda #ULTIMEM_RESET		; LED off, registers visible
	sta ULTIMEM_CFG
@wait:	jmp @wait
.endproc

;*******************************************************************************
; TEST RAM
; Test the same byte in every RAM bank, preserving its original contents.
; Write all banks before reading them back so aliased banks are detected too.
; Complementary bank-specific patterns exercise both states of every data bit.
; OUT:
;  - .C: set if a bank fails the check
;  - ram_bank: first failing bank
;  - __flasher_write_byte, read_byte: expected and actual values
.proc test_ram
	lda ULTIMEM_BLKS
	and #$cf
	ora #$30			; BLK3 = RAM read/write
	sta ULTIMEM_BLKS
	lda #$00
	sta ULTIMEM_BLK3+1

	ldx #$00
@save:	stx ULTIMEM_BLK3
	lda RAM_TEST_BYTE
	sta saved_ram,x
	inx
	cpx #RAM_BANKS
	bne @save

	lda #$55
	sta ram_pattern
@pass:	ldx #$00
@write:	stx ULTIMEM_BLK3
	txa
	eor ram_pattern
	sta RAM_TEST_BYTE
	inx
	cpx #RAM_BANKS
	bne @write

	ldx #$00
@verify:
	stx ULTIMEM_BLK3
	txa
	eor ram_pattern
	sta __flasher_write_byte
	lda RAM_TEST_BYTE
	cmp __flasher_write_byte
	bne @failed
	inx
	cpx #RAM_BANKS
	bne @verify

	lda ram_pattern
	eor #$ff
	sta ram_pattern
	cmp #$aa
	beq @pass
	clc
	bcc @restore

@failed:
	sta read_byte
	stx ram_bank
	sec

@restore:
	php
	ldx #$00
@restore_byte:
	stx ULTIMEM_BLK3
	lda saved_ram,x
	sta RAM_TEST_BYTE
	inx
	cpx #RAM_BANKS
	bne @restore_byte
	plp
	rts
.endproc

;*******************************************************************************
; OPEN IMAGE
; Open the selected image as a raw sequential input file.
.export __flasher_open_image
.proc __flasher_open_image
.ifdef UPDATER
	lda updater::namelen
.else
	lda #filename_end-__flasher_filename
.endif
	ldxy #__flasher_filename
	jsr krn::setnam
	; fall through to __flasher_open_named
.endproc

;*******************************************************************************
; OPEN NAMED
; Open the name already passed to SETNAM. OPEN_IMAGE falls through here.
.export __flasher_open_named
.proc __flasher_open_named
	lda #$00
	sta __flasher_eof_seen

	lda #2
.ifndef STANDALONE_FLASHER
	ldx __flash_device
.else
	ldx #DEFAULT_DEVICE
.endif
	ldy #$00
	jsr krn::setlfs
	jsr krn::open
	bcs @error

	lda #1
	sta __flasher_file_open
	ldx #2
	jsr krn::chkin
	bcs @error
	clc
	rts

@error:	sec
	rts
.endproc

;*******************************************************************************
.ifndef UPDATER
; STREAM BYTE
; Replays the header before continuing with the rest of the disk image.
.proc stream_byte
	ldx header_position
	cpx #FIRMWARE_VERSION+5
	bcs @read
	lda image_header,x
	sta __flasher_write_byte
	inc header_position
	clc
	rts
@read:	jmp __flasher_read_image_byte
.endproc
.endif

;*******************************************************************************
; READ IMAGE BYTE
; Read one file byte.  EOI ($40) accompanies a valid final byte.  Carry is
; returned set only for an actual KERNAL/device error.
.export __flasher_read_image_byte
.proc __flasher_read_image_byte
	jsr krn::chrin
	sta __flasher_write_byte
	jsr krn::readst
	beq @not_eof
	cmp #$40
	beq @eof
	sta io_status
	sec
	rts

@eof:	lda #1
	sta __flasher_eof_seen
	clc
	rts

@not_eof:
	sta __flasher_eof_seen
	clc
	rts
.endproc

;*******************************************************************************
; VERIFY ERASED BANK
; Checks every byte of the mapped 8 KiB bank.
; OUT:
;  - .C: set if any byte is not erased
;  - rb, read_byte: address and value of the first non-erased byte
.proc verify_erased_bank
@ptr=rb
	lda #$ff
	sta __flasher_write_byte
	lda #<FLASH_BASE
	sta @ptr
	lda #>FLASH_BASE
	sta @ptr+1
	ldy #$00
@byte:	lda (@ptr),y
	cmp #$ff
	bne @failed
	iny
	bne @byte
	inc @ptr+1
	lda @ptr+1
	cmp #FLASH_END_HI
	bne @byte
	clc
	rts

@failed:
	sta read_byte
	sty @ptr
	sec
	rts
.endproc

;*******************************************************************************
; PROGRAM BYTE
; Programs and verifies one byte, skipping bytes that already match.
; IN:
;  - rb: address in the mapped flash bank
;  - __flasher_write_byte: byte to program
; OUT:
;  - .C: set on timeout or verification failure
.export __flasher_program_byte
.proc __flasher_program_byte
@ptr=rb
	ldy #$00
	lda __flasher_write_byte
	cmp (@ptr),y
	beq @ok

	ldx #$a0
	jsr flash_command
	lda __flasher_write_byte
	; STA (zp),Y performs a dummy read from flash before the write, even
	; with Y=0.  UltiMem requires no such read after the program command.
	; Match the reference flasher's pre-indexed store instead.
	ldx #$00
	sta (@ptr,x)
	jsr poll_flash
	bcs @timeout

	lda #$f0
	sta FLASH_BASE
	lda (@ptr),y
	sta read_byte
	cmp __flasher_write_byte
	bne @verify
@ok:	clc
	rts

@timeout:
	lda #1
	sta program_error
	lda #$f0
	sta FLASH_BASE
	lda (@ptr),y
	sta read_byte
	sec
	rts

@verify:
	lda #2
	sta program_error
	sec
	rts
.endproc

;*******************************************************************************
; FLASH COMMAND
; Issue an S29GL064N command.  The 8 MiB UltiMem uses the $aaa/$555 unlock
; address pair within the currently selected 8 KiB aperture.
.proc flash_command
	lda #$aa
	sta FLASH_BASE+$aaa
	lda #$55
	sta FLASH_BASE+$555
	stx FLASH_BASE+$aaa
	rts
.endproc

;*******************************************************************************
; POLL FLASH
; Poll DQ6 until it stops toggling.  If DQ5 is asserted, sample DQ6 once more
; as required by the flash algorithm before reporting a timeout.
.proc poll_flash
@again:	lda FLASH_BASE
	sta poll_value
	lda FLASH_BASE
	sta poll_value+1
	eor poll_value
	and #FLASH_TOGGLE
	beq @ready

	lda poll_value+1
	and #FLASH_TIMEOUT
	beq @again

	lda FLASH_BASE
	sta poll_value
	lda FLASH_BASE
	eor poll_value
	and #FLASH_TOGGLE
	beq @ready
	sec
	rts
@ready:	clc
	rts
.endproc

;*******************************************************************************
; SCREEN
; Clears the screen and prints the heading above a centered group of lines.
; IN:
;  - .A: number of lines, including the heading (1..23)
.export __flasher_screen
.proc __flasher_screen
	eor #$ff
	clc
	adc #24
	lsr
	tax
	lda #$93
	jsr krn::chrout
	cpx #$00
	beq @title
@down:	lda #$11
	jsr krn::chrout
	dex
	bne @down
@title:
	lda #<msg_title
	ldy #>msg_title
	jmp __flasher_printz
.endproc

;*******************************************************************************
; STATUS
; Displays a status line beneath the heading.
; IN:
;  - .AY: address of the padded, 0-terminated message
.export __flasher_status
.proc __flasher_status
	pha
	tya
	pha
	lda #2
	jsr __flasher_screen
	pla
	tay
	pla
	jmp __flasher_printz
.endproc

;*******************************************************************************
; SHOW PROGRESS SCREEN
.proc show_progress_screen
	lda #3
	jsr __flasher_screen
	lda #<msg_version
	ldy #>msg_version
	jsr __flasher_printz
	ldx #$00
@version:
.ifdef UPDATER
	lda __flasher_filename+7,x
.else
	lda image_header+FIRMWARE_VERSION,x
.endif
	jsr krn::chrout
	inx
	cpx #5
	bne @version

	; Use at least two digits, widening both counts for larger images.
	ldx #2
	lda __flasher_image_blocks+1
	bne @hundreds
	lda __flasher_image_blocks
	cmp #100
	bcc @width
@hundreds:
	inx
	lda __flasher_image_blocks+1
	cmp #>1000
	bcc @width
	bne @thousands
	lda __flasher_image_blocks
	cmp #<1000
	bcc @width
@thousands:
	inx
@width:
	stx progress_digits
	; fall through
.endproc

;*******************************************************************************
; UPDATE PROGRESS
.proc update_progress
	lda #$13			; HOME
	jsr krn::chrout
	ldx #12			; third line of the centered group
@down:	lda #$11
	jsr krn::chrout
	dex
	bne @down

	; Center "BLOCK XX/NN" within the VIC-20's 22 columns.
	lda #7
	sec
	sbc progress_digits
	tax
@space:
	lda #' '
	jsr krn::chrout
	dex
	bne @space
	lda #<msg_block
	ldy #>msg_block
	jsr __flasher_printz

	lda blocks_done
	sta __flasher_decimal_value
	lda blocks_done+1
	sta __flasher_decimal_value+1
	jsr print_progress_count

	lda #<msg_slash
	ldy #>msg_slash
	jsr __flasher_printz

	lda __flasher_image_blocks
	sta __flasher_decimal_value
	lda __flasher_image_blocks+1
	sta __flasher_decimal_value+1
	jsr print_progress_count
	lda #$0d
	jmp krn::chrout
.endproc

;*******************************************************************************
; PRINT PROGRESS COUNT
.proc print_progress_count
	lda progress_digits
	sta decimal_width
	jmp print_decimal
.endproc

;*******************************************************************************
; PRINT DEC4
; Prints a value as four decimal digits, including leading zeroes.
; IN:
;  - __flasher_decimal_value: value to print (0000..1024)
; OUT:
;  - __flasher_decimal_value: remainder after printing
.export __flasher_print_dec4
.proc __flasher_print_dec4
	lda #4
	sta decimal_width
	; fall through
.endproc

;*******************************************************************************
; PRINT DECIMAL
; Prints the requested width, including leading zeroes.
.proc print_decimal
	lda #'0'
	sta decimal_digit
@thousands:
	lda __flasher_decimal_value+1
	cmp #>1000
	bcc @thousands_done
	bne @subtract_thousand
	lda __flasher_decimal_value
	cmp #<1000
	bcc @thousands_done
@subtract_thousand:
	sec
	lda __flasher_decimal_value
	sbc #<1000
	sta __flasher_decimal_value
	lda __flasher_decimal_value+1
	sbc #>1000
	sta __flasher_decimal_value+1
	inc decimal_digit
	jmp @thousands
@thousands_done:
	lda decimal_width
	cmp #4
	bcc :+
	lda decimal_digit
	jsr krn::chrout
:
	lda #'0'
	sta decimal_digit
@hundreds:
	lda __flasher_decimal_value+1
	bne @subtract_hundred
	lda __flasher_decimal_value
	cmp #100
	bcc @hundreds_done
@subtract_hundred:
	sec
	lda __flasher_decimal_value
	sbc #100
	sta __flasher_decimal_value
	lda __flasher_decimal_value+1
	sbc #$00
	sta __flasher_decimal_value+1
	inc decimal_digit
	jmp @hundreds
@hundreds_done:
	lda decimal_width
	cmp #3
	bcc :+
	lda decimal_digit
	jsr krn::chrout
:
	lda #'0'
	sta decimal_digit
@tens:	lda __flasher_decimal_value
	cmp #10
	bcc @tens_done
	sec
	sbc #10
	sta __flasher_decimal_value
	inc decimal_digit
	jmp @tens
@tens_done:
	lda decimal_digit
	jsr krn::chrout
	lda __flasher_decimal_value
	clc
	adc #'0'
	jmp krn::chrout
.endproc

;*******************************************************************************
; PRINTZ
; Prints a 0-terminated string. Self-modification preserves KERNAL zero page.
; IN:
;  - .AY: address of the string
.export __flasher_printz
.proc __flasher_printz
	sta @load+1
	sty @load+2
@load:	lda $ffff
	beq @done
	jsr krn::chrout
	inc @load+1
	bne @load
	inc @load+2
	bne @load
@done:	rts
.endproc

;*******************************************************************************
; PRINT HEX16
.proc print_hex16
	lda hex_value+1
	jsr print_hex8
	lda hex_value
	; fall through
.endproc

;*******************************************************************************
; PRINT HEX8
.proc print_hex8
	sta hex_byte
	lsr
	lsr
	lsr
	lsr
	jsr print_nibble
	lda hex_byte
	; fall through
.endproc

;*******************************************************************************
; PRINT NIBBLE
.proc print_nibble
	and #$0f
	cmp #10
	bcc @number
	clc
	adc #('A'-10)
	jmp krn::chrout
@number:
	clc
	adc #'0'
	jmp krn::chrout
.endproc

;*******************************************************************************
; LED ON
.proc led_on
	lda saved_cfg
	ora #1
	and #$3f			; keep registers visible and RESET clear
	sta ULTIMEM_CFG
	rts
.endproc

;*******************************************************************************
; CLEANUP
; Close the input and restore the mapping/configuration present on entry.
.proc cleanup
	lda __flasher_file_open
	beq :+
	jsr krn::clrchn
	lda #2
	jsr krn::close
	lda #$00
	sta __flasher_file_open
:	lda registers_saved
	beq @done
	lda flash_mapped
	beq :+
	lda #$f0
	sta FLASH_BASE
:
.ifdef UPDATER
	lda updater::saved_bank2
	sta ULTIMEM_BLK2
	lda updater::saved_bank2+1
	sta ULTIMEM_BLK2+1
.endif
	lda saved_bank
	sta ULTIMEM_BLK3
	lda saved_bank+1
	sta ULTIMEM_BLK3+1
	lda saved_blks
	sta ULTIMEM_BLKS
	lda saved_cfg
	and #$3e			; leave LED off, registers visible, and RESET clear
	sta ULTIMEM_CFG
@done:	cli
	rts
.endproc

;*******************************************************************************
; FAIL NO ULTIMEM
; Reports that the required 8 MiB UltiMem was not detected.
.proc fail_no_ultimem
	lda #<msg_no_ultimem
	ldy #>msg_no_ultimem
	jmp fail
.endproc

;*******************************************************************************
; FAIL MANUFACTURER
; Reports an unsupported manufacturer ID.
; IN:
;  - .A: manufacturer ID
.proc fail_manufacturer
	sta id_value

	lda #$f0
	sta FLASH_BASE
	lda #<msg_bad_manufacturer
	ldy #>msg_bad_manufacturer
	jmp fail_with_id
.endproc

;*******************************************************************************
; FAIL DEVICE
; Reports an unsupported device ID.
; IN:
;  - .A: device ID
.proc fail_device
	sta id_value

	lda #$f0
	sta FLASH_BASE
	lda #<msg_bad_device
	ldy #>msg_bad_device
	; fall through
.endproc

;*******************************************************************************
; FAIL WITH ID
; Prints the failure message and the ID byte after restoring the mappings.
; IN:
;  - .AY: address of the message
;  - id_value: ID byte
.proc fail_with_id
	sta failure_message
	sty failure_message+1
	jsr cleanup
	jsr failure_header

	lda failure_message
	ldy failure_message+1
	jsr __flasher_printz

	lda id_value
	jsr print_hex8
	rts
.endproc

;*******************************************************************************
; FAIL OPEN
; Reports that the input file could not be opened.
.proc fail_open
	lda #<msg_open_failed
	ldy #>msg_open_failed
	jmp fail
.endproc

;*******************************************************************************
; FAIL RAM
; Reports the RAM bank and values from the failed RAM check.
.proc fail_ram
	jsr cleanup
	jsr failure_header
	lda #<msg_ram_failed
	ldy #>msg_ram_failed
	jsr __flasher_printz

	lda ram_bank			; zero-based physical RAM bank
	jsr print_hex8
	jmp print_expected
.endproc

;*******************************************************************************
; FAIL READ
; Reports the KERNAL status from the failed read.
.proc fail_read
	lda io_status
	sta id_value
	lda #<msg_read_failed
	ldy #>msg_read_failed
	jmp fail_with_id
.endproc

;*******************************************************************************
; FAIL SHORT
; Reports that the image ended before its last bank.
.proc fail_short
	lda #<msg_short
	ldy #>msg_short
	jmp fail
.endproc

;*******************************************************************************
; FAIL ERASE TIMEOUT
; Reports that the flash chip did not finish erasing.
.proc fail_erase_timeout
	lda #<msg_erase_timeout
	ldy #>msg_erase_timeout
	jmp fail
.endproc

;*******************************************************************************
; FAIL ERASE
; Reports the first byte that failed erase verification.
.proc fail_erase
	lda #<msg_erase_failed
	ldy #>msg_erase_failed
	jmp fail_at_address
.endproc

;*******************************************************************************
; FAIL PROGRAM
; Reports a program timeout or verification failure.
.proc fail_program
	lda program_error
	cmp #1
	beq @timeout
	lda #<msg_verify_failed
	ldy #>msg_verify_failed
	jmp fail_at_address
@timeout:
	lda #<msg_program_timeout
	ldy #>msg_program_timeout
	; fall through
.endproc

;*******************************************************************************
; FAIL AT ADDRESS
; Prints the failure message and flash address.
; IN:
;  - .AY: address of the message
.proc fail_at_address
	sta failure_message
	sty failure_message+1
	jsr cleanup
	jsr failure_header

	lda failure_message
	ldy failure_message+1
	jsr __flasher_printz
	jsr print_failure_address
	; fall through to print_expected
.endproc

;*******************************************************************************
; PRINT EXPECTED
; Prints the expected and actual byte values.
.proc print_expected
	lda #<msg_expected
	ldy #>msg_expected
	jsr __flasher_printz
	lda __flasher_write_byte
	jsr print_hex8
	lda #<msg_got
	ldy #>msg_got
	jsr __flasher_printz
	lda read_byte
	jsr print_hex8
	rts
.endproc

;*******************************************************************************
; FAIL
; Prints the failure message after restoring the mappings.
; IN:
;  - .AY: address of the message
.proc fail
	sta failure_message
	sty failure_message+1
	jsr cleanup
	jsr failure_header

	lda failure_message
	ldy failure_message+1
	jsr __flasher_printz
	rts
.endproc

;*******************************************************************************
; FAILURE HEADER
.proc failure_header
	lda #7
	jsr __flasher_screen
	lda #$1c			; red text
	jsr krn::chrout
	lda #<msg_failed
	ldy #>msg_failed
	jmp __flasher_printz
.endproc

;*******************************************************************************
; PRINT FAILURE ADDRESS
.proc print_failure_address
@ptr=rb
	lda #<msg_block
	ldy #>msg_block
	jsr __flasher_printz

	clc
	lda __flasher_block_no
	adc #1
	sta __flasher_decimal_value
	lda __flasher_block_no+1
	adc #$00
	sta __flasher_decimal_value+1
	jsr __flasher_print_dec4

	lda #<msg_slash
	ldy #>msg_slash
	jsr __flasher_printz

	lda __flasher_image_blocks
	sta __flasher_decimal_value
	lda __flasher_image_blocks+1
	sta __flasher_decimal_value+1
	jsr __flasher_print_dec4
	lda #$0d
	jsr krn::chrout

	lda #<msg_offset
	ldy #>msg_offset
	jsr __flasher_printz

	sec
	lda @ptr
	sbc #<FLASH_BASE
	sta hex_value
	lda @ptr+1
	sbc #>FLASH_BASE
	sta hex_value+1
	jmp print_hex16
.endproc

.segment FLASH_RODATA

;*******************************************************************************
; STRINGS
.ifdef UPDATER
.export __flasher_filename
__flasher_filename:	.byte "MONSTER00000.BIN"
.else
.export __flasher_filename
__flasher_filename:	.byte "MONSTER.BIN"
.endif
filename_end:
msg_title:		.byte "   FLASHING MONSTER", $0d, 0
msg_testing_ram:	.byte "    TESTING RAM...", $0d, 0
msg_ram_failed:		.byte "RAM FAILED BANK $", 0
msg_erasing:		.byte "    ERASING FLASH", $0d, 0
msg_version:		.byte "    VERSION ", 0
msg_slash:		.byte "/", 0
msg_spaces:		.byte "    ", 0
msg_success:		.byte "    FLASH COMPLETE", $0d, 0
msg_written:		.byte " BLOCKS WRITTEN", $0d, "    RESTARTING...", 0
msg_failed:		.byte "     FLASH FAILED", $0d, 0
msg_no_ultimem:		.byte "8MB ULTIMEM REQUIRED", 0
msg_bad_manufacturer:	.byte "BAD MAKER ID $", 0
msg_bad_device:		.byte "BAD DEVICE ID $", 0
msg_open_failed:	.byte "CANNOT OPEN MONSTER.BIN", 0
msg_read_failed:	.byte "FILE READ STATUS $", 0
msg_short:		.byte "MONSTER.BIN TOO SHORT", 0
msg_erase_timeout:	.byte "ERASE TIMEOUT", 0
msg_erase_failed:	.byte "ERASE VERIFY FAILED", $0d, 0
msg_verify_failed:	.byte "VERIFY FAILED", $0d, 0
msg_program_timeout:	.byte "PROGRAM TIMEOUT", $0d, 0
msg_block:		.byte "BLOCK ", 0
msg_offset:		.byte "OFFSET $", 0
msg_expected:		.byte $0d, "EXPECTED $", 0
msg_got:		.byte " GOT $", 0

.segment FLASH_BSS

;*******************************************************************************
; VARIABLES
saved_cfg:		 .byte 0
saved_blks:		 .byte 0
saved_bank:		 .word 0
registers_saved:	 .byte 0
flash_mapped:		 .byte 0
.export __flasher_file_open
__flasher_file_open:	 .byte 0
.export __flasher_block_no
__flasher_block_no:	 .word 0
blocks_done:		 .word 0
.export __flasher_write_byte
__flasher_write_byte:	 .byte 0
read_byte:		 .byte 0
.export __flasher_eof_seen
__flasher_eof_seen:	 .byte 0
io_status:		 .byte 0
program_error:		 .byte 0
poll_value:		 .word 0
.export __flasher_decimal_value
__flasher_decimal_value: .word 0
decimal_digit:		 .byte 0
decimal_width:		 .byte 0
progress_digits:	 .byte 0
.ifndef UPDATER
header_position:	 .byte 0
image_header:		 .res FIRMWARE_VERSION+5
.endif
hex_value:		 .word 0
hex_byte:		 .byte 0
id_value:		 .byte 0
failure_message:	 .word 0
ram_pattern:		 .byte 0
ram_bank:		 .byte 0
saved_ram:		 .res RAM_BANKS

.export __flasher_image_blocks
__flasher_image_blocks:	.word 0
.export __flasher_image_last
__flasher_image_last:	.word 0
