;*******************************************************************************
; FLASH.ASM
; This file contains the UI for checking and flashing a VIC-20 update.
; The writer is copied from ROM to built-in RAM after confirmation.
;*******************************************************************************

.include "../alert.inc"
.include "../key.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../ram.inc"
.include "../screen.inc"
.include "../zeropage.inc"

.include "flashcheck.inc"
.include "flashrom.inc"

.import __FLASH_CODE_LOAD__
.import __FLASH_CODE_RUN__
.import __FLASH_CODE_SIZE__
.import __FLASH_RODATA_LOAD__
.import __FLASH_RODATA_RUN__
.import __FLASH_RODATA_SIZE__
.import __FLASH_BSS_RUN__
.import __FLASH_BSS_SIZE__

;*******************************************************************************
; RELOCATION
; ROM image size and the bounds of its RAM copy.
FLASH_COPY_SIZE = __FLASH_CODE_SIZE__ + __FLASH_RODATA_SIZE__
FLASH_COPY_PAGES = (FLASH_COPY_SIZE + $ff) / $100
.assert __FLASH_CODE_RUN__ = $1000, lderror, "flasher must run in built-in RAM"
.assert __FLASH_RODATA_LOAD__ = __FLASH_CODE_LOAD__ + __FLASH_CODE_SIZE__, lderror, "flasher ROM must be contiguous"
.assert __FLASH_RODATA_RUN__ = __FLASH_CODE_RUN__ + __FLASH_CODE_SIZE__, lderror, "flasher RAM must be contiguous"
.assert __FLASH_BSS_RUN__ + __FLASH_BSS_SIZE__ <= $1e00, lderror, "flasher overlaps KERNAL screen"
.assert $1000 + FLASH_COPY_PAGES*$100 <= $1e00, lderror, "flasher copy overlaps KERNAL screen"

.segment "FLASH_LAUNCH"

;*******************************************************************************
; LAUNCH
; Checks the update disk and asks for confirmation before leaving Monster.
; OUT:
;  - .C: clear if the user cancels or the disk check fails
;  - does not return after confirmation
.export __flash_launch
.proc __flash_launch
	jsr flashcheck::init

	lda #<checking
	ldy #>checking
	jsr flashcheck::message
	ldx #wait_prompt_end-wait_prompt-1
@wait_prompt:
	lda wait_prompt,x
	sta mem::linebuffer2,x
	dex
	bpl @wait_prompt

	ldxy #mem::linebuffer2
	stxy alert::prompt
	ldxy #mem::linebuffer
	CALLMAIN alert::open

	CALLMAIN scr::blank
	jsr flashcheck::check
	php
	jsr flashcheck::close
	CALLMAIN scr::unblank
	CALLMAIN alert::close
	plp
	bcc @ready

	; The validator left the reason in shared RAM. Stay in Monster.
	ldxy #mem::linebuffer
	CALLMAIN alert::show
	clc
	rts

@ready:
	; The alert runs in MAIN. Supply its strings through shared RAM so the
	; bank switch cannot hide them. Alert drawing has its own row buffer.
	ldx #message_end-message-1
@message:
	lda message,x
	sta mem::linebuffer,x
	dex
	bpl @message

	ldx #4
@version:
	lda flashcheck::filename+7,x
	sta mem::linebuffer+6,x
	dex
	bpl @version

	ldx #prompt_end-prompt-1
@prompt:
	lda prompt,x
	sta mem::linebuffer2,x
	dex
	bpl @prompt

	ldxy #mem::linebuffer2
	stxy alert::prompt
	ldxy #mem::linebuffer
	CALLMAIN alert::open

	CALLMAIN key::flush
	CALLMAIN key::waitch
	pha
	CALLMAIN alert::close
	pla

	and #$7f
	and #$df
	cmp #$59			; Y; all other keys cancel
	beq __flash_confirmed
	clc
	rts
.endproc

;*******************************************************************************
; CONFIRMED
; Copies the writer and its parameters to built-in RAM, then starts it.
; IRQs and NMIs must be disabled before overwriting the screen/character area.
; Does not return.
.export __flash_confirmed
.proc __flash_confirmed
@src=rb
@dst=rd
	sei
	lda #$7f
	sta $911e
	sta $912e

	lda #<__FLASH_CODE_LOAD__
	sta @src
	lda #>__FLASH_CODE_LOAD__
	sta @src+1
	lda #$00
	sta @dst
	lda #$10
	sta @dst+1
	ldx #<FLASH_COPY_PAGES
	ldy #$00
@copy:	lda (@src),y
	sta (@dst),y
	iny
	bne @copy
	inc @src+1
	inc @dst+1
	dex
	bne @copy

	; BSS isn't present in ROM; clear it explicitly, including partial pages.
	lda #<__FLASH_BSS_RUN__
	sta @dst
	lda #>__FLASH_BSS_RUN__
	sta @dst+1
	ldx #>__FLASH_BSS_SIZE__
	ldy #$00
	lda #$00
	cpx #$00
	beq @tail
@zero_page:
	sta (@dst),y
	iny
	bne @zero_page
	inc @dst+1
	dex
	bne @zero_page
@tail:	cpy #<__FLASH_BSS_SIZE__
	beq @enter
	sta (@dst),y
	iny
	bne @tail

@enter:	lda flashcheck::device
	sta flashrom::device

	lda flashcheck::namelen
	sta flashrom::namelen
	ldx #15
@filename:
	lda flashcheck::filename,x
	sta flashrom::filename,x
	dex
	bpl @filename

	jmp flashrom::start
.endproc

.segment "FLASH_LAUNCH"

;*******************************************************************************
; ALERT STRINGS
; Messages and prompts copied to shared RAM before calling the MAIN bank.
message:		.byte "flash 00000? save work", 0
message_end:
prompt:			.byte "y: flash  n: cancel", 0
prompt_end:

checking:		.byte "checking update disk", 0
wait_prompt:		.byte "please wait", 0
wait_prompt_end:
