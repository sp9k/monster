;*******************************************************************************
; FLASH.ASM
; This file contains the UI for checking and flashing a VIC-20 update.
; The writer is copied from ROM to built-in RAM after confirmation.
;*******************************************************************************

.include "../alert.inc"
.include "../border.inc"
.include "../draw.inc"
.include "../key.inc"
.include "../layout.inc"
.include "../alert-layout.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../ram.inc"
.include "../screen.inc"
.include "../text.inc"
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

;*******************************************************************************
; CONFIRMATION LAYOUT
CONFIRM_HEADING_ROW = (SCREEN_HEIGHT-4)/2
CONFIRM_TOP_ROW     = CONFIRM_HEADING_ROW-1

.segment "FLASH_CHECK_BSS"
heading_color:	.byte 0

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
	jsr center_message
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

	; linebuffer contains the validation error; center it
	jsr center_message
	ldxy #mem::linebuffer
	CALLMAIN alert::show
	clc
	rts

@ready:
	; copy alert message to shared RAM for display from MAIN bank
	ldx #current_end-current-1
@current:
	lda current,x
	sta mem::linebuffer+ALERT_TEXT_LEN+1,x
	dex
	bpl @current

	ldx #$04
@installed:
	lda flashcheck::installed,x
	sta mem::linebuffer+ALERT_TEXT_LEN+1+current_version-current,x
	dex
	bpl @installed

	ldx #message_end-message-1
@message:
	lda message,x
	sta mem::linebuffer,x
	dex
	bpl @message

	ldx #4
@version:
	lda flashcheck::filename+7,x
	sta mem::linebuffer+message_version-message,x
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
	jsr center_message
	jsr open_confirmation

	CALLMAIN key::flush
	CALLMAIN key::waitch
	pha
	jsr close_confirmation
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
; OPEN CONFIRMATION
; Displays the confirmation modal. This leverages the alert modal code with
; some modifications.
.proc open_confirmation
	ldxy #mem::linebuffer
	CALLMAIN alert::open

	; save an additional row's state (this modal is bigger than the default)
	lda mem::rowcolors_idx+CONFIRM_TOP_ROW
	sta heading_color
	lda #CONFIRM_TOP_ROW
	CALLMAIN scr::save_row
	ldx #CONFIRM_TOP_ROW
	CALLMAIN draw::resetline

	; configure bounds for printing modal
	lda #ALERT_LCOL
	sta text::puts_start
	lda #ALERT_RCOL+1
	sta text::puts_stop

	; hack in the installed version as the "top border" for the alert code
	lda #CONFIRM_TOP_ROW
	ldx #BORDER_TL
	ldy #BORDER_TR
	CALLMAIN alert::border

	; draw the real top border for this modal
	lda #ALERT_TEXT_COL
	sta alert::textcol
	ldxy #mem::linebuffer+ALERT_TEXT_LEN+1
	lda #CONFIRM_HEADING_ROW
	CALLMAIN alert::textrow

	; restore normal text bounds
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop
	rts
.endproc

;*******************************************************************************
; CLOSE CONFIRMATION
; Restores the area that was used by the confirmation dialog
.proc close_confirmation
	; close the default alert modal area
	CALLMAIN alert::close

	; restore one more row (this alert modal uses an extra)
	lda #CONFIRM_TOP_ROW
	CALLMAIN scr::restore_row

	lda heading_color
	ldx #CONFIRM_TOP_ROW
	JUMPMAIN draw::hline
.endproc

;*******************************************************************************
; CENTER MESSAGE
; Pads the message in the linebuffer so that it is centered for display in
; the alert modal.
; IN:
;  - mem::linebuffer: 0-terminated message
; OUT:
;  - mem::linebuffer: updated centered message
.proc center_message
@padding=r0
	ldx #$00

	; get length of the message
@len:	lda mem::linebuffer,x
	beq @center			; empty -> done
	inx
	cpx #ALERT_TEXT_LEN
	bcc @len
	rts

@center:
	; .Y = (TEXT_LEN - padding ) / 2 + padding
	stx @padding
	lda #ALERT_TEXT_LEN
	sec
	sbc @padding
	lsr
	beq @done		; if nothing to pad -> done
	sta @padding
	txa
	clc
	adc @padding
	tay

	; move the message and terminator to centered position
@shift:	lda mem::linebuffer,x
	sta mem::linebuffer,y
	dey
	dex
	bpl @shift

	; clear padded region before the message
	lda #' '
@fill:	sta mem::linebuffer,y
	dey
	bpl @fill
@done:	rts
.endproc

;*******************************************************************************
; ALERT STRINGS
; Messages and prompts (copied to shared RAM before calling the MAIN bank)
.define CURRENT_LABEL "installed "
current:
	.repeat .max(0, (ALERT_TEXT_LEN-.strlen(CURRENT_LABEL)-5)/2)
	.byte ' '
	.endrepeat
	.byte CURRENT_LABEL
current_version:
	.byte "00000", 0
current_end:
message:		.byte "update to "
message_version:	.byte "00000?", 0
message_end:
prompt:			.byte "y: flash  n: cancel", 0
prompt_end:

checking:		.byte "checking update disk", 0
wait_prompt:		.byte "please wait", 0
wait_prompt_end:
