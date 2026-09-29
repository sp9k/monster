;*******************************************************************************
; CURSOR.ASM
; This file contains procedures for interacting with the cursor.
; This includes functionality like limiting the bounds of the cursor as well
; as code to render it.
;*******************************************************************************

.include "../../config.inc"
.include "../../viewport.inc"
.include "../../edit.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../text.inc"
.include "../../zeropage.inc"
.include "bitmap.inc"

;*******************************************************************************
; CONSTANTS
L_INSERT_MASK  = $80	; mask for left half of 8x8 char in INSERT mode
R_INSERT_MASK  = $08	; mask for right half of 8x8 char in INSERT mode

END_INSERT_MASK = $01	; mask for offscreen column

L_REPLACE_MASK = $f0	; mask for left half of 8x8 char in REPLACE mode
R_REPLACE_MASK = $0f	; mask for right half of 8x8 char in REPLACE mode

.import __cur_status

.CODE

;*******************************************************************************
; MASK
; Returns the mask used to draw the cursor based on the current mode and
; cursor position.
; OUT:
;  - .A: the mask that will be EOR'd to draw the cursor
.proc mask
	lda zp::editor_mode
	cmp #MODE_COMMAND
	beq @replace		; use REPLACE mask for COMMAND mode
	lda text::insertmode
	beq @replace
@insert:
	lda zp::curx

	cmp #40
	bne :+
; column 40 has a special mask because this column is technically offscreen
@col40:
	lda #END_INSERT_MASK
	rts

:	and #$01
	beq :+
	lda #R_INSERT_MASK
	rts
:	lda #L_INSERT_MASK
	rts

@replace:
	lda zp::curx

	and #$01
	beq :+
	lda #R_REPLACE_MASK
	rts
:	lda #L_REPLACE_MASK
	rts
.endproc

;*******************************************************************************
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle
__cur_toggle:
@dst=r0
@mask=r2
	lda zp::curx
	pha
	ldx zp::curx
	jsr viewport::physical_x
	bcs @hidden
	stx zp::curx
	ldy zp::cury

	cpx #40
	bne :+
	dex

:	txa
	lsr
	tax

	tya
	asl
	asl
	asl
	adc bm::columnslo,x
	sta @dst
	lda #$00
	adc bm::columnshi,x
	sta @dst+1

	jsr mask
	sta @mask
	ldy #7
@l0:	lda @mask
	eor (@dst),y
	sta (@dst),y
	dey
	bpl @l0

	lda #1
	eor __cur_status
	sta __cur_status

@hidden:
@done:
	pla
	sta zp::curx
	ldx zp::curx
	ldy zp::cury
	rts
