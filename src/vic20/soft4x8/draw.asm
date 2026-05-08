.include "../../macros.inc"
.include "bitmap.inc"

.CODE


;******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
.export __draw_rvs_underline
.proc __draw_rvs_underline
	ldy #$07

	; fall through to __draw_rvs_line
.endproc

;******************************************************************************
; RVS LINE
; Reverses a horizontal line of pixels in the given character row
; IN:
;  - .A: character row to draw a horizontal line at
;  - .Y: pixel offset to draw the line at
.export __draw_rvs_line
.proc __draw_rvs_line
@dst=r0
@row=r2
	sty @row
	jsr bm::charaddr
	stxy @dst

	ldx #20
	ldy @row		; last line
@l0: 	lda #$ff
	eor (@dst),y
	sta (@dst),y
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0

	rts
.endproc

