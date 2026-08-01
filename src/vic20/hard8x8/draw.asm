.include "layout.inc"
.include "../../zeropage.inc"

.import __screen_rowslo
.import __screen_rowshi

.CODE

;*******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
.export __draw_rvs_underline
.proc __draw_rvs_underline
	; TODO:
	rts
.endproc

;*******************************************************************************
; RVS LINE
; Draws a horizontal rule on the row given in .A.
; IN:
;  - .A: the character row to draw the line at
;  - .Y: pixel offset of the line (ignored in text mode)
.export __draw_rvs_line
.proc __draw_rvs_line
@dst=r0
	tax
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	ldy #SCREEN_WIDTH-1
@l0:	lda (@dst),y
	cmp #$20		; blank?
	bne :+
	lda #$40		; horizontal line character
	sta (@dst),y
:	dey
	bpl @l0
	rts
.endproc
