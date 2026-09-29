;*******************************************************************************
; CURSOR.ASM
; Draws the cursor at its visible screen column.
;*******************************************************************************

.include "../viewport.inc"
.include "../zeropage.inc"

.include "c64.inc"

.import __cur_status

.CODE

;*******************************************************************************
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle_physical
.proc __cur_toggle_physical
@dst=r0
	ldx zp::curx
	jsr viewport::physical_x
	bcs @done
	txa
	pha
	; get the row to toggle
	ldx zp::cury
	lda c64::rowslo,x
	sta @dst
	lda c64::rowshi,x
	sta @dst+1
	pla
	tay
	cpy #40
	bne :+
	dey			; clamp to last column (cursor at end of full line)
:	lda (@dst),y
	eor #$80		; reverse
	sta (@dst),y

	lda #1
	eor __cur_status
	sta __cur_status

@done:	ldx zp::curx
	ldy zp::cury
	rts
.endproc
