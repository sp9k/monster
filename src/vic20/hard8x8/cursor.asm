;*******************************************************************************
; CURSOR.ASM
; This file contains procedures for interacting with the cursor.
; This includes functionality like limiting the bounds of the cursor as well
; as code to render it.
;*******************************************************************************

.include "screen.inc"
.include "../../config.inc"
.include "../../edit.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../text.inc"
.include "../../zeropage.inc"

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
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle
.proc __cur_toggle
@dst=r0
	ldx zp::cury
	lda scr::rowslo,x
	sta @dst
	lda scr::rowshi,x
	sta @dst+1

	ldy zp::curx
	cpy #LINESIZE
	bne :+
	dey			; clamp the end-of-line cursor to the last screen column
:
	lda (@dst),y
	eor #$80
	sta (@dst),y

	lda #1
	eor __cur_status
	sta __cur_status

	rts
.endproc
