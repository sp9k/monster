.include "config.inc"
.include "memory.inc"
.include "layout.inc"
.include "text.inc"
.include "zeropage.inc"

.import __cur_toggle

;*******************************************************************************
.BSS
.export __cur_mode
__cur_mode: .byte 0	; 0 = NORMAL, 1 = SELECT

.export __cur_status
__cur_status: .byte 0	; 0 = OFF, 1 = ON

.export __cur_minx
__cur_minx:
minx: .byte 0

.export __cur_maxx
__cur_maxx:
maxx: .byte 0

.export __cur_miny
__cur_miny:
miny: .byte 0

.export __cur_maxy
__cur_maxy:
maxy: .byte 0

.CODE

;*******************************************************************************
; OFF
; Turns off the cursor if it is on. Has no effect if the cursor is already off.
.export __cur_off
.proc __cur_off
	lda __cur_status
	beq :++			; -> rts
:	jmp __cur_toggle
.endproc

;*******************************************************************************
; ON
; Turns on the cursor if it is off. Has no effect if the cursor is already on.
.export __cur_on
.proc __cur_on
	lda __cur_mode
	bne :-			; always turn ON in SELECT mode
	lda __cur_status
	bne :+
	jmp __cur_toggle
:	rts
.endproc

;*******************************************************************************
; MOVE
; Updates the cursor's (x,y) position by the offsets given
; IN:
;  - .X: the signed number of columns to move
;  - .Y: the signed number of rows to move
.export __cur_move
.proc __cur_move
@xdst=r2
@ydst=r3
	stx @xdst
	sty @ydst
	lda __cur_mode
	bne :+
	jsr __cur_off
:	lda @xdst
	clc
	adc zp::curx
	bmi @movey
	cmp maxx
	bcs @movey
	cmp minx
	bcc @movey
	sta zp::curx

@movey: lda @ydst
	clc
	adc zp::cury
	bmi @done
	cmp maxy
	bcs @done
	cmp miny
	bcc @done
	sta zp::cury
@done:	rts
.endproc

;*******************************************************************************
; SET
; Sets the cursor position (x,y) to the values given
; IN:
;  .X: the column to set the cursor to
;  .Y: the row to set the cursor to
.export __cur_set
.proc __cur_set
	cpx maxx
	bcc :+
	ldx maxx
	dex
:	cpx minx
	bcs :+
	ldx minx
:	stx r2

	cpy maxy
	bcc :+
	ldy maxy
	dey
:	cpy miny
	bcs :+
	ldy miny
:	sty r3

	jsr __cur_off

	ldx r2
	ldy r3
	stx zp::curx
	sty zp::cury
	rts
.endproc

;*******************************************************************************
; UNLIMIT
.export __cur_unlimit
.proc __cur_unlimit
	ldx #$00
	stx minx
	stx miny

	ldx #LINESIZE
	ldy #SCREEN_HEIGHT-1

	; fall through to __cur_setmax
.endproc

;*******************************************************************************
; SETMAX
; Sets the maximum values for the cursor's X and Y values
; IN:
;  - .X: the column limit to set for the cursor
;  - .Y: the row limit to set for the cursor
.export __cur_setmax
.proc __cur_setmax
	stx maxx
	sty maxy
	rts
.endproc

;*******************************************************************************
; SETMIN
; Sets the minimum values for the cursor's X and Y values
; IN:
;  - .X: the column limit to set for the cursor
;  - .Y: the row limit to set for the cursor
.export __cur_setmin
.proc __cur_setmin
	stx minx
	sty miny
	rts
.endproc

;*******************************************************************************
; RIGHT
; Moves the cursor right a column
; If moving right would move the cursor outside its limits, has no effect
.export __cur_right
.proc __cur_right
	lda zp::curx
	cmp #LINESIZE
	bcs @done

	jsr text::char_index
	lda mem::linebuffer,y
	cmp #$09		; TAB
	bne :+
	jsr text::tabr_dist
	clc
	adc zp::curx
	sta zp::curx
	rts
:	inc zp::curx
@done:	rts
.endproc

;*******************************************************************************
; PUSH
; Pushes the cursor onto the stack. This call will leave 2 new elements on
; the stack, which cur::pop will restore
; cursor
.export __cur_push
.proc __cur_push
@ret=zp::util
	; save return address
	pla
	sta @ret
	pla
	sta @ret+1

	lda zp::curx
	pha
	lda zp::cury
	pha

	; push return address again
	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc

;*******************************************************************************
; POP
; Pops the cursor values from the stack (pushed with cur::push)
.export __cur_pop
.proc __cur_pop
@ret=zp::util
	; save return address
	pla
	sta @ret
	pla
	sta @ret+1

	pla
	sta zp::cury
	pla
	sta zp::curx

	; push return address again
	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc
