.include "c64.inc"
.include "layout.inc"
.include "macros.inc"
.include "prefs.inc"
.include "settings.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"

; mirrored from draw.inc
COLOR_TEXT    = 0
COLOR_NORMAL  = 1
COLOR_RVS     = 2
COLOR_BRKON   = 3
COLOR_BRKOFF  = 4
COLOR_SUCCESS = 5
COLOR_SELECT  = 6

.CODE

;******************************************************************************
; HILINE
.export __draw_hiline
.proc __draw_hiline
	lda #COLOR_RVS
	jmp __draw_hline
.endproc

;******************************************************************************
; RESETLINE
.export __draw_resetline
.proc __draw_resetline
@dst=r0
	lda #COLOR_NORMAL

	; fall through to __draw_hline
.endproc

;******************************************************************************
; HLINE
; Draws a horizontal line at the row given in .A
; IN:
;  - .A: the color to highlight with
;  - .X: the row to highlight
.export __draw_hline
.proc __draw_hline
@dst=r0
	sta mem::rowcolors_idx,x
	sty @savey

	; look up real color from palette
	tay
	lda prefs::palette,y

	; look up color from palette
	sta mem::rowcolors,x

	ldy c64::rowslo,x
	sty @dst
	ldy c64::rowshi,x
	sty @dst+1

	cmp prefs::normal_color
	bne @rvs
@norvs:
	; unreverse characters
	ldy #SCREEN_WIDTH-1
:	lda (@dst),y
	and #$7f
	sta (@dst),y
	dey
	bpl :-
	bmi @done

@rvs:	; reverse the characters for the row
	ldy #SCREEN_WIDTH-1
:	lda (@dst),y
	ora #$80
	sta (@dst),y
	dey
	bpl :-

@done:
@savey=*+1
	ldy #$00
	rts
.endproc

;******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
;  - .W: the pattern to draw
.export __draw_rvs_underline
.proc __draw_rvs_underline
@dst=r0
	; TODO:
	rts
.endproc

;******************************************************************************
; RVS LINE
; Draws a horizontal rule on the row given in .A.
; IN:
;  - .A: the character row to draw the line at
;  - .Y: pixel offset of the line (ignored on C64)
.export __draw_rvs_line
.proc __draw_rvs_line
@dst=r0
	tax
	lda c64::rowslo,x
	sta @dst
	lda c64::rowshi,x
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

;******************************************************************************
; SCROLLCOLORSU
; Scrolls all colors from the given start row to the given stop row up by the
; given amount
; IN:
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
;  - .A: the amount to scroll
.export __draw_scrollcolorsu
.proc __draw_scrollcolorsu
@n=r0
@last=r1
	sty @last
	cpx @last
	bcs @done
	sta @n

	; get start row + scroll amount
	txa
	clc
	adc @n
	tay
	cmp @last
	bcs @done		; if first row + n >= last row, don't scroll
@l0:	lda mem::rowcolors,y	; start+.X
	sta mem::rowcolors,x	; start+.A+.X
	lda mem::rowcolors_idx,y
	sta mem::rowcolors_idx,x
	inx
	iny
	cpx @last
	bne @l0
	lda prefs::normal_color
	sta mem::rowcolors,x	; clear last row
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
@done:	rts
.endproc

;******************************************************************************
; SCROLLCOLORSD1
; Scrolls all colors in the given range down by 1. See __draw_scrollcolorsd1
; IN:
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
.export __draw_scrollcolorsd1
.proc __draw_scrollcolorsd1
	lda #$01

	; fall through to __draw_scrollcolorsd
.endproc

;******************************************************************************
; SCROLLCOLORSD
; Scrolls all colors from the given start row to the given stop row down by the
; given amount
; IN:
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
;  - .A: the amount to scroll
.export __draw_scrollcolorsd
.proc __draw_scrollcolorsd
@last=r0
@start=r1
	stx @start
	sty @last

	clc
	adc @last
	tay

	ldx @last
	cpx @start
	beq @done		; nothing to scroll
@l0:	cpy @last		; is the target in the scroll range?
	beq :+
	bcs :++			; if not, skip it

:	lda mem::rowcolors,x	; last_row
	sta mem::rowcolors,y	; (last_row + amount)
	lda mem::rowcolors_idx,x
	sta mem::rowcolors_idx,y

:	; reset the line we just scrolled
	lda prefs::normal_color
	sta mem::rowcolors,x
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x

	dey
	dex
	bmi @done
	cpx @start
	bcs @l0
@done:	rts
.endproc

;******************************************************************************
; COLOROFF
; Disables color in the interrupt and sets the background to its default color
.export __draw_coloroff
.proc __draw_coloroff
	lda #$00
	sta mem::coloron
	rts
.endproc

;******************************************************************************
; REFRESH COLORS
.export __draw_refresh_colors
.proc __draw_refresh_colors
	ldx #SCREEN_HEIGHT-1
@l0:	ldy mem::rowcolors_idx,x
	lda prefs::palette,y
	sta mem::rowcolors,x
	dex
	bpl @l0
	rts
.endproc
