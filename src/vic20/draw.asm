.include "prefs.inc"
.include "settings.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"

COLOR_NORMAL  = 1
COLOR_RVS     = 2
COLOR_BRKON   = 3
COLOR_BRKOFF  = 4
COLOR_SUCCESS = 5
COLOR_FAILURE = 3
COLOR_SELECT  = 6

.CODE

;******************************************************************************
; HILINE
; Highlights the given line by setting it to the reverse of the
; default color (DEFAULT_RVS)
; IN:
;  - .X: the row to highlight
.export __draw_hiline
.proc __draw_hiline
	lda #COLOR_RVS
	jmp __draw_hline
.endproc

;******************************************************************************
; RESETLINE
; Resets the given line by setting its color to the default one
; (prefs::normal_color)
; IN:
;  - .X: the row to highlight
.export __draw_resetline
.proc __draw_resetline
	lda #COLOR_NORMAL

	; fall through to __draw_hline
.endproc

;******************************************************************************
; HLINE
; Sets the color for the row given in .A
; IN:
;  - .A: the color to highlight with
;  - .X: the row to highlight
.export __draw_hline
.proc __draw_hline
@savey=zp::text
	sta mem::rowcolors_idx,x
	sty @savey

	; look up real color from palette
	tay
	lda prefs::palette,y

	; look up color from palette
	sta mem::rowcolors,x

	; check if we need to color in the IRQ
	ldx #24
	lda prefs::normal_color
:	cmp mem::rowcolors-1,x
	bne @done
	dex
	bne :-
@done:	stx mem::coloron	; (en/dis)able color
	ldy @savey
	rts
.endproc

;******************************************************************************
; REFRESH COLORS
; Reads the current values for each row from the active palette and refreshes
; them
.export __draw_refresh_colors
.proc __draw_refresh_colors
	ldx #24-1
@l0:	ldy mem::rowcolors_idx,x
	lda prefs::palette,y
	sta mem::rowcolors,x
	dex
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
	lda mem::breakpoint_rows,y
	sta mem::breakpoint_rows,x
	inx
	iny
	cpx @last
	bne @l0
	lda prefs::normal_color
	sta mem::rowcolors,x	; clear last row
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	lda #$00
	sta mem::breakpoint_rows,x
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
	lda mem::breakpoint_rows,x
	sta mem::breakpoint_rows,y

:	; reset the line we just scrolled
	lda prefs::normal_color
	sta mem::rowcolors,x
	lda #$00
	sta mem::breakpoint_rows,x
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
	sei
	lda #$00
	sta mem::coloron
	lda prefs::normal_color
	sta $900f
	cli
	rts
.endproc
