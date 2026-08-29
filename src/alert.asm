;*******************************************************************************
; ALERT.ASM
; This file contains the code for "alerts": modal message windows that are
; drawn to inform the user what happened, and wait for them to acknowledge it.
;*******************************************************************************

.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "key.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "text.inc"
.include "zeropage.inc"

.import puts

;*******************************************************************************
; GEOMETRY
; Margin size, width, etc.
ALERT_HEIGHT = 4		; border, message, prompt, border

.if LINESIZE >= 40
ALERT_MARGIN = 4
.else
ALERT_MARGIN = 2
.endif

ALERT_WIDTH    = LINESIZE-(ALERT_MARGIN*2)
ALERT_ROW      = (SCREEN_HEIGHT-ALERT_HEIGHT)/2
ALERT_TEXT_COL = ALERT_MARGIN+2
ALERT_TEXT_LEN = ALERT_WIDTH-4

; the columns the left and right borders are drawn in
ALERT_LCOL = ALERT_MARGIN
ALERT_RCOL = ALERT_MARGIN+ALERT_WIDTH-1

;*******************************************************************************
; PROMPT
ALERT_PROMPT_MAX = 23		; sizeof("press [restore] to stop")

; the field is rounded out to even columns, and never wider than the text area
.if ALERT_TEXT_LEN < ((ALERT_PROMPT_MAX+1) & $fe)
ALERT_RVS_LEN = ALERT_TEXT_LEN & $fe
.else
ALERT_RVS_LEN = (ALERT_PROMPT_MAX+1) & $fe
.endif

ALERT_RVS_START = (ALERT_TEXT_COL+((ALERT_TEXT_LEN-ALERT_RVS_LEN)/2)) & $fe
ALERT_RVS_STOP  = ALERT_RVS_START+ALERT_RVS_LEN

.ifdef soft4x8
.assert (ALERT_LCOL .mod 2) = 0, error, "alert must start on an even column"
.assert ((ALERT_RCOL+1) .mod 2) = 0, error, "alert must end on an even column"
.assert (ALERT_RVS_START .mod 2) = 0, error, "reversed field must start even"
.assert (ALERT_RVS_STOP .mod 2) = 0, error, "reversed field must end even"
.endif

;*******************************************************************************
; BORDER CHARACTERS
.ifdef soft4x8
; 4x8 font box-drawing glyphs
ALERT_VBAR = 128
ALERT_HBAR = 140
ALERT_TL   = 136
ALERT_TR   = 137
ALERT_BL   = 138
ALERT_BR   = 139
.else
; PETSCII box-drawing glyphs
ALERT_VBAR = $dd	; screen code $5d
ALERT_HBAR = $c0	; screen code $40
ALERT_TL   = $b0	; screen code $70
ALERT_TR   = $ae	; screen code $6e
ALERT_BL   = $ad	; screen code $6d
ALERT_BR   = $bd	; screen code $7d
.endif

;*******************************************************************************
; the window's variables are kept out of RAM123, which is too tight to grow
.ifdef ultimem
.segment "SHAREBSS2"
.else
.BSS
.endif

msg:     .res ALERT_TEXT_LEN+1	; alert message string
rowbuf:  .res LINESIZE		; row being composed
colsave: .res ALERT_HEIGHT	; row colors the window is covering
cnt:     .byte 0		; row counter (0 = the window's top row)
textcol: .byte 0		; column the next text row starts its text at

.export __alert_prompt
__alert_prompt: .word 0		; the prompt "open" draws under the message

.segment "GUICODE"

;*******************************************************************************
; SHOW
; Draws an alert window containing the given message, waits for the user to
; press a key, and then restores everything the window covered.
; IN:
;  - .XY: the message to display
; OUT:
;  - .A: the key that was pressed
.export __alert_show
.proc __alert_show
	jsr copymsg
	ldxy #@anykey
	stxy __alert_prompt
	jsr drawwin

	jsr key::flush
	jsr key::waitch
	pha			; save the key
	jsr __alert_close
	pla			; restore key
	rts

.PUSHSEG
.RODATA
@anykey: .byte "press any key"
	.assert *-@anykey <= ALERT_PROMPT_MAX, error, "prompt too long for field"
	.byte 0
.POPSEG
.endproc

;*******************************************************************************
; OPEN
; Draws an alert window containing the given message
; IN:
;  - .XY:           the message to display
;  - alert::prompt: the prompt to display beneath it
.export __alert_open
.proc __alert_open
	jsr copymsg
	jmp drawwin
.endproc

;*******************************************************************************
; CLOSE
; Restores the screen and color under the alert window
.export __alert_close
.proc __alert_close
	; restore colors
	lda #ALERT_HEIGHT-1
	sta cnt
@uncolor:
	ldx cnt
	lda colsave,x
	pha
	txa
	clc
	adc #ALERT_ROW
	tax
	pla
	jsr draw::hline
	dec cnt
	bpl @uncolor

	; restore screen contents
	lda #ALERT_HEIGHT-1
	sta cnt
@restore:
	lda cnt
	clc
	adc #ALERT_ROW
	jsr scr::restore_row
	dec cnt
	bpl @restore
	rts
.endproc

;*******************************************************************************
; COPYMSG
; Copies the message to display to the msg buffer
; IN:
;  - .XY: the message to display
.proc copymsg
@src = r0
	stxy @src
	ldy #$00
:	lda (@src),y
	beq :+
	sta msg,y
	iny
	cpy #ALERT_TEXT_LEN
	bcc :-
:	lda #$00
	sta msg,y
	rts
.endproc

;*******************************************************************************
; DRAWWIN
; Saves what the window is about to cover and draws it
.proc drawwin
	jsr cur::off

	; save the rows the window is about to cover, and their colors
	lda #ALERT_HEIGHT-1
	sta cnt
@save:	ldx cnt
	lda mem::rowcolors_idx+ALERT_ROW,x
	sta colsave,x
	txa
	clc
	adc #ALERT_ROW
	jsr scr::save_row
	dec cnt
	bpl @save

	; reset colors for the row to draw
	lda #ALERT_HEIGHT-1
	sta cnt
@color:	lda cnt
	clc
	adc #ALERT_ROW
	tax
	jsr draw::resetline
	dec cnt
	bpl @color

	; set start/stop bounds to draw
	lda #ALERT_LCOL
	sta text::puts_start
	lda #ALERT_RCOL+1
	sta text::puts_stop

	lda #ALERT_ROW
	ldx #ALERT_TL
	ldy #ALERT_TR
	jsr border

	lda #ALERT_TEXT_COL
	sta textcol
	ldxy #msg
	lda #ALERT_ROW+1
	jsr textrow

	jsr draw_prompt

	lda #ALERT_ROW+ALERT_HEIGHT-1
	ldx #ALERT_BL
	ldy #ALERT_BR
	jsr border

	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop

	; reverse the field the prompt sits in
	ldy #ALERT_RVS_START
	ldx #ALERT_RVS_STOP
	lda #ALERT_ROW+2
	jmp scr::rvsline_part
.endproc

;*******************************************************************************
; PROMPT ROW
; Draws the prompt centered in the field that will be reversed
.proc draw_prompt
@src = r0
	ldxy __alert_prompt
	stxy @src

	; get the prompt's length, clamped to the field
	ldy #$00
:	lda (@src),y
	beq :+
	iny
	cpy #ALERT_RVS_LEN
	bcc :-

:	sty textcol
	lda #ALERT_RVS_LEN
	sec
	sbc textcol		; the space left over
	lsr			; half of it goes before the prompt
	clc
	adc #ALERT_RVS_START
	sta textcol

	ldxy __alert_prompt
	lda #ALERT_ROW+2
	jmp textrow
.endproc

;*******************************************************************************
; BORDER
; Builds/draws a horizontal border for the window
; IN:
;  - .A: row to draw the border at
;  - .X: character to use for the border's left corner
;  - .Y: character to use for its right corner
.proc border
	pha			; save the row
	tya
	pha			; and the characters for both corners
	txa
	pha

	jsr blankrow

	lda #ALERT_HBAR
	ldx #ALERT_WIDTH-2
:	sta rowbuf+ALERT_LCOL,x
	dex
	bne :-

	pla			; restore left corner char
	sta rowbuf+ALERT_LCOL
	pla			; restore right corner char
	sta rowbuf+ALERT_RCOL

	pla
	jmp showrow
.endproc

;*******************************************************************************
; TEXTROW
; Builds/draws one of the window's text rows
; IN:
;  - .XY:     text to display in the row
;  - .A:      row to draw the text at
;  - textcol: column to start the text at
.proc textrow
@src = r0
	pha			; save row
	stxy @src
	jsr blankrow

	ldx textcol
	ldy #$00
:	lda (@src),y
	beq :+
	sta rowbuf,x
	iny
	inx
	cpx #ALERT_RCOL
	bcc :-

:	; store left/right borders
	lda #ALERT_VBAR
	sta rowbuf+ALERT_LCOL
	sta rowbuf+ALERT_RCOL

	pla			; restore row
	jmp showrow
.endproc

;*******************************************************************************
; BLANKROW
; Fills the row to construct with spaces
.proc blankrow
	lda #' '
	ldx #LINESIZE-1
:	sta rowbuf,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; SHOWROW
; Draws rowbuf on the given row
; IN:
;  - .A: the row to draw it at
.proc showrow
	ldxy #rowbuf
	jmp puts
.endproc
