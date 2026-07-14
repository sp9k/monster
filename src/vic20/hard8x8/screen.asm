;*******************************************************************************
; SCREEN23.ASM
;*******************************************************************************

.include "../fastcopy.inc"
.include "../prefs.inc"
.include "../../config.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../settings.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

;*******************************************************************************
; CONSTANTS
COLMEM_ADDR = $9400	; page 1
;COLMEM_ADDR2 = $9600	; page 2

SCREEN_ADDR = $1800	; page 1

;NUM_COLS    = 22	; number of 8-pixel columns
;NUM_ROWS    = 23	; number of 8-pixel rows
.define NUM_COLS 22
.define NUM_ROWS 23

; address of "virtual screen"
VSCREEN_ADDR = $1a00
VSCREEN_W = 40

SCREEN_ROWS = 12	; number of physical rows per column
.segment "SETUP"
;*******************************************************************************
.export __text_init
.proc __text_init
	jsr $e5c3
	lda #(BG_COLOR<<4 | BORDER_COLOR)
	sta $900f
	rts
.endproc

.CODE
;*******************************************************************************
.export __screen_init
.proc __screen_init
	jsr $e5c3

	lda #$e2	; lowercase chars / screen @ $1800
	sta $9005

	lda prefs::normal_color
	sta $900f
	rts
.endproc

.CODE
;*******************************************************************************
; CLR
; Clears the screen
.export __screen_clr
.proc __screen_clr
	ldx #$00
	lda #$20
:	sta SCREEN_ADDR,x
	sta SCREEN_ADDR+$100,x
	dex
	bne :-

	; fall through to clrcolor
.endproc

;*******************************************************************************
; CLRCOLOR
; Reverts all color memory to the given color
; IN:
;  - .A: the color to fill the screen with
.export __screen_clrcolor
.proc __screen_clrcolor
	ldy #$00
	lda #TEXT_COLOR
@l0:    sta COLMEM_ADDR,y
        sta COLMEM_ADDR+$100,y
	dey
        bne @l0
        rts
.endproc

;*******************************************************************************
; CLR_PART
; Clears all rows below the given offset in every column
; IN:
;  - .A: the character row to start clearing at
.export __screen_clr_part
.proc __screen_clr_part
@cnt=r2
	sta @cnt

@l0:	lda @cnt
	jsr __screen_clrline
	inc @cnt
	lda @cnt
	cmp #NUM_ROWS
	bne @l0

        rts
.endproc

;*******************************************************************************
; BM CLRLINE
; Clears the given character row
; IN:
;  - .A: the row to clear
.export __screen_clrline
.proc __screen_clrline
@dst=r0
	jsr __screen_char_addr
	stx @dst
	sty @dst+1

	lda #$20
	ldy #NUM_COLS-1
@l0:	sta (@dst),y
	dey
	bpl @l0
	rts
.endproc

;*******************************************************************************
; RVSLINE
; Reverses 1 row of characters (8 pixels high) at the given row character row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
.export __screen_rvsline
.proc __screen_rvsline
@dst=r0
	jsr __screen_char_addr
	stxy @dst

	ldy #NUM_COLS-1
@l0: 	lda (@dst),y
	eor #$80
	sta (@dst),y
	dey
	bpl @l0
	rts
.endproc

;*******************************************************************************
; RVSLINE PART
; Reverses the given number of characters (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (in characters)
;  - .Y: the first column to reverse
;  - .X: the last column to reverse
.export __screen_rvsline_part
.proc __screen_rvsline_part
@dst=r0
@start=r2
@stop=r3
@row=r4
	sta @row

	; swap Y and X if (X < Y)
	sty @start
	cpx @start
	bcs :+
	txa
	tay
	ldx @start

:	stx @stop

	; get the row address to reverse
	ldx @row
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	ldy @stop
	beq @col0
	cpy #NUM_COLS+1
	bcc :+
	ldy #NUM_COLS
:	dey
@l0:	lda (@dst),y
	eor #$80
	sta (@dst),y
	dey
	cpy @start
	bne @l0

@col0:	; do last char
	lda (@dst),y
	eor #$80
	sta (@dst),y

@done:	rts
.endproc

;*******************************************************************************
; SAVE
; Saves the screen to the backup buffer. It may then be restored with a call
; to scr::restore
.export __screen_save
.proc __screen_save
	; TODO:
	;CALL FINAL_BANK_VSCREEN, save

	; save colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors,x
	sta mem::rowcolors_save,x
	lda #DEFAULT_900F
	sta mem::rowcolors,x
	dex
	bpl :-

	jmp __screen_init
.endproc

;*******************************************************************************
; RESTORE
; Restores screen from the the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
@buff=r0
@bm=r2
	; TODO
	;CALL FINAL_BANK_VSCREEN, restore

	; restore the per-row colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors,x
	dex
	bpl :-

	rts
.endproc

;*******************************************************************************
; RESTORE ROW
; Restores a single row of the screen from the backup buffer.
; IN:
;  - .A: the row to restore
.export __screen_restore_row
.proc __screen_restore_row
	; TODO: (see __screen_save)
	rts
.endproc

;*******************************************************************************
; SAVE ROW
; Saves a single row of the screen to the backup buffer.
; IN:
;  - .A: the row to save
.export __screen_save_row
.proc __screen_save_row
	; TODO: (see __screen_save)
	rts
.endproc

;*******************************************************************************
; CHAR ADDR
; Returns the address for the "character row" of the given row.
; IN:
;  - .A: the character row to get the address of
; OUT:
;  - .XY: the address
.export __screen_char_addr
.proc __screen_char_addr
	tax
	ldy __screen_rowshi,x
	lda __screen_rowslo,x
	tax
	rts
.endproc

;*******************************************************************************
; SCROLLUP
; Scrolls all lines from .X to .A up
; IN:
;  - .X: the top line that characters are scrolled to
;  - .A: the bottom line that is scrolled
.proc __text_scrollup
.export __text_scrollup
@src=zp::text
@dst=zp::text+2
@numrows=zp::text+4
	stx @numrows
	cmp @numrows
	bcc @done

	sec
	sbc @numrows
	sta @numrows

	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	lda __screen_rowslo+1,x
	sta @src
	lda __screen_rowshi+1,x
	sta @src+1

	ldx @numrows
@l0:	ldy #NUM_COLS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

@next:	lda @src
	clc
	adc #NUM_COLS
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #NUM_COLS
	sta @dst
	bcc :+
	inc @dst+1
	ldy #$00

:	dex
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; SCROLLDOWN
; Scrolls all rows from .A to .X
; IN:
;  - .A: the first column to scroll down
;  - .X: the last column to scroll down to
.export __text_scrolldown
.proc __text_scrolldown
	ldy #$01

	; fallthrough
.endproc

;*******************************************************************************
; SCROLLDOWNN
; Scrolls all rows in the given range down by the given number of rows
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row by
.export __text_scrolldownn
.proc __text_scrolldownn
@src=zp::text
@dst=zp::text+2
@rowstart=zp::text+4
@offset=zp::text+5
	sta @rowstart
	cpx @rowstart
	beq @done	; if first and last rows are equal, no scroll

	sty @offset

@l0:	lda __screen_rowslo,x
	sta @src
	lda __screen_rowshi,x
	sta @src+1
	txa
	clc
	adc @offset
	cmp #NUM_ROWS-1
	bcs @next		; destination is off screen, skip

	tay
	lda __screen_rowslo,y
	sta @dst
	lda __screen_rowshi,y
	sta @dst+1

	ldy #NUM_COLS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

@next:	dex		; decrement row counter
	bmi @done
	cpx @rowstart
	bcs @l0

@done:	rts
.endproc

;*******************************************************************************
; SCROLLRIGHTN
; Scrolls the screen right by the given number of characters
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row right by
.export __screen_scrollrightn
.proc __screen_scrollrightn
win_scroll: .byte 0	; current scroll
.endproc

;******************************************************************************
; PUTCH
; Puts the character given at the current cursor position
; IN:
;  - .A: the character to plot
.export putch
.proc putch
@dst=zp::text
	pha
	ldx zp::cury
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1
	ldy zp::curx
	pla
	jsr asc2scr
	sta (@dst),y
	rts
.endproc

;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, NUM_COLS characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A:  the row to display the text at
.export __text_puts
__text_puts:
.export puts
.proc puts
@src = zp::text
@dst = zp::text+2
	stxy @src

	tax
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	ldy #$00
	ldx #1+((LINESIZE-1)/NUM_COLS)	; # of extra pages to render
@l0:	lda (@src),y
	jsr asc2scr
	sta (@dst),y
	iny
	cpy #NUM_COLS
	bne @l0

	; move to next destination page
	inc @dst+1
	inc @dst+1
	lda @src
	clc
	adc #NUM_COLS
	sta @src
	bcc :+
	inc @src+1
	ldy #$00	; reset column
	dex		; decrement page count
	bne @l0		; repeat for all pages

.if (LINESIZE .mod NUM_COLS) <> 0
	; fill all spaces past line length
	ldy #(LINESIZE .mod NUM_COLS)-1
	lda #$ff	; symbol to fill uneditable spaces with
:	sta (@dst),y
	iny
	cpy #NUM_COLS
	bne :-
.endif
	rts
.endproc

;******************************************************************************
; SHL
.export __scr_shl
.proc __scr_shl
.endproc

;******************************************************************************
; SHR
.export __scr_shr
.proc __scr_shr
.endproc

;*******************************************************************************
; ASC2SCR
; Returns the screen code for the given ASCII character
; IN:
;   - .A: the ASCII code to convert
; OUT:
;   - .A: the screen code that corresponds to the given char
.proc asc2scr
@savex=zp::text+7
	stx @savex
	cmp #$ff
	beq @done
	cmp #$40
	bne :+
	lda #$00
	rts

:	ldx #$ff

:	inx
	cmp @convtab,x
	bcs :-
	clc
	adc @offset,x

	ldx @savex
@done:	rts
.PUSHSEG
.RODATA
;|  Code   | Conversion Offset |
;|-----------------------------|
;| $00-$1F | $80               |
;| $20-$3F | $00               |
;| $40-$5F | $C0               |
;| $60-$7F | $E0               |
;| $80-$9F | $40               |
;| $A0-$BF | $C0               |
;| $C0-$DF | $80               |
;| $E0-$FE | $80               |
;| $FF     | $00               |
@convtab:
.byte $20,$5b,$60,$80,$a0,$c0,$e0,$ff
@offset:
.byte $80,$00,$c0,$a0,$40,$c0,$80,$80
.POPSEG

.endproc

.RODATA
;*******************************************************************************
.linecont +
.define rows \
	SCREEN_ADDR+$000, \
	SCREEN_ADDR+$016, \
	SCREEN_ADDR+$02c, \
	SCREEN_ADDR+$042, \
	SCREEN_ADDR+$058, \
	SCREEN_ADDR+$06e, \
	SCREEN_ADDR+$084, \
	SCREEN_ADDR+$09a, \
	SCREEN_ADDR+$0b0, \
	SCREEN_ADDR+$0c6, \
	SCREEN_ADDR+$0dc, \
	SCREEN_ADDR+$0f2, \
	SCREEN_ADDR+$108, \
	SCREEN_ADDR+$11e, \
	SCREEN_ADDR+$134, \
	SCREEN_ADDR+$14a, \
	SCREEN_ADDR+$160, \
	SCREEN_ADDR+$176, \
	SCREEN_ADDR+$18c, \
	SCREEN_ADDR+$1a2, \
	SCREEN_ADDR+$1b8, \
	SCREEN_ADDR+$1ce, \
	SCREEN_ADDR+$1e4
.linecont -

.linecont +
.define vrows \
	VSCREEN_ADDR+VSCREEN_W*0,  \
	VSCREEN_ADDR+VSCREEN_W*1,  \
	VSCREEN_ADDR+VSCREEN_W*2,  \
	VSCREEN_ADDR+VSCREEN_W*3,  \
	VSCREEN_ADDR+VSCREEN_W*4,  \
	VSCREEN_ADDR+VSCREEN_W*5,  \
	VSCREEN_ADDR+VSCREEN_W*6,  \
	VSCREEN_ADDR+VSCREEN_W*7,  \
	VSCREEN_ADDR+VSCREEN_W*8,  \
	VSCREEN_ADDR+VSCREEN_W*9,  \
	VSCREEN_ADDR+VSCREEN_W*10, \
	VSCREEN_ADDR+VSCREEN_W*11, \
	VSCREEN_ADDR+VSCREEN_W*12, \
	VSCREEN_ADDR+VSCREEN_W*13, \
	VSCREEN_ADDR+VSCREEN_W*14, \
	VSCREEN_ADDR+VSCREEN_W*15, \
	VSCREEN_ADDR+VSCREEN_W*16, \
	VSCREEN_ADDR+VSCREEN_W*17, \
	VSCREEN_ADDR+VSCREEN_W*18, \
	VSCREEN_ADDR+VSCREEN_W*19, \
	VSCREEN_ADDR+VSCREEN_W*20, \
	VSCREEN_ADDR+VSCREEN_W*21, \
	VSCREEN_ADDR+VSCREEN_W*22
.linecont -

.export __screen_rowslo
.export __screen_rowshi
__screen_rowslo: .lobytes rows
__screen_rowshi: .hibytes rows

vrowslo: .lobytes vrows
vrowshi: .hibytes vrows

