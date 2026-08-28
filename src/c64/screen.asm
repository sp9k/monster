;*******************************************************************************
; SCREEN.ASM
;*******************************************************************************

.include "macros.inc"
.include "prefs.inc"
.include "reu.inc"
.include "../config.inc"
.include "../draw.inc"
.include "../irq.inc"
.include "../layout.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../settings.inc"
.include "../util.inc"
.include "../zeropage.inc"

.import __text_puts_start	; first column puts draws
.import __text_puts_stop	; one past the last column puts draws

;*******************************************************************************
; CONSTANTS
.define COLMEM_ADDR $d800
.define SCREEN_ADDR $0400
.define NUM_COLS 40
.define NUM_ROWS 25

.segment "VSCREEN"
.segment "VSCREEN_BSS"
.segment "FASTTEXT"
.segment "FASTTEXT_BSS"

.CODE
;*******************************************************************************
.export __text_init
.proc __text_init
	rts
.endproc
;*******************************************************************************
.export __screen_init
.proc __screen_init
	IO_BEGIN

	; screen @ $0400 character ROM @$1800
	lda $dd02	; DDRA: bank bits are outputs
	ora #$03
	sta $dd02
	lda $dd00
	ora #$03	; %11 -> VIC bank 0
	sta $dd00

	lda #$16	; lowercase chars / screen @ $0400
	sta $d018
	lda #$00
	sta $d020
	sta $d021
	IO_DONE

	rts
.endproc

;*******************************************************************************
; BLANK
; Prepares the screen for sensitive work that requires the IRQ to be disabled.
; The hardware text screen doesn't rely on the IRQ, so this only disables it.
.export __screen_blank
.proc __screen_blank
	jmp irq::off
.endproc

;*******************************************************************************
; UNBLANK
; Ends a "blank"; call when sensitive IRQ disabled work has finished
.export __screen_unblank
.proc __screen_unblank
	jmp irq::on
.endproc

;*******************************************************************************
; DRAW GUTTER
; No-op on the C64: breakpoints are rendered via the raster IRQ
.export __screen_draw_gutter
.export __screen_draw_gutter_row
__screen_draw_gutter:
__screen_draw_gutter_row:
	rts

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
	sta SCREEN_ADDR+$200,x
	dex
	bne :-

	ldx #$e8
:	sta SCREEN_ADDR+$300-1,x
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
	IO_BEGIN

	ldy #$00
	lda prefs::text_color
@l0:    sta COLMEM_ADDR,y
        sta COLMEM_ADDR+$100,y
        sta COLMEM_ADDR+$200,y
        sta COLMEM_ADDR+$300,y
	dey
        bne @l0

	IO_DONE

	; the row color shadows may live under the I/O space, so they must be
	; written with I/O banked out
	ldx #SCREEN_HEIGHT-1
:	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	lda prefs::normal_color
	sta mem::rowcolors,x
	dex
	bpl :-

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
	stxy @dst

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
	cpy @start
	beq @col0		; start==stop: reverse only the char at @start
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
	jsr __screen_savebuf
	jmp __screen_init
.endproc

;*******************************************************************************
; SAVEBUF
; Saves the screen to the backup buffer WITHOUT reinitializing it (used when
; overlaying another full-screen view that shares the same layout).
.export __screen_savebuf
.proc __screen_savebuf
@scr=r0
	; save colors
	ldx #SCREEN_HEIGHT-1
:	lda mem::rowcolors_idx,x
	sta mem::rowcolors_save,x
	lda prefs::normal_color
	sta mem::rowcolors,x
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	dex
	bpl :-

	; save the screen data
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2
	ldxy #$0400
	stxy reu::reuaddr
	stxy reu::c64addr
	stxy reu::txlen
	jsr reu::store

	; unreverse all characters on screen
	ldxy #SCREEN_ADDR
	stxy @scr
	ldy #$00
	ldx #$04
:	lda (@scr),y
	and #$7f
	sta (@scr),y
	dey
	bne :-
	inc @scr+1	; next page of screen memory
	dex
	bne :-

	rts
.endproc

;*******************************************************************************
; RESTORE
; Restores screen from the the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
@buff=r0
@bm=r2
	; restore the per-row colors
	ldx #SCREEN_HEIGHT-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors_idx,x
	dex
	bpl :-

	; restore the screen data
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2
	ldxy #$0400
	stxy reu::reuaddr
	stxy reu::c64addr
	stxy reu::txlen
	jsr reu::load

	jmp draw::refresh_colors
	rts
.endproc

;*******************************************************************************
; RESTORE ROW
; Restores a single row of the screen from the backup buffer.
; IN:
;  - .A: the row to restore
.export __screen_restore_row
.proc __screen_restore_row
	jsr row_addrs
	jmp reu::load
.endproc

;*******************************************************************************
; SAVE ROW
; Saves a single row of the screen to the backup buffer.
; IN:
;  - .A: the row to save
.export __screen_save_row
.proc __screen_save_row
	jsr row_addrs
	jmp reu::store
.endproc

;*******************************************************************************
; ROW ADDRS
; Sets up the REU registers for transferring the given screen row
; IN:
;  - .A: the row to set the transfer registers for
.proc row_addrs
	jsr __screen_char_addr	; .XY = the screen address of the row
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2
	ldxy #NUM_COLS
	stxy reu::txlen
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
	ldy #$01

	; fall through to __text_scrollupn
.endproc

;*******************************************************************************
; SCROLLUPN
; Scrolls all lines from .X to .A up by .Y rows
; IN:
;  - .X: the top line that characters are scrolled to
;  - .A: the bottom line that is scrolled
;  - .Y: the number of rows to scroll by
.export __text_scrollupn
.proc __text_scrollupn
@src=zp::text
@dst=zp::text+2
@cnt=zp::text+4
@n=zp::text+5
	sty @n
	stx @cnt		; temporarily store the top row
	sec
	sbc @cnt		; .A = bottom - top
	bcc @done
	sec
	sbc @n			; .A = bottom - top - n
	bcc @done		; range is smaller than scroll amount
	sta @cnt		; # of rows to copy (-1)

	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	txa
	clc
	adc @n
	tax
	lda __screen_rowslo,x
	sta @src
	lda __screen_rowshi,x
	sta @src+1

	ldx @cnt
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

:	dex
	bpl @l0
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
	sty @offset

	; .X is the last row to scroll INTO. Work from the last destination
	; down so no source row is overwritten before it is copied, and so
	; nothing below .X is ever touched. The first source is (last-offset).
	txa
	sec
	sbc @offset
	bcc @done		; offset > last: nothing fits
	cmp @rowstart
	bcc @done		; range smaller than the scroll amount
	tax

@l0:	lda __screen_rowslo,x
	sta @src
	lda __screen_rowshi,x
	sta @src+1
	txa
	clc
	adc @offset
	tay			; dst = src + offset (always <= last)
	lda __screen_rowslo,y
	sta @dst
	lda __screen_rowshi,y
	sta @dst+1

	ldy #NUM_COLS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	dex		; decrement row counter
	bmi @done
	cpx @rowstart
	bcs @l0

@done:	rts
.endproc

;*******************************************************************************
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
	pla
	jsr asc2scr

	ldy zp::curx
	sta (@dst),y
	rts
.endproc

;*******************************************************************************
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
@src   = zp::text
@dst   = zp::text+2
@color = zp::text+4
	stxy @src

	tax
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	; get the "color" for the row
	lda mem::rowcolors,x
	sta @color

	ldy __text_puts_start
	cpy __text_puts_stop
	bcs @done		; empty window -> nothing to draw
@l0:	lda (@src),y
	jsr asc2scr
	; check if we need to reverse
	ldx @color
	cpx prefs::normal_color
	beq :+
	ora #$80
:	sta (@dst),y
	iny
	cpy __text_puts_stop
	bcc @l0

@done:	rts
.endproc

;*******************************************************************************
; SHL
.export __scr_shl
.proc __scr_shl
.endproc

;*******************************************************************************
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
	cmp #$5a
	bne :+
:	stx @savex
	cmp #$ff
	beq @done
	cmp #$40
	bne :+
	lda #$00
	rts

:	cmp #$5f		; underscore ($5f) is the back-arrow key
	bne :+
	lda #$64		; render it as an underscore-like glyph
	rts

:	ldx #$ff
:	inx
	cmp @convtab,x
	bcs :-
	;clc
	adc @offset,x

	ldx @savex
@done:	rts

.PUSHSEG
; must be in always-visible RAM: called from banked code on the cart build
.segment "DATA"
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

; must be in always-visible RAM: called from banked code on the cart build
.segment "DATA"
;*******************************************************************************
.linecont +
.define rows \
	SCREEN_ADDR+$00, \
	SCREEN_ADDR+$28, \
	SCREEN_ADDR+$50, \
	SCREEN_ADDR+$78, \
	SCREEN_ADDR+$a0, \
	SCREEN_ADDR+$c8, \
	SCREEN_ADDR+$f0, \
	SCREEN_ADDR+$118, \
	SCREEN_ADDR+$140, \
	SCREEN_ADDR+$168, \
	SCREEN_ADDR+$190, \
	SCREEN_ADDR+$1b8, \
	SCREEN_ADDR+$1e0, \
	SCREEN_ADDR+$208, \
	SCREEN_ADDR+$230, \
	SCREEN_ADDR+$258, \
	SCREEN_ADDR+$280, \
	SCREEN_ADDR+$2a8, \
	SCREEN_ADDR+$2d0, \
	SCREEN_ADDR+$2f8, \
	SCREEN_ADDR+$320, \
	SCREEN_ADDR+$348, \
	SCREEN_ADDR+$370, \
	SCREEN_ADDR+$398, \
	SCREEN_ADDR+$3c0
.export __screen_rowslo
.export __screen_rowshi
__screen_rowslo: .lobytes rows
__screen_rowshi: .hibytes rows

.define crows \
	COLMEM_ADDR+$00, \
	COLMEM_ADDR+$28, \
	COLMEM_ADDR+$50, \
	COLMEM_ADDR+$78, \
	COLMEM_ADDR+$a0, \
	COLMEM_ADDR+$c8, \
	COLMEM_ADDR+$f0, \
	COLMEM_ADDR+$118, \
	COLMEM_ADDR+$140, \
	COLMEM_ADDR+$168, \
	COLMEM_ADDR+$190, \
	COLMEM_ADDR+$1b8, \
	COLMEM_ADDR+$1e0, \
	COLMEM_ADDR+$208, \
	COLMEM_ADDR+$230, \
	COLMEM_ADDR+$258, \
	COLMEM_ADDR+$280, \
	COLMEM_ADDR+$2a8, \
	COLMEM_ADDR+$2d0, \
	COLMEM_ADDR+$2f8, \
	COLMEM_ADDR+$320, \
	COLMEM_ADDR+$348, \
	COLMEM_ADDR+$370, \
	COLMEM_ADDR+$398, \
	COLMEM_ADDR+$3c0
.linecont -
.export __screen_crowslo
.export __screen_crowshi
__screen_crowslo: .lobytes crows
__screen_crowshi: .hibytes crows
