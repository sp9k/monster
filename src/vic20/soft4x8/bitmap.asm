;*******************************************************************************
; BITMAP.ASM
; This file contains procedures for initializing and writing to the the bitmap
; display.  The bitmap is configured as 20 columns of 12 rows of double-height
; user-defined characters for a total of 40 columns and 24 rows of 4x8
; characters.
; This configuration is popularly known as MINIGRAFIK, created by Mike
;*******************************************************************************

.include "layout.inc"
.include "../expansion.inc"
.include "../prefs.inc"
.include "../fastcopy.inc"
.include "../../draw.inc"
.include "../../irq.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../ram.inc"
.include "../../settings.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

MAX_SHIFT = NUM_COLS

;*******************************************************************************
; CONSTANTS
BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

SCREEN_ADDR = $1000
PHYS_COLS   = 21	; number of PHYSICAL columns (breakpoints use one)
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

;*******************************************************************************
.BSS
blank_backup: .res 16
blanked:      .byte 0	; !0 if the screen is currently blanked

.CODE

;*******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer and reinitializes the screen. It may
; then be restored with a call to scr::restore
.export __screen_save
.proc __screen_save
	jsr __screen_savebuf
	; fall through to __screen_init
.endproc

;*******************************************************************************
; INIT
; Initializes the screen layout
.export __screen_init
.proc __screen_init
@dst=r0
@col=r2
	ldy #$00
	sty @dst
	ldx #$10		; $10 is also our initial value for bitmap char
	stx @dst+1
	stx @col

@l0:	; column 0 is character $0f for first 10 rows
	; on last row, it switches to $00, which will be dynamically modified
	; and also used for breakpoint rendering
	lda #$0f
	sta (@dst),y
	iny

	; set up one row of the bitmap
	lda @col
	ldx #PHYS_COLS-1
@l1:	sta (@dst),y
	clc
	adc #$0c
	iny
	dex
	bne @l1

	inc @col
	cpy #PHYS_COLS*SCREEN_ROWS-1
	bcc @l0

	; set leftmost column of last row to the blank character
	jsr set_bp_cell

	; clear the aux character
	ldx #$10
	lda #$55
:	sta $10f0-1,x
	dex
	bne :-

	; configure VIC registers
	ldy #$05
@l2:	clc
	lda $ede4,y
	adc inittab,y
	sta $9000,y
	dey
	bpl @l2

	; set aux color
	lda #$02<<4
	sta $900e

	; set border/bg color
	lda prefs::normal_color
	sta $900f
	rts
.endproc

;*******************************************************************************
; SAVEBUF
; Saves the bitmap + per-row colors to the backup buffer WITHOUT reinitializing
; the screen.  Use this to overlay another full-screen view that shares the same
; layout (e.g. the monitor). Reinitializing would rewrite the breakpoint
; column's shared glyphs ($1000/$10f0) and contend with the active IRQ.
.export __screen_savebuf
.proc __screen_savebuf
	CALL FINAL_BANK_VSCREEN, save

	; save colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_idx,x
	sta mem::rowcolors_save,x
	dex
	bpl :-

	; fall through to __screen_clr_row_colors
.endproc

;*******************************************************************************
; CLR ROW COLORS
; Clears all "row" colors by restoring their "normal" color
.export __screen_clr_row_colors
.proc __screen_clr_row_colors
	ldx #SCREEN_ROWS*2-1
:	lda prefs::normal_color
	sta mem::rowcolors,x
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; UNBLANK
; Ends a "blank"; call when sensitive IRQ disabled work has finished
.export __screen_unblank
.proc __screen_unblank
	lda blanked
	bne :+
	rts
:	lda #$00
	sta blanked

	jsr irq::on

	lda #$0f
	jsr set_col0
	jsr set_bp_cell		; reset the last row's breakpoint cell

	; restore the final character from the backup (the last half is simply
	; cleared because we clobber if with the blank message)
	ldx #15
:	lda #$55
	sta $10f0,x
	lda blank_backup,x
	sta $1ff0,x
	dex
	bpl :-

	; reset the final character on the screen to the text color
	lda prefs::text_color
	sta COLMEM_ADDR+(PHYS_COLS*SCREEN_ROWS)-1

	rts
.endproc

;*******************************************************************************
; DRAW GUTTER
; No-op in the soft4x8 bitmap mode: breakpoints are rendered in the leftmost
; bitmap column via the raster IRQ (DYNAMIC_CHAR), not a text gutter (see the
; hard8x8 port).  Provided so the screen interface is uniform across targets.
.export __screen_draw_gutter
.export __screen_draw_gutter_row
__screen_draw_gutter:
__screen_draw_gutter_row:
	rts

;*******************************************************************************
; BLANK
; Simplifies the screen to avoid artifacts when the IRQ is disabled
; IN:
;   - .XY: a message to display while the screen is "blanked"
.export __screen_blank
.proc __screen_blank
	lda blanked
	beq :+
	jmp hide_gutter_glyph	; already blanked; just re-assert the glyph
:	inc blanked

	lda #$82
:	cmp $9004
	bne :-

	jsr irq::off

	; restore screen row 0
	lda #$0f
	sta $1000
	lda #$10
	ldx #$00
	clc
:	sta $1001,x
	adc #$0c
	inx
	cpx #NUM_COLS
	bcc :-

	; Restore the screen codes for the cells that share character $0f's
	; glyph ($10f0-$10fb, the right half of the last row); the row IRQ
	; normally rewrites them just before the last row is drawn and is about
	; to stop running.  The carry MUST be clear here: the row-0 loop above
	; leaves it set, which shifts every code but the first by one (so those
	; cells display row 0's bitmap) and drops the last cell entirely.
	lda #$7b
	ldx #$00
	clc
:	sta $10f0,x
	inx
	adc #$0c
	bcc :-

	; back up the final character; the gutter borrows it while blanked
	ldx #15
:	lda $1ff0,x
	sta blank_backup,x
	dex
	bpl :-

	jsr hide_gutter_glyph

	; set the leftmost column to the final character ($ff)
	lda #$ff

	; fall through to set_col0
.endproc

;*******************************************************************************
; SET COL 0
; Sets the physical screen column 0 so that all characters are the given
; value
.proc set_col0
@scr=r0
@val=r2
	sta @val
	lda #>SCREEN_ADDR
	sta @scr+1
	ldy #$00
	sty @scr

	ldx #SCREEN_ROWS
@l0:	lda @val
	sta (@scr),y
	tya
	clc
	adc #PHYS_COLS
	tay
	dex
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; SET BP CELL
; Sets the last row's leftmost (breakpoint) cell at the blank char ($0f), whose
; glyph $10f0 is left as $55,$55,... by init/unblank).  This is the default
; state whenever the IRQ is NOT running.
.proc set_bp_cell
	lda #$0f
	sta $10e7
	rts
.endproc

;*******************************************************************************
; HIDE GUTTER GLYPH
; Fills character $ff, which the gutter column borrows while the screen is
; blanked, with the "empty" pattern ($55, all border color in multicolor) and
; hides it in the bottom-right cell of the screen, which displays that same
; character.  That cell is colored with the background color that irq::off
; leaves in $900f, so the pattern resolves to the background color whatever
; palette is in use (and whichever way $900f's reverse bit points) instead of
; showing a dither.  Colors 8-15 are background-only, so mask down to their
; low-intensity twin rather than flipping the cell into multicolor.
; Anything that draws over the bottom two text rows while blanked (a second
; blank message, for example) writes through to this glyph, so re-run this
; afterwards or the gutter column will show whatever was drawn there.
.proc hide_gutter_glyph
	lda prefs::normal_color
	lsr
	lsr
	lsr
	lsr
	and #$07
	sta COLMEM_ADDR+(PHYS_COLS*SCREEN_ROWS)-1

	lda #$55
	ldx #15
:	sta $1ff0,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; CLR
; Clears the screen
.export __screen_clr
.proc __screen_clr
@bm=r0
	lda #$11
	sta @bm+1
	lda #$00
	tay
	sta @bm

:	sta (@bm),y
	iny
	bne :-
	inc @bm+1
	ldx @bm+1
	cpx #>$2000
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
	ldy #PHYS_COLS*SCREEN_ROWS
	ldx #PHYS_COLS
@l0:    lda prefs::text_color
	sta COLMEM_ADDR-1,y
	ora #$80
	dex
	bne :+
	; column 0, use multicolor mode
	ora #$08
	ldx #PHYS_COLS
:	sta COLMEM_ADDR-1,y
        dey
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

	ldx #(SCREEN_WIDTH/2)
@l0:	ldy #$07
	lda #$00
@l1:	sta (@dst),y
	dey
	bpl @l1
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

;*******************************************************************************
; RVSLINE PART
; Reverses the characters (8 pixels high) in columns [.Y, .X) of the given row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
;  - .Y: the first column to reverse
;  - .X: one past the last column to reverse
.export __screen_rvsline_part
.proc __screen_rvsline_part
@dst=r0
@odd=r2		; !0 if the character to end at is odd
@start=r3
@stop2=r4
@start2=r5
	asl
	asl
	asl
	cpx #40
	bcc :+
	ldx #40

:	; swap Y and X if (X < Y)
	sty @start
	cpx @start
	pha		; save character row

	bcs :+		; if stop > start, swap
	txa
	ldx @start
	tay

:	sty @start2
	stx @stop2
	lda #$00
	sta @odd

	; check whether the start column is even/odd
	tya
	lsr
	sta @start
	ror @odd

	; get the first column to reverse
	tay
	pla
	adc __screen_columnslo,y
	sta @dst
	lda __screen_columnshi,y
	adc #$00
	sta @dst+1

	lda @odd
	beq @cont

@odd0:	; reverse right half of the first column
	ldy #$07
@col0:	lda (@dst),y
	eor #$0f
	sta (@dst),y
	dey
	bpl @col0

	; move to next column
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	inc @start	; first column is done
	inc @start2

	lda @start2
	cmp @stop2
	beq @done

@cont:	; divide character # by 2 to get bitmap column
	txa
	lsr
	tax

	; check if end column is even or odd
	lda #$00
	rol
	sta @odd

	cpx @start
	beq @lastcol
	bcc @done

@l0:	ldy #$07
@l1: 	lda (@dst),y
	eor #$ff
	sta (@dst),y
	dey
	bpl @l1
	lda @dst
	clc
	adc #$c0
	sta @dst
	lda @dst+1
	adc #$00
	sta @dst+1
	dex
	cpx @start
	bne @l0

	; check if we need to do the odd column
	lda @odd
	beq @done

@lastcol:
	; reverse half of the last column
	ldy #$07
@l2:	lda (@dst),y
	eor #$f0
	sta (@dst),y
	dey
	bpl @l2

@done:	rts
.endproc

;*******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
@buff=r0
@bm=r2
	CALL FINAL_BANK_VSCREEN, restore

	; restore the per-row colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors_idx,x
	dex
	bpl :-
	jmp draw::refresh_colors
.endproc

;*******************************************************************************
; RESTORE ROW
; Restores a single character row of the bitmap from the backup buffer.
; IN:
;  - .A: the character row to restore
.export __screen_restore_row
.proc __screen_restore_row
	CALL FINAL_BANK_VSCREEN, restore_row
	rts
.endproc

;*******************************************************************************
; SAVE ROW
; Saves a single character row of the bitmap to the backup buffer.
; IN:
;  - .A: the character row to save
.export __screen_save_row
.proc __screen_save_row
	CALL FINAL_BANK_VSCREEN, save_row
	rts
.endproc

;*******************************************************************************
; CHAR ADDR
; Returns the bitmap address for the "character row" of the given row.
; Characters are 8 pixels tall, so this is BITMAP_ADDR+(8*row) where row is
; the provided row.
; IN:
;  - .A: the character row to get the bitmap address of
; OUT:
;  - .XY: the bitmap address
.export __screen_char_addr
.proc __screen_char_addr
	asl
	asl
	asl
	adc #<BITMAP_ADDR
	tax
	ldy #>BITMAP_ADDR
	rts
.endproc

.RODATA
;*******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

.export __screen_columnslo
.export __screen_columnshi
__screen_columnslo: .lobytes cols
__screen_columnshi: .hibytes cols

inittab:	.byte $00	; +$0c (PAL) +$05 (NTSC) $9000
		.byte $fe	; +$26 (PAL) +$19 (NTSC) $9001
		.byte $ff	; +$16                   $9002
		.byte $eb	; +$2e                   $9003
		.byte $00	; +$00                   $9004
		.byte $0c	; +$c0                   $9005

inittab_blank:	.byte $02,$fe,$fe,$eb,$00,$0c

.segment "VSCREEN"

;*******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer. It may then be restored with a call
; to scr::restore
.proc save
@buff=r0
@bm=r2
	ldxy #backbuff
	stxy @buff

	lda #>BITMAP_ADDR
	sta @bm+1
	ldy #$00
	sty @bm

	; save bitmap to back-buffer
:	lda (@bm),y
	sta (@buff),y
	iny
	bne :-
	inc @bm+1
	inc @buff+1
	lda @bm+1
	cmp #>$2000
	bne :-
	rts
.endproc

;*******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; to scr::restore
.proc restore
@buff=r0
@bm=r2
	ldxy #backbuff
	stxy @buff
	ldxy #$1100
	stxy @bm

	; restore screen from the backbuff
	ldy #$00
:	lda (@buff),y
	sta (@bm),y
	iny
	bne :-
	inc @bm+1
	inc @buff+1
	lda @bm+1
	cmp #>$2000
	bne :-
	rts
.endproc

;*******************************************************************************
; ROW PTRS
; Computes the bitmap and backup buffer addresses for the given character row.
; NOTE: this code runs in the VSCREEN bank, so it must not reference any
; data outside of this segment (e.g. the column LUTs)
; IN:
;  - .A: the character row
; OUT:
;  - r0: the address of the row in the backup buffer
;  - r2: the address of the row in the bitmap (first column)
.proc row_ptrs
@buff=r0
@bm=r2
	; @bm = address of the row in the first bitmap column
	; (BITMAP_ADDR + row*8; BITMAP_ADDR is page aligned)
	asl
	asl
	asl
	sta @bm
	lda #>BITMAP_ADDR
	sta @bm+1

	; @buff = backbuff + (@bm - BITMAP_ADDR)
	lda @bm
	clc
	adc #<(backbuff-BITMAP_ADDR)
	sta @buff
	lda @bm+1
	adc #>(backbuff-BITMAP_ADDR)
	sta @buff+1
	rts
.endproc

;*******************************************************************************
; RESTORE ROW
; Restores a single character row of the bitmap from the backup buffer.
; IN:
;  - .A: the character row to restore
.proc restore_row
@buff=r0
@bm=r2
	jsr row_ptrs

	ldx #NUM_COLS
@l0:	ldy #$07
:	lda (@buff),y
	sta (@bm),y
	dey
	bpl :-

	; move both pointers to the next column ($c0 bytes per column)
	lda @bm
	clc
	adc #$c0
	sta @bm
	bcc :+
	inc @bm+1
:	lda @buff
	clc
	adc #$c0
	sta @buff
	bcc :+
	inc @buff+1
:	dex
	bne @l0
	rts
.endproc

;*******************************************************************************
; SAVE ROW
; Saves a single character row of the bitmap to the backup buffer.
; IN:
;  - .A: the character row to save
.proc save_row
@buff=r0
@bm=r2
	jsr row_ptrs

	ldx #NUM_COLS
@l0:	ldy #$07
:	lda (@bm),y
	sta (@buff),y
	dey
	bpl :-

	; move both pointers to the next column ($c0 bytes per column)
	lda @bm
	clc
	adc #$c0
	sta @bm
	bcc :+
	inc @bm+1
:	lda @buff
	clc
	adc #$c0
	sta @buff
	bcc :+
	inc @buff+1
:	dex
	bne @l0
	rts
.endproc

;*******************************************************************************
; SHL
; Shifts the CHARACTER data of the screen to the left.
; See SHR for documentation
; The bottom 2 rows are NOT shifted
.export __scr_shl
.proc __scr_shl
@src=r0
@dst=r2
	lda shiftamount
	cmp #MAX_SHIFT
	bcs @done

	ldxy #SCREEN_ADDR
	stxy @dst
	inx
	stxy @src

	ldx #NUM_ROWS

@l0:	ldy #$00
	lda (@dst),y
	pha

@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #NUM_COLS-1
	bne @l1

	; wrap character at column 0 to last column
	pla
	sta (@dst),y

	lda @dst
	adc #NUM_COLS-1	; .C always set
	sta @dst
	sta @src
	inc @src	; src = dst+1

	dex
	bne @l0

	inc shiftamount
@done:	rts
.endproc

;*******************************************************************************
; SHR
; Shifts the CHARACTER data of the screen to the right.  This means that the
; bitmap addresses for each column will shift by $c0
; So the default bitmap address arrangement:
; | $1100 | $11c0 |  ...  |
; will now be:
; | $1f40 | $1100 | $11c0 |  ...  |
; after the shift
; The bottom 2 character rows are NOT shifted
.export __scr_shr
.proc __scr_shr
@src=r0
@dst=r2
	lda shiftamount
	cmp #$01
	beq @done

	ldxy #SCREEN_ADDR
	stxy @src
	inx
	stxy @dst

	ldx #NUM_ROWS

@l0:	ldy #NUM_COLS-2
	lda (@dst),y	; save rightmost char
	pha

@l1:	lda (@src),y	; get char to shift
	sta (@dst),y	; shift it right
	dey
	bpl @l1

	; last character: wrap around
	iny
	pla		; get rightmost char
	sta (@src),y	; store it in leftmost position

	; move to the next row
	lda @src
	clc
	adc #NUM_COLS
	sta @src
	sta @dst
	inc @dst

	dex
	bne @l0

;--------------------------------------
; now copy the bitmap data in
	; get column of bitmap data to copy's address
	ldx shiftamount
	lda vcolumnslo,x
	sta @src
	lda vcolumnshi,x
	sta @src+1

	; get address to copy bitmap column to
	dec shiftamount
	ldx shiftamount
	lda bm_columnslo,x
	sta @dst
	lda bm_columnshi,x
	sta @dst+1

	; copy the column in
	ldy #NUM_ROWS*16-1
:	lda #$ff ;(@src),y
	sta (@dst),y
	dey
	bne :-

	; copy the last byte
	lda #$ff ;(@src),y
	sta (@dst),y

@done:	rts
.endproc

shiftamount: .byte NUM_COLS	; number of columns the screen is shifted

; these pointers are one less than the real addresses they reference
bmptr:	.word BITMAP_ADDR-1

vcolumnslo:
.repeat (VSCREEN_WIDTH/2), i
	.byte <(screen + ($c0*i))
.endrepeat

vcolumnshi:
.repeat VSCREEN_WIDTH/2, i
	.byte >(screen + ($c0*i))
.endrepeat

.linecont +
.define vcols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40, \
$1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

bm_columnslo: .lobytes vcols
bm_columnshi: .hibytes vcols

;*******************************************************************************
; RESTORE
; Initializes the screen using shiftamount to determine the layout
; IN:
;  - .A: the number of columns to shift the screen
;.proc restore
;@scr=r0
;@row=r2
;	lda #$00
;	sta @row
;	ldxy #SCREEN_ADDR
;	stxy @scr
;
;@l0:	lda shiftamount
;	beq @done
;	lda #NUM_COLS
;	sec
;	sbc shiftamount
;	tay
;
;	lda #$10
;	clc
;	adc @row
;
;	ldx #NUM_COLS
;@l1:	sta (@scr),y
;	iny
;	cpy #NUM_COLS
;	bcc :+
;	ldy #$00
;:	clc
;	adc #SCREEN_ROWS
;	dex
;	bne @l1
;
;	; move to next screen row
;	lda @scr
;	clc
;	adc #NUM_COLS
;	sta @scr
;
;	inc @row
;	lda @row
;	cmp #NUM_ROWS
;	bne @l0
;@done:	rts
;.endproc

.segment "VSCREEN_BSS"
;*******************************************************************************
; SCREEN
; The "virtual" screen.  This is a continuation of the bitmap at address $1000
screen:   .res $2000	; bitmap for 200 columns
backbuff: .res $f00	; backup for 1 bitmap screen
