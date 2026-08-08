;*******************************************************************************
; SCREEN23.ASM
;*******************************************************************************

.include "layout.inc"
.include "../fastcopy.inc"
.include "../prefs.inc"
.include "../../config.inc"
.include "../../irq.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../ram.inc"
.include "../../settings.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

.import __vscreen_save
.import __vscreen_restore

;*******************************************************************************
; CONSTANTS
COLMEM_ADDR = $9400	; page 1
;COLMEM_ADDR2 = $9600	; page 2

SCREEN_ADDR = $1800	; page 1

; offset from a screen cell to its color-RAM cell (low byte is 0)
COLMEM_OFFSET = COLMEM_ADDR - SCREEN_ADDR

.define NUM_COLS 22	; number of 8-pixel columns
.define NUM_ROWS 23	; number of 8-pixel rows

; address of "virtual screen"
VSCREEN_ADDR = $1a00
VSCREEN_W = 40

SCREEN_ROWS = 12	; number of physical rows per column

;*******************************************************************************
HL_MARKER       = $3e		; '>' current-line indicator glyph
HL_MARKER_COLOR = TEXT_COLOR	; match the editor text color


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
; INIT
.export __screen_init
.proc __screen_init
	jsr $e5c3		; set up base screen matrix

	lda #$e2		; lowercase chars / screen @ $1800
	sta $9005

	lda #PHYS_COLS		; $17: bit 7 clear + 23 columns
	sta $9002
	lda #$03		; horizontal pos
	sta $9000

	lda prefs::normal_color
	sta $900f

	jmp __screen_draw_gutter
.endproc

.CODE
;*******************************************************************************
; CLR
; Clears the screen's content columns.
.export __screen_clr
.proc __screen_clr
@dst=r0
	ldx #NUM_ROWS-1
@l0:	lda __screen_rowslo,x	; content start (physical column CONTENT_COL)
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	lda #$20
	ldy #NUM_COLS-1
:	sta (@dst),y
	dey
	bpl :-
	dex
	bpl @l0

	; fall through to clrcolor
.endproc

;*******************************************************************************
; CLRCOLOR
; Reverts all color memory to the given color
; IN:
;  - .A: the color to fill the screen with (currently ignored; uses TEXT_COLOR)
.export __screen_clrcolor
.proc __screen_clrcolor
@dst=r0
	lda #$00
	sta mem::blink_active		; stop any gutter blink
	ldx #NUM_ROWS-1
@l0:	lda #$00
	sta mem::highlight_rows,x	; drop the current-line marker

	; color cell = content screen cell + (COLMEM_ADDR - SCREEN_ADDR)
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	clc
	adc #>COLMEM_OFFSET
	sta @dst+1

	lda #TEXT_COLOR
	ldy #NUM_COLS-1
:	sta (@dst),y
	dey
	bpl :-
	dex
	bpl @l0
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
	cpy #NUM_COLS+1		; clamp stop to the screen width
	bcc :+
	ldy #NUM_COLS
:	sty @stop
	ldy @start
@l0:	cpy @stop		; toggle columns [start, stop) ascending
	bcs @done		; y >= stop -> done (handles single/empty ranges)
	lda (@dst),y
	eor #$80
	sta (@dst),y
	iny
	bne @l0			; branch always (y < NUM_COLS < 256)
@done:	rts
.endproc

;*******************************************************************************
; BLANK
; Prepares the screen for sensitive work that requires the IRQ to be disabled.
; The per-row color raster IRQ is turned off; to avoid leaving the screen in
; whatever color the last raster split happened to set, $900f is forced to the
; normal color so the whole screen is uniform while blanked.
.export __screen_blank
.proc __screen_blank
	jsr irq::off
	lda prefs::normal_color
	sta $900f

	; disable per-row gutter handling
	lda #$00
	sta mem::coloron
	jmp __screen_draw_gutter
.endproc

;*******************************************************************************
; UNBLANK
; Ends a "blank"; call when sensitive IRQ disabled work has finished
.export __screen_unblank
.proc __screen_unblank
	jsr irq::on
	lda #$01
	sta mem::coloron		; restore per-row color in IRQ
	jmp __screen_draw_gutter	; restore reverse-aware gutter handling
.endproc

;*******************************************************************************
; SAVE
; Saves the screen to the backup buffer and reinitializes it. It may then be
; restored with a call to scr::restore
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
	; back up the character matrix to the off-screen VSCREEN buffer
	CALL FINAL_BANK_VSCREEN, __vscreen_save

	; save the per-row colors and reset them to the default
	ldx #NUM_ROWS-1
:	lda mem::rowcolors,x
	sta mem::rowcolors_save,x
	lda prefs::normal_color
	sta mem::rowcolors,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; RESTORE
; Restores screen from the the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
	; restore the character matrix from the off-screen VSCREEN buffer
	CALL FINAL_BANK_VSCREEN, __vscreen_restore

	; restore the per-row colors
	ldx #NUM_ROWS-1
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
BRK_NONE = $a0			; reverse space (blends into border)
BRK_OFF  = $02			; lowercase b (disabled breakpoint)
BRK_ON   = $42			; capital 'B' (enabled breakpoint)

GUTTER_BG_COLOR  = BORDER_COLOR & $07	; border color
BRK_OFF_COLOR    = $02			; red
BRK_ON_COLOR     = $02			; red

;*******************************************************************************
; DRAW GUTTER ROW
; Redraws the breakpoint gutter cell (physical column 0) for a single row from
; mem::breakpoint_rows. The gutter glyph is XORed with $80 on reverse-cleared
; rows so it renders uniformly whether the line is "reversed"
; or not
; IN:
;  - .X: the row whose gutter cell to redraw
; PRESERVES: .A, .X
.export __screen_draw_gutter_row
.proc __screen_draw_gutter_row
@dst=r0
@col=r2
@row=r4
@glyph=r5
	pha
	stx @row

	; physical column 0 of row .X is CONTENT_COL to the left of its content
	lda __screen_rowslo,x
	sec
	sbc #CONTENT_COL
	sta @dst
	sta @col
	lda __screen_rowshi,x
	sbc #$00
	sta @dst+1
	clc
	adc #>COLMEM_OFFSET	; color cell = screen cell + $7c00 (low byte 0)
	sta @col+1

	; select the gutter contents for this row
	ldy mem::breakpoint_rows,x
	lda mem::highlight_rows,x
	beq @chk_brk			; not highlighted row, check breakpoints
	tya				; .Y == 0? (breakpoint active)?
	beq @markeronly			; no breakpoint -> display '>' only

;-------------------------------------------------------------------------------
; both breakpoint and highlight: cell alternates between the breakpoint glyph
; and the '>' one.
	lda #$00
	sta mem::blink_active		; disable blinking temporarily

	lda gutter_glyphs,y
	eor #HL_MARKER
	sta mem::blink_dchar		; glyph delta (marker <-> breakpoint)

	lda prefs::normal_color			; default to border color
	cpy #$00
	beq :+
	lda gutter_colors,y
:	eor prefs::text_color
	sta mem::blink_dclr		; color delta

	lda @dst
	sta mem::blink_cell
	lda @dst+1
	sta mem::blink_cell+1

	lda @row
	sta mem::blink_row

;-------------------------------------------------------------------------------
; > or b/B
; draw '>' or 'b/B' based on current blink phase
	lda mem::blink_phase
	bne @markeronly			; phase 1 -> '>' marker

;-------------------------------------------------------------------------------
; b/B
@brkpt:
	lda gutter_glyphs,y		; phase 0 -> breakpoint glyph
	sta @glyph

	lda prefs::normal_color
	cpy #$00
	beq @setcolor
	lda gutter_colors,y
	jmp @setcolor

;-------------------------------------------------------------------------------
; >
@markeronly:
	lda #HL_MARKER
	sta @glyph
	lda #HL_MARKER_COLOR
	jmp @setcolor

;-------------------------------------------------------------------------------
; b/B or nothing
@chk_brk:
	; breakpoint (or empty) cell
	lda gutter_glyphs,y
	sta @glyph
	tya
	bne @brkclr			; breakpoint set -> use its color
	lda prefs::normal_color		; empty cell     -> border color
	and #$07			; use just border color (low 3 bits)
	jmp @setcolor
@brkclr:
	lda gutter_colors,y

;-------------------------------------------------------------------------------
@setcolor:
	ldy #$00
	sta (@col),y

	; when per-row color is off (during blanking) skip gutter reverse check
	lda mem::coloron
	beq :+

	; if the row is already reversed, XOR glyph to unreverse it
	ldx @row
	lda mem::rowcolors,x
	and #$08
	bne :+
	lda @glyph
	eor #$80
	sta @glyph
:	lda @glyph
	ldy #$00
	sta (@dst),y

	; check if we need to (re)enable blinking in the IRQ to handle multiple
	; glyphs in the gutter column
	ldx @row
	lda mem::highlight_rows,x
	beq @rts
	lda mem::breakpoint_rows,x
	beq @rts
	lda #$01
	sta mem::blink_active

@rts:	pla			; .X already restored to @row by the ldx above
	rts
.endproc

;*******************************************************************************
; gutter glyphs and colors, indexed by mem::breakpoint_rows (0/1/2)
gutter_glyphs: .byte BRK_NONE,        BRK_OFF,       BRK_ON
gutter_colors: .byte GUTTER_BG_COLOR, BRK_OFF_COLOR, BRK_ON_COLOR

;*******************************************************************************
; DRAW GUTTER
; Redraws the breakpoint gutter (physical column 0) for every content row from
; mem::breakpoint_rows.
.export __screen_draw_gutter
.proc __screen_draw_gutter
	ldx #NUM_ROWS-1
@l0:	jsr __screen_draw_gutter_row
	dex
	bpl @l0
	rts
.endproc

;*******************************************************************************
; SET GUTTER MARKER
; Marks the given row as the debugger's current line and repaints only the cells
; that change.  The marker is stored in mem::highlight_rows (one flag per row) so
; it scrolls with the screen alongside breakpoint_rows, and is rendered by
; draw_gutter_row.
; IN:
;  - .A: the row to mark as the current line ($ff to clear)
.export __screen_set_gutter_marker
.proc __screen_set_gutter_marker
	pha			; save target row

	; find currently-marked row (only one is ever marked), clear it and
	; restore underlying breakpoint/empty glyph
	ldx #NUM_ROWS-1
@find:	lda mem::highlight_rows,x
	bne @clrold
	dex
	bpl @find
	bmi @setnew			; nothing marked

@clrold:
	lda #$00
	sta mem::highlight_rows,x	; turn off the old highlight
	jsr __screen_draw_gutter_row	; restore underlying cell

@setnew:
	pla
	cmp #NUM_ROWS
	bcs @done			; $out of range -> leave marker cleared
	tax
	lda #$01
	sta mem::highlight_rows,x
	jmp __screen_draw_gutter_row	; draw new cell (.X = row)
@done:	rts
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
	adc #PHYS_COLS		; matrix row stride (content width + gutter)
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #PHYS_COLS		; matrix row stride (content width + gutter)
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
	ldy zp::curx
	pla
	jsr asc2scr
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
@src = zp::text
@dst = zp::text+2
	stxy @src

	tax
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	; render exactly NUM_COLS characters into the row's content columns.
	; The row table points at CONTENT_COL, so this never touches the gutter
	; (physical column 0); reverse rows are handled by the per-row $900f
	; color in the IRQ, not here.
	ldy #$00
@l0:	lda (@src),y
	jsr asc2scr
	sta (@dst),y
	iny
	cpy #NUM_COLS
	bne @l0
	rts
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
; Each content row starts at physical column CONTENT_COL (1) of its matrix row,
; leaving physical column 0 (GUTTER_COL) as the breakpoint gutter.  The matrix
; row stride is PHYS_COLS (23).
.linecont +
.define rows \
	SCREEN_ADDR+PHYS_COLS*0+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*1+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*2+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*3+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*4+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*5+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*6+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*7+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*8+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*9+CONTENT_COL,  \
	SCREEN_ADDR+PHYS_COLS*10+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*11+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*12+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*13+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*14+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*15+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*16+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*17+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*18+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*19+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*20+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*21+CONTENT_COL, \
	SCREEN_ADDR+PHYS_COLS*22+CONTENT_COL
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

