;*******************************************************************************
; MEMCFG.ASM
; This file contains the code/UI to configure the BLK's to enable during
; free-run of the active user program.
;
; The configuration is a bitmask of enabled blocks (see MEMCFG_* in banks.inc)
; along with the UltiMem register values to enable them.
;*******************************************************************************

.include "banks.inc"
.include "../../alert.inc"
.include "../../border.inc"
.include "../../config.inc"
.include "../../key.inc"
.include "../../keycodes.inc"
.include "../../layout.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../ram.inc"
.include "../../runtime.inc"
.include "../../screen.inc"
.include "../../text.inc"
.include "../../zeropage.inc"

;*******************************************************************************
; GEOMETRY
; The window is drawn from the top of the screen down and is framed like the
; directory viewer's
MC_LCOL     = 0				; column of left border
MC_RCOL     = LINESIZE-1		; column of right border
MC_TEXT_COL = MC_LCOL+1			; column a row's text starts at
MC_TEXT_LEN = 20			; width of a block row's text

; the selectable rows: one per block, then the "apply" action
MEMCFG_NUM_ROWS = MEMCFG_NUM_BLOCKS+1
MC_APPLY        = MEMCFG_NUM_BLOCKS		; row index of the apply action

MC_TOP_ROW   = 0				; top border's row
MC_TITLE_ROW = MC_TOP_ROW+1			; title's row
MC_FIRST_ROW = MC_TITLE_ROW+1			; first selectable row
MC_BOT_ROW   = MC_FIRST_ROW+MEMCFG_NUM_ROWS	; bottom border's row

; column the checkbox's contents sit in
MC_CHECK_COL = MC_TEXT_COL+1

.assert MC_TEXT_COL+MC_TEXT_LEN <= MC_RCOL, error, "memory config rows are too wide"
.assert MC_BOT_ROW < SCREEN_HEIGHT, error, "memory config window doesn't fit"

.ifdef soft4x8
.assert (MC_LCOL .mod 2) = 0, error, "memory config must start on an even column"
.assert ((MC_RCOL+1) .mod 2) = 0, error, "memory config must end on an even column"
.endif

;*******************************************************************************
.DATA

;*******************************************************************************
; BLOCKS
; Bitmask of blocks that are mapped for the user's program.
.export __memcfg_blocks
__memcfg_blocks: .byte MEMCFG_ALL

;*******************************************************************************
; REG9FF1 / REG9FF2
; Virtual mirrors of the Ultimem registers.  Used to configure the Ultimem
; before entering a free-run
.export __memcfg_reg9ff1
__memcfg_reg9ff1: .byte $2b	; RAM123 r/w, IO2 and IO3 read-only
.export __memcfg_reg9ff2
__memcfg_reg9ff2: .byte $ff	; BLK 1/2/3/5 all RAM r/w

;*******************************************************************************
; ALERT STRINGS
; The confirmation modal text for the apply command
applymsg:    .byte "this will reinitialize basic",0
applyprompt: .byte "are you sure? (y/n)",0

;*******************************************************************************
; ROWBUF
; The row being composed
rowbuf = mem::spare

;*******************************************************************************
.segment "ULTICFG"

;*******************************************************************************
; APPLY
; Recomputes the UltiMem register values for the current block mask.
; Must be called after every change to memcfg::blocks.
.export __memcfg_apply
.proc __memcfg_apply
	lda __memcfg_blocks
	and #MEMCFG_RAM123
	beq :+
	lda #$03		; RAM123: RAM, read/write
:	ora #$28		; IO2, IO3: RAM, read-only (debugger NMI etc.)
	sta __memcfg_reg9ff1

	; $9ff2 holds one 2-bit field per block, BLK1 in the lowest.
	; %11 maps the block's RAM read/write, %00 leaves it unmapped
	ldx #$00
	ldy #$03		; start with BLK5
@l0:	txa
	asl
	asl			; make room for this block's field
	tax
	lda __memcfg_blocks
	and @blkbits,y
	beq :+
	inx
	inx
	inx			; %11: the block is mapped
:	dey
	bpl @l0
	stx __memcfg_reg9ff2
	rts
@blkbits: .byte MEMCFG_BLK1, MEMCFG_BLK2, MEMCFG_BLK3, MEMCFG_BLK5
.endproc

;*******************************************************************************
; SET BASIC PTRS
; Points the KERNAL's memory pointers (MEMSTR, MEMSIZ, and HIBASE) at the RAM
; that the configuration actually maps to
.export __memcfg_set_basic_ptrs
.proc __memcfg_set_basic_ptrs
	lda __memcfg_blocks
	and #MEMCFG_BLK1
	bne @expanded

@small:	; without block RAM the screen stays at $1e00 and BASIC ends below it
	ldx #$1e		; MEMSIZ = $1e00
	ldy #$10		; unexpanded: BASIC starts above the KERNAL's
	lda __memcfg_blocks	;   tables at $1000
	and #MEMCFG_RAM123
	beq :+
	ldy #$04		; +3K: BASIC starts at $0400
:	lda #$1e		; screen at $1e00
	bne @set		; branch always

@expanded:
	; with BLK1 mapped the screen moves down to $1000 and BASIC starts
	; above it
	ldy #$12		; BASIC starts at $1200
	ldx #$40		; BLK1 alone: MEMSIZ = $4000
	lda __memcfg_blocks
	and #MEMCFG_BLK2
	beq :+
	ldx #$60		; +BLK2 (contiguous): $6000
	lda __memcfg_blocks
	and #MEMCFG_BLK3
	beq :+
	ldx #$80		; +BLK3 (contiguous): $8000
:	lda #$10		; screen at $1000

@set:	sta $0288		; HIBASE: page the screen lives on
	lda #$00
	sta $0281
	sta $0283
	sty $0282		; MEMSTR: start of BASIC RAM
	stx $0284		; MEMSIZ: one past the end of BASIC RAM
	rts
.endproc

;*******************************************************************************
.CODE

;*******************************************************************************
; EDIT
; MAIN-bank entry point for the configuration window.
.export __memcfg_edit
.proc __memcfg_edit
	CALL FINAL_BANK_MEMCFG, edit
	bcc @done		; nothing to apply

	jsr scr::blank
	jsr run::clr
	jsr scr::unblank
@done:	rts
.endproc

.segment "MEMCFG"

;*******************************************************************************
; EDIT
; Displays the memory configuration and lets the user toggle the blocks that
; the user's program runs with.
; OUT:
;  - .C: set if the user asked for the configuration to be applied now
.proc edit
@select=r8
	lda #$00
	sta @select
	jsr drawwin

;-------------------------------------------------------------------------------
@key:	CALLMAIN key::waitch
	cmp #K_QUIT
	beq @exit
	cmp #K_WIN_CLOSE
	beq @exit
	cmp #K_RETURN
	beq @activate
	cmp #' '
	beq @activate

	CALLMAIN key::isup
	beq @up
	CALLMAIN key::isdown
	beq @down
	bne @key		; branch always

@up:	lda @select
	beq @key		; already at the first row
	jsr highlight		; un-highlight the row we're leaving
	dec @select
	jsr highlight
	jmp @key

@down:	lda @select
	cmp #MEMCFG_NUM_ROWS-1
	bcs @key		; already at the last row
	jsr highlight
	inc @select
	jsr highlight
	jmp @key

;-------------------------------------------------------------------------------
; RETURN/SPACE: toggle the selected block, or run the selected action
@activate:
	lda @select
	cmp #MC_APPLY
	beq @apply

	tax
	lda blockbits,x
	eor __memcfg_blocks
	sta __memcfg_blocks
	jsr __memcfg_apply

	lda @select
	jsr drawrow		; redraw the row with its new state
	jsr highlight		; and put the selection back on it
	jmp @key

;-------------------------------------------------------------------------------
; cold start BASIC to update its pointers with the new configuration
@apply:
	CALLMAIN scr::restore
	jsr confirm
	bcs @applynow

	jsr drawwin		; user declined; put the window back up
	jmp @key

@applynow:
	sec			; tell the caller to run the cold start
	rts

@exit:	CALLMAIN scr::restore
	clc
	rts
.endproc

;*******************************************************************************
; CONFIRM
; Warns the user about destructive apply and waits for an answer
; OUT:
;  - .C: set if the user confirmed
.proc confirm
	ldxy #applyprompt
	stxy alert::prompt
	ldxy #applymsg
	CALLMAIN alert::open

	CALLMAIN key::flush	; don't let a typed-ahead key answer this
@getch:	CALLMAIN key::waitch
	and #$df		; accept either case for 'y'/'n'
	cmp #$4e		; N
	beq @no
	cmp #K_QUIT
	beq @no			; STOP declines too
	cmp #$59		; Y
	bne @getch

	CALLMAIN alert::close
	sec			; confirmed
	rts

@no:	CALLMAIN alert::close
	clc
	rts
.endproc

;*******************************************************************************
; DRAWWIN
; Saves the screen the window is about to cover and draws the window on it
.proc drawwin
	CALLMAIN scr::save

	; the window is drawn at full width
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop

	; frame the window
	lda #MC_TOP_ROW
	ldx #BORDER_TL
	ldy #BORDER_TR
	jsr border

	lda #MC_BOT_ROW
	ldx #BORDER_BL
	ldy #BORDER_BR
	jsr border

	; draw the title and highlight it
	ldxy #title
	jsr buildrow
	lda #MC_TITLE_ROW
	jsr showrow
	ldy #MC_TEXT_COL
	ldx #MC_RCOL
	lda #MC_TITLE_ROW
	CALLMAIN scr::rvsline_part

	; draw every selectable row
	ldx #MEMCFG_NUM_ROWS-1
:	txa
	pha
	jsr drawrow
	pla
	tax
	dex
	bpl :-

	jmp highlight
.endproc

;*******************************************************************************
; HIGHLIGHT
; Reverses the text of the selected row.
.proc highlight
@select=r8
	ldy #MC_TEXT_COL
	ldx #MC_TEXT_COL+MC_TEXT_LEN
	lda @select
	clc
	adc #MC_FIRST_ROW
	CALLMAIN scr::rvsline_part
	rts
.endproc

;*******************************************************************************
; DRAW ROW
; Draws one of the window's selectable rows.  Block rows are checked or
; unchecked to match the configuration.
; IN:
;  - .A: the row's index
.proc drawrow
@row=r6
	sta @row
	tax

	; compose the row from its (fixed) description
	ldy rowtextshi,x
	lda rowtextslo,x
	tax
	jsr buildrow

	lda @row
	cmp #MC_APPLY
	bcs @show		; the apply row has no checkbox to fill in

	; check or clear the block's checkbox
	tax
	lda blockbits,x
	and __memcfg_blocks
	beq @off
	lda #'x'
	bne @check		; branch always
@off:	lda #' '
@check:	sta rowbuf+MC_CHECK_COL

@show:	lda @row
	clc
	adc #MC_FIRST_ROW
	; fall through to showrow
.endproc

;*******************************************************************************
; SHOWROW
; Draws rowbuf on the given row
; IN:
;  - .A: the row to draw it at
.proc showrow
	ldxy #rowbuf
	CALLMAIN text::puts
	rts
.endproc

;*******************************************************************************
; BORDER
; Builds and draws one of the window's horizontal borders
; IN:
;  - .A: row to draw the border at
;  - .X: character to draw in the leftmost column
;  - .Y: character to draw in the rightmost column
.proc border
	pha			; save the row
	tya
	pha			; and the characters for both corners
	txa
	pha

	jsr blankrow

	lda #BORDER_HBAR
	ldx #MC_RCOL-1
:	sta rowbuf,x
	dex
	bne :-			; stop at MC_LCOL; the corner goes there

	pla			; restore left corner char
	sta rowbuf+MC_LCOL
	pla			; restore right corner char
	sta rowbuf+MC_RCOL

	pla			; restore the row
	jmp showrow
.endproc

;*******************************************************************************
; BUILDROW
; Composes the given string into rowbuf between the window's borders
; IN:
;  - .XY: the string to draw
.proc buildrow
@src=r0
	stxy @src
	jsr blankrow

	; copy the string into the row; its column is its index in the buffer
	ldy #$00
	ldx #MC_TEXT_COL
:	lda (@src),y
	beq :+
	sta rowbuf,x
	iny
	inx
	cpx #MC_RCOL
	bcc :-

:	lda #BORDER_VBAR
	sta rowbuf+MC_LCOL
	sta rowbuf+MC_RCOL
	rts
.endproc

;*******************************************************************************
; BLANKROW
; Fills rowbuf with spaces
.proc blankrow
	lda #' '
	ldx #MC_RCOL
:	sta rowbuf,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; ROW TABLE
; One entry per selectable row of the window, in the order they are displayed.
; kept in-bank: read directly by banked code
blockbits: .byte MEMCFG_RAM123, MEMCFG_BLK1, MEMCFG_BLK2, MEMCFG_BLK3, MEMCFG_BLK5

rowtextslo: .lobytes ram123row, blk1row, blk2row, blk3row, blk5row, applyrow
rowtextshi: .hibytes ram123row, blk1row, blk2row, blk3row, blk5row, applyrow

title:     .byte "memory config",0

; the block rows' checkboxes are patched by drawrow, so they are all laid out
; on the same MC_TEXT_LEN-wide grid; the apply row is just a label
ram123row: .byte "[ ] ram123  3k $0400",0
blk1row:   .byte "[ ] blk1    8k $2000",0
blk2row:   .byte "[ ] blk2    8k $4000",0
blk3row:   .byte "[ ] blk3    8k $6000",0
blk5row:   .byte "[ ] blk5    8k $a000",0
applyrow:  .byte "apply",0

.assert (blk1row-ram123row)-1 = MC_TEXT_LEN, error, "block row is not MC_TEXT_LEN wide"
