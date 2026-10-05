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
