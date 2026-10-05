;*******************************************************************************
; DRIVECFG.ASM
; This file contains procedures for the drive configuration menu. This menu
; allows the user to configures source/object drives and the target's optional
; memory configuration.
;*******************************************************************************

.include "alert.inc"
.include "border.inc"
.include "config.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "runtime.inc"
.include "screen.inc"
.include "text.inc"
.include "zeropage.inc"
.macpack longbranch
.if .defined(vic20) .and .defined(ultimem)
.import __memcfg_blocks, __memcfg_apply
.scope memcfg
blocks = __memcfg_blocks
apply = __memcfg_apply
.endscope
.endif

MC_LCOL = 0
MC_RCOL = LINESIZE-1
MC_TEXT_COL = 1
MC_TEXT_LEN = 20
MC_TOP_ROW = (SCREEN_HEIGHT-10)/2
MC_TITLE_ROW = MC_TOP_ROW+1
MC_FIRST_ROW = MC_TITLE_ROW+1
MC_CHECK_COL = MC_TEXT_COL+1
.if .defined(vic20) .and .defined(ultimem)
MEMCFG_NUM_ROWS = MEMCFG_NUM_BLOCKS+1
MC_APPLY = MEMCFG_NUM_BLOCKS
.else
MEMCFG_NUM_ROWS = 6
.endif
MC_BOT_ROW = MC_FIRST_ROW+MEMCFG_NUM_ROWS+1
rowbuf = mem::spare
selection = r8
page = r9
rowcount = ra

.DATA
; zero follows the currently selected input device
.export __drivecfg_output_device
__drivecfg_output_device: .byte $00

.if .defined(vic20) .and .defined(ultimem)
applymsg: .byte "reinitialize basic",0
applyprompt: .byte "are you sure? (y/n)",0
.endif

.CODE
;*******************************************************************************
; EDIT
; Opens the configuration window.
.export __drivecfg_edit
.proc __drivecfg_edit
.if .defined(vic20) .and .defined(ultimem)
	CALL FINAL_BANK_VIEWERS, edit
	bcc @done
	jsr scr::blank
	jsr run::clr
	jsr scr::unblank
@done:	rts
.else
	JUMP FINAL_BANK_FILEDIR, edit
.endif
.endproc

.if .defined(vic20) .and .defined(ultimem)
.export __memcfg_edit = __drivecfg_edit
.segment "VIEWERS"
CUR_BANK .set FINAL_BANK_VIEWERS
.else
BANKED_SEG "FILEDIR", FINAL_BANK_FILEDIR
.endif

;*******************************************************************************
; EDIT
; Displays the memory configuration and lets the user toggle the blocks that
; the user's program runs with.
; OUT:
;  - .C: set if the user asked for the configuration to be applied now
.proc edit
@select=selection
	lda #$00
.if .defined(vic20) .and .defined(ultimem)
	sta page
.else
	lda #$01
	sta page
.endif
	lda #$00
	sta @select
	jsr drawwin

;-------------------------------------------------------------------------------
@key:	CALLMAIN key::waitch
	cmp #K_QUIT
	jeq @exit
	cmp #K_WIN_CLOSE
	jeq @exit
	cmp #K_GO_BASIC		; F1 changes the configuration page
	beq @page
	cmp #K_RETURN
	beq @activate
	cmp #' '
	beq @activate

	CALLMAIN key::isleft
	beq @previous
	CALLMAIN key::isright
	beq @activate
	CALLMAIN key::isup
	beq @up
	CALLMAIN key::isdown
	beq @down
	bne @key		; branch always

@page:
.if .defined(vic20) .and .defined(ultimem)
	lda page
	eor #$01
	sta page
	lda #$00
	sta @select
	; redraw the title and rows inside the existing frame
	lda #MC_TEXT_COL
	sta text::puts_start
	lda #MC_RCOL
	sta text::puts_stop
	jsr drawpage
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop
	jmp @key
.else
	jmp @key
.endif

@previous:
	lda page
	beq @key
	lda #$ff
	bne @change_drive

@up:	lda @select
	beq @key		; already at the first row
	jsr highlight		; un-highlight the row we're leaving
	dec @select
	jsr highlight
	jmp @key

@down:	lda @select
	clc
	adc #$01
	cmp rowcount
	jcs @key		; already at the last row
	jsr highlight
	inc @select
	jsr highlight
	jmp @key

;-------------------------------------------------------------------------------
; RETURN/SPACE: toggle the selected block, or run the selected action
@activate:
	lda page
	beq @memory
	lda #$01
@change_drive:
	jsr change_drive
	lda @select
	jsr drawrow
	jsr highlight
	jmp @key

@memory:
.if .defined(vic20) .and .defined(ultimem)
	lda @select
	cmp #MC_APPLY
	beq @apply

	tax
	lda blockbits,x
	eor memcfg::blocks
	sta memcfg::blocks
	jsr memcfg::apply

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
.else
	jmp @key
.endif

@exit:	CALLMAIN scr::restore
	clc
	rts
.endproc

;*******************************************************************************
; CONFIRM
; Warns the user about destructive apply and waits for an answer
; OUT:
;  - .C: set if the user confirmed
.if .defined(vic20) .and .defined(ultimem)
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

.endif

;*******************************************************************************
; DRAWWIN
; Saves the screen the window is about to cover and draws the window on it
.proc drawwin
	CALLMAIN scr::savebuf

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

	ldxy #hint
	jsr buildrow
	lda #MC_BOT_ROW-1
	jsr showrow
	jmp drawpage
.endproc

;*******************************************************************************
; DRAW PAGE
; Draws the configuration title and selectable rows within the current bounds.
; IN:
;  - page: configuration page
;  - selection: selected row
;  - text::puts_start: first column to redraw
;  - text::puts_stop: one past the last column to redraw
; OUT:
;  - rowcount: number of selectable rows
.proc drawpage
	; draw the title and highlight it
	ldxy #title
	lda page
	beq :+
	ldxy #drivetitle
:	jsr buildrow
	lda #MC_TITLE_ROW
	jsr showrow
	ldy #MC_TEXT_COL
	ldx #MC_RCOL
	lda #MC_TITLE_ROW
	CALLMAIN scr::rvsline_part_physical

	lda #MEMCFG_NUM_ROWS
	ldx page
	beq :+
	lda #$02
:	sta rowcount

	; replace populated rows and clear only unused rows
	ldx #MEMCFG_NUM_ROWS-1
@row:
	txa
	pha
	cpx rowcount
	bcc @filled
	ldxy #empty
	jsr buildrow
	pla
	pha
	clc
	adc #MC_FIRST_ROW
	jsr showrow
	jmp @next
@filled:
	jsr drawrow
@next:
	pla
	tax
	dex
	bpl @row

	jmp highlight
.endproc

;*******************************************************************************
; HIGHLIGHT
; Reverses the text of the selected row.
.proc highlight
@select=selection
	ldy #MC_TEXT_COL
	ldx #MC_TEXT_COL+MC_TEXT_LEN
	lda @select
	clc
	adc #MC_FIRST_ROW
	CALLMAIN scr::rvsline_part_physical
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
	ldx page
	beq :+
	jmp drive_row
:
.if .defined(vic20) .and .defined(ultimem)
	tax

	; compose the row from its (fixed) description
	ldy rowtextshi,x
	lda rowtextslo,x
	tax
	jsr buildrow

	lda @row
	cmp #MC_APPLY
	bcs @show		; the apply row has no checkbox to fill in

	; Fill the block number and address in the shared BLK row template.
	tax
	beq @checkbox		; RAM123 has its own row
	lda blocknums-$01,x
	sta rowbuf+MC_TEXT_COL+7
	lda blockaddrs-$01,x
	sta rowbuf+MC_TEXT_COL+16
@checkbox:
	lda blockbits,x
	and memcfg::blocks
	beq @off
	lda #'x'
	bne @check		; branch always
@off:	lda #' '
@check:	sta rowbuf+MC_CHECK_COL

@show:
.endif
	lda @row
	clc
	adc #MC_FIRST_ROW
	jmp showrow
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
.if .defined(vic20) .and .defined(ultimem)
blockbits: .byte MEMCFG_RAM123, MEMCFG_BLK1, MEMCFG_BLK2, MEMCFG_BLK3, MEMCFG_BLK5

rowtextslo: .lobytes ram123row, blkrow, blkrow, blkrow, blkrow, applyrow
rowtextshi: .hibytes ram123row, blkrow, blkrow, blkrow, blkrow, applyrow

title:     .byte "memory config",0

; the block rows' checkboxes are patched by drawrow, so they are all laid out
; on the same MC_TEXT_LEN-wide grid; the apply row is just a label
ram123row: .byte "[ ] ram123  3k $0400",0
blkrow:   .byte "[ ] blk1    8k $2000",0
blocknums: .byte "1235"
blockaddrs: .byte "246a"
applyrow:  .byte "apply",0

.assert (blkrow-ram123row)-1 = MC_TEXT_LEN, error, "block row is not MC_TEXT_LEN wide"

.else
title: .byte "drive config",0
.endif

;*******************************************************************************
drivetitle: .byte "drive config",0
inputrow: .byte "input drive:  00",0
outputrow: .byte "output drive: 00",0
empty: .byte $00
.if .defined(vic20) .and .defined(ultimem)
hint: .byte "f1:next  stop:close",0
.else
hint: .byte "stop:close",0
.endif

;*******************************************************************************
; DRIVE ROW
; Composes and displays an input or output device row.
; IN:
;  - r6: selected row index
.proc drive_row
@row=r6
	ldxy #inputrow
	lda @row
	beq :+
	ldxy #outputrow
:	jsr buildrow
	lda zp::device
	ldx @row
	beq @number
	lda __drivecfg_output_device
	bne @number
	lda #'s'
	sta rowbuf+MC_TEXT_COL+14
	lda #'a'
	sta rowbuf+MC_TEXT_COL+15
	lda #'m'
	sta rowbuf+MC_TEXT_COL+16
	lda #'e'
	sta rowbuf+MC_TEXT_COL+17
	bne @show

@number:
	ldx #'0'
@tens:	cmp #10
	bcc @digits
	sec
	sbc #10
	inx
	bne @tens
@digits:
	stx rowbuf+MC_TEXT_COL+14
	ora #'0'
	sta rowbuf+MC_TEXT_COL+15
@show:	lda @row
	clc
	adc #MC_FIRST_ROW
	jmp showrow
.endproc

;*******************************************************************************
; CHANGE DRIVE
; Cycles a device through 8..30; output also offers "same" as the input.
; IN:
;  - .A: step ($01 or $ff)
;  - selection: input ($00) or output ($01)
.proc change_drive
@step=r7
	sta @step
	lda zp::device
	ldx selection
	beq @advance
	lda __drivecfg_output_device
	bne @advance
	lda #$07
@advance:
	clc
	adc @step
	cmp #$08
	bcc @below
	cmp #31
	bcc @store
	lda #$08
	cpx #$00
	beq @store
	lda #$00
	beq @store
@below:
	lda #30
	cpx #$00
	beq @store
	ldy __drivecfg_output_device
	beq @store
	lda #$00
@store:
	cpx #$00
	beq @input
	sta __drivecfg_output_device
	rts
@input:
	sta zp::device
	rts
.endproc
