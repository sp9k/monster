;*******************************************************************************
; VIEWPORT.ASM
; This file contains the code to manage the 80 column character cache for the
; editor viewport
;*******************************************************************************

.include "config.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "zeropage.inc"
.include "edit.inc"
.include "source.inc"
.include "text.inc"
.include "cursor.inc"
.include "key.inc"
.include "ram.inc"
.include "screen.inc"
.include "draw.inc"
.include "viewportcache.inc"


;*******************************************************************************
; SHARED STATE
; Viewport position and repaint state.

.segment "VIEW_SHARED"
.export __view_x
__view_x:   .byte 0	; first visible source column
manual:     .byte 0	; suppress following after a manual pan
repainting: .byte 0	; preserve selection masks during a repaint

;*******************************************************************************
; MAIN-BANK ENTRY POINTS
.CODE
.export __view_init, __view_draw, __view_follow, __view_pan
.export __view_clear, __view_scroll_up, __view_scroll_down
.export __view_reverse, __view_physical_x
__view_init: JUMP FINAL_BANK_VSCREEN, init

;*******************************************************************************
; DRAW
; Expands and displays the source row in linebuffer.
; IN:
;  - .A: the screen row to draw
__view_draw: JUMP FINAL_BANK_VSCREEN, draw_row
.export __view_command
__view_command:     JUMP FINAL_BANK_VSCREEN, command
__view_pan:         JUMP FINAL_BANK_VSCREEN, pan
__view_clear:       JUMP FINAL_BANK_VSCREEN, clear
__view_scroll_up:   JUMP FINAL_BANK_VSCREEN, scroll_up
__view_scroll_down: JUMP FINAL_BANK_VSCREEN, scroll_down
__view_reverse:     JUMP FINAL_BANK_VSCREEN, reverse

.export __view_join
__view_join: JUMP FINAL_BANK_VSCREEN, join

.export __view_invalidate
__view_invalidate: JUMP FINAL_BANK_VSCREEN, invalidate

;*******************************************************************************
; PHYSICAL X
; Converts a source column to its visible screen column.
; IN:
;  - .X: logical column (physical column for prompts/windows)
; OUT:
;  - .X: physical column
;  - .C: set if the source cursor is outside the viewport
.proc __view_physical_x
	lda edit::height
	bmi @physical
	cmp zp::cury
	bcs @source
@physical:
	clc
	rts
@source:
	txa
	sec
	sbc __view_x
	bcc @hidden
	tax
	cmp #SCREEN_WIDTH+1
	rts
@hidden:
	sec
	rts
.endproc

;*******************************************************************************
; FOLLOW CURSOR
; Scrolls when the source cursor moves outside the visible columns.
; Skips the first call after a manual pan.
.proc __view_follow
	lda manual
	beq :+
	dec manual
	rts

:	lda edit::height
	bmi @done
	cmp zp::cury
	bcc @done
	lda zp::curx
	cmp __view_x
	bcc @move
	sec
	sbc __view_x
	cmp #SCREEN_WIDTH
	bcc @done

	lda __view_x
	cmp #MAX_LINE_LEN-SCREEN_WIDTH
	beq @done

@move:	JUMP FINAL_BANK_VSCREEN, follow
@done:	rts
.endproc

;*******************************************************************************
; CACHE STORAGE
.segment "VSCREEN_BSS"
; Maps screen rows to character-cache slots.
.export __view_slots, __view_valid
__view_slots=slots
__view_valid=valid
slots:    .res SCREEN_HEIGHT		  ; slot assigned to each screen row
valid:    .res SCREEN_HEIGHT		  ; !0 if slot contains expanded chars
selected: .res SCREEN_HEIGHT		  ; !0 if slot has selected characters

row:      .byte 0		; current screen row
slot:     .byte 0		; current cache slot
paintrow: .byte 0		; next row to draw during a full repaint
joined:   .res MAX_LINE_LEN+1

;*******************************************************************************
; SCROLL SCRATCHPAD
; Stores row bounds and loop state during cache-slot rotation.
scroll_first     = zp::text
scroll_last      = zp::text+1
scroll_count     = zp::text+2
scroll_savedslot = zp::text+3
scroll_total     = zp::text+4

.segment "VSCREEN"
SET_CUR_BANK FINAL_BANK_VSCREEN

;*******************************************************************************
; INIT
; Resets horizontal position, row ownership, and selection masks.
.proc init
	lda #$00
	sta __view_x
	sta manual
	sta repainting

	ldx #SCREEN_HEIGHT-1
:	sta valid,x
	sta selected,x
	txa
	sta slots,x
	lda #$00
	dex
	bpl :-

	jmp viewcache::init
.endproc

;*******************************************************************************
; INVALIDATE
; Invalidates cached rows after restoring a saved screen.
.proc invalidate
	lda #$00
	ldx #SCREEN_HEIGHT-1
:	sta valid,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; JOIN
; Copies both source lines into joined and checks the expanded width.
; Removes the newline if the combined line fits.
; OUT:
;  - .C: set if there is no next line or the joined line is too wide
.proc join
	CALLMAIN src::on_last_line
	bne :+
	sec
	rts

:	CALLMAIN src::pushp
	CALLMAIN src::home
	CALLMAIN src::getwide

	ldy #$00
:	lda mem::linebuffer,y
	sta joined,y
	beq @first
	iny
	bne :-

@first: tya
	pha			; save the first line's length
	CALLMAIN src::lineend
	CALLMAIN src::next
	CALLMAIN src::getwide
	pla
	tax
	ldy #$00

@append:
	lda mem::linebuffer,y
	beq @terminate
	cpx #MAX_LINE_LEN
	bcs @abort
	sta joined,x
	inx
	iny
	bne @append

@terminate:
	sta joined,x
	ldy #$00
:	lda joined,y
	sta mem::linebuffer,y
	beq :+
	iny
	bne :-
:	CALLMAIN text::rendered_line_len
	bcs @abort
	CALLMAIN src::popp
	CALLMAIN src::backspace
	CALLMAIN edit::refreshline
	CALLMAIN edit::sync_cur
	clc
	rts

@abort:
	CALLMAIN src::popgoto
	CALLMAIN edit::refreshline
	sec
	rts
.endproc

;*******************************************************************************
; POINTERS
; Finds the character and selection buffers for the current row.
; OUT:
;  - r0:   character buffer pointer
;  - r2:   selection mask pointer
;  - slot: cache slot for this row
.proc pointers
	ldx row
	lda slots,x
	sta slot
	tax
	jmp viewcache::row
.endproc

;*******************************************************************************
; RESET MASK
; Clears the current row's selection mask if it has selected characters.
.proc reset_mask
@mask=r2
	jsr pointers
	ldx slot
	lda selected,x
	beq @done

	lda #$00
	sta selected,x
	ldy #MAX_LINE_LEN-1
:	sta (@mask),y
	dey
	bpl :-

@done:	rts
.endproc

;*******************************************************************************
; DRAW ROW
; Expands and paints a source row, clearing its previous selection on edits.
; IN:
;  - .A: screen row
.proc draw_row
	sta row
	lda repainting
	bne :+
	jsr reset_mask
:	jsr expand
	jmp paint
.endproc

;*******************************************************************************
; EXPAND
; Expands linebuffer into the current cache row, including tabs and padding.
; Fills the remaining columns with spaces.
.proc expand
@index=zp::text
@column=zp::text+1
@tabstop=zp::text+2
@tabend=zp::text+3
	jsr pointers
	lda #$00
	sta @index
	sta @column
	lda #TAB_WIDTH
	sta @tabstop

@next:	ldx @index
	lda mem::linebuffer,x
	beq @pad
	cmp #13
	beq @pad
	inc @index
	cmp #9
	beq @tab
	cmp #32
	bcs :+
	lda #' '
:	jsr append
	bcc @next
	bcs @done

@tab:	lda @tabstop
	sta @tabend
@tabchar:
	lda #' '
	ldx text::show_ws
	beq :+
	lda #VIS_WS_CHAR
:	jsr append
	bcs @done
	lda @column
	cmp @tabend
	bne @tabchar
	jmp @next

@pad:	lda #' '
	jsr append
	bcc @pad

@done:	ldx slot
	lda #1
	sta valid,x
	rts
.endproc

;*******************************************************************************
; APPEND
; Appends one character to the expanded cache row and advances the tab stop.
; IN:
;  - .A: character
;  - r0: character buffer pointer
;  - zp::text+1: expanded column
;  - zp::text+2: next tab stop
; OUT:
;  - .C: set when the expanded row is full
.proc append
@chars=r0
@column=zp::text+1
@tabstop=zp::text+2
	ldy @column
	sta (@chars),y
	iny
	sty @column
	cpy @tabstop
	bcc :+

	; advance the tab stop when the column reaches it
	php
	lda @tabstop
	clc
	adc #TAB_WIDTH
	sta @tabstop
	plp
:	cpy #MAX_LINE_LEN
	rts
.endproc

;*******************************************************************************
; PAINT
; Draws the visible slice of a cached row and reapplies its selection mask.
.proc paint
@chars=r0
@maskptr=r2
@column=zp::text	; physical column in the selection redraw loop
	jsr pointers
	ldy __view_x
	ldx #$00
:	lda (@chars),y
	sta mem::linebuffer2,x
	iny
	inx
	cpx #SCREEN_WIDTH
	bne :-

	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop
	ldxy #mem::linebuffer2
	lda row
	CALLMAIN text::puts
	ldx slot
	lda selected,x
	beq @done

	; reverse each visible character marked in the selection mask
	lda #0
	sta @column
@mask: jsr pointers
	lda @column
	clc
	adc __view_x
	tay
	lda (@maskptr),y
	beq @next

	ldy @column
	tya
	clc
	adc #1
	tax
	lda row
	CALLMAIN scr::rvsline_part_physical

@next:	inc @column
	lda @column
	cmp #SCREEN_WIDTH
	bne @mask
@done:
	rts
.endproc

;*******************************************************************************
; REPAINT
; Redraws the source window, reading invalid rows through the editor.
.proc repaint
	lda #$01
	sta repainting
	lda #$00
	sta paintrow

@row:	lda paintrow
	sta row
	lda row
	tax
	lda slots,x
	tax
	lda valid,x
	beq @read
	jsr paint
	jsr highlight
	jmp @next

@read:	lda paintrow
	CALLMAIN edit::render_row

@next:	inc paintrow
	lda edit::height
	cmp paintrow
	bcs @row
	lda #0
	sta repainting
	sta cur::status
	lda cur::mode
	beq :+
	CALLMAIN cur::on
:	rts
.endproc

;*******************************************************************************
; HIGHLIGHT
; Restores the debugger underline after painting a cached row.
.proc highlight
	; match the current buffer and painted row against the highlighted line
	lda edit::highlight_en
	beq @done
	CALLMAIN edit::currentfile
	bcs @done
	cmp edit::highlight_file
	bne @done

	ldxy edit::highlight_line
	CALLMAIN edit::src2screen
	bcs @done
	cmp paintrow
	bne @done
	JUMPMAIN draw::rvs_underline

@done:	rts
.endproc

;*******************************************************************************
; FOLLOW
; Moves the viewport to follow the source cursor and repaints the window.
.proc follow
	lda zp::curx
	cmp __view_x
	bcc @left
	sec
	sbc #SCREEN_WIDTH-VIEW_SCROLL_STEP
	bcs @clamp

@left:	sec
	sbc #VIEW_SCROLL_STEP-1
	bcs @clamp
	lda #$00

@clamp: cmp #MAX_LINE_LEN-SCREEN_WIDTH+1
	bcc :+
	lda #MAX_LINE_LEN-SCREEN_WIDTH
:	and #256-VIEW_SCROLL_ALIGN
	sta __view_x
	jmp repaint
.endproc

;*******************************************************************************
; COMMAND
; Reads h/l and pans one step without changing the source position.
.proc command
	CALLMAIN key::waitch
	cmp #$68
	beq @left
	cmp #$6c
	bne @done
	lda #VIEW_SCROLL_STEP
	bne pan

@left:	lda #256-VIEW_SCROLL_STEP
	bne pan

@done:	rts
.endproc

;*******************************************************************************
; PAN
; Moves the window without moving the source cursor.
; IN:
;  - .A: signed number of columns to pan
.proc pan
	tax
	lda #$01
	sta manual
	txa
	pha
	CALLMAIN cur::off
	pla
	clc
	adc __view_x
	bpl :+

	lda #$00
:	cmp #MAX_LINE_LEN-SCREEN_WIDTH+1
	bcc :+
	lda #MAX_LINE_LEN-SCREEN_WIDTH
:	cmp __view_x
	beq @done
	sta __view_x
	lda #$01
	sta manual
	jsr repaint

@done:	rts
.endproc

;*******************************************************************************
; CLEAR
; Clears a cache row and its selection mask.
; IN/OUT:
;  - .A: screen row
.proc clear
@chars=r0
	sta row
	jsr reset_mask
	lda #' '
	ldy #MAX_LINE_LEN-1
:	sta (@chars),y
	dey
	bpl :-

	ldx slot
	lda #$01
	sta valid,x
	lda row
	rts
.endproc

;*******************************************************************************
; SCROLL SETUP
; Saves the scroll bounds before rotating cache slots.
; IN:
;  - r1: first screen row
;  - r2: last screen row
;  - r3: number of rows to scroll
.proc scroll_setup
@first=r1
@last=r2
@count=r3
	lda @first
	sta scroll_first
	lda @last
	sta scroll_last
	lda @count
	sta scroll_count
	sta scroll_total
	rts
.endproc

;*******************************************************************************
; SCROLL RESTORE
; Restores the caller's first row, last row, and count in r1/r2/r3.
.proc scroll_restore
@first=r1
@last=r2
@count=r3
	lda scroll_first
	sta @first
	lda scroll_last
	sta @last
	lda scroll_total
	sta @count
	rts
.endproc

;*******************************************************************************
; SCROLL UP
; Rotates cache slots upward and invalidates the newly exposed rows.
; IN/PRESERVED:
;  - r1: first screen row
;  - r2: last screen row
;  - r3: number of rows to scroll
.proc scroll_up
	jsr scroll_setup
	lda scroll_count
	beq scroll_restore

@again: ldx scroll_first
	lda slots,x
	sta scroll_savedslot
:	cpx scroll_last
	beq @last
	lda slots+1,x
	sta slots,x
	inx
	bne :-

@last:	lda scroll_savedslot
	sta slots,x
	tax
	lda #$00
	sta valid,x
	dec scroll_count
	bne @again
	jmp scroll_restore
.endproc

;*******************************************************************************
; SCROLL DOWN
; Rotates cache slots downward and invalidates the newly exposed rows.
; IN/PRESERVED:
;  - r1: first screen row
;  - r2: last screen row
;  - r3: number of rows to scroll
.proc scroll_down
	jsr scroll_setup
	lda scroll_count
	beq scroll_restore

@again: ldx scroll_last
	lda slots,x
	sta scroll_savedslot
:	cpx scroll_first
	beq @first
	lda slots-1,x
	sta slots,x
	dex
	bpl :-

@first: lda scroll_savedslot
	sta slots,x
	tax
	lda #$00
	sta valid,x
	dec scroll_count
	bne @again
	jmp scroll_restore
.endproc

;*******************************************************************************
; REVERSE
; Updates the logical selection mask and reverses its visible portion.
; IN:
;  - .A: screen row
;  - .Y: first logical column
;  - .X: end logical column (exclusive)
.proc reverse
@mask=r2
@left=r4
@right=r5
	sta row
	sty @left
	stx @right
	cpx @left
	bcs :+

	sty @right
	stx @left
:	jsr pointers
	ldx slot
	lda #$01
	sta selected,x
	ldy @left

@toggle:
	cpy #MAX_LINE_LEN
	bcs @clip
	cpy @right
	bcs @clip
	lda (@mask),y
	eor #$ff
	sta (@mask),y
	iny
	bne @toggle

@clip:	lda @right
	sec
	sbc __view_x
	bcc @done
	beq @done
	cmp #SCREEN_WIDTH+1
	bcc :+
	lda #SCREEN_WIDTH
:	tax
	lda @left
	sec
	sbc __view_x
	bcs :+
	lda #$00
:	cmp #SCREEN_WIDTH
	bcs @done
	tay
	lda row
	JUMPMAIN scr::rvsline_part_physical

@done:	rts
.endproc

