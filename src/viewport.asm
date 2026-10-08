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
.include "errlog.inc"
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
; Viewport position and draw status
.segment "VIEW_SHARED"
.export __view_x
__view_x:   .byte 0	; first visible source column
manual:     .byte 0	; suppress following after a manual pan
redrawing:  .byte 0	; preserve selection masks during a redraw

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

;*******************************************************************************
; NAVIGATE
; Redraws the current source row after edits, cache invalidation, or selection
; IN:
;  - zp::cury: destination screen row
.ifdef ultimem
.export __view_navigate
.proc __view_navigate
	JUMP FINAL_BANK_VSCREEN, navigate
.endproc
.else
.export __view_navigate
__view_navigate = edit::redrawline
.endif

.export __view_command
__view_command:     JUMP FINAL_BANK_VSCREEN, command
__view_pan:         JUMP FINAL_BANK_VSCREEN, pan
__view_clear:       JUMP FINAL_BANK_VSCREEN, clear
__view_scroll_up:   JUMP FINAL_BANK_VSCREEN, scroll_up
__view_scroll_down: JUMP FINAL_BANK_VSCREEN, scroll_down
__view_reverse:     JUMP FINAL_BANK_VSCREEN, reverse
.export __view_toggle_cursor
__view_toggle_cursor: JUMP FINAL_BANK_VSCREEN, toggle_cursor

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
	;sec
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
drawrow:  .byte 0		; next row to draw during a full redraw
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
	sta redrawing

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
; Expands and draws a source row, clearing its previous selection on edits.
; IN:
;  - .A: screen row
.proc draw_row
	sta row
	lda redrawing
	bne :+
	jsr reset_mask
:	jsr expand
	jmp draw_row_slice
.endproc

;*******************************************************************************
; NAVIGATE
; Redraws the current source row after edits, cache invalidation, or selection
; IN:
;  - zp::cury: destination screen row
.ifdef ultimem
.proc navigate
	lda cur::mode
	bne @draw
	lda errlog::editpending
	bne @draw

	ldx zp::cury
	lda slots,x
	tax
	lda valid,x
	bne @done

@draw:	JUMPMAIN edit::redrawline
@done:	rts
.endproc
.endif

;*******************************************************************************
; EXPAND
; Expands linebuffer into the current cache row, including tabs and padding.
; IN:
;  - mem::linebuffer: source text to expand
;  - row: destination cache row
; OUT:
;  - cache row: expanded, space-padded characters
;  - valid: destination slot is valid
.proc expand
@chars=r0
@tabstop=zp::text
	jsr pointers
	lda #TAB_WIDTH
	sta @tabstop
	ldx #$00
	ldy #$00

@next:	lda mem::linebuffer,x
	beq @pad
	cmp #$0d
	beq @pad
	inx
	cmp #$09
	beq @tab
	cmp #' '
	bcs @char
	lda #' '
@char:	sta (@chars),y
	iny
	cpy #MAX_LINE_LEN
	bcc @next
	bcs @done

@tab:	; locate the next tab stop from the expanded column
	cpy @tabstop
	bcc @tabfill
	lda @tabstop
	clc
	adc #TAB_WIDTH
	sta @tabstop
	bne @tab

@tabfill:
	lda text::show_ws
	beq :+
	lda #VIS_WS_CHAR
	bne @tabchar
:	lda #' '
@tabchar:
	sta (@chars),y
	iny
	cpy #MAX_LINE_LEN
	bcs @done
	cpy @tabstop
	bcc @tabchar
	bcs @next

@pad:	lda #' '
@space:	sta (@chars),y
	iny
	cpy #MAX_LINE_LEN
	bcc @space

@done:	ldx slot
	lda #$01
	sta valid,x
	rts
.endproc

;*******************************************************************************
; DRAW ROW SLICE
; Draws the visible slice of a cached row and reapplies its selection mask.
.proc draw_row_slice
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
	lda #$00
	sta @column
@mask:	jsr pointers
	lda @column
	clc
	adc __view_x
	tay
	lda (@maskptr),y
	beq @next

	ldy @column
	tya
	clc
	adc #$01
	tax
	lda row
	CALLMAIN scr::rvsline_part_physical

@next:	inc @column
	lda @column
	cmp #SCREEN_WIDTH
	bne @mask
@done:	rts
.endproc

;*******************************************************************************
; REPAINT
; Redraws the source window, reading invalid rows through the editor.
.proc redraw
	ldx #$00
	stx drawrow
	inx
	stx redrawing

@row:	ldx drawrow
	stx row
	lda slots,x
	tax
	lda valid,x
	beq @read

	jsr draw_row_slice
	jsr highlight
	jmp @next

@read:	lda drawrow
	CALLMAIN edit::render_row

@next:	inc drawrow
	lda edit::height
	cmp drawrow
	bcs @row
	lda #$00
	sta redrawing

	; Selection cursors were already restored by the cached masks.
	; The editor redraws an ordinary cursor after following the viewport.
	ldx cur::mode
	bne :+
	sta cur::status
:	rts
.endproc

;*******************************************************************************
; HIGHLIGHT
; Restores the debugger underline after drawing a cached row.
.proc highlight
	; match the current buffer and drawn row against the highlighted line
	lda edit::highlight_en
	beq @done
	CALLMAIN edit::currentfile
	bcs @done
	cmp edit::highlight_file
	bne @done

	ldxy edit::highlight_line
	CALLMAIN edit::src2screen
	bcs @done
	cmp drawrow
	bne @done
	JUMPMAIN draw::rvs_underline

@done:	rts
.endproc

;*******************************************************************************
; FOLLOW
; Moves the viewport to follow the source cursor and redraws the window.
.proc follow
	lda zp::curx
	cmp __view_x
	bcc @left
	;sec
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
	jmp redraw
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
	bne pan				; branch always

@left:	lda #256-VIEW_SCROLL_STEP
	bne pan				; branch always

@done:	rts
.endproc

;*******************************************************************************
; PAN
; Moves the window without moving the source cursor.
; IN:
;  - .A: signed number of columns to pan
.proc pan
	ldx #$01
	stx manual

	pha
	lda cur::mode
	bne :+			; a selection cursor belongs to the cached selection
	CALLMAIN cur::off
:	pla
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
	jsr redraw

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

;*******************************************************************************
; TOGGLE CURSOR
; Includes a selection cursor in the logical mask before clipping to the screen.
.proc toggle_cursor
	; save TAB count and deselect flag (used by horizontal movement)
	lda r3
	pha
	lda r4
	pha
	lda r5
	pha
	ldy zp::curx
	ldx zp::curx
	inx
	lda zp::cury
	jsr reverse
	pla
	sta r5
	pla
	sta r4
	pla
	sta r3
	lda #1
	eor cur::status
	sta cur::status
	ldx zp::curx
	ldy zp::cury
	rts
.endproc
