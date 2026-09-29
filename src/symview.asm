;*******************************************************************************
; SYMVIEW.ASM
; This file contains the code for the symbol viewer, which allows the user to
; see all (non-local/anonymous) symbols in their program.
; The user can navigate to the definition location stored on each symbol.
;*******************************************************************************

.include "alert.inc"
.include "border.inc"
.include "cursor.inc"
.include "debuginfo.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "fp.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "alert-layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "settings.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.include "ram.inc"
.macpack longbranch

;*******************************************************************************
; CONSTANTS
HEIGHT     = SCREEN_HEIGHT-1
.if SCREEN_WIDTH >= 40
DETAIL_HEIGHT = 14
.else
DETAIL_HEIGHT = 17
.endif

DETAIL_TOP    = (SCREEN_HEIGHT-DETAIL_HEIGHT)/2
DETAIL_BOTTOM = DETAIL_TOP+DETAIL_HEIGHT-1
DETAIL_PROMPT = DETAIL_BOTTOM-1

SORT_ALPHA = 0	; sort by label name alphabetically
SORT_ADDR  = 1	; sort by label address

;*******************************************************************************
; ZEROPAGE/MEMORY LOCATIONS
lbl      = zp::editortmp	; ID of the current line's label
addr     = zp::editortmp+2	; address corresponding to current line's label
filename = zp::tmp10		; the filename for the current line
line     = zp::tmp12		; the line number for the current line
sortby   = zp::tmp14		; the sort order (ALPHA, ADDR)
mode     = zp::tmp15		; mode (0=ZP, 1=ABS)
draw_stop = zp::tmp16		; row past the end of the current redraw
selection = zp::tmp17		; selected row on the current list page
page_top = r8			; index of the first symbol on the current page
name     = lbl::namebuffer

;*******************************************************************************
; Shared scratchpad registers used throughout viewer routines
output_row = r0			; screen row the composed text
output_col = r1          	; next column in the composition buffer
output_src = r2			; pointer to the field being appended

.if .defined(vic20) .and .defined(soft4x8)
; Row composition uses only SCREEN_WIDTH bytes of the 81-byte assembly buffer.
; Stage the final glyph here while blanking borrows it for the gutter.
redraw_corner = mem::asmbuffer+SCREEN_WIDTH
; Keep complete final rows outside the renderer's string/composition buffers.
redraw_lastrow = mem::spareend-2*SCREEN_WIDTH
redraw_footer  = mem::spareend-SCREEN_WIDTH
.endif

;*******************************************************************************
.DATA
sym_value:    .byte "$", ESCAPE_VALUE, 0
sym_location: .byte ESCAPE_STRING, ":", ESCAPE_VALUE_DEC, 0
sym_id:       .byte "id: ", ESCAPE_VALUE_DEC, 0

; Persistent across item lookups and banked drawing calls.
view_page:    .byte 0    ; 0=values, 1=locations
view_details: .byte 0    ; wrap output inside the modal instead of clipping it
draw_row:     .byte 0    ; screen row being drawn, including modal restoration

.RODATA
.if SCREEN_WIDTH >= 40
values_msg:    .byte "1/2 values <> ", ESCAPE_STRING, " space details", 0
locations_msg: .byte "2/2 locations <> ", ESCAPE_STRING, " space details", 0
.else
values_msg:    .byte "1/2 val <> ", ESCAPE_STRING, " spc", 0
locations_msg: .byte "2/2 loc <> ", ESCAPE_STRING, " spc", 0
.endif

sort_by_name_msg: .byte "f1 name",0
sort_by_addr_msg: .byte "f1 addr",0

.CODE
.ifdef vic20
CUR_BANK .set FINAL_BANK_MAIN
.endif
;*******************************************************************************
; MAIN-BANK ENTRY POINT
; Enters the symbol viewer in the bank containing its navigation code.
.export __symview_enter

.ifdef vic20
__symview_enter: JUMP FINAL_BANK_VIEWERS, enter
.elseif .defined(CART) .and .defined(c64)
__symview_enter: JUMP FINAL_BANK_DBGUI, enter
.else
__symview_enter = enter
.endif

.ifdef vic20
.segment "VIEWERS"
CUR_BANK .set FINAL_BANK_VIEWERS
.else
BANKED_CODE "DBGUI", FINAL_BANK_DBGUI
.endif

get_item          = get_row_item
item_index_here   = item_index
drawrows          = draw_rows
print_details     = print_details_impl
modal_bounds_here = modal_bounds
full_bounds_here  = full_bounds

;*******************************************************************************
space_msg:       .byte " ", 0
no_location_msg: .byte "(no location)", 0
name_msg:        .byte "name:", 0
value_msg:       .byte "value:", 0
location_msg:    .byte "location:", 0

.if SCREEN_WIDTH >= 40
details_msg: .byte "press any key", 0
.else
details_msg: .byte "spc back", 0
.endif
DETAIL_PROMPT_LEN = *-details_msg-1
DETAIL_PROMPT_PAD = (ALERT_TEXT_LEN-DETAIL_PROMPT_LEN)/2
DETAIL_PROMPT_COL = ALERT_TEXT_COL+DETAIL_PROMPT_PAD

;*******************************************************************************
; GET ITEM
; Returns the label ID at the given index based on the current sortby value.
; IN:
;   - .XY: the item index
; OUT:
;   - lbl:      ID of the label at a given index
;   - addr:     address of the label at the requested index
;   - name:     pointer to name of the symbol
;   - filename: pointer to name of the file containing the symbol (if any)
;   - line:     line number that contains the symbol
;   - .XY:      ID of the label at the given index (determined by sortby)
;   - lbl::namebuffer: complete symbol name
.proc get_item_impl
@namedst = r0           ; destination pointer required by lbl::getname
	lda sortby
	beq @sortalpha

@sortaddr:
	CALLMAIN lbl::idbyaddrindex	; lookup via sorted addresses
	jmp @getinfo

@sortalpha:
	CALLMAIN lbl::id_by_alpha_index

@getinfo:
	stxy lbl			; store the ID for the label

	; destination buffer for getname
	lda #<name
	sta @namedst
	lda #0		; default filename to nothing
	sta filename
	sta filename+1

	lda #>name
	sta @namedst+1
	CALLMAIN lbl::getname	; read the complete symbol name

	ldxy lbl
	CALLMAIN lbl::get_line	; definition location, independent of symbol value
	stxy line
	cpx #$00
	bne @getfile
	cpy #$00
	beq @value		; no definition location
@getfile:
	CALLMAIN dbgi::get_filename
	bcs @value
	stxy filename
@value:
	ldxy lbl
.if FP_SUPPORTED
	CALLMAIN lbl::getsegment
	cmp #SEG_FLOAT
	bne @address
	ldxy lbl
	CALLMAIN lbl::getaddr

	CALL FINAL_BANK_EXPR, expr::fconst_get
	bcs @done
	lda #$02
	sta mode
	rts
@address:
	ldxy lbl
.endif
	CALLMAIN lbl::addr_and_mode	; get the symbol address
	stxy addr
	sta mode

@done:	rts
.endproc

;*******************************************************************************
; RENDER VALUE
; Formats the current symbol's integer or floating-point value
; IN:
;   - addr:	      current symbol's value (address)
;   - mode:           current symbol's address mode
;   - expr::floatval: the current value for a float symbol
; OUT:
;   - .XY: pointer to the formatted value for display
.proc render_value
.if FP_SUPPORTED
	lda mode
	cmp #$02
	bne @integer
	CALL FINAL_BANK_EXPR, expr::float_format
	ldxy #expr::floatstr
	rts
@integer:
.endif
	lda addr
	pha
	lda mode
	beq @zp
	lda addr+1
	pha

	lda #ESCAPE_VALUE
	bne @format
@zp:	lda #ESCAPE_BYTE
@format:
	sta sym_value+1
	ldxy #sym_value
	RENDER_STR
	rts
.endproc

;*******************************************************************************
; RENDER LOCATION
; Formats the current symbol's definition as filename:line, or a
; "missing location" message when no definition is available.
; IN:
;   - filename, line: the current symbol's definition location
; OUT:
;   - .XY: pointer to the formatted location or the missing-location message
.proc render_location
	lda filename
	ora filename+1
	bne @file
	ldxy #no_location_msg
	rts

@file:	; push line #
	lda line
	pha
	lda line+1
	pha

	; push filename
	lda filename+1
	pha
	lda filename
	pha

	ldxy #sym_location
	RENDER_STR
	rts
.endproc

;*******************************************************************************
; BEGIN OUTPUT
; Clears the composition buffer and resets its next column to zero.
; OUT:
;   - output_col: zero
;   - mem::asmbuffer: a blank screen-width row
.proc begin_output
	lda #$00
	sta output_col
	ldx #SCREEN_WIDTH-1
	lda #' '
@clear:
	sta mem::asmbuffer,x
	dex
	bpl @clear
	rts
.endproc

;*******************************************************************************
; END OUTPUT
; Draws the symbol row and clears the display buffer for the following row's use
; IN:
;   - output_row:     the screen row to draw
;   - view_details:   nonzero to draw inside the alert-style frame
;   - mem::asmbuffer: the composed text
; OUT:
;   - output_row: advanced to the following screen row
;   - output_col: zero
.proc end_output
	ldxy #mem::asmbuffer
	lda view_details
	beq @list

	lda #$00
	sta mem::asmbuffer+ALERT_TEXT_LEN
	lda output_row
	pha				; save output_row
	CALLMAIN alert::textrow
	pla				; restore output_row
	sta output_row
	jmp @next

@list:
.if .defined(vic20) .and .defined(soft4x8)
	lda output_row
	cmp #SCREEN_HEIGHT-2
	bne @draw
	; This row shares its final character with the blanked gutter. Defer
	; the whole row so the renderer never writes the borrowed glyph.
	ldx #SCREEN_WIDTH-1
@save:	lda mem::asmbuffer,x
	sta redraw_lastrow,x
	dex
	bpl @save
	jmp @next
@draw:
.endif
	lda output_row
	CALLMAIN text::puts

@next:	inc output_row
	jmp begin_output
.endproc

;*******************************************************************************
; WRITE FIELD
; Appends a 0-terminated field to the row under construction. If the row
; overflows, ends it with a '>'
; IN:
;   - .XY: pointer to the field to append
;   - view_details: nonzero to wrap inside the modal instead of clipping
; OUT:
;   - output_row, output_col: position following the appended text
.proc write_field
	stxy output_src

@next:	ldy #$00
	lda (output_src),y
	beq @done
	ldx output_col
	lda view_details
	beq @listwidth
	cpx #ALERT_TEXT_LEN
	jmp @width

@listwidth:
	cpx #SCREEN_WIDTH
@width: bcc @put
	lda view_details
	bne @wrap
	lda #'>'
	sta mem::asmbuffer+SCREEN_WIDTH-1
@done:	rts

@wrap:	jsr end_output
	jmp @next

@put:	lda (output_src),y
	sta mem::asmbuffer,x
	inc output_col
	incw output_src
	jmp @next
.endproc

;*******************************************************************************
; WRITE LINE
; Appends a field and draws the completed output row, wrapping modal text as
; needed.
; IN:
;   - .XY: pointer to the zero-terminated field
; OUT:
;   - output_row: next screen row
;   - output_col: zero
.proc write_line
	jsr write_field
	jmp end_output
.endproc

;*******************************************************************************
; BLANK ITEM
; Clears the given row within the current horizontal drawing bounds.
; IN:
;   - .A: the screen row to clear
.proc blank_item_impl
	sta output_row
	jsr begin_output
	jmp end_output
.endproc

;*******************************************************************************
; PRINT ITEM
; Builds and draws the current symbol's entry on the selected field page.
; IN:
;   - .A:            screen row to draw
;   - view_page:     0=VALUES, !0=LOCATIONS
;   - symbol fields: populated by the most recent call to get_item
.proc print_item_impl
	sta output_row
	jsr begin_output
	lda view_page
	beq @value

	ldxy #name
	jsr write_field

	ldxy #space_msg
	jsr write_field

	jsr render_location
	jmp @last

@value: jsr render_value
	jsr write_field

	ldxy #space_msg
	jsr write_field

	ldxy #name
@last:	jmp write_line
.endproc

;*******************************************************************************
; PRINT DETAILS
; Draws the current symbol's fields in a centered alert-style modal, leaving the
; surrounding list and footer visible. Long fields are wrapped inside the frame
; IN:
;   - view_details: nonzero
;   - symbol fields: populated by the most recent call to get_item
; OUT:
;   - horizontal drawing bounds: restored to the full screen width
.proc print_details_impl
	jsr modal_bounds

	lda #ALERT_TEXT_COL
	sta alert::textcol

	lda #DETAIL_TOP
	ldx #BORDER_TL
	ldy #BORDER_TR
	CALLMAIN alert::border

	; start row
	lda #DETAIL_TOP+1
	sta output_row
	jsr begin_output

	; write name
	ldxy #name_msg
	jsr write_line
	ldxy #name
	jsr write_line
	jsr end_output

	; write value
	ldxy #value_msg
	jsr write_line
	jsr render_value
	jsr write_line
	jsr end_output

	; write location
	ldxy #location_msg
	jsr write_line
	jsr render_location
	jsr write_line
	jsr end_output

	lda lbl
	pha
	lda lbl+1
	pha
	ldxy #sym_id
	RENDER_STR
	jsr write_line
@blank:
	lda output_row
	cmp #DETAIL_PROMPT
	beq @prompt
	jsr end_output
	jmp @blank

@prompt:
	lda #DETAIL_PROMPT_PAD
	sta output_col
	ldxy #details_msg
	jsr write_line
	lda #DETAIL_BOTTOM
	ldx #BORDER_BL
	ldy #BORDER_BR
	CALLMAIN alert::border

	; reverse the "press any key" text
	lda #DETAIL_PROMPT
	ldy #DETAIL_PROMPT_COL
	ldx #DETAIL_PROMPT_COL+DETAIL_PROMPT_LEN
	CALLMAIN scr::rvsline_part

	jmp full_bounds
.endproc

;*******************************************************************************
; MODAL BOUNDS
; Sets the bounds for the modal displayed around the "details" pop-up when a
; symbol is selected.
.proc modal_bounds
	lda #ALERT_LCOL
	sta text::puts_start
	lda #ALERT_RCOL+1
	sta text::puts_stop
	rts
.endproc

;*******************************************************************************
; FULL BOUNDS
; Restores the horizontal drawing bounds to the full screen width (when the
; details modal is closed)
.proc full_bounds
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop
	rts
.endproc

;*******************************************************************************
; ITEM INDEX
; Converts a screen row to its symbol index on the current list page.
; IN:
;   - .A: screen row
; OUT:
;   - .XY: symbol index (which may be past the end of the table)
.proc item_index
	clc
	adc page_top
	tax
	lda page_top+1
	adc #$00
	tay
	rts
.endproc

;*******************************************************************************
; DRAW ROWS
; Draws the given range of rows, blanking rows beyond the last symbol
; IN:
;   - .A: first screen row
;   - .X: row past the end of the range
.proc draw_rows
	sta draw_row
	stx draw_stop

@next:	lda draw_row
	jsr item_index
	cmpw lbl::num
	bcs @blank		; out of labels -> blank row
	jsr get_item_impl
	lda draw_row
	jsr print_item_impl
	jmp @advance

@blank: lda draw_row
	jsr blank_item_impl

@advance:
	inc draw_row
	lda draw_row
	cmp draw_stop
	bcc @next
	rts
.endproc

;*******************************************************************************
; GET ROW ITEM
; Looks up the symbol on a screen row of the current list page.
; IN:
;   - .A: screen row
; OUT:
;   - symbol fields: populated by get_item_impl
.proc get_row_item
	jsr item_index
	jmp get_item_impl
.endproc

.ifdef vic20
.ifdef soft4x8
; Preserve the final glyph copied back by scr::restore before unblanking.
.proc save_corner
	ldx #15
	bne copy_corner
.endproc
.proc copy_corner
@copy:	lda $1ff0,x
	sta redraw_corner,x
	lda #$55
	sta $1ff0,x
	dex
	bpl @copy
	rts
.endproc
.endif
;*******************************************************************************
; FINISH RESTORE
; Unblank after restoring the saved editor screen. The soft bitmap's blanking borrows
; the bottom-right glyph; preserve the newly drawn pixels instead of letting
; unblank restore the glyph that was there before the redraw.
.proc finish_restore
.ifdef soft4x8
	CALLMAIN scr::unblank
	ldx #15
@restore:
	lda redraw_corner,x
	sta $1ff0,x
	dex
	bpl @restore
	rts
.else
	JUMPMAIN scr::unblank
.endif
.endproc
.endif

.ifdef vic20
.proc finish_redraw
	CALLMAIN scr::unblank
.ifdef soft4x8
	lda #SCREEN_HEIGHT-2
	ldxy #redraw_lastrow
	CALLMAIN text::puts
	lda #HEIGHT
	ldxy #redraw_footer
	JUMPMAIN text::puts
.else
	rts
.endif
.endproc
.endif

;*******************************************************************************
; ENTER
; Enters the symbol viewer. Page position always refers to the first item, so
; switching field pages or closing details does not need to reconstruct it.
.proc enter
.ifdef vic20
	CALLMAIN scr::savebuf
.else
	CALLMAIN scr::save
.endif
	lda #$00
	sta sortby
	sta view_page
	sta view_details
	ldx #HEIGHT
	CALLMAIN draw::hiline

@start: lda #$00
	sta selection
	sta page_top
	sta page_top+1
@redraw:
.if .defined(vic20) .and .defined(soft4x8)
	; Every row is replaced below. Clearing the bitmap here would overwrite
	; the glyph borrowed by blanking. Reset colors before borrowing it.
	lda #CUR_OFF
	sta cur::status
	CALLMAIN scr::clrcolor
	CALLMAIN scr::blank
.else
.ifdef vic20
	CALLMAIN scr::blank
.endif
	CALLMAIN edit::clear
.endif
	ldxy #sort_by_addr_msg
	lda sortby
	beq :+
	ldxy #sort_by_name_msg
:	tya
	pha
	txa
	pha

	; draw values (view_page==0) or locations (view_page!=0)
	ldxy #values_msg
	lda view_page
	beq :+
	ldxy #locations_msg
:	RENDER_STR
.if .defined(vic20) .and .defined(soft4x8)
	; The rendered footer is a plain string. Pad it to a full row and keep
	; it out of the bottom-right glyph until the gutter is restored.
	stxy output_src
	ldy #$00
@footer:
	lda (output_src),y
	beq @pad
	sta redraw_footer,y
	iny
	cpy #SCREEN_WIDTH
	bcc @footer
	bcs @drawrows
@pad:	lda #' '
@spaces:
	sta redraw_footer,y
	iny
	cpy #SCREEN_WIDTH
	bcc @spaces
@drawrows:
.else
	lda #HEIGHT
	CALLMAIN text::print
.endif

	lda #$00
	ldx #HEIGHT
	jsr drawrows
.ifdef vic20
	jsr finish_redraw
.endif

;-------------------------------------------------------------------------------
@menu:	ldx selection
	CALLMAIN draw::hiline

@key:	CALLMAIN key::waitch
	pha
	ldx selection
	CALLMAIN draw::resetline
	pla
	ldx view_details
	beq @listkeys
	lda #$00
	sta view_details

	; redraw the rows that were covered by the modal
.ifdef vic20
	CALLMAIN scr::blank
.endif
	jsr modal_bounds_here
	lda #DETAIL_TOP
	ldx #DETAIL_BOTTOM+1
	jsr drawrows
	jsr full_bounds_here
.ifdef vic20
	CALLMAIN scr::unblank
.endif
	jmp @menu
@quit:
.ifdef vic20
	CALLMAIN scr::blank
	CALLMAIN scr::restore
.ifdef soft4x8
	jsr save_corner
.endif
	jmp finish_restore
.else
	JUMPMAIN scr::restore
.endif

@listkeys:
	cmp #K_QUIT
	beq @quit
	cmp #K_WIN_CLOSE
	beq @quit
	cmp #K_RETURN
	jeq @select
	cmp #$85               ; F1
	bne :+
	lda sortby
	eor #$01
	sta sortby
	jmp @start

:	cmp #' '
	beq @details
	CALLMAIN key::isleft
	beq @page
	CALLMAIN key::isright
	beq @page
	CALLMAIN key::isdown
	beq @down
	CALLMAIN key::isup
	beq @up
	jmp @menu
@page:
	lda view_page
	eor #$01
	sta view_page
	jmp @redraw

@details:
	lda lbl::num
	ora lbl::num+1
	jeq @menu
.ifdef vic20
	CALLMAIN scr::blank
.endif
	jsr @selected_item
	lda #$01
	sta view_details
	jsr print_details
.ifdef vic20
	CALLMAIN scr::unblank
.endif
	jmp @key

@down:	lda selection
	clc
	adc #$01
	jsr item_index_here
	cmpw lbl::num
	jcs @menu
	inc selection
	lda selection
	cmp #HEIGHT
	jcc @menu
	stxy page_top           ; first item on the next page
	lda #$00
	sta selection
	jmp @redraw

@up:	lda selection
	beq @previous_page
	dec selection
	jmp @menu

@previous_page:
	lda page_top
	ora page_top+1
	jeq @menu
	lda page_top
	sec
	sbc #HEIGHT
	sta page_top
	bcs :+
	dec page_top+1
:	lda #HEIGHT-1
	sta selection
	jmp @redraw

@select:
	lda lbl::num
	ora lbl::num+1
	jeq @quit
.ifdef vic20
	CALLMAIN scr::blank
.endif
	jsr @selected_item
	CALLMAIN scr::restore
.ifdef vic20
.ifdef soft4x8
	jsr save_corner
.endif
	jsr finish_restore
.endif
	ldxy lbl
	JUMPMAIN dbg::gotolabel

;-------------------------------------------------------------------------------
@selected_item:
	lda selection
	jmp get_item
.endproc
