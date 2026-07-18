;*******************************************************************************
; GUI.ASM
; This file contains the window manager for the windows that share the bottom
; of the screen: list menus (error log, breakpoints, watches, buffers) and
; custom windows (the monitor and memory viewer).
;
; Open windows are kept in a stack. The top of the stack is the "active"
; window, which is displayed with its contents at the bottom of the window
; area (gui::baserow).  The title of every open window is displayed above the
; active window's contents so that it is apparent which windows are open.
; The editor is resized to fit above the window area.
;
; Windows are created from a table with the following layout:
;	0: type id (see guis.inc)
;	1: class (GUI_CLASS_LIST or GUI_CLASS_CUSTOM)
;	2: initial height (content rows)
;	3: minimum height
;	4: maximum height
;	5: address of the title string
;	LIST class:                     CUSTOM class:
;	7: key handler                  7: draw handler
;	9: get line handler             9: enter handler
;	B: pointer to number of items   B: resize handler (0 for none)
;
; LIST handlers:
;  key handler:  IN:  .A: the key code, .X: the selected item's index
;                OUT: .C: set to exit the window with the code in .A
;                     (GUI_RET_QUIT or GUI_RET_SWITCH)
;  get line:     IN:  .A: the item index to get the line of data for
;                OUT: .XY: the rendered line to display
;
; While a window has focus, gui::cursave_x/y hold the editor's cursor (the
; actual cursor belongs to the focused window).  Handlers that move the
; editor's cursor on purpose (e.g. navigating to an error's line) must update
; the saved cursor to match.
;
; CUSTOM handlers (all called with .A: top content row, .X: bottom row):
;  draw handler:   draws the full contents of the window
;  enter handler:  interacts until done; returns a GUI_RET_x code in .A
;  resize handler: (optional) called instead of the draw handler when the
;                  window grows or shrinks.  The window's contents are
;                  anchored at the bottom row, so rows that remain on screen
;                  keep their contents; the handler only needs to draw the
;                  rows revealed by growing (its own saved geometry still
;                  holds the pre-resize rows when it is called).  A window
;                  with no resize handler (0) gets a full draw instead
;
; All handler vectors must reside in the MAIN bank.  Windows implemented in
; another bank (e.g. the monitor) must register MAIN-bank stubs that forward
; to their bank.
; Handlers should stay away from the gui zeropage area (see zeropage.inc)
;*******************************************************************************

.include "beep.inc"
.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "edit.inc"
.include "guis.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "settings.inc"
.include "string.inc"
.include "text.inc"
.include "zeropage.inc"

;*******************************************************************************
MAX_WINDOWS = 6

;*******************************************************************************
; WINdow record offsets
WIN_TYPE   = 0		; window type id (GUI_x)
WIN_CLASS  = 1		; GUI_CLASS_LIST or GUI_CLASS_CUSTOM
WIN_HEIGHT = 2		; current content height in rows
WIN_MINH   = 3		; minimum content height
WIN_MAXH   = 4		; maximum content height
WIN_TITLE  = 5		; address of the title string
WIN_V0     = 7		; LIST: key handler           CUSTOM: draw handler
WIN_V1     = 9		; LIST: getline handler       CUSTOM: enter handler
WIN_V2     = $b		; LIST: pointer to # of items CUSTOM: resize handler
WIN_SCROLL = $d		; LIST: scroll offset
WIN_SELECT = $e		; LIST: selection offset
WIN_PREMAX = $f		; height before the window was maximized (0 if none)
WIN_SIZE   = $10

WIN_DESC_SIZE = WIN_SCROLL	; the part initialized from the descriptor

;*******************************************************************************
; zeropage state for the active window (see zeropage.inc)
wtop    = zp::gui	; first (top) row of the active window's contents
getkey  = zp::gui+1	; LIST: key handler
getline = zp::gui+3	; LIST: get line handler
numptr  = zp::gui+5	; LIST: pointer to the number of items
num     = zp::gui+7	; LIST: number of items (loaded from numptr)
hght    = zp::gui+8	; effective content height of the active window
scroll  = zp::gui+9	; LIST: scroll offset
select  = zp::gui+$a	; LIST: selection offset
wbot    = zp::gui+$b	; last (bottom) row of the active window's contents

.BSS
;*******************************************************************************
windows: .res MAX_WINDOWS*WIN_SIZE	; the window stack (0 = bottom)
rectmp:  .res WIN_SIZE			; scratch record for reordering

; dispatch vector for window handlers
gvec: .word 0

; active window's effective height before a grow/shrink (see resized)
oldh: .byte 0

; save area for the focused window's input line (see savelb/restorelb)
lbsave: .res LINESIZE

; cursor position of the editor, saved while a window has focus.
; window code that redraws the editor behind the manager's back (e.g. the
; debugger's source view) must keep these in sync
.export __gui_cursave_x
.export __gui_cursave_y
__gui_cursave_x: .byte 0
__gui_cursave_y: .byte 0

.DATA
;*******************************************************************************
depth: .byte 0		; number of open windows

; The bottom row of the window area.  Windows are anchored here; rows below
; belong to the status bar (and, while debugging, the debug info rows)
.export __gui_baserow
__gui_baserow:
baserow: .byte STATUS_ROW-1

; type id of the active window (0 if no windows are open)
.export __gui_active_type
__gui_active_type: .byte 0

; set while a window has focus (used to keep the editor's cursor saved)
infocus: .byte 0

.PUSHSEG
.RODATA
; byte offset of each window record in the stack
recoffs:
.repeat MAX_WINDOWS+1, i
	.byte i*WIN_SIZE
.endrepeat
.POPSEG

.segment "GUICODE"

;*******************************************************************************
; RECPTR
; Returns a pointer to the window record at the given stack index
; IN:
;   - .X: the stack index of the record
; OUT:
;   - r0: address of the record
.proc recptr
	lda recoffs,x
	clc
	adc #<windows
	sta r0
	lda #>windows
	adc #$00
	sta r0+1
	rts
.endproc

;*******************************************************************************
; ACTIVE
; Returns a pointer to the active (top of stack) window record
; OUT:
;   - r0: address of the active record
;   - .C: set if no windows are open
.proc active
	ldx depth
	beq @none
	dex
	jsr recptr
	clc
	rts
@none:	sec
	rts
.endproc

;*******************************************************************************
; HEIGHT
; Returns the effective content height of the active window.  For LIST
; windows this is the smaller of the window's height and its item count
; (at least 1 row).
; IN:
;   - r0: address of the active record
; OUT:
;   - .A: the effective height
.proc height
@num=zp::guitmp
	ldy #WIN_CLASS
	lda (r0),y
	bne @custom

	; LIST: min(height, number of items)
	ldy #WIN_V2
	lda (r0),y
	sta @num
	iny
	lda (r0),y
	sta @num+1
	ldy #$00
	lda (@num),y
	ldy #WIN_HEIGHT
	cmp (r0),y
	bcc @clamp	; fewer items than rows: shrink to fit

@custom:
	ldy #WIN_HEIGHT
	lda (r0),y

@clamp:	bne @done
	lda #$01	; at least 1 row
@done:	rts
.endproc

;*******************************************************************************
; LAYOUT
; Computes the geometry for the active window: its content rows
; [wtop, wbot] and the editor height that leaves room for the window and
; one title row per open window.  The height is clamped so that the title
; rows never leave the screen; a maximized window leaves the editor no rows
; at all (.A = $ff).
; IN:
;   - r0: address of the active record
; OUT:
;   - wtop/wbot/hght: geometry of the active window's contents
;   - .A: new editor height (index of its last row)
;   - .C: clear if editor is completely hidden by GUI window(s)
.proc layout
	jsr height
	sta hght

	; clamp to the rows above the baserow not needed for title rows
	lda baserow
	sec
	sbc depth
	adc #$00	; .A = baserow - depth + 1 (.C was set)
	cmp hght
	bcs :+
	sta hght

:	lda baserow
	sta wbot
	sec
	sbc hght	; .A = the active window's title row
	tax
	inx
	stx wtop	; contents begin below the title

	; editor's last row = baserow - height - (1 title row per window)
	sbc depth
	rts
.endproc

;*******************************************************************************
; SWAPCUR
; If a window has focus, exchanges the live cursor with the saved editor
; cursor.  Used around editor drawing so that it sees its own cursor
.proc swapcur
	lda infocus
	beq @done
	ldx zp::curx
	lda __gui_cursave_x
	stx __gui_cursave_x
	sta zp::curx
	ldx zp::cury
	lda __gui_cursave_y
	stx __gui_cursave_y
	sta zp::cury
@done:	rts
.endproc

;*******************************************************************************
; SAVELB / RESTORELB
; If a window has focus, saves/restores the linebuffer.  The focused window's
; input line (edit::gets) lives in the linebuffer, which editor reflows
; overwrite (they read the new current source line into it).
; The editor's own linebuffer contents are reloaded from the source when
; focus returns to it (see enter)
.proc savelb
	lda infocus
	beq @done
	ldx #LINESIZE-1
:	lda mem::linebuffer,x
	sta lbsave,x
	dex
	bpl :-
@done:	rts
.endproc

.proc restorelb
	lda infocus
	beq @done
	ldx #LINESIZE-1
:	lda lbsave,x
	sta mem::linebuffer,x
	dex
	bpl :-
@done:	rts
.endproc

;*******************************************************************************
; DRAW ALL
; Redraws the entire window area: resizes the editor (rendering any rows it
; gains), draws the active window's contents, and draws the title row of
; every open window.
; Does nothing if no windows are open.
.export __gui_refresh
__gui_refresh:
.proc draw_all
	jsr active
	bcc :+
	rts		; no windows are open

:	jsr layout
	jsr update_editor

	; draw the active window's contents
	jsr active
	ldy #WIN_CLASS
	lda (r0),y
	beq @list
	ldy #WIN_V0
	jsr callvec
	jmp draw_titles

@list:	jsr list_loadvars
	jsr list_draw
	jmp draw_titles
.endproc

;*******************************************************************************
; UPDATE EDITOR
; Resizes the editor to the height computed by layout, rendering any new rows
; as needed.
; Must be called with the .A and .C values returned by layout intact.
; IN:
;   - .A: the new editor height (index of its last row)
;   - .C: set if the editor should have at least one row; clear if the windows
;         cover every row and the editor must be hidden (.A is then invalid)
.proc update_editor
@row=zp::guitmp
@edh=zp::guitmp+2
	bcs @gotedh	; the editor keeps at least one row (0 = only row 0)
	lda #$ff	; the windows fill every row above the baserow

@gotedh:
	sta @edh

	; resize the editor to fit above the windows
	cmp zp::editor_height
	beq @done
	cmp #$ff
	beq @edhide
	ldx zp::editor_height
	inx		; was the editor hidden?
	beq @edunhide
	cmp zp::editor_height
	bcs @edgrow

@edshrink:
	; the reflow (edit::resize) reads the new current line into the
	; linebuffer, which the focused window's input may live in: save it
	; around the call.  The editor's linebuffer is reloaded when focus
	; returns (see enter)
	jsr swapcur
	jsr savelb
	lda @edh
	jsr edit::resize	; shrink: reflows the cursor, no redraw
	jsr restorelb
	jmp swapcur

@edhide:
	; hide the editor; its cursor is clamped when it is revealed again
	sta zp::editor_height
@done:	rts

@edunhide:
	; The editor had no rows: reflow its cursor into the new height and
	; render every row.  The reflow may redraw rows below the new height
	; as it walks the cursor up, so this path must only run when the
	; whole window area is redrawn afterwards (see resized, which falls
	; back to draw_all for the unhide transition)
	jsr swapcur
	jsr savelb	; protect the focused window's input line
	lda @edh
	jsr edit::resize
	jsr restorelb
	ldx #$00
	beq @grow1	; render rows 0..@edh (branch always)

@edgrow:
	; render the source rows revealed by the shrinking window area
	jsr swapcur
	ldx zp::editor_height
	inx
	lda @edh
	sta zp::editor_height
@grow1:	stx @row
@grow0:	ldx @row
	lda #$00
	sta mem::breakpoint_rows,x
	jsr draw::resetline	; clear the old title/window color
	lda @row
	jsr edit::render_row
	inc @row
	lda @row
	cmp @edh
	bcc @grow0
	beq @grow0
	jmp swapcur
.endproc

;*******************************************************************************
; DRAW TITLES
; Draws one title row per open window; the active window's title is directly
; above its contents (centered), older windows' titles are drawn above it at the
; far left of their rows.
; Also updates the active window type.
.proc draw_titles
@row=zp::guitmp
@i=zp::guitmp+1
@off=zp::guitmp+2
@title=r2
	ldx wtop
	dex
	stx @row
	ldx depth
	dex
	stx @i
@t0:	lda @row
	jsr scr::clrline
	ldx @i
	jsr recptr
	ldy #WIN_TITLE
	lda (r0),y
	sta @title
	iny
	lda (r0),y
	sta @title+1

	ldx #$00	; column for inactive windows: far left
	ldy @i
	iny
	cpy depth
	bne @pad

	; active window: center the title
	ldxy @title
	jsr str::len
	eor #$ff		; .A = -(len+1)
	sec
	adc #SCREEN_WIDTH	; .A = SCREEN_WIDTH - len
	lsr			; /2
	tax

@pad:	; build the title line with .X leading spaces
	stx @off
	lda #' '
	ldx #$00
@sp:	cpx @off
	bcs @cp
	sta mem::spare,x
	inx
	bne @sp

@cp:	ldy #$00
@cp0:	lda (@title),y
	sta mem::spare,x
	beq @print	; done at the 0 terminator
	inx
	iny
	bne @cp0

@print:	ldxy #mem::spare
	lda @row
	jsr text::print
	ldx @row
	lda #COLOR_RVS
	jsr draw::hline
	dec @row
	dec @i
	bpl @t0

	; update the active window type
	jsr active
	ldy #WIN_TYPE
	lda (r0),y
	sta __gui_active_type
	rts
.endproc

;*******************************************************************************
; CALLVEC
; Calls the active window's handler whose vector is at the given record
; offset, passing the window's content geometry
; IN:
;   - .Y: record offset of the vector (WIN_V0 or WIN_V1)
;   - r0: address of the active record
.proc callvec
	lda (r0),y
	sta gvec
	iny
	lda (r0),y
	sta gvec+1
	lda wtop
	ldx wbot
	jmp (gvec)
.endproc

;*******************************************************************************
; LIST LOADVARS
; Copies the active LIST window's state to the zeropage and clamps the
; scroll/selection to the item count, which may have changed since the state
; was saved (e.g. an item was deleted)
; IN:
;   - r0: address of the active record
.proc list_loadvars
	; copy the three handler vectors
	ldy #WIN_V0
	ldx #$00
:	lda (r0),y
	sta getkey,x
	iny
	inx
	cpx #$06
	bcc :-

	ldy #WIN_SCROLL
	lda (r0),y
	sta scroll
	ldy #WIN_SELECT
	lda (r0),y
	sta select

	ldy #$00
	lda (numptr),y
	sta num

	; clamp scroll/selection to the item count
	lda scroll
	clc
	adc select
	cmp num
	bcc @inwin	; scroll+select < num: still a valid item

	ldx num
	bne :+
	stx scroll	; no items: home the selection
	stx select
	rts

:	dex		; .X = index of the last item
	txa
	cmp hght
	bcs @scrl
	stx select	; all items fit: select the last one
	lda #$00
	sta scroll
	rts

@scrl:	;sec
	sbc hght	; .A = last - hght
	adc #$00	; scroll = last - hght + 1
	sta scroll
	ldx hght
	dex
	stx select	; select the bottom row
	rts

@inwin:	lda select
	cmp hght
	bcc @done	; select < hght: still within the window
	;sec
	sbc hght	; .A = select - hght
	;sec
	adc #$00	; .A = select - hght + 1
	clc
	adc scroll	; .A = scroll + (select - hght + 1)
	sta scroll
	ldx hght
	dex
	stx select	; select the bottom row
@done:	rts
.endproc

;*******************************************************************************
; LIST SAVEVARS
; Saves the zeropage scroll/selection state back to the active record
.proc list_savevars
	jsr active
	bcs @done
	ldy #WIN_SCROLL
	lda scroll
	sta (r0),y
	ldy #WIN_SELECT
	lda select
	sta (r0),y
@done:	rts
.endproc

;*******************************************************************************
; LIST DRAW
; Draws the contents of the active LIST window (item 0 on the bottom row,
; increasing upwards) and highlights the selected item
.proc list_draw
	lda #$00	; draw from the bottom row

	; fall through to list_draw_from
.endproc

;*******************************************************************************
; LIST DRAW FROM
; Draws the active LIST window's contents from the given item offset up to
; the top of the window and highlights the selected item.  Used to draw only
; the rows revealed by growing the window (the rows below are unchanged)
; IN:
;   - .A: the item offset (number of rows above the bottom) to draw from
.proc list_draw_from
@row=zp::guitmp
@i=zp::guitmp+1
	sta @i
	lda wbot
	sec
	sbc @i
	sta @row

@l0:	lda scroll
	clc
	adc @i
	cmp num
	bcs @blank	; row is past the last item

	jsr @callgetline ; get the next line of data

	lda @row
	jsr text::print
	jmp @next

@blank:	lda @row
	jsr scr::clrline

@next:	ldx @row
	jsr draw::resetline
	dec @row
	inc @i
	lda @i
	cmp hght
	bcc @l0
	jmp list_highlight

@callgetline:
	jmp (getline)
.endproc

;*******************************************************************************
; LIST HIGHLIGHT
; Highlights the selected item of the active LIST window (does nothing if
; the window has no items)
.proc list_highlight
	lda num
	beq @done
	lda wbot
	sec
	sbc select
	tax
	lda #COLOR_SELECT
	jmp draw::hline
@done:	rts
.endproc

;*******************************************************************************
; LIST ENTER
; The interaction loop shared by all LIST windows.  Handles selection
; movement and the common window keys; all other keys are passed to the
; window's key handler.
; OUT:
;   - .A: the GUI_RET_x code to return to the manager
.proc list_enter
@loop:	jsr key::waitch
	pha

	; unhighlight the selection in case we move
	lda num
	beq :+
	lda wbot
	sec
	sbc select
	tax
	jsr draw::resetline

:	pla
	cmp #K_QUIT
	beq @quit
	cmp #K_CLOSE_WINDOWS
	beq @quit

	cmp #K_SWAP_WINS
	bne :+
	jsr list_savevars
	lda #GUI_RET_CYCLE
	rts

:	cmp #K_WIN_GROW
	bne :+
	jsr __gui_grow
	jmp @rehl

:	cmp #K_WIN_SHRINK
	bne :+
	jsr __gui_shrink

@rehl:	jsr list_highlight	; rehighlight (it was cleared above)
	jmp @loop
:	cmp #K_WIN_MAXIMIZE
	bne :+
	jsr __gui_maximize
	jmp @loop

:	jsr key::isup
	bne @chkdown
@up:	lda select
	clc
	adc scroll
	adc #$01	; need to check num-1
	cmp num
	bcs @redraw	; out of bounds: rehighlight (it was cleared above)
	ldx select
	inx
	cpx hght
	bcc @goup	; if selection is below the top row, just move it
	inc scroll
	jmp @redraw
@goup:	inc select
	jmp @redraw

@chkdown:
	jsr key::isdown
	bne @getch
	lda select
	beq @scrolldn
	dec select
	jmp @redraw
@scrolldn:
	lda scroll
	beq @redraw	; can't move: rehighlight (it was cleared above)
	dec scroll

@redraw:
	jsr list_savevars
	jsr draw_all
	jmp @loop

@quit:	jsr list_savevars
	lda #GUI_RET_QUIT
	rts

;-------------------------------------------------------------------------------
@getch:	pha
	jsr list_savevars	; the handler may change the active window
	pla
	jsr @keycallback
	bcc @redraw		; if the handler didn't exit, redraw & continue
	rts			; return the handler's GUI_RET_x code

@keycallback:
	; get the selected item's index
	pha
	lda scroll
	clc
	adc select
	tax
	pla
	jmp (getkey)
.endproc

;*******************************************************************************
; ENTER
; Gives focus to the active window until the user returns focus to the
; editor.  Beeps if no windows are open.
.export __gui_enter
.proc __gui_enter
	lda depth
	bne :+
	jmp beep::short	; no windows are open

:	lda zp::curx
	sta __gui_cursave_x
	lda zp::cury
	sta __gui_cursave_y
	lda #$01
	sta infocus

@loop:	jsr draw_all
	jsr dispatch_enter
	cmp #GUI_RET_CYCLE
	beq @cycle
	cmp #GUI_RET_SWITCH
	beq @loop

	; GUI_RET_QUIT: return focus to the editor
	jsr unhide_editor	; give the editor its rows back if it has none
	lda #$00
	sta infocus
	jsr cur::unlimit
	lda __gui_cursave_x
	sta zp::curx
	lda __gui_cursave_y
	sta zp::cury
	; reload the editor's line: resizes during the session may have moved
	; the cursor, and the window's input shared the linebuffer
	jmp edit::refreshline

@cycle:	jsr rotate
	jmp @loop
.endproc

;*******************************************************************************
; DISPATCH ENTER
; Runs the active window's interaction handler
; OUT:
;   - .A: the GUI_RET_x code it returned
.proc dispatch_enter
	jsr active
	ldy #WIN_CLASS
	lda (r0),y
	beq @list
	ldy #WIN_V1
	jmp callvec
@list:	jmp list_enter
.endproc

;*******************************************************************************
; ROTATE
; Moves the active window to the bottom of the stack, making the next most
; recently used window active
.proc rotate
	lda depth
	cmp #$02
	bcc @done	; nothing to rotate

	; save the active record
	ldx depth
	dex
	jsr recptr
	ldy #WIN_SIZE-1
:	lda (r0),y
	sta rectmp,y
	dey
	bpl :-

	; shift every other record up one slot
	ldx depth
	lda recoffs,x
	tax
	dex
@l0:	lda windows-WIN_SIZE,x
	sta windows,x
	dex
	cpx #WIN_SIZE
	bcs @l0

	; and put the old active window on the bottom
	ldx #WIN_SIZE-1
:	lda rectmp,x
	sta windows,x
	dex
	bpl :-
@done:	rts
.endproc

;*******************************************************************************
; SELECT
; Makes the window for the given descriptor the active window without giving
; it focus.  If a window of the descriptor's type is already open it is
; raised (keeping its state); otherwise a new window is created.
; IN:
;   - .XY: address of the window descriptor
.export __gui_select
.proc __gui_select
@desc=r2
@type=zp::guitmp
@end=zp::guitmp+1
	stxy @desc

	; look for an open window of this type
	ldy #WIN_TYPE
	lda (@desc),y
	sta @type
	ldx #$00
@find:	cpx depth
	bcs @create
	jsr recptr
	ldy #WIN_TYPE
	lda (r0),y
	cmp @type
	beq @found
	inx
	bne @find

;-------------------------------------------------------------------------------
; not open yet; create a new window on top of the stack
@create:
	ldx depth
	cpx #MAX_WINDOWS
	bcs @done	; too many windows
	jsr recptr

	; copy the descriptor
	ldy #WIN_DESC_SIZE-1
:	lda (@desc),y
	sta (r0),y
	dey
	bpl :-

	; initialize scroll/selection offsets and pre-maximize height
	lda #$00
	ldy #WIN_SCROLL
	sta (r0),y
	ldy #WIN_SELECT
	sta (r0),y
	ldy #WIN_PREMAX
	sta (r0),y

	inc depth
	bne @settype	; branch always
@done:	rts

;-------------------------------------------------------------------------------
; window .X is already open; raise it to the top of the stack
@found:	txa
	tay
	iny
	cpy depth
	beq @settype	; already active

	; save the record
	jsr recptr
	ldy #WIN_SIZE-1
:	lda (r0),y
	sta rectmp,y
	dey
	bpl :-

	; shift the records above it down one slot
	ldy depth
	dey
	lda recoffs,y
	sta @end	; byte offset of the top record
	lda recoffs,x
	tax
@l0:	lda windows+WIN_SIZE,x
	sta windows,x
	inx
	cpx @end
	bcc @l0

	; and put the raised window on top
	ldy #$00
:	lda rectmp,y
	sta windows,x
	inx
	iny
	cpy #WIN_SIZE
	bcc :-

@settype:
	lda @type
	sta __gui_active_type
	rts
.endproc

;*******************************************************************************
; OPEN
; Opens (or raises) the window for the given descriptor and gives it focus
; until the user returns focus to the editor
; IN:
;   - .XY: address of the window descriptor
.export __gui_open
.proc __gui_open
	jsr __gui_select
	jmp __gui_enter
.endproc

;*******************************************************************************
; GROW
; Grows the active window by one row (if it has room to grow by).
; Redraws only what the resize changed on screen (see resized).
; OUT:
;   - .A: the top content row of the active window
;   - .X: the bottom content row of the active window
.export __gui_grow
.proc __gui_grow
	jsr active
	bcs geom
	jsr layout	; get the effective height before growing
	lda hght
	sta oldh
	ldy #WIN_HEIGHT
	lda (r0),y
	ldy #WIN_MAXH
	cmp (r0),y
	bcs geom	; already at max height

	adc #$01	; (.C clear)
	ldy #WIN_HEIGHT
	sta (r0),y

	jsr layout
	bcs @ok		; the editor keeps at least one row

	; editor would be hidden: allow it only if the window has focus
	; and the new height is displayable
	lda infocus
	beq @revert	; the editor is active: make sure it has > 0 rows

	lda hght
	ldy #WIN_HEIGHT
	cmp (r0),y
	bne @revert	; the new height exceeds the displayable rows

	lda oldh	; save the pre-grow height so un-maximizing
	ldy #WIN_PREMAX	; (or returning focus to the editor) restores it
	sta (r0),y
	jsr layout	; restore .A/.C (the editor height)
@ok:	jmp resized

@revert:
	ldy #WIN_HEIGHT
	lda (r0),y
	sec
	sbc #$01
	sta (r0),y
	jsr layout	; restore the active geometry
	jmp geom
.endproc

;*******************************************************************************
; SHRINK
; Shrinks the active window by one row (if it is above its minimum height).
; Redraws only what the resize changed on screen (see resized).
; OUT:
;   - .A: the top content row of the active window
;   - .X: the bottom content row of the active window
.export __gui_shrink
.proc __gui_shrink
	jsr active
	bcs geom
	jsr layout	; get the effective height before shrinking
	lda hght
	sta oldh
	ldy #WIN_HEIGHT
	lda (r0),y
	ldy #WIN_MINH
	cmp (r0),y
	beq geom	; already at min height
	bcc geom

	sec
	sbc #$01
	ldy #WIN_HEIGHT
	sta (r0),y

	jsr layout
	jmp resized
.endproc

;*******************************************************************************
; REDRAW
; redraw the window area and return the active window's geometry
redraw:	jsr draw_all
geom:	lda wtop
	ldx wbot
	rts

;*******************************************************************************
; RESIZED
; Redraws only what a grow/shrink of the active window changed on screen.
; The window's contents are anchored at the baserow, so rows that remain in
; the window keep their contents: growing draws the newly revealed top rows
; and shrinking draws no content at all (the title rows and the editor
; absorb the difference).
; Must be entered with the .A and .C values returned by layout intact (they
; are forwarded to update_editor).
; IN:
;   - oldh: effective height before the resize
;   - .A:   new editor height (index of its last row)
;   - .C:   set if the editor should keet at least one row; clear if not
; OUT:
;   - .A: the top content row of the active window
;   - .X: the bottom content row of the active window
.proc resized
	; if the editor is coming back from hidden, its cursor reflow may
	; draw on rows that still belong to the windows: use a full redraw,
	; which repaints the whole window area afterwards
	bcc :+			; if editor hidden -> continue
	ldx zp::editor_height
	inx			; WAS the editor hidden ($ff)?
	beq redraw		; unhide: redraw everything

:	jsr update_editor
	jsr active
	ldy #WIN_CLASS
	lda (r0),y
	bne @custom

	; LIST: if clamping the scroll/selection to the new height moved
	; them, the contents shift: redraw them all
	jsr list_loadvars
	ldy #WIN_SCROLL
	lda (r0),y
	cmp scroll
	bne @full
	ldy #WIN_SELECT
	lda (r0),y
	cmp select
	bne @full

	lda hght
	cmp oldh
	beq geom	; same effective height: nothing changed
	bcc @titles	; shrank: the remaining rows are unchanged
	lda oldh	; grew: draw only the revealed top rows
	jsr list_draw_from
	jmp @titles

@full:	jsr list_draw
	jmp @titles

@custom:
	lda hght
	cmp oldh
	beq geom	; same effective height: nothing changed
	ldy #WIN_V2
	lda (r0),y
	iny
	ora (r0),y
	beq @fulldraw	; no resize handler: draw the full contents

	dey		; .Y=WIN_V2
	jsr callvec	; call resize handler
	jmp @titles

@fulldraw:
	ldy #WIN_V0
	jsr callvec

@titles:
	jsr draw_titles	; the title rows moved with the resize
	jmp geom
.endproc

;*******************************************************************************
; MAXIMIZE
; Maximizes the active window: gives it every row above the baserow not
; needed for the open windows' title rows.  LIST windows are still limited
; to their item count (see height).  The editor keeps no rows while a
; window is maximized; they are given back when the window shrinks or when
; focus returns to the editor.
; If the window is already maximized, restores the height it had before.
; OUT:
;   - .A: the top content row of the active window
;   - .X: the bottom content row of the active window
.export __gui_maximize
.proc __gui_maximize
	jsr active
	bcs geom

	; biggest height that leaves a row for each open window's title
	lda baserow
	sec
	sbc depth
	adc #$00	; .A = baserow - depth + 1 (.C was set)
	ldy #WIN_HEIGHT
	cmp (r0),y
	beq @restore	; already maximized: put back the saved height
	bcc @restore

	; save the current height (if the editor is visible at it) and maximize
	pha
	ldx zp::editor_height
	inx
	beq :+		; editor already hidden: not a height worth restoring
	lda (r0),y
	ldy #WIN_PREMAX
	sta (r0),y
:	pla
	ldy #WIN_HEIGHT
	sta (r0),y
	jmp redraw

@restore:
	jsr restore_height
	jmp redraw
.endproc

;*******************************************************************************
; RESTORE HEIGHT
; Returns the active window to the height it had before it was maximized.
; Falls back to the minimum height if no height was saved or if the saved
; one no longer leaves the editor a row.
; IN:
;   - r0: address of the active record
.proc restore_height
	ldy #WIN_PREMAX
	lda (r0),y
	beq @min
	ldy #WIN_HEIGHT
	sta (r0),y
	jsr layout
	bcs @done	; editor gets at least one row back

@min:	; resize window to its minimum height
	ldy #WIN_MINH
	lda (r0),y
	ldy #WIN_HEIGHT
	sta (r0),y

@done:	rts
.endproc

;*******************************************************************************
; UNHIDE EDITOR
; If the active window covers the entire window area, shrinks it back so
; that the editor regains its rows.  Called when focus returns to the editor
.proc unhide_editor
	lda zp::editor_height
	cmp #$ff
	bne @done
	jsr active
	bcs @done
	jsr restore_height
	jmp draw_all
@done:	rts
.endproc

;*******************************************************************************
; CLOSEALL
; Closes all windows.  Restoring the editor's size is left to the caller.
.export __gui_closeall
.proc __gui_closeall
	lda #$00
	sta depth
	sta __gui_active_type
	sta infocus
	rts
.endproc
