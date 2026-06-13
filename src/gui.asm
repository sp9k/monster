;*******************************************************************************
; GUI.ASM
; This file contains procedures for GUI functionality like graphical menus.
; GUI windows are created by defining a structure containing handlers for
; retrieving the lines of data to draw and handling key presses.
; Handlers should stay away from the gui zeropage area (see zeropage.inc)
;*******************************************************************************

.include "beep.inc"
.include "draw.inc"
.include "edit.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "screen.inc"
.include "settings.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"
.include "prefs.inc"

;*******************************************************************************
MAX_WINDOWS = 3

;*******************************************************************************
.BSS
.exportzp scroll
.exportzp select

guidata = zp::gui
guidata_size=$c
; the following is the sequence stored
maxh    = zp::gui	; max height of the GUI window
getkey	= zp::gui+1	; pointer to get key handler function
getdata = zp::gui+3	; pointer to get data handler function
numptr  = zp::gui+5	; pointer to the number of items in the GUI
title	= zp::gui+7	; pointer to the title string for the GUI

; the following are not part of the initialization struct, they are initialized
; upon constructing the window and persisted (along side the above fields)
; for the duration of it
scroll  = zp::gui+9	; amount that the GUI window is scrolled
select	= zp::gui+$a	; offset that is currently selected (highlighted)
baserow = zp::gui+$b	; base row for active GUI (set on creation of GUI)

; the following are not stored in the stack, but just used locally within
; a single procedure call
num    = zp::guitmp
height = zp::guitmp+1
guitmp = zp::guitmp+2

;*******************************************************************************
; GUISTACK
; The guistack holds the gui data for each active window.
; Activating a new window will push the current window (if any), and
; deactivating the window will pop it back into the active gui data memory.
.export guistack
guistack:	.res MAX_WINDOWS*guidata_size

;*******************************************************************************
.DATA
guisp:		.word guistack	; pointer to end of all GUIs
stackdepth:	.byte 0

;*******************************************************************************
.CODE

;*******************************************************************************
; REENTER
; Activates the most recently created GUI without reinitializing it.
.export __gui_reenter
__gui_reenter = __gui_activate

;*******************************************************************************
; LIST_MENU
; Displays a user selectable menu of options as rows of text that the given
; callbacks provide.
; The list will take up to the given number of rows, but will draw less if
; there isn't enough data to fill them.
;
; The provided key handler will be called with the offset to the item
; that is currently being selected in .A
; This occurs on any key other than <-, which quits the selection list and
; the up/down keys
;
; The provided get data handler is called to populate the lines of options.
; It is also called with the offset of the item in .A
;
; IN:
;  - .XY: pointer to the menu data struct
;	- 0: height of the view
;	- 1: address of key handler
;    		- IN:  .A: the key code, .X: the item index
;    		- OUT: .C: set if the menu should exit
;	- 3: address to the get data handler
;    		- IN:  .A: the item index to get the line of data for
;		- OUT: mem::linebuffer: the line to display
;	- 5: pointer to number of items in the GUI
;	- 7: address of menu title
;	- 9: scroll value for the menu
;	- A: selection offset for the menu
.export  __gui_listmenu
.proc __gui_listmenu
@src=r0
@stack=r2
@tmp=r4
@tmp2=r5
@dst=r6
@type=r8
	pha
	stxy @src

	; search the GUI stack to see if this editor is open already
	ldy #$00
	lda (@src),y		; get a "TYPE" byte
	sta @type
	ldx stackdepth
	bne :+
	jmp @cont		; if stack is empty, must be a new GUI

:	lda @type
	cmp guistack,y
	php

	; .Y += guidata_size+1 (move to the next record)
	tya
	clc
	adc #guidata_size+1
	tay

	plp
	beq @already_active
	dex
	bne :-
	beq @cont		; not found

;-------------------------------------------------------------------------------
; GUI is already in stack, reload it
@already_active:
	stx @tmp

	; copy the item to activate to guidata in the zeropage
	; set guisp to point to element above our chosen one
	ldx #guidata_size
:	dey
	lda guistack,y
	sta guidata-1,x		; -1 because we don't store TYPE byte
	dex
	bne :-

	; tmp = .X = sizeof(guidata)*num_elements_to_move
	lda @tmp		; restore number of elements we need to move
	cmp #$01
	beq @reactivate_cont	; if none -> continue
	asl			; *2
	asl			; *4
	sta @tmp2
	asl			; *8
	adc @tmp2		; *$c
	adc @tmp		; *$d
	sta @tmp

	sec
	sbc #guidata_size+1
	tax

	; @dst = guisp - (num_elements_to_move*sizeof(guidata))
	lda guisp
	sec
	sbc @tmp
	sta @dst
	lda guisp+1
	sbc #$00
	sta @dst+1

	; @stack = @dst + guidata_size+1
	lda @dst
	clc
	adc #guidata_size+1
	sta @stack
	lda @dst+1
	adc #$00
	sta @stack+1

	; shift all elements above the one we moved down to take its place
	ldy #$00
:	lda (@stack),y
	sta (@dst),y
	iny
	dex
	bne :-

	; and now replace the top of the stack with our new data
	lda @type
	sta (@dst),y
	iny

	ldx #$00
:	lda guidata,x
	sta (@dst),y
	inx
	cpx #guidata_size
	bne :-

@reactivate_cont:
	ldy #$00
	lda (numptr),y
	sta num

	cmp maxh
	bcc :+		; if # of items is > max height, use full height
	lda maxh	; else, only resize to the size needed to fit all items
:	clc		; ok
	sta height
	pla		; clear stack

	ldxy guisp
	stxy @stack
	bne draw_gui	; continue to redraw

;-------------------------------------------------------------------------------
; GUI isn't already in stack, load it as new
@cont:	lda guisp
	ldy guisp+1
	ldx stackdepth
	cpx #MAX_WINDOWS
	bcc :+

	; too many windows open
	; TODO: close the oldest
	pla			; clean stack
	rts			; and return

:	inc stackdepth

;-------------------------------------------------------------------------------
; copy the menu definition to the GUI stack
@copyvars:
	sta @stack
	sty @stack+1

	; update GUI stack pointer
	clc
	adc #guidata_size+1
	sta guisp
	bcc :+
	inc guisp+1

:	ldy #baserow-zp::gui+1
	pla
	sta (@stack),y		; set base row
	sta baserow

	; initialize scroll and selection offset to 0
	dey
	lda #$00
	sta (@stack),y		; select
	dey
	sta (@stack),y		; scroll
	dey

@copytostack:
	; copy the the data for this GUI from its definition
@l0:	lda (@src),y
	sta (@stack),y
	dey
	bpl @l0

	; fall through to __gui_activate
.endproc

;*******************************************************************************
; ACTIVATE
; Activates the most recently created (top of the GUI stack) GUI window.
.export __gui_activate
.proc __gui_activate
	lda stackdepth
	bne :+

	; no windows active
	jmp beep::short

:	; copy the persistent GUI state to the zeropage
	jsr copyvars

	; fall through to draw_gui
.endproc

;*******************************************************************************
; DRAW GUI
; Entrypoint for drawing GUI once guidata has been set to the contents of the
; GUI to draw
.proc draw_gui
@stack=r0
	; draw GUI before entering the main GUI loop
	dec height
@loop:	inc height
	jsr redraw_state
	dec height

	; highlight the current line
	lda num
	beq @getkey
	lda baserow
	sec
	sbc select
	tax
	lda #COLOR_SELECT
	jsr draw::hline

@getkey:
	jsr key::waitch
	pha		; save the key

	; unhighlight the current line in case we move lines
	lda num
	beq :+
	lda baserow
	sec
	sbc select
	tax
	jsr draw::resetline

:	pla		; get the key
	cmp #K_QUIT
	bne @chkup

@quit:	; TODO: copy current state back to (guisp)
	jmp savevars

@chkup:	jsr key::isup
	bne @chkdown
@up:	lda select
	clc
	adc scroll
	adc #$01		; need to check num-1
	cmp num
	bcs @loop		; out of bounds

	lda select
	cmp height
	bcc @goup		; if selection is < maxheight, just move cursor

@scrollup:
	lda select
	cmp height
	bcc @goup		; if < maxheight, just move cursor
	clc
	inc scroll
	bne @loop		; redraw the scrolled display
@goup:	inc select
	bne @loop

@chkdown:
	jsr key::isdown
	bne @getch		; if not down, call handler for all other keys

	lda select
	beq @scrolldown

	dec select
	bpl @loop
@scrolldown:
	clc
	adc scroll
	beq @loop		; can't move

	dec scroll
	bpl @loop		; redraw the scrolled display

;-------------------------------------------------------------------------------
@getch: jsr @keycallback
	bcs @quit
	jsr savevars
	jsr __gui_refresh
	jmp draw_gui

;-------------------------------------------------------------------------------
@keycallback:
	; get the item index
	pha
	lda scroll
	clc
	adc select
	tax
	pla
	jmp (getkey)
.endproc

;*******************************************************************************
; REFRESH
; Redraws the active GUI. This should be called after making changes that
; would affect the state of the GUI window, e.g. setting a breakpoint could
; cause the breakpoints GUI to populate with new data.
.export __gui_refresh
.proc __gui_refresh
	lda stackdepth
	beq exit

	; copy the persistent GUI state to the zeropage
	jsr copyvars
	bcc redraw_state

	; fall through to exit
.endproc

exit:	rts				; no GUI to draw

;*******************************************************************************
; REDRAW STATE
; entrypoint to draw the already copied zeropage state
.proc redraw_state
@row     = guitmp
@rowstop = guitmp+1
@i       = zp::gui+$c
@getline = zp::jmpvec
	; resize the main editor window to fit the GUI (may increase editor
	; size if the GUI window has shrunk since the last call)
	lda baserow
	sec
	sbc height
	sbc #$01
	jsr edit::resize

	lda baserow
	sta @row
	sec
	sbc height
	sta @rowstop

	; draw the title
	ldxy title
	jsr text::print

	ldx @rowstop
	jsr draw::hiline

	inc @rowstop

	lda #$00
	sta @i

@dloop: lda @row
	cmp @rowstop			; are we at the top row yet?
	bcc @highlight_selection	; if so, continue to highlight selected

	; copy address for getline
	lda getdata
	sta @getline
	lda getdata+1
	sta @getline+1

	lda @i
	clc
	adc scroll

	jsr zp::jmpaddr ; get the next line of data

	lda @row
	jsr text::print

	ldx @row
	jsr draw::resetline

	dec @row
	inc @i
	bne @dloop

;-------------------------------------------------------------------------------
@highlight_selection:
	lda num
	beq exit
	lda baserow
	sec
	sbc select
	tax
	lda #COLOR_SELECT
	jmp draw::hline
.endproc

;*******************************************************************************
; DEACTIVATE
; Exits the GUI that is at the top of the GUI stack
.export __gui_deactivate
.proc __gui_deactivate
	lda stackdepth
	beq @done		; do nothing if stack is empty
	lda guisp
	sec
	sbc #guidata_size+1
	sta guisp
	bcs :+
	dec guisp+1
:	dec stackdepth
@done:	rts
.endproc

;*******************************************************************************
; CLOSEALL
; Frees the GUI stack by setting it back to its base.
.export __gui_closeall
.proc __gui_closeall
	ldxy #guistack
	stxy guisp		; reset stack
	lda #$00
	sta stackdepth
	rts
.endproc

;*******************************************************************************
; SAVEVARS
; Saves the state for the active GUI vars (guidata) to the top of the GUI stack
.proc savevars
@stack=r0
	; peek the address of the top element and set @stack to it
	lda guisp
	sec
	sbc #guidata_size+1
	sta @stack
	lda guisp+1
	sbc #$00
	sta @stack+1
	incw @stack

	; copy the data from the GUI stack to the zeropage area
	ldy #guidata_size
@l0:	lda guidata,y		; -1 because we don't store 1st byte (TYPE)
	sta (@stack),y
	dey
	bne @l0
	rts
.endproc

;*******************************************************************************
; COPYVARS
; Copies the GUI state to the zeropage
; OUT:
;  - .C: set if there is no GUI active
.proc copyvars
@stack=r0
	lda stackdepth
	beq @done

	; peek the address of the top element and set @stack to it
	lda guisp
	sec
	sbc #guidata_size+1
	sta @stack
	lda guisp+1
	sbc #$00
	sta @stack+1

	; copy the data from the GUI stack to the zeropage area
	ldy #guidata_size
@l0:	lda (@stack),y
	sta guidata-1,y	; -1 because we don't store 1st byte (TYPE)
	dey
	bne @l0

	; get the number of items in the GUI (may change between activations)
	lda (numptr),y
	sta num

	cmp maxh
	bcc :+		; if # of items is > max height, use full height
	lda maxh	; else, only resize to the size needed to fit all items
:	clc		; ok
	sta height
@done:	rts
.endproc
