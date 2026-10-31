;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "monitor".
; The monitor is a text-based interface that can be used for interacting with
; program state as well as debugging.
; It runs as a window managed by the window manager (see gui.asm).  The
; monitor's screen buffer is anchored to the bottom of the window: buffer row
; (HEIGHT-1) is displayed on the window's bottom row and "winoff" maps buffer
; rows to screen rows.
;*******************************************************************************

.include "asm.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "gui.inc"
.include "guis.inc"
.include "monitorcmd.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "irq.inc"
.include "kernal.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "runtime.inc"
.include "sim6502.inc"
.include "screen.inc"
.include "settings.inc"
.include "string.inc"
.include "strings.inc"
.include "target.inc"
.include "text.inc"
.include "zeropage.inc"

.include "ram.inc"

.import is_whitespace	; from monitorcmd.asm

;*******************************************************************************
HEIGHT = SCREEN_HEIGHT

; default height of the monitor window's contents
WIN_DEFAULT_HEIGHT = 8

; loop counter/scratch used when redrawing the window
winrow = zp::monitor

.segment "SHAREBSS"
.export CMD_BUFF

; The last command entered, kept so that an empty line repeats it.
CMD_BUFF: .res LINESIZE+1

;*******************************************************************************
; WINDOWED
; Nonzero while the monitor window is open (see gui.asm)
.export __monitor_windowed
__monitor_windowed: .byte 0

; pending window action with bit 7 set (0 if none)
winreq: .byte 0

.segment "CONSOLE_VARS"

; the buffer row that the monitor's input is on
.export __monitor_line
__monitor_line:
line:      .byte 0
repeatcmd: .byte 0	; if set, empty line repeats last command
prompt_len: .byte $00	; number of characters before the command

; screen rows printed by the current command
.export __monitor_rows_printed
__monitor_rows_printed: .byte $00

wintop: .byte 0		; first screen row of the monitor's contents
winbot: .byte 0		; last screen row (the input line's row)
winoff: .byte 0		; buffer row - winoff = screen row

;*******************************************************************************
; OUTFILE
; Screen (0) or file handle to output mon::puts to
.export __monitor_outfile
__monitor_outfile: .byte 0

;*******************************************************************************
; SIGNALS
.export __monitor_int
.export __monitor_quit
__monitor_quit: .byte 0	; if !0, console will quit when command returns to it
__monitor_int: .byte 0	; if !0, behaved commands will stop running gracefully

.segment "CONSOLE_BSS"

;*******************************************************************************
; SCREEN
; This buffer stores the complete contents of the monitor.  It is used to
; restore the monitor to its last state when it is re-entered
.export screen
screen: .res LINESIZE*HEIGHT

.RODATA
;*******************************************************************************
; WINDOW
; The window descriptor for the monitor (see gui.asm)
.export __monitor_window
__monitor_window:
.byte GUI_MONITOR		; 0 id for the monitor
.byte GUI_CLASS_CUSTOM		; 1
.byte WIN_DEFAULT_HEIGHT	; 2 initial height
.byte 2				; 3 min height (input line + 1 row of history)
.byte SCREEN_HEIGHT		; 4 max height (layout clamps to the rows that
				; fit; at full height the editor is hidden)
.word strings::monitor_title	; 5 title
.word __monitor_windraw		; 7 draw handler
.word __monitor_winenter	; 9 enter handler
.word __monitor_winresize	; $b resize handler
.byte 0				; $d unused
.byte 0				; $e unused
.byte 0				; $f pre-maximized height
.byte 0				; $10 unused

.CODE
;*******************************************************************************
; WINDRAW / WINENTER
; MAIN-bank stubs for the window manager's handler vectors
.export __monitor_windraw
.proc __monitor_windraw
	JUMP FINAL_BANK_MONITOR, windraw
.endproc

.export __monitor_winenter
.proc __monitor_winenter
	JUMP FINAL_BANK_MONITOR, winenter
.endproc

.export __monitor_winresize
.proc __monitor_winresize
	JUMP FINAL_BANK_MONITOR, winresize
.endproc

;*******************************************************************************
; GETCH
; handles the key read by the monitor input handler
; OUT:
;   - .A: input key (0 if handled) or K_QUIT to end input
.export __monitor_getch
.proc __monitor_getch
	jsr key::getui
	beq @done

	; handle special keys
	; C=+l: clear monitor
	;   f1: show virtual machine state
	;   f2: run machine state
	cmp #K_MON_CLEAR
	bne :+
	CALL FINAL_BANK_MONITOR, __monitor_clear
	jmp @redrawline		; clear reset the linebuffer to a prompt

:	cmp #K_WIN_GROW
	bne :+
	jsr gui::grow
	jmp @redrawline

:	cmp #K_WIN_SHRINK
	bne :+
	jsr gui::shrink
	jmp @redrawline

:	cmp #K_WIN_MAXIMIZE
	bne :+
	jsr gui::maximize
@redrawline:
	lda zp::cury
	jsr text::drawline	; redraw the input line being edited
	jmp @handled

:	cmp #K_QUIT
	beq @done		; RUN/STOP cancels only the monitor input
	cmp #K_WIN_CLOSE
	bne :+
	ldx __monitor_windowed
	beq @handled		; no window to close
:	jsr gui::handlekey
	bcc :+
	ora #$80		; mark even GUI_RET_QUIT as a pending action
	sta winreq
	lda #K_QUIT		; force the input to end
	rts

:	cmp #K_SWAP_USERMEM_TUI
	bne @done
	jsr dbg::swapusermem

@handled:
	lda #$00
@done:	rts			; propagate keypress
.endproc

BANKED_SEG "CONSOLE", FINAL_BANK_MONITOR

;*******************************************************************************
; PUTS MAIN
; Copies a plain MAIN-bank string to shared RAM before printing it.
; IN:
;   - .XY: address of a zero-terminated string in the MAIN bank
; OUT:
;   - .C: set on output error
.export __monitor_puts_main
.proc __monitor_puts_main
	RENDER_STR
	; fall through to __monitor_puts
.endproc

;*******************************************************************************
; PUTS
; Prints the given line to the monitor
; IN:
;   - .XY: the address of the line to print
; OUT:
;   - .A: error code on failure
;   - .C: set on error (failed to write to output file)
.export __monitor_puts
.proc __monitor_puts
@msg=r0
@scr0=r2
@scr1=r4
@tmp=r6
	stxy @msg

	; if output is redirected, write to file ONLY
	lda __monitor_outfile
	beq @count
	jmp @file

@count:	inc __monitor_rows_printed

	; check if we need to scroll
	lda line
	cmp #HEIGHT-1
	bcc @print

	; scroll everything up (only the window's rows if windowed)
	ldx wintop
	lda winbot
	CALLMAIN text::scrollup

	; scroll the monitor screen buffer
	ldxy #screen
	stxy @scr0
	ldxy #screen+LINESIZE
	stxy @scr1

	; scroll the screen buffer
	ldx #HEIGHT-1
@scroll:
	ldy #LINESIZE-1
:	lda (@scr1),y
	sta (@scr0),y
	dey
	bpl :-
	lda @scr0
	clc
	adc #LINESIZE
	sta @scr0
	bcc :+
	inc @scr0+1
:	lda @scr0
	clc
	adc #LINESIZE
	sta @scr1
	bcc :+
	inc @scr1+1
:	dex
	bne @scroll

	dec line

@print:	; store the text we are about to draw to the monitor buffer
	lda line
	jsr rowptr
	stxy @scr0

	ldy #LINESIZE-1
@copy:	lda (@msg),y
	sta (@scr0),y
	dey
	bpl @copy

@screen:
	lda line
	inc line
	sec
	sbc winoff	; screen row = buffer row - winoff
	ldxy @msg
	CALLMAIN text::print
	RETURN_OK

;-------------------------------------------------------------------------------
@file:	; Write the line to the output file.
	ldx __monitor_outfile
	jsr krn::chkout		; select the output file
	bcs @fileerr

	ldy #$00
@fileputs:
	lda (@msg),y
	beq @newline
	sty @tmp
	jsr krn::chrout
	jsr krn::readst
	cmp #$00
	bne @fileerr

	ldy @tmp
	iny
	cpy #LINESIZE
	bcc @fileputs

@newline:
	lda #$0d
	jsr krn::chrout
	jsr krn::readst
	cmp #$00
	bne @fileerr

	RETURN_OK

@fileerr:
	RETURN_ERR ERR_IO_ERROR
.endproc

;*******************************************************************************
; LOG
; Appends the given line to the monitor's screen buffer without drawing to the
; screen directly.
; IN:
;   - .XY: the address of the line to append
.export __monitor_log
.proc __monitor_log
@msg=r0
@scr0=r2
@scr1=r4
	stxy @msg

	; check if we need to scroll
	lda line
	cmp #HEIGHT-1
	bcc @copy

	; scroll the monitor screen buffer (not screen)
	ldxy #screen
	stxy @scr0
	ldxy #screen+LINESIZE
	stxy @scr1

	ldx #HEIGHT-1
@scroll:
	ldy #LINESIZE-1
:	lda (@scr1),y
	sta (@scr0),y
	dey
	bpl :-
	lda @scr0
	clc
	adc #LINESIZE
	sta @scr0
	bcc :+
	inc @scr0+1
:	lda @scr0
	clc
	adc #LINESIZE
	sta @scr1
	bcc :+
	inc @scr1+1
:	dex
	bne @scroll

	dec line

@copy:	; copy text to buffer row line (screen + line*LINESIZE)
	lda line
	jsr rowptr
	stxy @scr0

	ldy #LINESIZE-1
:	lda (@msg),y
	sta (@scr0),y
	dey
	bpl :-

	inc line
	rts
.endproc

;*******************************************************************************
; INIT
; Initializes the monitor
.export __monitor_init
.proc __monitor_init
	lda #MON_MODE_VIRTUAL
	sta moncmd::mode
	sta moncmd::default_addr+2
	sta line
	sta __monitor_rows_printed
	sta __monitor_windowed
	sta winreq
	sta wintop
	sta winoff
	lda #HEIGHT-1
	sta winbot

	ldxy sim::pc
	stxy moncmd::default_addr
	rts
.endproc

;*******************************************************************************
; CLEAR
; Clears the monitor's contents
.export __monitor_clear
.proc __monitor_clear
@scr=r0
	; clear the window's rows
	lda wintop
:	pha
	CALLMAIN scr::clrline
	pla
	clc
	adc #$01
	cmp winbot
	bcc :-
	beq :-

	; clear the monitor buffer
	ldxy #screen
	stxy @scr
	ldx #HEIGHT
	ldy #$00
@l0:	lda #$00
	sta (@scr),y

	; move to next line
	lda @scr
	clc
	adc #LINESIZE
	sta @scr
	bcc :+
	inc @scr+1
:	dex
	bne @l0

	; move the input back to the window's bottom row
	lda #HEIGHT-1
	sta line
	lda winbot
	sta zp::cury
	ldxy #mem::linebuffer
	jsr write_prompt
	stx zp::curx
	rts
.endproc

;*******************************************************************************
; REENTER
; Activates the monitor without clearing the screen.
; OUT:
;   - .A: GUI_RET_x code for the window manager
.export __monitor_reenter
.proc __monitor_reenter
	; initialize QUIT and INT signal states
	lda #$00
	sta __monitor_quit
	sta winreq

	; set the interface so the debugger knows to return to the monitor
	; and not editor (GUI)
	lda #$01		; DEBUG_IFACE_TEXT
	sta dbg::interface

	jsr __monitor_install_nmi

	; treat whitespace as separator for expressions in the monitor
	lda #$01
	CALL FINAL_BANK_EXPR, expr::end_on_ws

@prompt:
	ldxy #mem::linebuffer
	jsr __monitor_inputrow
	CALLMAIN text::print
@clrline:
	ldxy #mem::linebuffer
	jsr write_prompt

@loop:	jsr __monitor_inputrow
	sta zp::cury		; screen row of the input line

	ldx prompt_len
	stx zp::curx		; move past the prompt
	ldy #$00
	CALLMAIN cur::setmin

	ldxy #__monitor_getch
	CALLMAIN edit::gets
	bcc @submit

@cancel:
	; return the pending action to the window manager
	lda winreq
	bpl @clrline
	and #$7f
	ldx #$00
	stx dbg::interface
	cmp #GUI_RET_CLOSE
	beq @close
	cmp #GUI_RET_CLOSEALL
	bne @leavewin
@close:	stx __monitor_windowed
@leavewin:
	jmp @leave

@submit:
	pha
	ldx prompt_len
	ldy #$01
	CALLMAIN str::toupper_unquoted ; preserve literals in monitor assembly/expressions

	ldx prompt_len
	ldy #$00
	lda $100,x
	beq @exec

:	lda $100,x
	sta CMD_BUFF,y
	beq @exec
	inx
	iny
	cpy #LINESIZE
	bcc :-
	lda #$00
	sta CMD_BUFF,y		; ran out of room: terminate what we did copy

@exec:	lda #$00
	sta __monitor_outfile	; default to screen
	sta __monitor_int	; reset SIGINT

	ldxy #mem::linebuffer
	jsr __monitor_puts

	; clear the input line
	lda winbot
	CALLMAIN scr::clrline

	ldxy #mem::linebuffer
	jsr set___monitor_outfile
	bcs @redirerr

	pla
	cmp prompt_len		; only the prompt means no input
	bne @run

	; no input, run the last command (if there is one)
	lda CMD_BUFF
	bne @run

	; no previous prompt, get input again
	jmp @prompt		; if command length is 0, there is no command

@redirerr:
	; the output file couldn't be opened; don't run the command
	pla			; discard the input length
	jmp @prompt

@run:	; run the command
	jsr __monitor_install_nmi	; install INT signal NMI
	ldxy #CMD_BUFF
	jsr moncmd::run
	php			; save error bit (.C)
	pha

	; close the output file (if not screen)
	lda __monitor_outfile
	beq :+
	CALLMAIN file::close
	jsr unblank
	lda #$00
	sta __monitor_outfile	; back to the screen (the handle is closed now)

:	pla
	plp			; restore error bit (.C)
	bcc @ok			; if it succeeded, continue
	CALLMAIN err::get
	jsr __monitor_puts

@ok:	lda winreq
	bpl :+
	jmp @cancel
:	lda __monitor_quit	; was QUIT signal sent?
	bne @done
	jmp @prompt

@done:	TRACE_OFF

	; the window manager restores the editor's state
	lda #GUI_RET_QUIT
	sta dbg::interface

@leave:	; restore expression parsing so that whitespace is not considered
	; a separator
	pha
	lda #$00
	CALL FINAL_BANK_EXPR, expr::end_on_ws
	pla
	rts
.endproc

;*******************************************************************************
; WINENTER
; The window manager's "enter" handler: interacts with the monitor until the
; user quits it or asks to cycle windows
; IN:
;   - .A: the first row of the window's contents
;   - .X: the last row of the window's contents
; OUT:
;   - .A: the GUI_RET_x code for the manager
.proc winenter
	jsr set_geometry

	; keep the input line on the buffer's bottom row and redraw in case
	; anchoring moved the buffer's contents
	jsr anchor_bottom
	jsr redraw_win

	; start with a clean prompt
	ldxy #mem::linebuffer
	jsr write_prompt

	jmp __monitor_reenter
.endproc

;*******************************************************************************
; SET GEOMETRY
; Sets the window geometry from the rows given by the window manager
; IN:
;   - .A: the first row of the window's contents
;   - .X: the last row of the window's contents
.proc set_geometry
	sta wintop
	stx winbot
	lda #HEIGHT-1
	sec
	sbc winbot
	sta winoff
	lda #$01
	sta __monitor_windowed
	rts
.endproc

;*******************************************************************************
; WINDRAW
; The window manager's draw handler: draws the monitor's screen buffer in
; the window's rows (the last "height" rows of the buffer)
; IN:
;   - .A: the first row to draw at
;   - .X: the last row to draw at
.proc windraw
	jsr set_geometry

	; fall through to redraw_win
.endproc

;*******************************************************************************
; REDRAW WIN
; Redraws the window's rows from the monitor's screen buffer
.proc redraw_win
	lda winbot
	sta winrow
@l0:	jsr draw_row
	lda winrow
	cmp wintop
	beq @done
	dec winrow
	jmp @l0
@done:	rts
.endproc

;*******************************************************************************
; WINRESIZE
; The window manager's resize handler: called with the new geometry after
; the window grows or shrinks.  The screen buffer is anchored at the
; window's bottom row, so the rows that remain on screen already hold the
; right contents; only the rows revealed by growing need to be drawn.
; IN:
;   - .A: the first row of the window's contents
;   - .X: the last row of the window's contents
.proc winresize
@oldtop=zp::monitor+1
	ldy wintop	; the pre-resize top row
	sty @oldtop
	jsr set_geometry

	; if the window grew, draw the newly revealed rows [wintop, oldtop)
	lda @oldtop
@l0:	sec
	sbc #$01
	bcc @done	; the old top was row 0
	cmp wintop
	bcc @done	; below the new top: nothing (more) was revealed
	sta winrow
	jsr draw_row
	lda winrow
	jmp @l0
@done:	rts
.endproc

;*******************************************************************************
; DRAW ROW
; Draws the screen row given in "winrow" from the monitor's screen buffer.
; The input row is drawn as an empty prompt (its live contents are managed
; by the input loop when the monitor has focus)
.proc draw_row
@scr=r0
	; get the buffer row that belongs at this screen row
	lda winrow
	clc
	adc winoff
	cmp line
	beq @input	; if input row, draw a prompt
	bcs @clr	; if BELOW the input row, clear the row

	; copy the buffered line to shared memory and print it
	jsr rowptr
	stxy @scr
	ldy #LINESIZE-1
:	lda (@scr),y
	sta mem::spare,y
	dey
	bpl :-
	lda #$00
	sta mem::spare+LINESIZE	; 0 terminate
	beq @print		; branch always

@input:	ldxy #mem::spare
	jsr write_prompt

@print:	ldx winrow
	CALLMAIN draw::resetline
	ldxy #mem::spare
	lda winrow
	JUMPMAIN text::print

@clr:	lda winrow
	pha
	CALLMAIN scr::clrline
	pla
	tax
	JUMPMAIN draw::resetline
.endproc

;*******************************************************************************
; UPDATE PC VIEW
; Updates the source view above the monitor window to follow the debugger's
; PC (see dbg::update_pc_view).
; The editor's redraw logic requires the cursor to be in sync with the
; source position, so the monitor's cursor is swapped out for the saved
; editor cursor around the call.
.export __monitor_update_pc_view
.proc __monitor_update_pc_view
	; swap in the editor's cursor (the monitor owns the live one)
	lda zp::cury
	pha
	lda zp::curx
	pha
	lda gui::cursave_y
	sta zp::cury
	lda gui::cursave_x
	sta zp::curx

	CALLMAIN dbg::update_pc_view

	; save the editor's cursor (it moved to the new PC's line) and
	; restore the monitor's
	lda zp::cury
	sta gui::cursave_y
	lda zp::curx
	sta gui::cursave_x
	pla
	sta zp::curx
	pla
	sta zp::cury
	rts
.endproc

;*******************************************************************************
; ANCHOR BOTTOM
; Moves the contents of the monitor's screen buffer down so that the last line
; of history is just above the input line, which is moved to the bottom row
; of the buffer
.proc anchor_bottom
@src=r0
@dst=r2
@cnt=r4
	lda #HEIGHT-1
	sec
	sbc line	; .A = # of rows to move everything down by
	beq @done	; already anchored to the bottom

	lda line
	beq @clr	; no content to move

	sta @cnt

	; copy rows line-1..0 to rows HEIGHT-2..D-1 (bottom up)
	lda line
	sec
	sbc #$01
	jsr rowptr
	stxy @src
	lda #HEIGHT-2
	jsr rowptr
	stxy @dst

@l0:	ldy #LINESIZE-1
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-

	; move both pointers up one row
	lda @src
	sec
	sbc #LINESIZE
	sta @src
	bcs :+
	dec @src+1
:	lda @dst
	sec
	sbc #LINESIZE
	sta @dst
	bcs :+
	dec @dst+1
:	dec @cnt
	bne @l0

@clr:	; clear the rows that were left behind (0 to D-1)
	lda #HEIGHT-1
	sec
	sbc line
	sta @cnt
	ldxy #screen
	stxy @dst
	ldy #$00
@l1:	lda #$00
	sta (@dst),y	; terminate the row (empty line)
	lda @dst
	clc
	adc #LINESIZE
	sta @dst
	bcc :+
	inc @dst+1
:	dec @cnt
	bne @l1

	lda #HEIGHT-1
	sta line
@done:	rts
.endproc

;*******************************************************************************
; WRITE PROMPT
; Writes an empty prompt for the selected monitor mode.
; IN:
;   - .XY: destination buffer
; OUT:
;   - .X: prompt length
.proc write_prompt
@buff=r0
	stxy @buff
	ldy #$00
	lda moncmd::mode
	cmp #MON_MODE_IMAGE
	bne :+
	lda #'i'
	sta (@buff),y
	iny
:	lda #MONITOR_PROMPT
	sta (@buff),y
	iny
	sty prompt_len
	lda #$00
	sta (@buff),y
	tya
	tax
	rts
.endproc

;*******************************************************************************
; INPUT ROW
; Calculates the SCREEN row that the monitor's input line is displayed on based
; on its current (local) line number
; OUT:
;   - .A: the screen row of the input line
.export __monitor_inputrow
.proc __monitor_inputrow
	lda line
	sec
	sbc winoff
	rts
.endproc

;*******************************************************************************
; WINHEIGHT
; Returns the height, in rows, of the monitor's contents (including the input
; line).  This is the full screen height when the monitor isn't windowed.
; OUT:
;   - .A: the number of rows
.export __monitor_winheight
.proc __monitor_winheight
	lda winbot
	sec
	sbc wintop
	clc
	adc #$01
	rts
.endproc

;*******************************************************************************
; ROWPTR
; Returns the address of the given row in the monitor's screen buffer
; IN:
;   - .A: the row to get the buffer address of
; OUT:
;   - .XY: the address of the row (screen + row*LINESIZE)
.proc rowptr
	tax
	ldy rowshi,x
	lda rowslo,x
	tax
	rts
.endproc

;*******************************************************************************
rowslo:
.repeat HEIGHT, row
	.byte <(screen + row*LINESIZE)
.endrepeat
rowshi:
.repeat HEIGHT, row
	.byte >(screen + row*LINESIZE)
.endrepeat

;*******************************************************************************
; SET OUTFILE
; Parses the line for any redirection operator ('>') and sets the output file
; for the debug command to be executed as appropriate.
; The default output "file" is the screen.
; OUT:
;   - __monitor_outfile: the file ID to store to
;   - .C: set on error (failed to open file)
.proc set___monitor_outfile
@quote=r0
	lda #$00
	sta @quote
	ldx prompt_len

@findredir:
	cpx #MAX_LINE_LEN+1
	bcs @noredir
	lda mem::linebuffer,x	; start after the prompt
	beq @noredir		; no redirect, return

	ldy @quote		; are we reading a quote?
	bne @quoted		; if so, continue to handle that
	cmp #';'		; ignore redirection in comments
	beq @noredir
	cmp #'"'		; double quote?
	beq @string		; if so, start processing quoted string
	cmp #$27		; single quote?
	beq @character		; if so, get one char

	cmp #'>'		; redirect?
	bne @next
	lda mem::linebuffer+1,x	; redirect must be followed by whitespace
	jsr is_whitespace	; (to disambiguate from the MSB operator '>')
	beq @redir

@next:	inx
	bne @findredir
@noredir:
	RETURN_OK

@quoted:
	cmp @quote		; matching quote?
	bne @next		; if not, read next char
	lda #$00
	sta @quote		; clear quote flag
	beq @next		; branch always

@character:
	sta @quote
	inx			; eat extra character (to handle the ''' case)
	cpx #MAX_LINE_LEN+1
	bcs @noredir
	lda mem::linebuffer,x	; end of line?
	beq @noredir		; if so, we're done
	bne @next		; if not, continue processing character

@string:
	sta @quote
	jmp @next

@redir:	; get the filename to redirect the ouput to
	lda #$00
	sta mem::linebuffer,x	; terminate the line where the redirect was
	sta $100,x
	txa
	sec
	sbc prompt_len
	tay
	lda #$00
	sta CMD_BUFF,y		; also terminate the command that will run
@l0:	inx
	lda mem::linebuffer,x
	beq @err_nofile

	jsr is_whitespace
	beq @l0			; eat whitespace

	txa
	pha

	; disable IRQ for file IO
	jsr blank

	; found the start of the filename
	; open the output file
	pla
	clc
	adc #<mem::linebuffer
	tax
	lda #>mem::linebuffer
	adc #$00
	tay
	CALLMAIN file::open_w
	bcs @err

	sta __monitor_outfile
@done:	RETURN_OK

@err:	; display error
	pha			; save the error code (consumed by RENDER_STR)
	jsr unblank		; re-enable the IRQ (disabled for file IO)
	ldxy #strings::file_open_failed
	RENDER_STR		; render the message (consumes the pushed error code)
	jsr __monitor_puts
	sec
	rts

@err_nofile:
	; display error
	ldxy #strings::nofile
	jsr __monitor_puts_main
	sec
	rts
.endproc

;*******************************************************************************
; BLANK
; Blanks the screen for I/O or other operations that require interrupts to be
; off
.proc blank
	JUMPMAIN scr::blank
.endproc

;*******************************************************************************
; UNBLANK
; Blanks the screen for I/O or other operations that require interrupts to be
; off
.proc unblank
	JUMPMAIN scr::unblank
.endproc

;*******************************************************************************
; INSTALL NMI
; installs the RESTORE handler used by the monitor on the current platform
.export __monitor_install_nmi
.proc __monitor_install_nmi
.ifdef vic20
	ldxy #monitor_nmi
	stxy $0318
	lda #$82
	sta $911e		; enable NMIs from RESTORE key
	rts
.else
	JUMPMAIN run::install_restore
.endif
.endproc

.ifdef vic20
.PUSHSEG
.segment "INTS"
;*******************************************************************************
; MONITOR NMI
; The monitor's NMI handler. Sets the INT signal so to gracefully halt long
; running commands.
monitor_nmi:
	pha
	lda $9111		; ack CA1 (RESTORE) so future NMIs can fire
	lda exp::bank
	pha			; save current bank
	lda #FINAL_BANK_MONITOR
	SELECT_BANK_A
	lda #$01
	sta __monitor_int	; set INT flag
	pla
	SELECT_BANK_A		; restore bank
	pla
	rti
.POPSEG
.endif
