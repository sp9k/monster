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

NMI_HANDLER_ADDR = mem::spare+120

;*******************************************************************************
HEIGHT = SCREEN_HEIGHT

; default height of the monitor window's contents
WIN_DEFAULT_HEIGHT = 8

; loop counter/scratch used when redrawing the window
winrow = zp::monitor

.segment "SHAREBSS"
.export CMD_BUFF
CMD_BUFF: .res LINESIZE		; written by edit::gets

;*******************************************************************************
; WINDOWED
; Nonzero while the monitor window is open (see gui.asm)
.export __monitor_windowed
__monitor_windowed: .byte 0

; set (by the key handler) when the user asks to cycle to the next window
cyclereq: .byte 0

; set (by the key handler) when the user asks to close the monitor window
closereq: .byte 0

.segment "CONSOLE_VARS"
.export __monitor_line

__monitor_line:
line:      .byte 0	; the buffer row that the monitor's input is on
repeatcmd: .byte 0	; if set, empty line repeats last command

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
; Handles the key (called by the keyboard gets handler)
.export __monitor_getch
.proc __monitor_getch
	jsr key::getch
	beq @done

	; handle special keys
	; C=+l: clear monitor
	;   f1: show virtual machine state
	;   f2: run machine state
	cmp #K_MON_CLEAR
	bne :+
	CALL FINAL_BANK_MONITOR, __monitor_clear
	lda zp::cury		; (clear reset the linebuffer to a prompt)
	jmp text::drawline

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

:	cmp #K_WIN_CLOSE
	bne :+
	lda __monitor_windowed
	beq @handled		; not in a window: nothing to close
	inc closereq
	lda #K_QUIT		; force the input to end
	rts

:	cmp #K_SWAP_WINS
	bne :+
	inc cyclereq
	lda #K_QUIT		; force the input to end
	rts

:	cmp #K_SWAP_USERMEM_TUI
	bne :+
	jsr dbg::swapusermem
	jmp @handled

:	cmp #K_GO_BASIC_TUI
	bne @done
	jsr run::go_basic
@handled:
	lda #$00
@done:	rts			; propagate keypress
.endproc

BANKED_SEG "CONSOLE", FINAL_BANK_MONITOR

;*******************************************************************************
; PUTS
; Prints the given line to the monitor
; IN:
;   - .XY: the address of the line to print
; OUT:
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
	beq :+
	jmp @file

	; check if we need to scroll
:	lda line
	cmp #HEIGHT-1
	bcc @print

	; scroll everything up (only the window's rows if windowed)
	ldx wintop
	lda winbot
	CALLMAIN text::scrollup

	; scroll the monitor screen buffer
	ldxy #screen
	stxy @scr0
	ldxy #screen+40
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

@print: lda #$00
	sta @scr0+1

	; copy the rendered text to the current line of the buffer
	; the buffer destination is screen + (line*40)
	lda line
	asl		; *2
	sta @tmp
	asl		; *4
	asl		; *8
	adc @tmp	; *10
	asl		; *20
	rol @scr0+1
	asl		; *40
	rol @scr0+1
	adc #<screen
	sta @scr0
	lda @scr0+1
	adc #>screen
	sta @scr0+1

	; store the text we are about to draw to the monitor buffer
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
	JUMPMAIN text::print

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
	ldy @tmp
	iny
	cpy #LINESIZE
	bcc @fileputs

@newline:
	lda #$0d
	jsr krn::chrout
	RETURN_OK

@fileerr:
	sec
	rts
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
@tmp=r6
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
	lda #$00
	sta @scr0+1
	lda line
	asl		; *2
	sta @tmp
	asl		; *4
	asl		; *8
	adc @tmp	; *10
	asl		; *20
	rol @scr0+1
	asl		; *40
	rol @scr0+1
	adc #<screen
	sta @scr0
	lda @scr0+1
	adc #>screen
	sta @scr0+1

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
	lda #$00
	sta line
	sta __monitor_windowed
	sta cyclereq
	sta closereq
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
	lda #$01
	sta zp::curx
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
	rts
.endproc

;*******************************************************************************
; REENTER
; Activates the monitor without clearing the screen.
; Returns a GUI_RET_x code for the window manager in .A
.export __monitor_reenter
.proc __monitor_reenter
@err=r0
	; initialize QUIT and INT signal states
	lda #$00
	sta __monitor_quit
	sta cyclereq
	sta closereq

	; set the interface so the debugger knows to return to the monitor
	; and not editor (GUI)
	lda #$01		; DEBUG_IFACE_TEXT
	sta dbg::interface

	jsr install_nmi

	; treat whitespace as separator for expressions in the monitor
	lda #$01
	CALL FINAL_BANK_EXPR, expr::end_on_ws

@prompt:
	ldxy #mem::linebuffer
	lda line
	sec
	sbc winoff
	CALLMAIN text::print
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
@clrline:
	lda #$00
	sta mem::linebuffer+1

@loop:	lda line
	sec
	sbc winoff
	sta zp::cury		; screen row of the input line

	lda #$01
	sta zp::curx		; move to start of line
	ldx #$01
	ldy #$00
	CALLMAIN cur::setmin

	ldxy #__monitor_getch
	CALLMAIN edit::gets
	bcc @submit

	; did user ask to close the window?
	lda closereq
	beq @chkcycle
	lda #$00
	sta closereq
	sta __monitor_windowed	; the window is closing
	lda #GUI_RET_CLOSE
	rts

@chkcycle:
	; did user prompt us to cycle windows?
	lda cyclereq
	beq @clrline
	lda #$00
	sta cyclereq
	lda #GUI_RET_CYCLE
	rts

@submit:
	pha
	ldxy #$101
	CALLMAIN str::toupper	; commands are case insensitive

	ldx #$00
	lda $101
	beq @exec

:	lda $101,x
	sta CMD_BUFF,x
	beq @exec
	inx
	bne :-

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
	cmp #$02		; 2 because prompt makes min length 1
	bcs @run

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
	ldxy #CMD_BUFF
	jsr moncmd::run
	ror @err		; save error bit
	pha

	; close the output file (if not screen)
	lda __monitor_outfile
	beq :+
	CALLMAIN file::close
	jsr unblank
	lda #$00
	sta __monitor_outfile	; back to the screen (the handle is closed now)

:	pla
	rol @err		; restore error bit
	bcc @ok			; if it succeeded, continue
	CALLMAIN err::get
	jsr __monitor_puts

@ok:	lda __monitor_quit	; was QUIT signal sent?
	bne @done
	jmp @prompt

@done:	TRACE_OFF

	; the window manager restores the editor's state
	lda #GUI_RET_QUIT
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
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1

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

@input:	lda #MONITOR_PROMPT
	sta mem::spare
	lda #$00
	sta mem::spare+1

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
; ROWPTR
; Returns the address of the given row in the monitor's screen buffer
; IN:
;   - .A: the row to get the buffer address of
; OUT:
;   - .XY: the address of the row (screen + row*LINESIZE)
.proc rowptr
@tmp=r6
	sta @tmp
	asl		; *2
	asl		; *4
	adc @tmp	; *5
	sta @tmp
	lda #$00
	sta @tmp+1
	asl @tmp
	rol @tmp+1	; *10
	asl @tmp
	rol @tmp+1	; *20
	asl @tmp
	rol @tmp+1	; *40 (LINESIZE)
	lda @tmp
	clc
	adc #<screen
	tax
	lda @tmp+1
	adc #>screen
	tay
	rts
.endproc

;*******************************************************************************
; SET OUTFILE
; Parses the line for any redirection operator ('>') and sets the output file
; for the debug command to be executed as appropriate.
; The default output "file" is the screen.
; OUT:
;   - __monitor_outfile: the file ID to store to
;   - .C: set on error (failed to open file)
.proc set___monitor_outfile
	ldx #$00

@findredir:
	cpx #LINESIZE-2
	beq @done
	lda mem::linebuffer+1,x	; start after prompt (+1)
	beq @done		; no redirect, return
	cmp #'>'		; redirect?
	bne @next
	lda mem::linebuffer+2,x	; redirect must be followed by whitespace
	jsr is_whitespace	; (to disambiguate from the MSB operator '>')
	beq @redir
@next:	inx
	bne @findredir
	rts

@redir:	; get the filename to redirect the ouput to
	lda #$00
	sta mem::linebuffer+1,x	; terminate the line where the redirect was
	sta $100+1,x
	sta CMD_BUFF,x		; also terminate the command that will run
@l0:	inx
	lda mem::linebuffer+1,x
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
	adc #<(mem::linebuffer+1)
	tax
	lda #>(mem::linebuffer+1)
	adc #$00
	tay
	CALLMAIN file::open_w
	bcs @err

	sta __monitor_outfile
@done:	RETURN_OK

@err:	; display error
	jsr unblank		; re-enable the IRQ (disabled for file IO)
	ldxy #strings::file_open_failed
	jsr __monitor_puts
	sec
	rts

@err_nofile:
	; display error
	ldxy #strings::nofile
	jsr __monitor_puts
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
; Copies the NMI handler to shared RAM and enables CA1 (RESTORE key)
; interrupts to catch INT signal
.proc install_nmi
.ifdef vic20
@src=r0
@dst=r2
	ldxy #@nmi_handler
	stxy @src
	ldxy #NMI_HANDLER_ADDR
	stxy @dst

	ldy #@nmi_handler_end-@nmi_handler-1
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-

	ldxy #NMI_HANDLER_ADDR
	stxy $0318
	lda #$82
	sta $911e		; enable NMIs from RESTORE key
	rts

; The NMI handler - simply sets the INT signal
@nmi_handler:
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
@nmi_handler_end:
.else
	rts
.endif
.endproc
