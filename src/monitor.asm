;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "monitor".
; The monitor is a text-based interface that can be used for interacting with
; program state as well as debugging.
;*******************************************************************************

.include "asm.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
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
.include "screen.inc"
.include "settings.inc"
.include "string.inc"
.include "strings.inc"
.include "target.inc"
.include "text.inc"
.include "zeropage.inc"

.include "ram.inc"

.import is_whitespace	; from monitorcmd.asm
.import __mon_default_start_set

NMI_HANDLER_ADDR = mem::spare+120

;*******************************************************************************
HEIGHT = SCREEN_HEIGHT

; default first row of the monitor window's contents (when windowed)
WIN_DEFAULT_TOP = SCREEN_HEIGHT-8

; minimum row the top of the monitor window may be moved to (leaves room for the
; border row and one row of editor)
WIN_MIN_TOP = 2

; loop counter/scratch used when redrawing the window
winrow = zp::monitor

.segment "SHAREBSS"
.export CMD_BUFF
CMD_BUFF: .res LINESIZE		; written by edit::gets

;*******************************************************************************
; WINTOP
; If nonzero, the monitor is displayed as a window whose contents begin at this
; row and continue to the bottom of the screen.
; If zero, the monitor takes the entire screen.
.export __monitor_wintop
__monitor_wintop: .byte 0

.segment "CONSOLE_VARS"
.export __monitor_line

__monitor_line:
line:      .byte 0	; the line that the monitor is on
repeatcmd: .byte 0	; if set, empty line repeats last command

cursave_x: .byte 0
cursave_y: .byte 0

; first row of the saved screen (scr::save) that does NOT hold valid editor
; content (rows at/below it were covered by the window when the screen was
; saved).
winsave_top: .byte 0

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

.CODE
;******************************************************************************
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
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
	jmp text::drawline
	rts

:	cmp #K_MONWIN_GROW
	bne :+
	CALL FINAL_BANK_MONITOR, __monitor_win_grow
	jmp @handled

:	cmp #K_MONWIN_SHRINK
	bne :+
	CALL FINAL_BANK_MONITOR, __monitor_win_shrink
	jmp @handled

:	cmp #K_SWAP_USERMEM_TUI
	bne :+
	jsr dbg::swapusermem
	lda __monitor_wintop
	bne @handled		; windowed: leave per-row color enabled
	dec mem::coloron	; (re-disable color)
	jmp @handled

:	cmp #K_GO_BASIC_TUI
	bne @done
	jsr run::go_basic
@handled:
	lda #$00
@done:	rts			; propagate keypress
.endproc

.segment "CONSOLE"

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

	; check if we need to scroll
	lda line
	cmp #HEIGHT-1
	bcc @print

	; scroll everything up (only window's rows if windowed)
	ldx __monitor_wintop
	lda #HEIGHT-1
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

	lda __monitor_outfile
	beq @screen

@file:	; write the line to file
	ldy #$00
:	lda (@msg),y
	beq @write_to_file
	iny
	cpy #LINESIZE
	bcc :-

@write_to_file:
	tya
	clc
	adc @msg
	sta file::save_address_end
	lda @msg+1
	adc #$00
	sta file::save_address_end+1

	ldxy @msg
	lda __monitor_outfile
	CALLMAIN file::savebin

	; write a newline
	lda #$0d
	jsr krn::chrout

@screen:
	lda line
	inc line
	ldxy @msg
	JUMPMAIN text::print
.endproc

;******************************************************************************
; INIT
; Initializes the monitor
.export __monitor_init
.proc __monitor_init
	lda #$00
	sta line
	sta __monitor_wintop
	sta __mon_default_start_set
	rts
.endproc

;******************************************************************************
; CLEAR
; Clears the monitor's contents
.export __monitor_clear
.proc __monitor_clear
@scr=r0
	; clear the screen (or just the window's rows if windowed)
	lda __monitor_wintop
	bne @clrwin
	CALLMAIN scr::clr
	jmp @clrbuff

@clrwin:
:	pha
	CALLMAIN scr::clrline
	pla
	clc
	adc #$01
	cmp #HEIGHT
	bcc :-

@clrbuff:
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

	; move back to the first line (bottom row if windowed)
	lda __monitor_wintop
	beq :+
	lda #HEIGHT-1
:	sta zp::cury
	sta line
	lda #$01
	sta zp::curx
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
	rts
.endproc

;******************************************************************************
; ENTER
; Activates the monitor.
.export __monitor_enter
.proc __monitor_enter
@scr=r0
@line=r2
@linebuff=mem::spare
	CALLMAIN scr::clr
	CALLMAIN asm::reset

	lda line
	beq @cont

	; restore the contents of the monitor screen buffer
	lda #$00
	sta @line
	ldxy #screen
	stxy @scr

@l0:	ldy #$00
:	lda (@scr),y
	sta @linebuff,y
	beq @linedone
	iny
	cpy #40
	bne :-

@linedone:
	; redraw the line
	lda @line
	ldxy #@linebuff
	CALLMAIN text::print

	; move to next line
	lda @scr
	clc
	adc #LINESIZE
	sta @scr
	bcc :+
	inc @scr+1
:	inc @line
	lda @line
	cmp line
	bne @l0

@cont:	; save cursor state of caller
	lda zp::curx
	sta cursave_x
	lda zp::cury
	sta cursave_y

	; fall through to __monitor_reenter
.endproc

;******************************************************************************
; REENTER
; Activates the monitor without clearing the screen
.export __monitor_reenter
.proc __monitor_reenter
@err=r0
	; initialize QUIT and INT signal states
	lda #$00
	sta __monitor_quit

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
	CALLMAIN text::print
	lda #MONITOR_PROMPT
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
@clrline:
	lda #$00
	sta mem::linebuffer+1

@loop:	lda line
	sta zp::cury

	lda #$01
	sta zp::curx		; move to start of line
	ldx #$01
	ldy #$00
	CALLMAIN cur::setmin

	ldxy #__monitor_getch
	CALLMAIN edit::gets
	bcs @clrline
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
	lda #HEIGHT-1
	CALLMAIN scr::clrline

	ldxy #mem::linebuffer
	jsr set___monitor_outfile
	bcs @redirerr

	lda line
	cmp #HEIGHT-1
	bcc :+

:	pla
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

:	pla
	rol @err		; restore error bit
	bcc @ok			; if it succeeded, continue
	CALLMAIN err::get
	jsr __monitor_puts

@ok:	lda __monitor_quit	; was QUIT signal sent?
	bne @done
	jmp @prompt

@done:	TRACE_OFF

	; restore the cursor
	lda cursave_x
	sta zp::curx
	lda cursave_y
	sta zp::cury

	; if windowed, leave the window onscreen (editor will resize to fit)
	lda __monitor_wintop
	beq :+
	rts

:	; debug interface changed back to GUI, refresh editor
	JUMPMAIN edit::refresh
.endproc

;******************************************************************************
; ENTER WIN
; Activates the monitor as a window on the lower part of the screen.
; If the monitor window is already open, re-activates it.
.export __monitor_enter_win
.proc __monitor_enter_win
	CALLMAIN asm::reset

	; save cursor state of caller
	lda zp::curx
	sta cursave_x
	lda zp::cury
	sta cursave_y

	; save the screen so that the editor's rows can be restored from the
	; backup when the window shrinks
	CALLMAIN scr::save

	lda __monitor_wintop
	bne @reactivate

@activate:
	lda #HEIGHT-1
	sta winsave_top
	lda #WIN_DEFAULT_TOP
	sta __monitor_wintop

	; move the monitor's history down so that the input line is on the
	; bottom row of the screen (and therefore within the window)
	jsr anchor_bottom
	jmp @draw

@reactivate:
	; the rows in/below the window hold monitor content in the backup;
	; rows revealed beyond this point are rendered on demand by the editor
	sec
	sbc #$01
	sta winsave_top

@draw:	jsr draw_window
	jmp __monitor_reenter
.endproc

;******************************************************************************
; WIN GROW
; Grows the monitor window by one row (moves its top row up).
; Does nothing if the monitor is fullscreen or the window is at its max size.
.export __monitor_win_grow
.proc __monitor_win_grow
	lda __monitor_wintop
	beq @done		; if fullscreen, nothing to do
	cmp #WIN_MIN_TOP+1
	bcc @done		; already at max size
	dec __monitor_wintop

	; draw the border at its new position and the single row of history
	; that the window revealed
	jsr draw_border
	lda __monitor_wintop
	sta winrow
	jmp draw_row
@done:	rts
.endproc

;******************************************************************************
; WIN SHRINK
; Shrinks the monitor window by one row (moves its top row down).
; Does nothing if the monitor is fullscreen or the input line would be pushed
; out.
.export __monitor_win_shrink
.proc __monitor_win_shrink
	lda __monitor_wintop
	beq @done		; fullscreen; nothing to do
	cmp line
	bcs @done		; don't shrink past the input line
	inc __monitor_wintop

	; redraw the revealed row (the old border row): restore it from the
	; saved screen if it holds editor content there, else have the editor
	; render the source line that belongs at that row
	lda __monitor_wintop
	sec
	sbc #$02
	pha

	; clear the border highlight from the row before it is drawn
	tax
	CALLMAIN draw::resetline

	pla
	cmp winsave_top
	bcs @render
	CALLMAIN scr::restore_row
	jmp @border

@render:
	pha
	jsr render_hidden_row

	; cache the rendered row in the screen backup so that, if it's
	; covered and revealed again, it can be restored instead of re-rendered
	pla
	CALLMAIN scr::save_row
	inc winsave_top

@border:
	jmp draw_border
@done:	rts
.endproc

;******************************************************************************
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
	lda cursave_y
	sta zp::cury
	lda cursave_x
	sta zp::curx

	CALLMAIN dbg::update_pc_view

	; redraw the border in case the source redraw disturbed it
	lda __monitor_wintop
	beq :+
	jsr draw_border

:	; save the editor's cursor (it moved to the new PC's line) and
	; restore the monitor's
	lda zp::cury
	sta cursave_y
	lda zp::curx
	sta cursave_x
	pla
	sta zp::curx
	pla
	sta zp::cury
	rts
.endproc

;******************************************************************************
; WIN RESYNC
; Re-saves the screen backup and the caller's cursor after the display above
; the monitor window has been redrawn behind the monitor's back (e.g. by the
; debugger moving the source view to the new PC while stepping).
.export __monitor_win_resync
.proc __monitor_win_resync
	; re-save the caller's cursor (it may have moved with the redraw)
	lda zp::curx
	sta cursave_x
	lda zp::cury
	sta cursave_y

	; re-save the rows above the window (the ones the redraw changed)
	lda #$00
@l0:	pha
	CALLMAIN scr::save_row
	pla
	clc
	adc #$01
	cmp __monitor_wintop
	bcc @l0			; repeat for rows 0 to wintop-1

	; rows at/below the border hold monitor content in the backup; rows
	; revealed beyond this point are rendered on demand by the editor
	lda __monitor_wintop
	sec
	sbc #$01
	sta winsave_top
	rts
.endproc

;******************************************************************************
; RENDER HIDDEN ROW
; Has the editor render the source line that belongs at the given row.
; Used when the window shrinks past the point captured in the screen backup.
; The editor's render is based on its cursor row, so the monitor's cursor is
; swapped out for the saved editor cursor around the call.
; IN:
;   - .A: the row to render
.proc render_hidden_row
	sta winrow
	lda zp::cury
	pha
	lda cursave_y
	sta zp::cury
	lda winrow
	CALLMAIN edit::render_row
	pla
	sta zp::cury
	rts
.endproc

;******************************************************************************
; DRAW WINDOW
; Redraws the monitor window: the border row at wintop-1 and the contents of
; the monitor's screen buffer from wintop to the bottom of the screen.
; Redraws the input line from the linebuffer and clears rows below it
.proc draw_window
	jsr draw_border

	lda __monitor_wintop
	sta winrow
@l0:	jsr draw_row
	inc winrow
	lda winrow
	cmp #HEIGHT
	bcc @l0
	rts
.endproc

;******************************************************************************
; DRAW BORDER
; Draws the border row above the monitor window (at wintop-1).
; Normally the border doubles as the status row; while debugging it is
; drawn as a clean bitmap separator line instead (the status contents are
; not meaningful there).
.export __monitor_draw_border
__monitor_draw_border:
.proc draw_border
	lda edit::debugging
	bne @separator

	; draw the status row as the border
	lda __monitor_wintop
	sec
	sbc #$01
	pha
	CALLMAIN text::status
	pla
	tax
	lda #COLOR_RVS
	JUMPMAIN draw::hline

@separator:
	; draw a horizontal line as the border
	lda __monitor_wintop
	sec
	sbc #$01
	pha
	CALLMAIN scr::clrline
	pla
	pha
	ldy #$03		; draw the line in the middle of the row
	CALLMAIN draw::rvs_line
	pla
	tax
	JUMPMAIN draw::resetline
.endproc

;******************************************************************************
; DRAW ROW
; Draws the row given in "winrow" from the monitor's screen buffer.
; Draws the input row from the linebuffer and clears rows below it.
.proc draw_row
@scr=r0
	lda winrow
	cmp line
	beq @input	; if input row, draw current contents of the linebuffer
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

	ldx winrow
	CALLMAIN draw::resetline
	ldxy #mem::spare
	lda winrow
	JUMPMAIN text::print

@input:	ldx winrow
	CALLMAIN draw::resetline
	ldxy #mem::linebuffer
	lda winrow
	JUMPMAIN text::print

@clr:	lda winrow
	pha
	CALLMAIN scr::clrline
	pla
	tax
	JUMPMAIN draw::resetline
.endproc

;******************************************************************************
; ANCHOR BOTTOM
; Moves the contents of the monitor's screen buffer down so that the last line
; of history is just above the input line, which is moved to the bottom row
; of the screen
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

;******************************************************************************
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
