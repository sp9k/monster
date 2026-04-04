;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "monitor".
; The monitor is a text-based interface that can be used for interacting with
; program state as well as debugging.
;*******************************************************************************

.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
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

.segment "SHAREBSS"
.export CMD_BUFF
CMD_BUFF: .res LINESIZE		; written by edit::gets

.segment "CONSOLE_VARS"
.export __monitor_line

__monitor_line:
line:      .byte 0	; the line that the monitor is on
repeatcmd: .byte 0	; if set, empty line repeats last command

cursave_x: .byte 0
cursave_y: .byte 0

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

:	cmp #K_SWAP_USERMEM_TUI
	bne :+
	jsr dbg::swapusermem
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

	; scroll everything up
	ldx #$00
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

@print:
	lda #$00
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
	sta __mon_default_start_set
	rts
.endproc

;******************************************************************************
; CLEAR
; Clears the monitor's contents
.export __monitor_clear
.proc __monitor_clear
@scr=r0
	; clear the screen
	CALLMAIN scr::clr

	; clear the monitor buffer
	ldxy #screen
	stxy @scr
	ldx #HEIGHT
	ldy #$00
@l0:	tya
	sta (@scr),y
	lda @scr
	clc
	adc #LINESIZE
	sta @scr
	bcc :+
	inc @scr+1
:	dex
	bne @l0
	stx zp::cury
	stx line	; go back to first line
	inx
	stx zp::curx
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

	lda line
	beq @cont

	; restore the contents of the monitor screen buffer
	lda #$00
	sta @line
	ldxy #screen
	stxy @scr

@l0:	ldy #LINESIZE-1
:	lda (@scr),y
	sta @linebuff,y
	dey
	bpl :-

	lda @line
	ldxy #@linebuff
	CALLMAIN text::print
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

	CALLMAIN irq::on

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

@run:	; run the command
	ldxy #CMD_BUFF
	jsr moncmd::run
	php
	pha

	; close the output file (if not screen)
	lda __monitor_outfile
	beq :+
	CALLMAIN file::close
	CALLMAIN irq::on

:	pla
	plp
	bcc @ok			; if it succeeded, continue

@err:	CALLMAIN err::get
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

	; debug interface changed back to GUI, refresh editor
	JUMPMAIN edit::refresh
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
	cpx #LINESIZE-1
	beq @done
	lda mem::linebuffer+1,x	; start after prompt (+1)
	beq @done		; no redirect, return
	cmp #'>'		; redirect?
	beq @redir
	inx
	bne @findredir
	rts

@redir:	; get the filename to redirect the ouput to
	lda #$00
	sta mem::linebuffer+1,x	; terminate the line where the redirect was
	sta $100+1,x
@l0:	inx
	lda mem::linebuffer+1,x
	beq @err_nofile

	jsr is_whitespace
	beq @l0			; eat whitespace

	txa
	pha

	; disable IRQ for file IO
	CALLMAIN irq::off

	; found the start of the filename
	; open the output fil$100e
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
@done:	rts

@err:	; display error
	ldxy #strings::file_open_failed
	jmp __monitor_puts

@err_nofile:
	; display error
	ldxy #strings::nofile
	jmp __monitor_puts
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
