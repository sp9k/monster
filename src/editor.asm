;*******************************************************************************
; EDITOR.ASM
; This file contains the code for the editor, which, in normal operation, is
; the main loop of this program.  From the editor, the user can assemble code,
; debug it, load/save files, view symbols, etc.
; Most of the README is dedicated to the instructions on operating the editor.
;*******************************************************************************

.include "asm.inc"
.include "beep.inc"
.include "breakpoints.inc"
.include "codes.inc"
.include "config.inc"
.include "copybuff.inc"
.include "ctx.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "directory.inc"
.include "draw.inc"
.include "expr.inc"
.include "errors.inc"
.include "errlog.inc"
.include "file.inc"
.include "format.inc"
.include "gui.inc"
.include "guis.inc"
.include "io.inc"
.include "irq.inc"
.include "kernal.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "labels.inc"
.include "linebuffer.inc"
.include "linker.inc"
.include "macros.inc"
.include "memory.inc"
.include "memview.inc"
.include "monitor.inc"
.include "object.inc"
.include "prefs.inc"
.include "ram.inc"
.include "runtime.inc"
.include "screen.inc"
.include "sim6502.inc"
.include "settings.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "symview.inc"
.include "target.inc"
.include "text.inc"
.include "util.inc"
.include "vmem.inc"
.include "zeropage.inc"

.ifdef vic20
.include "vic20/udgedit.inc"
.endif

.ifdef c64
.include "c64/c64.inc"
.endif

;*******************************************************************************
; CONSTANTS
MODE_COMMAND     = 1
MODE_INSERT      = 2
MODE_VISUAL      = 3
MODE_VISUAL_LINE = 4

START_MODE = MODE_COMMAND

MAX_JUMPS = 8

VISUAL      = 1
VISUAL_LINE = 2

;*******************************************************************************
; ZEROPAGE
height = zp::editor+1	; height of the text-editor (shrinks when displaying
			; error, showing debugger, etc.
mode = zp::editor_mode	; editor mode (COMMAND, INSERT)

.export __edit_height
__edit_height = height

.segment "SHAREBSS"
.export __edit_debugging
__edit_debugging:
debugging: .byte 0	; if !0, debugger is active

.BSS
;*******************************************************************************
readonly:  .byte 0	; if !0 no edits are allowed to be made via the editor

; jump list lines and current index
jumplist_lo: .res MAX_JUMPS
jumplist_hi: .res MAX_JUMPS
jumpptr:  .byte 0

visual_start_line:	.word 0	; the line # a selection began at
visual_start_x:		.byte 0	; the x-position a selection began at
selection_type:    	.byte 0 ; the type of selection (VISUAL_LINE or VISUAL)
format:            	.byte 0	; if 0, formatting is not applied on line-end

overwrite: .byte 0	; for SAVE commands, if !0, overwrite existing file

.export __edit_binary_flag
__edit_binary_flag: .byte 0	; flag used by some commands

cmdreps:      .byte 0		; number of times to REPEAT current command
bufferedkeys: .byte 0		; number of keys "buffered" for current command

.export __edit_highlight_en
.export __edit_highlight_line

; highlight variables
__edit_highlight_en: .byte 0	; highlight flag: if !0 highlight highlight_line

__edit_highlight_line:	.word 0 	; the line we are highlighting
highlight_file:   	.word 0		; filename of line we are highlighting
highlight_status:	.byte 0		; if !0 highlight is active

; the status row is where the text status is displayed.
; It is also where the program accepts commands (see get_command)
.export status_row
status_row: .byte 0

autoindent: .byte 0		; auto-indent enable flag (0=don't auto-indent)

.CODE

;*******************************************************************************
; INIT
; Initializes the editor state
.export __edit_init
.proc __edit_init
	sei
	ldx #$ff
	txs
	cli

	inx			; ldx #$00
	stx debugging
	stx zp::gendebuginfo

	inx			; ldx #$01
	stx zp::verify		; don't assemble code (just check syntax)
	stx fmt::enable		; enable formatting

	stx format		; enable formatting
	stx autoindent		; enable auto-indent

	jsr edit		; initialize size/mode/etc.
	jsr enter_command

	; rewind source and get the first line's contents in textbuffer
	jsr src::rewind
	jsr src::get

	; move cursor to first character on line
	lda #$00
	sta asm::mode
	sta zp::cury
	jsr gotoindex

	jsr refresh

	; fall through to __edit_run
.endproc

;*******************************************************************************
; RUN
; Runs the main loop for the editor
.export __edit_run
.proc __edit_run
	jsr text::update
	lda status_row
	jsr text::status

main:	jsr key::getch
	beq @done

	jsr is_visual
	beq :+ 		; leave cursor on if in VISUAL/VISUAL_LINE mode
	pha
	jsr cur::off
	pla

:	jsr __edit_handle_key
@done:	jsr text::update
	bne main	; branch always (continue main loop)
.endproc

;*******************************************************************************
; HANDLE_KEY
; Handles a keypress within the editor
; IN:
;  - .A: the key that was pressed (as from key::getch)
; OUT:
;  - .C: set if the key resulted in no action in the editor
.export __edit_handle_key
.proc __edit_handle_key
	jsr is_visual
	beq @cmd		; handle keys in VISUAL mode like COMMAND

	cpx #MODE_COMMAND
	bne @ins
@cmd:	jsr onkey_cmd
	jmp @done
@ins:	jsr onkey

@done:	jsr text::update	; update status in case something was changed
	jsr is_visual
	beq :+
	jsr cur::on
:	lda status_row
	jmp text::status
.endproc

;*******************************************************************************
; LABEL_ADDR_OR_ORG
; Returns the address of the label given in .XY or, if no label is given (a
; 0-length string is given) the address of the program origin
; IN:
;  - .XY: the address of the label to get the address of
; OUT:
;  - .XY: the address of the given label or the address of the origin if no
;         label was given.
;  - .C: set on error, clear on success
.proc label_addr_or_org
@lbl=r0
	stxy zp::line
	ldy #$00
	lda (zp::line),y
	bne @label
	ldxy asm::origin ; use ORG if no label given
	RETURN_OK
@label:	jmp expr::eval
.endproc

;*******************************************************************************
; ENTER MONITOR
; Activates the monitor
.export __edit_enter_monitor
.proc __edit_enter_monitor
	jsr draw::coloroff
	JUMP FINAL_BANK_MONITOR, mon::enter
.endproc

;*******************************************************************************
; MONITOR
; Activates the monitor and restores the editor when it exits
.proc monitor
	jsr scr::save
	pushcur
	jsr __edit_enter_monitor
	jsr __edit_exit_monitor
	popcur
	jmp scr::restore
.endproc

;*******************************************************************************
; EXIT CONSOLE
; Returns from the console
.export __edit_exit_monitor
.proc __edit_exit_monitor
	inc mem::coloron
	rts
.endproc

;*******************************************************************************
; COMMAND_GO
; :g <symbol>
; Begins execution at the address of the given symbol.
; If no label is given (a 0-length string is given) then begins execution at
; the program's origin.
; IN:
;  - .XY: the address of the label to start debugging at
.proc command_go
	jsr label_addr_or_org
	bcc :+
	rts			; address not found
:	stxy zp::jmpvec
	jmp (zp::jmpvec)
.endproc

;*******************************************************************************
; COMMAND_DEBUG
; Starts the debugger at the address specified by the given label.
; If no label is given (a 0-length string is given) then begins debugging at
; the program's origin.
; IN:
;  - .XY: the label name or address to start debugging at
.proc command_debug
@addr=zp::editortmp
	lda debugging
	bne @ret		; if already debugging, don't do anything

	jsr label_addr_or_org	; get address to begin debugging at
	stxy @addr
	bcc :+
@ret:	rts			; address not found

:	jsr src::anydirty
	beq :+			; if no dirty buffers, continue
	jsr prompt_saveall	; ask user if they want to save buffers

:	jsr enter_command
	inc debugging
	jsr reset_size
	inc readonly	; enable read-only mode

	lda #DEBUG_MESSAGE_LINE
	sta status_row

	ldxy @addr
	jmp dbg::start	; start debugging at address in .XY
.endproc

;*******************************************************************************
; COMMAND ASSEMBLE FILE
; :a <filename>
; Assembles the given filename
; IN:
;  - .XY: the filename of the file to assemble
.proc command_assemble_file
@filename=zp::editortmp
	stxy @filename

	ldxy #strings::assembling
	jsr print_info

	jsr cancel		; close errlog (if open)
	jsr dbgi::init
	jsr obj::init
	jsr errlog::clear
	jsr asm::reset

	lda #$01
	sta zp::gendebuginfo	; enable debug info
	jsr asm::startpass

	jsr irq::off

	; do the first pass of assembly
	ldxy @filename
	jsr asm::include	; assemble the file (pass 1)
	bcs @done		; error, we're done

	jsr obj::close_section	; close final OBJ section

	; if there were any errors after pass 1, abort
	lda errlog::numerrs
	bne @done

	; do the second assembly pass
	lda #$02
	jsr asm::startpass
	ldxy @filename
	jsr asm::include	; assemble the file (pass 2)

	ldxy zp::asmresult
	jsr dbgi::endblock	; end the final block
	jsr obj::close_section	; close final OBJ section

@done:	jmp display_result
.endproc

;*******************************************************************************
; COMMAND_LINK
; Opens the "LINK" file from disk, parses it, and links all object files
; on the same disk
.proc command_link
@file=r8
@linkbuffer=mem::spare
;@objlist=mem::spare
	CALL FINAL_BANK_LINKER, link::init

	jsr irq::off

	; display linking...
	ldxy #strings::linking
	jsr print_info

	; parse the LINK file to setup the linking context
	ldxy #strings::link
	CALL FINAL_BANK_LINKER, link::parse
	bcs @done				; error

	; get all object files on disk
	lda #$4f			; 'O'
	ldxy #link::objfiles
	jsr dir::get_by_type

	CALL FINAL_BANK_LINKER, link::link

	; display the success/failure

@done:	jmp irq::on
.endproc

;*******************************************************************************
; COMMAND ASM TO OBJ
; Assembles the entire source of the active source buffer to an object file
; of the given name
.proc command_asm_obj
@filename=zp::editortmp
@fileid=zp::editortmp
@savename=mem::filename
	; save the filename
	stxy @filename
	ldy #$00
:	lda (@filename),y
	sta @savename,y
	beq :+
	iny
	cpy #16		; max filename len in CBM DOS
	bne :-

:	; assemble obj file to the vmem address $0000
	ldx #$00
	stx zp::asmresult
	stx zp::asmresult+1

	inx			; .X=1
	stx asm::mode		; assemble to object
	jsr command_asmdbg

	; restore filename
	ldxy #@savename

	php			; save error (.C)
	jsr irq::off
	plp			; restore error (.C)
	bcs @done		; don't write result if assembly failed
	jsr file::open_w	; open the output filename
	bcs @done
	sta @fileid
	tax
	jsr krn::chkout		; CHKOUT, file in .X is output

	ldxy #strings::dumping
	jsr print_info

	CALL FINAL_BANK_LINKER, obj::dump
	lda @fileid
	jsr file::close

@done:	dec asm::mode		; switch back to DIRECT assembly mode
	jmp irq::on
.endproc

;*******************************************************************************
; COMMAND_ASM
; Assembles the entire source of the active source buffer
; OUT:
;   - .C: set if assembly failed
.proc command_asm
	ldxy #strings::assembling
	jsr print_info

	lda zp::gendebuginfo
	beq :+

	; ensure that the buffer we are assembling has a name
	jsr __edit_current_file
	bcc :+
	lda #ERR_UNNAMED_BUFFER
	jmp report_typein_error

:	jsr irq::off

	jsr cancel		; close errlog (if open)
	jsr dbgi::init
	jsr obj::init
	jsr errlog::clear

	; save the current source position and rewind it for assembly
	jsr text::savebuff
	jsr src::pushp
	jsr src::rewind

;--------------------------------------
; Pass 1
; do a pass on the source to simply get labels and basic debug info
; (# of lines and # of segments/file)
@pass1:
	lda #$01
	jsr asm::startpass

@pass1loop:
	jsr src::currline
	stxy asm::linenum
	jsr src::readline
	ldxy #mem::linebuffer
	lda #FINAL_BANK_MAIN
	jsr asm::tokenize_pass1
	bcc @ok

	jsr errlog::log
	bcs @done		; if max errors reached, abort

@ok:	jsr src::end
	bne @pass1loop

	; if there were any errors after pass 1, abort
	lda errlog::numerrs
	bne @done

	jsr obj::close_section	; close final OBJ section

;--------------------------------------
; Pass 2
; now we have defined labels and enough debug info to generate both the
; program binary and the full debug info (if enabled)
@pass2: ; set the initial file for debugging
	; sections are closed by the .SEG directive
	; need to manually close the last one
	ldxy #$01
	stxy asm::linenum

	; set debug file id if debug info generation is enabled
	lda zp::gendebuginfo
	beq @cont
	stxy dbgi::srcline
	; get active filename (r0 = name)
	jsr src::current_filename
	bcc :+
	jsr errlog::log
	jmp @done		; can't get buffer name is fatal, go to end
:	jsr dbgi::setfile

@cont:	inc zp::pass		; pass 2
	jsr src::rewind
	lda #$02
	jsr asm::startpass

@pass2loop:
	jsr src::currline
	stxy asm::linenum
@asm:	jsr src::readline
	ldxy #mem::linebuffer
	lda #FINAL_BANK_MAIN
	jsr asm::tokenize_pass2
	bcc @next		; no error, continue
	jsr errlog::log		; if error, add it to errors log
	bcc @next		; continue if we haven't reached error threshold

@next:	jsr src::end		; check if we're at the end of the source
	bne @pass2loop		; repeat if not

@done:	ldxy zp::asmresult
	jsr dbgi::endblock	; end the final debug info block
	jsr obj::close_section	; close final OBJ section

	jsr src::popgoto
	jsr text::restorebuff	; restore the linebuffer

	; fall through to display_result
.endproc

;*******************************************************************************
; ASM DONE
; Displays the result of the assembly. Prints an error if one occurred or
; the size of the assembled program if not.
; IN:
;   - zp::asmresult: pointer to the end of the program
; OUT:
;   - .C: set on error
.proc display_result
	jsr irq::on

	jsr clrerror
	lda #$01
	sta zp::verify		; re-enable verify

	lda errlog::numerrs
	beq @printresult

@err:	lda height
	jsr errlog::activate
	sec			; assembly failed
	rts

@printresult:
	; get the size of the assembled program and print it
	ldxy #@success0
	lda asm::pcset		; did this program assemble > 0 bytes?
	beq @print 		; if not, print a simple "done"

	; get the size of the assembled program (top - origin)
	lda asm::top
	sec
	sbc asm::origin
	pha
	lda asm::top+1
	sbc asm::origin+1
	pha

	lda asm::top
	pha
	lda asm::top+1
	pha

	lda asm::origin
	pha
	lda asm::origin+1
	pha

@success:
	ldxy #@success_msg
@print: lda #STATUS_ROW
	jsr text::print

	ldx #STATUS_ROW
	lda #COLOR_SUCCESS
	jsr draw::hline
	jsr key::waitch		; wait for key
	ldx #STATUS_ROW
	jsr draw::hiline
	jsr lbl::index		; index labels for debugging, etc.
	RETURN_OK

.PUSHSEG
.RODATA
@success_msg: .byte "done. from $", $fe, "-$", $fe, " ($", $fe, " bytes)", 0
@success0:    .byte "done.",0
.POPSEG
.endproc

;*******************************************************************************
; PROMPT SAVE ALL
; Asks the user if they would like to save all modified buffers and does so
; if they confirm
; OUT:
;   - .C: set on error
.proc prompt_saveall
	jsr src::anydirty
	beq @done		; no dirty buffers

	; ask the user if they want to save any modified buffers
	ldxy #strings::saveall
	lda #STATUS_ROW
	jsr text::print
@getch:	jsr key::waitch
	cmp #$79		; 'y'?
	beq command_saveall	; save all dirty buffers
	cmp #$6e		; 'n'?
	bne @getch
@done:	RETURN_OK
.endproc

;*******************************************************************************
; COMMAND_ASMDBG
; assembles the source and generates debug information for it
.proc command_asmdbg
	jsr prompt_saveall

	lda #$01
	sta zp::gendebuginfo	; enable debug info
	jsr command_asm
	dec zp::gendebuginfo	; turn off debug-info
	rts
.endproc

;*******************************************************************************
; COMMAND SAVE ALL
; Save all open buffers that have been modified since they were last saved.
; OUT
;   - .C: set on error
.proc command_saveall
.endproc
.proc saveall
@cnt=zp::editortmp
	lda #$00
	sta @cnt
	cmp src::numbuffers
	beq @retok		; if there are 0 buffers, we're done

	jsr src::pushp		; save current source pos
	jsr src::save
	lda src::activebuff
	pha			; save active buffer

@l0:	; check if this buffer needs to be saved (is "dirty")
	lda @cnt
	jsr src::getflags
	and #FLAG_DIRTY
	beq @next

	; save the "dirty" buffer to a file of its name
	lda @cnt
	jsr src::setbuff
	jsr src::current_filename
	bcs @next		; buffer has no name, don't save
	jsr command_save	; save the buffer's changes

@next:	inc @cnt		; next buffer
	lda @cnt
	cmp src::numbuffers	; have we checked all buffers?
	bcc @l0			; repeate til we have

@done:	pla			; restore original buffer
	jsr src::setbuff	; restore buffer
	jsr src::popgoto	; restore source pos
@retok:
	clc			; ok
	rts
.endproc

;*******************************************************************************
; GETS
; Accepts user input at the current cursor position and returns the input
; after the user presses RETURN
; IN:
;  - .XY: callback to a function to accept/validate input (e.g. key::getch)
; OUT:
;  - .XY: 	      address of the string that was read
;  - .A:              length of the string that was read
;  - .C:              set if no input was given (e.g. <-)
.export __edit_gets
.proc __edit_gets
@result_offset=r8
@len=r9
	stxy zp::jmpvec

	; save insert mode etc.
	lda zp::editor_mode
	pha
	lda text::insertmode	; save current insertion mode
	pha

	lda #MODE_INSERT
	sta zp::editor_mode
	lda #TEXT_INSERT
	sta text::insertmode

	ldx zp::curx
	stx @result_offset	; offset to the user-input in line buffer
	lda #$00
	sta mem::linebuffer,x

	jmp @redraw
@getloop:
	jsr text::update

	jsr zp::jmpaddr		; call key-get func
	cmp #$00
	beq @getloop

	pha
	jsr cur::off
	pla

	cmp #$09		; TAB
	beq @getloop		; don't allow tabs in gets
	cmp #K_LEFT
	bne :+
	ldx #$ff
	ldy #$00
	jsr cur::move
	jmp @redraw
:	cmp #K_RIGHT
	bne :+

	ldx zp::curx
	lda mem::linebuffer,x
	beq @redraw
	jsr cur::right
	jmp @redraw

:	cmp #$00
	beq @getloop
	cmp #$80		; > $80 -> not printable
	bcs @getloop
	cmp #K_RETURN
	beq @done
	cmp #K_QUIT		; (done)
	beq @exit
	cmp #K_DEL
	beq :+			; let DELETE through
	jsr key::isprinting
	bcs @getloop		; don't print if not printable
:	jsr text::putch
@redraw:
	jsr draw_active_line
	jsr cur::on
	jmp @getloop

@done:	clc 		; clear carry for success
@exit:	    		; carry is implicitly set by CMP for ==
	php		; save success state
	jsr cur::off	; turn off the cursor

	; move the read text into $100
	ldx @result_offset
@saveres:
	lda mem::linebuffer,x
	sta $100,x
	beq :+
	inx
	bne @saveres

:	stx @len		; store the length of the string
	jsr cur::off

	; get address of the result ($1xx)
	ldx @result_offset
	ldy #$01

	plp			; get success state
	pla
	sta text::insertmode	; restore insert mode
	pla
	sta zp::editor_mode	; restore editor mode
	lda @len
	rts
.endproc

;*******************************************************************************
; GO BASIC
; Installs an NMI to return to the editor and (re)enters BASIC.
.proc go_basic
	; set interface to GUI so that NMI (RESTORE) returns to editor
	lda #$00
	sta dbg::interface
	jmp run::go_basic
.endproc

;*******************************************************************************
; READINPUT
; Reads command input and returns it (0-terminated) in mem::linebuffer
; a prompt may be given in the address XY. If XY is 0, a ':' will be
; used
; IN:
;  - .XY: a prompt to display or $0000 for no prompt
; OUT:
;  - .XY: address to the buffer that was read
;  - .C: set if no input was read (the user pressed <-)
.proc readinput
@prompt=r2
@result_offset=r8
	stxy @prompt
	jsr cur::off
	jsr text::savebuff
	jsr text::clrline

	pushcur			; save the cursor state

	ldy #$00
	iszero @prompt
	beq @terminate_prompt

	; read the user prompt into the linebuffer
:	lda (@prompt),y
	beq @terminate_prompt
	sta mem::linebuffer,y
	iny
	bne :-			; branch always

@terminate_prompt:
	;lda #$00
	sta mem::linebuffer+1,y	; 0-terminate prompt
	lda #':'
	sta mem::linebuffer,y
	iny
	sty cur::minx
	sty zp::curx

	lda status_row
	sta zp::cury
	jsr text::drawline	; clear line & display prompt
	ldxy #key::getch	; key-input callback
	jsr __edit_gets		; read the user input
	php			; save success state

	lda #LINESIZE
	sta cur::maxx		; restore cursor x limit

	jsr text::restorebuff
	ldx @result_offset	; set .X to LSB of buffer
	ldy #$00
	sty cur::minx		; reset minx
	iny			; set .Y to 1 (MSB of result)

	plp			; get success state
	popcur			; restore cursor

:	rts			; <- ONKEY
.endproc

;*******************************************************************************
; ONKEY
; Handles a keypress from the user in INSERT mode
.proc onkey
@insert:
	jsr handle_universal_keys
	bcs :-			; -> RTS
	jmp insert
.endproc

;*******************************************************************************
; ONKEY_CMD
; Handles a keypress from the user in COMMAND mode
.proc onkey_cmd
	jsr handle_universal_keys
	bcc :+
	rts

:	ldx #$01
	stx cmdreps		; init reps to 1
@getreps:
	cmp #$5f		; <-
	bne :+
	rts			; exit this command
:	; check if a number is given (to repeat the following commands)
	jsr key::isdec
	bcc @check_cmds
	cmp #'0'
	beq @check_cmds		; don't handle 0
	jsr buffer_key		; add digit to the input buffer
	sec
	sbc #'0'
	sta cmdreps

	jsr key::waitch		; get another key for the command to do cmdreps times

@check_cmds:
	ldy #numcommands-1
:	cmp commands,y
	beq @found
	dey
	bpl :-
	rts		; no key found

@found:	; if we're in RO mode, make sure command can be run
	lda readonly
	beq :+		; not in RO mode -> continue to run command

	; if in RO mode, and command is an RW one, just return
	cpy #num_rw_commands
	bcc @done

:	lda command_vecs_lo,y
	sta zp::jmpvec
	lda command_vecs_hi,y
	sta zp::jmpvec+1

:	jsr @dorep
	dec cmdreps
	bne :-
@done:	rts

; repeat the command for the number of reps the user requested
@dorep:
	jmp (zp::jmpvec)
.endproc

;*******************************************************************************
; ENTER_INSERT
; Enters INSERT mode
.proc enter_insert
@tabcnt=r2
	lda #MODE_INSERT
	cmp mode
	beq @done

	sta mode
	lda #TEXT_INSERT
	sta text::insertmode
	lda #'i'
	sta text::statusmode

	lda zp::curx
	beq @done

	jsr text::char_index
	cmp #$09		; if on a TAB, move cursor to start of it
	bne @done

	jsr text::tabl_dist
	sta @tabcnt
@tabl:	dec zp::curx
	jsr text::char_index
	inc zp::curx
	cmp #$09
	bne @done
	dec zp::curx
	dec @tabcnt
	bne @tabl
@done:	rts
.endproc

;*******************************************************************************
; FMT AND ENTER COMMAND
; Attempts to format the current line and then enters command mode
.proc fmt_and_enter_command
	jsr cancel
	jmp fmt_line
.endproc

;*******************************************************************************
; CANCEL
; If not in COMMAND mode, just enters command mode
; If already in command mode: clears auxiliary views (if any active), errors,
; etc.
; Calling this from within a command handler (e.g. command_asm) will do the
; latter as the editor is guaranteed to already be in COMMAND mode.
.proc cancel
	lda mode
	cmp #MODE_COMMAND
	bne enter_command

	lda #TEXT_REPLACE
	sta text::insertmode

	; fall through to reset_size
.endproc

;*******************************************************************************
; RESET SIZE
; Resets the display to the largest size
.proc reset_size
	jsr gui::closeall		; close any open windows
	lda debugging
	beq :+
	lda #DEBUG_MESSAGE_LINE-1
	sta height
	rts
:	lda #EDITOR_HEIGHT
	sta height
	jmp refresh
.endproc

;*******************************************************************************
; ENTER_COMMAND
; Enters COMMAND mode
.proc enter_command
	jsr is_visual
	bne :+
	jsr src::popp		; VIS pushes source pos, clean it off stack

:	lda mode
	cmp #MODE_COMMAND
	beq @done

	lda #CUR_NORMAL
	sta cur::mode
	jsr is_visual
	bne @left

	jsr refresh	; unhighlight selection (if we were in VISUAL mode)

; entering command mode moves us one character to the left
@left:	; if we're on a TAB, move cursor to the end of it
	lda mode
	cmp #MODE_INSERT
	bne :+
	jsr ccleft	; insert places cursor after char

:	lda #MODE_COMMAND
	sta mode

	; if we're on a TAB after moving left, move to the end of it
	jsr src::after_cursor
	cmp #$09
	bne @done
	jsr advance_tab

@done:  lda #MODE_COMMAND
	sta mode
	lda #'c'
	sta text::statusmode
	RETURN_OK
.endproc

;*******************************************************************************
; ENTER VISUAL
; Enters VISUAL mode
.proc enter_visual
	jsr is_visual
	bne :+
	rts		; already in VISUAL mode
:	jsr cur::on

	lda #MODE_VISUAL
	sta mode
	lda #CUR_SELECT
	sta cur::mode
	lda #TEXT_REPLACE
	sta text::insertmode

	; save current editor position
	jsr src::currline
	stxy visual_start_line
	lda zp::curx
	sta visual_start_x

	lda #'v'
	sta text::statusmode

	; save current source position
	jmp src::pushp
.endproc

;*******************************************************************************
; ENTER VISUAL LINE
; Enters VISUAL_LINE mode
.proc enter_visual_line
	jsr is_visual
	bne :+
	rts		; already in VISUAL mode

:	jsr enter_visual
	jsr cur::off
	lda #MODE_VISUAL_LINE
	sta mode

	jsr home			; go to column 0
	lda zp::curx
	sta visual_start_x

	lda #'l'
	sta text::statusmode

	; get the length of the current line
	jsr text::rendered_line_len
	ldy #$00
	lda zp::cury
	jmp scr::rvsline_part
.endproc

;*******************************************************************************
; REPLACE
.proc replace
	lda #MODE_INSERT
	sta mode
	lda #TEXT_REPLACE
	sta text::insertmode
	lda #CUR_NORMAL
	sta cur::mode
	lda #'r'
	sta text::statusmode
	rts
.endproc

;*******************************************************************************
; REPLACE_CHAR
.proc replace_char
	jsr key::waitch		; get the character to replace with
	jsr key::isprinting
	bcs @done		; do nothing if not printable
	pha
	jsr src::replace
	bcc :+
	; failed to replace char
	pla			; clean stack
	rts

:	jsr text::char_index
	pla
	sta mem::linebuffer,y	; replace character in linebuffer

	; if a TAB / something differently sized than the char we replaced
	; was inserted, update cursor appropriately
	tya
	jsr gotoindex
	jsr draw_active_line
@done:	rts
.endproc

;*******************************************************************************
; INSERT_START
; moves cursor to the first non-whitespace character of the line and enters
; INSERT mode
.proc insert_start
	jsr enter_insert

	; fall through to home_col
.endproc

;*******************************************************************************
; HOME COL
; Moves cursor to the first non-whitespace character on the line
.proc home_col
	jsr home
@l1:	jsr src::after_cursor
	jsr util::is_whitespace
	bne @done
	jsr ccright
	bcc @l1
@done:	rts
.endproc

;*******************************************************************************
; APPEND_TO_LINE
; Moves the cursor to the end of the line and enters INSERT mode
.proc append_to_line
	jsr enter_insert
@l0:	jsr ccright
	bcc @l0
	rts
.endproc

;*******************************************************************************
; APPEND_CHAR
; Moves the cursor after the current character and enters INSERT mode
.proc append_char
	jsr enter_insert
	jsr src::end
	beq @done
	jsr src::before_newl
	beq @done
	jmp ccright
@done:	rts
.endproc

;*******************************************************************************
; END_OF_WORD
; Moves the cursor to the end of the current "word"
.proc endofword
	jsr ccright
	bcs @done
	jsr ccright
	bcs @done

@l0:	jsr ccright
	bcs @done
	jsr src::after_cursor
	jsr util::isalphanum
	bcc @l0
	jsr ccleft	; move back to the last char
@done:	rts
.endproc

;*******************************************************************************
; END_OF_LINE
; Moves the cursor to the end of the current line
.proc end_of_line
@l0:	jsr ccright
	bcc @l0
	rts
.endproc

;*******************************************************************************
; PREV EMPTY LINE
; Moves the cursor to the last empty line. Empty lines are used as logical
; separators
.proc prev_empty_line
@target=zp::editortmp
	jsr src::currline
	stxy @target

	; locate the previous empty line
	jsr src::pushp

@l0:	jsr src::up
	bcs @move
	decw @target
	jsr src::after_cursor
	cmp #$0d
	bne @l0

@move:	jsr src::popgoto
	ldxy @target
	jmp gotoline
.endproc

;*******************************************************************************
; NEXT EMPTY LINE
; Moves the cursor to the next empty line. Empty lines are used as logical
; separators
.proc next_empty_line
@target=zp::editortmp
	jsr src::currline
	stxy @target

	; locate the next empty line
	jsr src::pushp

@l0:	jsr src::down
	bcs @move
	incw @target
	jsr src::after_cursor
	cmp #$0d
	bne @l0

@move:	jsr src::popgoto
	ldxy @target
	jmp gotoline
.endproc

;*******************************************************************************
; BEGINWORD
.proc beginword
@l0:	jsr ccleft
	bcs @done

	jsr src::atcursor
	jsr util::isalphanum
	bcc @l0
@done:	rts
.endproc

;*******************************************************************************
; BEGIN_NEXT_LINE
.proc begin_next_line
	jsr ccdown
	jmp home
.endproc

;*******************************************************************************
; DELETE TO BEGIN (d0)
; Deletes everything from the current cursor position to the beginning of the
; line
.proc delete_to_begin
	jsr text::bufferon
	jsr enter_insert
:	jsr src::start
	beq @done
	jsr src::atcursor
	cmp #$0d
	beq @done
	jsr backspace
	jmp :-
@done:	jsr text::bufferoff
	jsr draw_active_line
	jmp enter_command
.endproc

;*******************************************************************************
; DELETE LINE (dd)
; Deletes the entire contents of the line that the cursor is on and scrolls
; everything below it up.
.proc delete_line
	; delete the contents of the current line
	jsr src::lineend	; go to end of line
	jsr buff::clear		; clear copy buffer
@l0:	jsr src::atcursor
	cmp #$0d
	beq @moveup
	jsr src::backspace
	bcs @moveup		; break if at start of source buffer
	jsr buff::putch		; put the character that was deleted into the copy buffer
	jmp @l0

@moveup:
	lda #$00
	sta zp::curx
	jsr enter_insert
	jsr ccdown
	jsr backspace
	jsr enter_command
	jsr src::pushp
	jsr home
	jsr src::get
	jsr src::popp

	lda #MODE_VISUAL_LINE
	sta selection_type	; set copy mode to LINE

	jmp draw_active_line
.endproc

;*******************************************************************************
; CHANGE LINE
; Deletes everything from the current character under the cursor to the end of
; the line and enters insert mode.
.proc change_line
	jsr delete_to_end
	jmp append_char
.endproc

;*******************************************************************************
; SUB CHAR
; Deletes the character under the cursor and enters insert mode.
.proc sub_char
	jsr delete_char
	jmp enter_insert
.endproc

;*******************************************************************************
; SUB LINE
; Deletes the contents of the line that the cursor is on and enters insert mode.
.proc sub_line
	jsr home
	jsr delete_to_end
	jmp enter_insert
.endproc

;*******************************************************************************
; DELETE CHAR
; Deletes the character under the cursor
.proc delete_char
	jsr buff::clear
	jsr delch
	bcc @ok
	rts
@ok:	jmp redraw_to_end_of_line
.endproc

;*******************************************************************************
; DELETE TO END
; Deletes everything from the character under the cursor to the end of the line
.proc delete_to_end
	jsr buff::clear

@l0:	jsr delch
	jsr src::after_cursor
	bcs :+			; if at end of buffer, we're done
	cmp #$0d
	bne @l0
:	jsr buff::reverse
	jmp redraw_to_end_of_line
.endproc

;*******************************************************************************
; DELETE WORD
; This is a subcommand of the 'd' (delete) command.  Deletes everything from
; the character under the cursor to the first non-whitespace (if we're on a WS
; character) or until the first whitespace character if we are.
.proc delete_word
@endonalpha=r1
	jsr buff::clear

	; if we're on an alphanum char, end on the first non-alphanum char
	; if we're NOT on an alphanum char, end on the first alphanum char
	jsr src::after_cursor
	jsr util::isalphanum
	lda #$00
	rol		; 0 if starting char is alphanum, 1 if not
	sta @endonalpha	; flag if we are to end on an alphanum char or not

@l0:	jsr delch
	bcs @done

	; check if this is a character we're looking to end on
	jsr src::after_cursor
	jsr util::isalphanum
	ldx @endonalpha
	bne :+			; if we don't want to end on alphanum, skip
	bcs @l0			; if alphanum, continue
:	bcc @l0

@done:  jsr buff::reverse
	jmp redraw_to_end_of_line
.endproc

;******************************************************************************
; PASTE BELOW
; Pastes the contents of the copy buffer to the line below the cursor
.proc paste_below
	jsr exit_visual

	lda selection_type
	cmp #MODE_VISUAL
	beq @vis

@line:	jsr open_line_below_noindent
	jsr paste_buff

	jsr buff::lines_copied
	bcs :+
	jmp src::left

	; paste_buff will assume cursor should be moved to the end of the line
	; where the paste ended, but VISUAL LINE pastes leave the source cursor
	; at the start of the line- fix the cursor to match
:	jsr gotoindex0
@ret:	rts

@vis:	; visual, move to next character and paste there
	jsr append_char
	jsr paste_buff
	bcs @ret			; if we failed to paste, return
	jsr buff::lines_copied
	bcs @ret
	jmp src::left
.endproc

;******************************************************************************
; PASTE ABOVE
; Pastes the contents of the copy buffer to the line above the cursor
.proc paste_above
	jsr exit_visual

	lda selection_type
	cmp #MODE_VISUAL
	beq @vis

@line:	jsr open_line_above_noindent
	jsr paste_buff

	jsr buff::lines_copied
	bcs :+
	jmp src::left

	; paste_buff will assume cursor should be moved to the end of the line
	; where the paste ended, but VISUAL LINE pastes leave the source cursor
	; at the start of the line- fix the cursor to match
:	jsr gotoindex0
@ret:	rts

@vis:	; visual mode, just paste
	jsr enter_insert
	jsr paste_buff
	jsr buff::lines_copied
	bcs @ret
	jmp src::left
.endproc

;*******************************************************************************
; VALIDATE PASTE
; Checks if the paste can be completed at the cursor's current position
; OUT:
;  - .C: set if the cursor cannot be completed based on the current cursor pos
.proc validate_paste
@splitindex=re
@posttext=$100
	lda selection_type
	cmp #MODE_VISUAL
	beq @validate

	; if pasting from visual LINE mode, we won't be affecting any existing
	; lines- return OK
	RETURN_OK

@validate:
	; we must consider 2 cases:
	; 1) if there's no newline in our paste:
	;      then [ pretext | paste | posttext ]
	;    must all fit in one line
	; 2) if there is a newline:
	;       then [pretext | paste[0] ] and [ paste[n] | posttext ]
	;    must all fit in one line (where paste[0] is the first line of
	;    our copy buffer and paste[n] is the last)

	; first, get the index we are "splitting" at
	jsr text::char_index
	sty @splitindex

	; save the part of the line after the split, we call this "posttext"
	ldx @splitindex		; get index of text to save
	ldy #$00
:	lda mem::linebuffer,x
	sta @posttext,y
	beq @copydone
	inx
	iny
	bne :-

@copydone:
	jsr text::savebuff
	jsr buff::push

	; populate mem::linebuffer with [ pretext | paste[0] ], we will need
	; this to validate both cases
	lda @splitindex
	clc
	adc #<mem::linebuffer
	tax
	lda #>mem::linebuffer
	adc #$00
	tay
	jsr buff::getline	; append paste[0] to the linebuffer
	bcc :+
	clc
	bcc @done		; buffer is empty (paste is NOP)

:	jsr text::rendered_line_len	; check length of [pretext | paste[0]]
	bcs @done			; oversized -> return

	; is there a newline in the lines we're pasting?
	jsr buff::lines_copied
	bcs @case2		; if there is a newline, continue to case 2

@case1:	; validate case 1: build [ pretext | paste[0] | posttext ] and make
	; sure it is not oversized
	; since we already have [ pretext | paste[0] ] in the linebuffer, just
	; append posttext to the linebuffer and check it
	jsr text::linelen
	ldy #$00
:	lda @posttext,y
	sta mem::linebuffer,x
	beq @checklen
	iny
	inx
	bne :-
	beq @checklen		; should be unreachable

@case2:	; validate case 2: build [ pretext | paste[0] ] and
	; [ posttext | paste[n]] and make sure it is not oversized
	; we've already validated [ pretext | paste[0] ],
	; make sure [paste[n] | posttext] isn't too big
	ldxy #mem::linebuffer
	jsr buff::lastline
	bcc @cont
	clc
	bcc @done		; last line empty -> return OK

	; copy posttext to the end of the linebuffer
@cont:	ldx #$00
:	lda @posttext,x
	sta mem::linebuffer,y
	beq @checklen
	inx
	iny
	bne :-

@checklen:
	; get the line length- .C will be set if oversized
	jsr text::rendered_line_len
@done:	php
	jsr text::restorebuff
	jsr buff::pop
	plp
	rts
.endproc

;*******************************************************************************
; PASTE BUFF
; Inserts the contents of the buffer at the current cursor position and returns
; to command mode
; OUT:
;   - .C: set if the paste could not be completed (e.g. no room)
.proc paste_buff
@row=rd
@splitindex=re
@linelen=rf
@posttext=$148
@mode=zp::tmp10
	; save the current buffer pointer
	jsr buff::push

	jsr validate_paste
	bcc :+
	jsr beep::short
	jsr enter_command
	sec
	rts

; paste between [linebuffer, char_index(curx)] with the first line from buffer
:	lda zp::cury
	sta @row
	jsr text::char_index
	sty @splitindex

	jsr buff::lines_copied
	cmp #$00
	beq @scrolldone

	pha			; save scroll amount
	tay			; .Y = number of rolls to scroll

	; multi-line pastes don't move the cursor / source position
	jsr src::pushp

	; scroll down by the number of lines we're pasting (.Y)
	ldx height
	lda @row
	jsr text::scrolldownn

	; scroll colors down by number of lines we're pasting
	ldx @row
	ldy height
	pla
	jsr draw::scrollcolorsd

@scrolldone:
	ldx @splitindex	; get index of text to save
	ldy #$00

	; save the part of the line that we're inserting BEFORE
:	lda mem::linebuffer,x
	sta @posttext,y
	beq @copydone
	inx
	iny
	bne :-

@copydone:
	; read the first buffer line into the proper textbuffer location
	jsr text::savebuff
	lda @splitindex
	clc
	adc #<mem::linebuffer
	tax
	lda #>mem::linebuffer
	adc #$00
	tay
	jsr buff::getline
	bcs @done		; buffer is empty

@ok:	pha			; save newline flag
	ldxy r9
	jsr src::insertline	; insert the first line from the paste buffer
	pla
	cmp #$0d		; did line end with a newline?
	bne @lastline		; if not, this is a < 1 line paste
	jsr src::insert

	; redraw the line and move to the next one
	lda @row
	jsr text::drawline
	inc @row

@middlerows:
	; for full rows ($0d terminated), just get a line and draw it
@l1:	ldxy #mem::linebuffer
	jsr buff::getline
	bcs @lastline		; if empty, continue

	sty @linelen
	pha			; save last char read
	ldxy r9			; (getline leaves result in r0)
	jsr src::insertline	; insert the line read
	pla			; restore last char read
	cmp #$0d		; was this line a newline?
	bne @lastline		; if not continue to merge it with last line

	jsr src::insert			; insert the newline
	lda @row
	jsr draw_line_if_visible	; redraw current row
	inc @row			; move to the next row
	bne @l1				; and continue

@lastline:
	; copy the text after the cursor upon insertion to the line buffer again
	jsr text::linelen
	txa
	pha			; save the index of where the paste ended
	ldy #$00
:	lda @posttext,y
	sta mem::linebuffer,x
	beq @lastdone
	inx
	iny
	bne :-

@lastdone:
	lda @row
	jsr draw_line_if_visible

	lda selection_type
	cmp #MODE_VISUAL_LINE
	beq @finish_multi

	jsr buff::lines_copied
	bcs @finish_multi

	; fix source pos - cursor will be on char after paste (if there is one)
	jsr src::right_rep
	jmp @setcur

@finish_multi:
	; if we pasted multiple lines, restore source position and don't move cursor
	jsr src::popgoto
	jsr src::pushp
	jsr src::home
	jsr src::get
	jsr src::popgoto

@setcur:
	pla				; restore index where paste ended
	jsr text::index2cursor
	stx zp::curx

@done:	; restore the buffer pointer
	jsr buff::pop
	lda #MODE_COMMAND
	sta mode
	RETURN_OK
.endproc

;******************************************************************************
; DRAW_LINE_IF_VISIBLE
; If the given row is within the current screen range, (0, height], draws the
; linebuffer.  If not, does nothing.
; IN:
;  - .A:	      the row of the line
;  - mem::linebuffer: the line to draw
.proc draw_line_if_visible
	cmp height
	beq :+
	bcs @done
:	jmp text::drawline
@done:	rts
.endproc

;******************************************************************************
; COMMAND YANK
; In SELECT mode, copies the selected text to the copy buffer. If not in SELECT
; mode, does nothing
.proc command_yank
	jsr yank
	bcc @done
	jsr report_typein_error
@done:	jmp enter_command
.endproc

;******************************************************************************
; COMMAND_MOVE_SCR
; Accepts another key and moves the screen around depending on what that key is:
;  - l: move screen 2 characters to the left
;  - h: move screen 2 characters to the right
.proc command_move_scr
.if 0
; TODO:
	jsr key::waitch
	cmp #$68	; 'h'
	beq @right
	cmp #$6c	; 'l'
	beq @left
	rts
@left:  JUMP FINAL_BANK_VSCREEN, scr::shr
@right: JUMP FINAL_BANK_VSCREEN, scr::shl
.endif
.endproc

;******************************************************************************
; YANK
; In SELECT mode, copies the selected text to the copy buffer. If not in SELECT
; mode, does nothing
; Leaves the source cursor at the beginning of the selection
; OUT:
;  - .C:		set if selection was not able to be yanked
;  - zp::editortmp+1:	address of the beginning of the selection
;  - zp::editortmp+3:	address of the end of the selection
.proc yank
@cur=zp::editortmp+1
@end=zp::editortmp+3
@moveback=zp::editortmp+5	; flag to move to beginning of selection
	jsr is_visual
	beq @yank_selection

; if not in visual mode, prompt for another key
@prompt:
	lda #$79		; #'y'
	jsr buffer_key
	jsr key::waitch
	cmp #$79	; if yy was entered, yank current line
	beq :+
	RETURN_OK

:	; clear the current contents of the copy buffer
	jsr buff::clear

	; go to the end of the line and copy everything to the start of it
	jsr src::lineend

;--------------------------------------
; yy (yank line)
@yankline:
	jsr src::left
	bcs @yydone
	jsr buff::putch
	bcc @yankline
@yydone:
	jsr src::popgoto	; restore source pos from VISUAL
	RETURN_OK

;--------------------------------------
; visual mode, copy the selected text
@yank_selection:
	; get the bounds of the text we're copying and move the source cursor
	; to the end of the selection
	jsr get_selection_bounds
	bcs @restoresrc

	; clear the current contents of the copy buffer
	jsr buff::clear

	; set the selection type so we know how to handle the eventual paste
	lda mode
	sta selection_type

@copy:	jsr src::atcursor
	cmp #$00
	bne :+
	lda #$0d
:	jsr buff::putch		; add the character to the copy buffer
	bcc @next

	; copy selection is too large
	jsr @restoresrc
	RETURN_ERR ERR_COPY_TOO_BIG

@next:	jsr src::pos
	cmpw @cur		; are we back at the START of the selection yet?
	beq @restoresrc
	jsr src::prev
	bcc @copy

@restoresrc:
	jsr src::popgoto	; restore source position to copy's origin

	lda @moveback		; do we need to move to top of selection?
	beq @done		; if end was also the top, no

	; move back to the line the selection began on
	ldxy visual_start_line
	jsr gotoline

	; move right until we're back at the start of the selection
	jmp :++
:	jsr ccright
	bcs @done
:	lda zp::curx
	cmp visual_start_x
	bcc :--

@done:	jmp enter_command
.endproc

;******************************************************************************
; GET SELECTION BOUNDS
; Returns the start and stop source positions for the current selection
; Leaves the source buffer cursor at the end of the selection
;
; OUT:
;  - .C:              set if nothing is selected
;  - zp::editortmp+1: the beginning of the selection
;  - zp::editortmp+3: the end position
;  - zp::editortmp+5: flag to move back to start of selection (if start was
;                     above current location)
.proc get_selection_bounds
@cur=zp::editortmp+1
@end=zp::editortmp+3
@moveback=zp::editortmp+5	; flag to move to beginning of selection
	lda #$00
	sta @moveback

	jsr src::pos	; get the current source position
	stxy @cur
	jsr src::popp	; get the source position we started at
	stxy @end

	jsr src::pushp	; push current source pos

	ldxy @end
	lda mode
	cmp #MODE_VISUAL_LINE
	beq :+			; if in VISUAL LINE mode, allow start == stop

	cmpw @cur
	beq @done	; nothing copied

:	cmpw @cur
	bcs @cont	; end >= cur, don't swap

	; end < cur; swap them
	lda @cur
	sta @end
	lda @cur+1
	sta @end+1
	stxy @cur
	inc @moveback	; flag that we don't need to move source cursor

@cont:	incw @cur

	lda mode
	cmp #MODE_VISUAL_LINE	; are we selecting in LINE mode?
	bne @ok

@line:	ldxy @cur
	jsr src::goto

	jsr src::home	; if cursor is not at start of line, move it there
	jsr src::next
	jsr src::pos
	stxy @cur

	ldxy @end
	jsr src::goto
	jsr src::lineend	; if selecting the whole line, go to end of it
	jsr src::pos
	stxy @end
	RETURN_OK

@ok:	; Update end pointer:
	; the source pos ends on the character BEFORE the one we want to copy
	incw @end
	ldxy @end
	jsr src::goto	; go to the end of the selection to copy
	clc		; ok
@done:	rts
.endproc

;*******************************************************************************
.proc comment_out
	jsr key::waitch	; get a key to decide what to comment out

	cmp #$3b	; if another comment, generate a banner
	bne @check_ban_up
	jsr text::linelen

	dex
	bpl @ban_down	; if line is not empty open line and add banner to it
@ban_cur:
	jsr enter_insert
	jsr comment_banner
	jmp newl

@ban_down:
	jsr open_line_below_noindent
	jsr comment_banner
	jmp ccup

@check_ban_up:
	cmp #':'	; SHIFT-; (generate banner above)
	bne @done
	jsr open_line_above_noindent
	jsr comment_banner
	jmp ccdown

@done:	rts
.endproc

;*******************************************************************************
.proc comment_banner
@cnt=zp::editortmp+2
	jsr text::bufferon
	lda #LINESIZE
	sta @cnt
:	lda #';'
	jsr insert
	dec @cnt
	bne :-
	jsr draw_active_line
	jmp text::bufferoff
.endproc

;*******************************************************************************
.proc word_advance
	jsr src::end
	beq @done
@l0:	jsr ccright
	bcs @done
	jsr src::after_cursor
	jsr util::isseparator
	bne @l0
	jsr ccright	; move after separator
@done:	rts
.endproc

;*******************************************************************************
.proc last_line
@l0:	jsr ccdown
	bcs @done
	lda zp::cury
	cmp height
	bcc @l0
@done:	jmp home
.endproc

;*******************************************************************************
.proc home_line
	jsr src::start	; at start of file?
	beq @done	; if so, we're done

@l0:	lda zp::cury	; top row?
	beq @done
	jsr ccup	; move UP until cursor is at top row
	bcc @l0
@done:	jmp home
.endproc

;*******************************************************************************
.proc goto_end
	jsr add_jump_point
	ldxy #$ffff
	jmp gotoline
.endproc

;*******************************************************************************
; GOTO_START
; Accepts another key and, if it is 'g', moves to the start of the buffer.
.proc goto_start
	lda #$67		; #'g'
	jsr buffer_key
	jsr key::waitch
	cmp #$64		; 'd' (goto definition)
	beq @gotodef
	cmp #$67		; get second 'g' to confirm movement
	bne @ret

@top:	jsr add_jump_point
	ldxy #1
	jmp gotoline

;--------------------------------------
@gotodef:
@word=r6
@len=r8
@addr=ra
	jsr src::pushp
	ldxy #mem::spare
	stxy @word

; get the name of the label to goto
	jsr src::atcursor
	jsr util::isalphanum
	bcs :+
@l0:	jsr src::prev
	bcs :+
	jsr util::isalphanum
	bcc @l0

:	lda #$00
	sta @len
@readword:
	; at start of word, now read the word
	jsr src::right
	bcs :+
	jsr util::isalphanum
	bcs :+
	ldy @len
	sta (@word),y
	inc @len
	bne @readword

:	jsr src::popgoto
	ldy @len
	beq @err		; no symbol under cursor, exit
	lda #$00
	sta (@word),y
	ldxy #mem::spare
	jsr str::toupper
	ldxy #mem::spare
	jsr lbl::addr		; get the address of the line
	bcs @err
	stxy @addr
	bcc @ok

@err:	jmp beep::short

@ok:	jsr add_jump_point
	ldxy @addr
	jsr dbg::gotoaddr	; goto it
	bcs @err
@ret:	rts			; return ok
.endproc

;*******************************************************************************
; JOIN LINE
; Joins the contents of the line below the current one with the current line
.proc join_line
@i=r6
@join_idx=r7
	jsr exit_visual
	jsr src::on_last_line
	beq :-			; -> rts

	jsr text::linelen
	stx @i
	stx @join_idx
	txa
	bne :+
	jmp delete_line

:	jsr end_of_line		; set curx to the correct index
	jsr src::pushp		; save our start position in the source
	jsr src::next		; move to the newline
	jsr src::next		; move past the newline

	; read the new line contents into the text buffer
@readline:
	jsr src::right
	bcs @validate
	ldx @i
	cpx #LINESIZE
	bcs @validate
	sta mem::linebuffer,x
	inc @i
	bne @readline

	; make sure the rendered line is <= LINESIZE characters
@validate:
	ldx @i
	cpx #LINESIZE
	bcs :+
	lda #$00
	sta mem::linebuffer,x

:	jsr text::rendered_line_len
	cpx #LINESIZE+1			; too long?
	bcc @bump			; if not, continue

	; the join would have made the line too long, don't join it
	ldx @join_idx
	lda #$00
	sta mem::linebuffer,x	; re-terminate the line buffer
	jmp src::popgoto	; clean up and exit

@bump:	jsr src::popgoto
	jsr src::next		; move to the newline
	jsr src::delete		; delete the newline
	jsr src::prev
	jsr text::savebuff
	inc zp::cury
	jsr bumpup
	jsr text::restorebuff
	jmp draw_active_line
.endproc

;******************************************************************************
; OPEN LINE ABOVE NO INDENT
; Opens a line above the cursor without indenting
.proc open_line_above_noindent
	lda autoindent
	pha
	lda #$00
	sta autoindent		; temporarily overwrite autoindent flag
	jsr open_line_above
	pla
	sta autoindent		; restore autoindent flag
	rts
.endproc

;******************************************************************************
.proc open_line_above
	jsr enter_insert
	jsr home
	jsr src::after_cursor
	pha

	jsr newl
	jsr draw_active_line
	jsr ccup		; go up

	pla			; did line start with TAB?
	cmp #$09
	bne @done

	; indent if auto-indent is enabled
	ldx autoindent
	beq @done
	jmp insert
@done:	rts
.endproc

;******************************************************************************
; OPEN_LINE_BELOW_NO_INDENT
; See OPEN_LINE_BELOW.  This entry point will not indent regardless of the
; contents of the current line
.proc open_line_below_noindent
	lda autoindent
	pha
	lda #$00
	sta autoindent		; temporarily overwrite autoindent flag
	jsr open_line_below
	pla
	sta autoindent		; restore autoindent flag
	rts
.endproc

;******************************************************************************
; OPEN_LINE_BELOW
; Creates a new empty line below the current one. This entry point will indent
; based on the contents of the current line.  If it starts with a TAB, the new
; line will begin with a TAB.
.proc open_line_below
	jsr enter_insert
	jsr end_of_line		; move to end of current line
	jmp newl		; and insert a newline
.endproc

;******************************************************************************
; HANDLE_UNIVERSAL_KEYS
; Handles keys that behave the same regardless of which mode the editor is in
; IN:
;  - .A: the key to handle
; OUT:
;  - .C: set if the key was handled or clear if the caller must handle it
.proc handle_universal_keys
	ldx #@num_special_keys-1
@l0:	cmp @specialkeys,x
	beq @special
	dex
	bpl @l0

@normal:
	RETURN_OK	; not a universal key code; return to be handled

@special:
	; TODO: what is corrupting this (on rare occasion)?
	; lda #$4c
	; sta zp::jmpaddr

	lda @specialvecslo,x
	sta zp::jmpvec
	lda @specialvecshi,x
	sta zp::jmpvec+1
	jsr zp::jmpaddr
	sec		; key was handled
	rts

.PUSHSEG
.RODATA
@specialkeys:
	.byte K_LEFT		; left
	.byte K_RIGHT		; right
	.byte K_UP		; up arrow
	.byte K_DOWN		; down
	.byte K_HOME		; HOME
	;.byte K_ASM 		; assemble
	.byte K_ASM_DEBUG	; debug
	.byte K_SHOW_BUFFERS	; show buffers
	.byte K_SHOW_PROJECT	; show project
	.byte K_REFRESH		; refresh
	.byte K_LIST_SYMBOLS	; list symbols
	.byte K_LINK            ; link program
	.byte K_CLOSE_BUFF	; close buffer
	.byte K_NEW_BUFF	; new buffer
	.byte K_SET_BREAKPOINT	; set breakpoint
	.byte K_JUMPBACK	; jump back

	.byte K_GOTO_BUFF1	; go-to buffer 1
	.byte K_GOTO_BUFF2	; go-to buffer 2
	.byte K_GOTO_BUFF3	; go-to buffer 3
	.byte K_GOTO_BUFF4	; go-to buffer 4
	.byte K_GOTO_BUFF5	; go-to buffer 5
	.byte K_GOTO_BUFF6	; go-to buffer 6
	.byte K_GOTO_BUFF7	; go-to buffer 7
	.byte K_GOTO_BUFF8 	; go-to buffer 8

	.byte K_NEXT_BUFF	; C= + > next buffer
	.byte K_PREV_BUFF	; C= + < previous buffer
	.byte K_UDG_EDIT	; C= + U activate udg editor
	.byte K_QUIT		; <- (return to COMMAND mode)
	.byte K_GO_BASIC	; F1 (enter BASIC)

	.byte K_NEXT_PAL
	.byte K_PREV_PAL
	.byte K_TOGGLE_FMT
@num_special_keys=*-@specialkeys
.linecont +
.define specialvecs ccleft, ccright, ccup, ccdown, \
	home, \
	command_asmdbg, show_buffers, show_proj, refresh, \
	symview::enter, command_link, \
	close_buffer, new_buffer, set_breakpoint, jumpback, \
	buffer1, buffer2, buffer3, buffer4, buffer5, buffer6, buffer7, buffer8,\
	next_buffer, prev_buffer, udgedit, fmt_and_enter_command, go_basic, \
	gprefs::next_pal, gprefs::prev_pal, toggle_autoformat
.linecont -
@specialvecslo: .lobytes specialvecs
@specialvecshi: .hibytes specialvecs
.POPSEG
.endproc

;******************************************************************************
; HOME
; Moves the cursor to start of the current line
.proc home
@l0:	jsr ccleft
	bcc @l0
	rts
.endproc

;******************************************************************************
; CLEAR
; Clears the screen as well as any relevant state
.export __edit_clear
.proc __edit_clear
	lda #CUR_OFF
	sta cur::status
	jmp scr::clr
.endproc

;*******************************************************************************
; DELETE
; Handles the delete key.
; In VISUAL mode, this deletes the selection
; In COMMAND mode, prompts for another key and deletes characters depending on
; the following command ('w' for word, 'd' for line, etc.)
.proc delete
	jsr is_visual
	bne @cont

;--------------------------------------
; VISUAL mode; delete the selection
@delvis:
@start=zp::editortmp+1	; set by yank
@cnt=zp::editortmp+3
	jsr yank			; yank the selection
	bcs @notfound			; quit if error occurred or no selection

	jsr buff::len
	lda selection_type
	cmp #MODE_VISUAL_LINE
	bne :+
	inx
	bne :+
	iny
:	stxy @cnt

@delsel:
	jsr src::delete
	dec @cnt
	lda @cnt
	cmp #$ff
	bne :+
	dec @cnt+1
:	ora @cnt+1		; are LSB and MSB of @cnt 0?
	bne @delsel
	beq refresh		; done, refresh to clear deleted text

;--------------------------------------
; get a key to decide what to delete
@cont:	jsr key::waitch
	ldx #@numcmds-1
:	cmp @subcmds,x
	beq @found
	dex
	bpl :-
@notfound:
	rts

@found:	lda @subcmdslo,x
	sta zp::jmpvec
	lda @subcmdshi,x
	sta zp::jmpvec+1

	jsr buff::clear		; clear the copy buffer
	jmp (zp::jmpvec)	; execute the delete command

.PUSHSEG
.RODATA
@subcmds:
.byte $77	; w delete word
.byte $64	; d delete line
.byte $24	; $ (end of line)
.byte $30	; 0 (beginning of line)
@numcmds=*-@subcmds

.define subcmds delete_word, delete_line, delete_to_end, delete_to_begin
@subcmdshi: .hibytes subcmds
@subcmdslo: .lobytes subcmds
.POPSEG
.endproc

;*******************************************************************************
; EXIT VISUAL
; If in visual mode, clears highlights from visual mode (if any)
.proc exit_visual
	lda mode
	cmp #MODE_VISUAL
	bcs :+			; anything below MODE_VISUAL is not visual mode
	rts

:	lda #MODE_COMMAND
	sta mode

	; fall through to refresh
.endproc

;*******************************************************************************
; REFRESH
; Redraws the screen
.export __edit_refresh
__edit_refresh:
.proc refresh
	lda zp::cury
	pha			; save cursor Y-coord

	jsr src::pushp

	jsr src::home
	jsr src::get		; get the contents of current line
	jsr text::savebuff	; save the line buffer

	ldx zp::cury		; get # of rows to go up in source

	ldy #$00
	sty highlight_status	; disable highlight
	sty zp::cury		; go to top row

	jsr src::upn		; move source to top line on screen

	; redraw the visible lines
@l0:	jsr src::readline	; read line into text buffer
	php
	bcs :+

	jsr src::prev	; print_line expects source and zp::cury to be synced

:	ldx zp::cury
	jsr draw::resetline	; reset color for the row

	jsr print_current_line	; draw the line of text
	plp			; restore EOF flag
	bcs @clr		; if EOF, clear rest of lines

	jsr src::next		; move past the $0d again
	inc zp::cury
	lda zp::cury
	cmp height
	beq @l0
	bcc @l0

@clr:	; clear the rest of the lines (including highlights)
	ldx zp::cury
	cpx height
	inx
	bcs @done
	stx zp::cury
	jsr draw::resetline
	lda zp::cury
	jsr scr::clrline		; clear the bitmap data for this row
	jmp @clr

@done:	lda #COLOR_RVS
	ldx #STATUS_ROW
	jsr draw::hline		; re-init status row's color

	; restore source position
	jsr src::popgoto
	pla
	sta zp::cury		; restore .Y position
	lda #CUR_OFF
	sta cur::status
	jmp text::restorebuff	; restore current line data
.endproc

;*******************************************************************************
; RESIZE
; Resizes the editor to the given number of rows. The cursor is moved to be
; within the new size if it would be out of bounds.
; IN:
;  - .A: the new size of the editor in rows.
.export __edit_resize
.proc __edit_resize
	cmp height	; is the new height bigger or smaller?
	sta height
	beq @done	; same size
	bcs @bigger	; new size is bigger, redraw screen

@smaller:
@l0:	lda zp::cury
	cmp height
	bcc @done	; cursor is in new height's range
	jsr ccup
	bcc @l0		; loop until cursor is on screen
@done:	rts
@bigger:
	jmp refresh
.endproc

;*******************************************************************************
; PRINT CURRENT LINE
; Prints the contents of the linebuffer at the cursor's current y-coordinate
.proc print_current_line
	lda zp::cury

	; fall through to print_line
.endproc

;*******************************************************************************
; PRINT LINE
; Prints the line buffer at the given y-position and handles
; highlighting/coloring (if applicable).
; IN:
;  - .A: the row to print the linebuffer to
.proc print_line
	jsr draw_src_line

	lda __edit_highlight_en
	beq @done

	; check if the current line is the highlighted one
	jsr src::currline
	cmpw __edit_highlight_line
	bne @done
	lda #$01
	sta highlight_status

	; the highlight was destroyed by drawing the line, re-highlight it
	jmp toggle_highlight
@done:	rts
.endproc

;*******************************************************************************
; SET BREAKPOINT
; Creates a breakpoint at the cursor's current file/line number or removes it
; if one already exists
.export __edit_set_breakpoint
__edit_set_breakpoint:
.proc set_breakpoint
@addr=zp::editortmp
	jsr __edit_current_file	; get the debug file ID and line #
	bcc :+
@noname:
	lda #ERR_UNNAMED_BUFFER
	jmp report_typein_error

:	jsr brkpt::getbyline	; is there already a breakpoint?
	bcs @set		; if not, add one

@remove:
	jsr dbg::removebreakpointbyid
	lda #COLOR_NORMAL
	bne @done

@set:	jsr __edit_current_file	; get the debug file ID and line #
	jsr dbg::setbrkatline	; create the breakpoint

	; if possible try to map the address to the breakpoint
	jsr __edit_current_file
	jsr dbgi::line2addr

	; if we can't get the address, but we are not debugging, that's
	; fine, but we will need to reassemble before it takes affect
	bcs @on

	stxy @addr
	jsr __edit_current_file	; get the debug file ID and line #
	pha
	lda @addr
	sta r0
	lda @addr+1
	sta r0+1
	pla
	jsr dbg::brksetaddr

@on:	lda #COLOR_BRKON
@done:	ldx zp::cury
	jsr draw::hline
	jmp gui::refresh
.endproc

;*******************************************************************************
; NEW BUFFER
; Creates a new buffer and sets it as the new active buffer
; or returns an error if one could not be made
; OUT:
;  - .C: set if a buffer could not be made
.proc new_buffer
	ldx zp::curx
	ldy zp::cury
	jsr src::new
	ldxy #strings::null
	jsr src::name		; rename buffer to empty name
	lda #$00
	sta zp::curx
	sta zp::cury
	jmp refresh
.endproc

;*******************************************************************************
; CLOSE_BUFFER
; Frees the current source buffer and moves to the previous buffer (if there
; is one) or a new source.
.proc close_buffer
	jsr src::close
	bcc :+
	; if there was no buffer to switch to, reset cursor
	lda #$00
	sta zp::curx
	sta zp::cury
:	jmp refresh	; refresh the new buffer
.endproc

;*******************************************************************************
; NEXT_BUFFER
; Moves to the source buffer before the active one. If we are already at the
; first buffer, does nothing
.proc next_buffer
	jsr src::save		; save the active buffer's state
	ldx src::activebuff
	inx
	txa
	jsr src::setbuff
	bcs @done
	jmp refresh
@done:	rts
.endproc

;*******************************************************************************
; PREV_BUFFER
; Moves to the source buffer after the active one. If we are already at the
; last buffer, does nothing
.proc prev_buffer
	jsr src::save		; save the active buffer's state
	ldx src::activebuff
	dex
	bmi @done
	txa
	jsr src::setbuff
	bcs @done
	jmp refresh
@done:	rts
.endproc

;*******************************************************************************
; UDG_EDIT
; Activates the UDG character editor module
.proc udgedit
@cnt=zp::editortmp
@save=zp::editortmp+1
@result=r7
@udg=r8
.ifdef vic20
	pushcur
	jsr scr::save
	inc text::statusfmt

	CALL FINAL_BANK_UDGEDIT, udg::edit
	sta @result

	dec text::statusfmt
	jsr scr::restore
	popcur
	lda @result
	beq @ret	; no UDG created
	lsr		; check if result was 1 (new udg created)
	bcc @update	; if not, must be 2 (udg updated)

@new:	jsr open_line_below_noindent
	jmp @write

@update:
	jsr home
	jsr delete_to_end
	jsr enter_insert

@write: ; write .udg to the source buffer
	jsr text::bufferon

	; write ".db "
	ldx #4
	stx @cnt
:	ldx @cnt
	lda @db_text-1,x
	jsr insert
	dec @cnt
	bne :-

	; convert the binary to hex and write the UDG
	lda #$00
	sta @cnt

@l0:	lda #'$'
	jsr insert

	ldx @cnt
	lda @udg,x
	jsr util::hextostr
	stx @save
	tya
	jsr insert
	lda @save
	jsr insert

	lda @cnt
	cmp #$07
	beq @done
	lda #','
	jsr insert
	inc @cnt
	bpl @l0

@done:	jsr text::bufferoff
	jmp draw_active_line
@ret:	rts
.PUSHSEG
.RODATA
@db_text:
	.byte " bd."	; ".db " (backwards)
.POPSEG
.else
	rts
.endif
.endproc

;*******************************************************************************
; BUFFER NAVIGATION HANDLERS
buffer1: lda #$00
	 skw
buffer2: lda #$01
	 skw
buffer3: lda #$02
	 skw
buffer4: lda #$03
	 skw
buffer5: lda #$04
	 skw
buffer6: lda #$05
	 skw
buffer7: lda #$06
	 skw
buffer8: lda #$07
goto_buffer:
	pha
	jsr src::save
	pla
	jsr src::setbuff
	bcs @done		; if we can't set the buffer, exit
	jmp refresh
@done:	rts

;*******************************************************************************
; SHOW_PROJ
; Displays the project configuration for the current project
.proc show_proj
	; TODO:
	rts
.endproc

;*******************************************************************************
; SHOW_BUFFERS
; Displays the filenames and their respective ID's for every open buffer.
; Then prompts the user for a selection via the standard GUI.  This window
; is closed upon exiting or selecting a buffer
.proc show_buffers
	ldxy #@menu
	lda height
	jsr gui::listmenu

	; we don't need to keep this window open, close it
	jmp cancel

.PUSHSEG
.RODATA
@menu:
.byte GUI_BUFFERS	; id for this GUI
.byte 8			; max height
.word @getkey		; key handler
.word @getdata		; get line handler
.word src::numbuffers	; num ptr
.word strings::buffers	; title

;--------------------------------------
@getdata:
@offset=r9
	sta @offset
	; push the buffer's name (for printing)
	jsr src::filename
	tya
	pha
	txa
	pha

	; display a '*' before filename if the buffer is dirty
	lda @offset
	jsr src::getflags
	ldx #' '
	and #FLAG_DIRTY
	beq :+
	ldx #'*'
:	txa
	pha

@id:	lda @offset
	clc
	adc #$01	; display buffer ID as 1-based
	pha

	; print the buffer name at its corresponding row
	ldxy #@buffer_line
	jsr text::render
	rts

@buffer_line:  .byte ESCAPE_BYTE," :",ESCAPE_CHAR, ESCAPE_STRING, 0

;--------------------------------------
@getkey:
	cmp #K_RETURN
	bne :+
	txa
	bpl @gotobuff

:	cmp #'1'
	bcc @done
	cmp #'8'+1
	bcs :+
	sec
	sbc #'1'
@gotobuff:
	jsr goto_buffer
	sec		; flag to exit GUI
	rts
:	clc		; flag to get more keys
@done:	rts
.endproc

.POPSEG

;******************************************************************************
; COMMAND RENAME
; :r <filename>
; Renames the current source buffer to the given name. Does NOT save the
; file, only the buffer.
; IN:
;  - .XY: the command parameter (the name to give to the buffer)
.proc command_rename
@file=zp::editortmp
	stxy @file
	jsr dbgi::getfileid			; .A = id of the file
	bcs @ok
	RETURN_ERR ERR_BUFFER_NAME_EXISTS	; the chosen name is taken

@ok:	jsr src::filename	; check if there is already a name
	bcs @new		; no debug ID for current file, create one

@rename:
	; ID exists for the current buffer, rename it
	jsr dbgi::getfileid	; get the ID from the existing name
@new:	ldxy @file		; restore new file name
	jsr dbgi::setfile	; replace existing name for this ID
	ldxy @file		; restore new file name
	jmp src::name		; rename to the "source" name to string in .XY
.endproc

;*******************************************************************************
; GET COMMAND
; Prompts the user for a command and executes it (if valid).
.proc get_command
@cmdbuff=$100
@arg=r0
	ldxy #$0000
	stx overwrite		; clear OVERWRITE flag (for SAVEs)
	stx __edit_binary_flag	; clear binary flag

	jsr readinput
	bcs @done

; read the command (the first character)
	lda @cmdbuff+1
	ldx #@num_ex_commands-1

@l0:	cmp @ex_commands,x
	beq @found
	dex
	bpl @l0
	ldxy #(@cmdbuff+1)
	jmp command_gotoline	; if command isn't found try GOTOLINE

; set the jump vector for the appropriate command
@found:
	lda @exvecslo,x
	sta zp::jmpvec
	lda @exvecshi,x
	sta zp::jmpvec+1

	ldx #$01

	lda @cmdbuff+2
	cmp #'@'		; check overwrite flag (for SAVE commands)
	bne :+
	stx overwrite		; set OVERWRITE flag
	inx			; move past '@'
:	cmp #$62		; check binary ('b') flag (also used by DEBUG)
	bne @parsearg
	inc __edit_binary_flag	; set binary flag
	inx			; move past 'b'

; get the argument for the command and send it along to the vector
@parsearg:
:	lda @cmdbuff+1,x
	inx
	cmp #' '
	beq :-
	ldy #>@cmdbuff

	; clear the existing status/error/etc
	jsr text::clrinfo

	; run the command
	jmp (zp::jmpvec)
@done:  rts			; no input

.PUSHSEG
.RODATA
@ex_commands:
	.byte $67	; g - go
	.byte $64	; d - debug
	.byte $65	; e - open file
	.byte $72	; r - rename
	.byte $73	; s - save file
	.byte $53	; S - save all files
	.byte $78	; x - scratch file
	.byte $61	; a - assemble file
	.byte $42	; B - create .BIN
	.byte $50	; P - create .PRG
	.byte $44	; D - create .D (debug)
	.byte $4c	; L - laod .D (debug)
	.byte $6f	; o - create .OBJ file
@num_ex_commands=*-@ex_commands

.linecont +
.define ex_command_vecs command_go, command_debug, \
	__edit_load, command_rename, command_save, command_saveall, \
	command_scratch, command_assemble_file, \
	command_savebin, command_saveprg, command_savedbg, command_loaddbg, \
	command_asm_obj
.linecont -
@exvecslo: .lobytes ex_command_vecs
@exvecshi: .hibytes ex_command_vecs

.POPSEG
.endproc

;******************************************************************************
; EDIT
; Configures the cursor/screen/etc. for editing
.proc edit
.ifdef vic20
	lda #$7f
	sta $911e		; disable CA1 (RESTORE key) interrupts
.endif

	lda #TEXT_INSERT
	sta text::insertmode

	jsr reset_size

	ldx #$00
	stx readonly
	ldy #EDITOR_ROW_START
	jsr cur::setmin
	ldx #LINESIZE
	ldy #STATUS_ROW
	sty status_row
	jmp cur::setmax
.endproc

;******************************************************************************
; COMMAND LOADDBG
; :L <filename>
; Loads the given DEBUG file (program binary, global symbol table, and debug
; information)
; IN:
;  - .XY: the argument to the command (filename)
.proc command_loaddbg
@file=zp::editortmp
@addr=zp::editortmp+1
	jsr irq::off

	stxy @file
	jsr file::exists
	bcc @open

@err:	jsr irq::on
	jmp report_drive_error

@open:	ldxy @file
	jsr file::open_r	; open the file
	bcs @err

	sta @file
	tax
	jsr krn::chkin		; CHKIN, file in .X is input

	; read the start address of the program
	jsr krn::chrin
	sta asm::origin
	sta @addr
	jsr krn::chrin
	sta asm::origin+1
	sta @addr+1

	; read the size of the program and calculate the "top" address
	jsr krn::chrin
	clc
	adc asm::origin
	sta asm::top
	php
	jsr krn::chrin
	plp
	adc asm::origin+1
	sta asm::top+1

	; read the CODE (binary data)
:	jsr krn::chrin
	ldxy @addr
	jsr vmem::store
	incw @addr
	ldxy @addr
	cmpw asm::top
	bne :-

	; read the symbol table
	jsr lbl::load

	; read the debug information
	lda #$00				; no relocation
	CALL FINAL_BANK_DEBUG, dbgi::load

@done:	lda @file
	jsr file::close

@ret:	jmp irq::on
.endproc

;******************************************************************************
; SAVE D
; :D <filename>
; Stores the program binary, the global symbol table, and debug information
; as a .D (debug) file to <filename>.
; IN:
;  - .XY: the argument to the command (filename)
.proc command_savedbg
@file=zp::editortmp
@addr=zp::editortmp+2
	jsr irq::off
	jsr file::open_w	; open the output filename
	bcc :+

@err:	jsr irq::on
	jmp report_drive_error

:	sta @file
	tax
	jsr krn::chkout		; CHKOUT, file in .X is output

	; write the start address of the program
	lda asm::origin
	sta @addr
	jsr krn::chrout
	lda asm::origin+1
	sta @addr+1
	jsr krn::chrout

	; write the size of the program
	lda asm::top
	sec
	sbc asm::origin
	php
	jsr krn::chrout
	plp
	lda asm::top+1
	sbc asm::origin+1
	jsr krn::chrout

	; write the CODE (binary data)
:	ldxy @addr
	jsr vmem::load
	jsr krn::chrout
	incw @addr
	ldxy @addr
	cmpw asm::top
	bne :-

	; write the symbol table
	jsr lbl::dump

	; write the debug information
	CALL FINAL_BANK_DEBUG, dbgi::dump

@done:	lda @file
	jsr file::close
	jmp irq::on
.endproc

;******************************************************************************
; SAVE PRG
; :P <filename>
; Stores the assembled program as a .PRG file to <filename>. This is the
; 2 byte load address (asm::origin) followed by the raw program binary
; Does nothing if no program has not been successfully assembled.
; IN:
;  - .XY: the argument to the command (filename)
.proc command_saveprg
@file=r4
	jsr irq::off
	jsr file::open_w	; open the output filename
	bcc :+
	jmp irq::on		; failed to open file

:	sta @file

	; write the .PRG header
	tax
	jsr krn::chkout		; CHKOUT, file in .X is output
	lda asm::origin
	jsr krn::chrout
	lda asm::origin+1
	jsr krn::chrout

	jmp write_asm		; write the assembled program
.endproc

;******************************************************************************
; SAVE BIN
; :B <filename>
; Stores the assembled program as raw binary to <filename>
; Does nothing if no program has not been successfully assembled.
; IN:
;  - .XY: the argument to the command (filename)
.proc command_savebin
@file=r4
	jsr irq::off
	jsr file::open_w	; open the output filename
	bcc :+
	jmp irq::on		; failed to open file
:	sta @file
	; fall through
.endproc

;******************************************************************************
; WRITE_ASM
; Writes the assembled program to the file in r0 and then closes the file
; IN:
;  - r4: the file handle to write out
.proc write_asm
@file=r4
	; write the assembled program
	ldxy asm::top
	stxy file::save_address_end
	ldxy asm::origin	; get the base address of the program (in vmem)
	lda @file
	jsr file::savebin	; write the binary to file
	lda @file
	jsr file::close
@done:	jmp irq::on
.endproc

;******************************************************************************
; COMMAND SAVE
; :s[@] <filename>
; Saves the active source buffer to the given filename.  If the overwrite
; flag ('@') is given, e.g. "s@ file.txt", then the existing file is
; overwritten if it exists
; If no filename is given, then the buffer name is used as the filename
; IN:
;   - .XY: the filename of the file to save
.proc command_save
@file=r8
	stxy @file

	ldxy #strings::saving
	jsr print_info

	ldxy @file
	jsr str::len		; get the length of the file to save
	bne @rename		; >0: filename was given

	; get active filename (r0 = name)
	jsr src::current_filename
	bcc :+

	; err no filename
	lda #ERR_NO_FILENAME
	jmp report_typein_error

:	stxy @file
@rename:
	jsr irq::off
	lda overwrite
	beq @open		; if overwrite flag isn't set, continue
@scratch:
	ldxy @file
	jsr file::scratch	; (try to) delete the existing file
	bcs @err

	; open the file, write the source to it, and close the file
@open:	ldxy @file
	jsr command_rename	; first, rename the buffer to our filename
	ldxy @file
	jsr file::open_w	; open file for writing
	bcs @err
	sta @file
	jsr src::pushp		; save current source pos
	lda @file
	jsr file::savesrc	; save the buffer
	jsr src::popgoto	; restore source pos
	lda @file
	jsr file::close		; close the file
	cmp #$00
	bne @err

	jsr text::clrinfo	; erase SAVING message
	jsr src::setflags	; clear flags on the source buffer and return
	jmp irq::on

@err:	jsr irq::on
	jmp report_drive_error
.endproc

;******************************************************************************
; COMMAND SCRATCH
; :x <filename>
; Deletes the given file
; IN:
;  - .XY: the filename of the file to delete
.proc command_scratch
@file=r8
	stxy @file
	jsr irq::off

	jsr file::exists
	bcs @err

	ldxy #strings::deleting
	jsr print_info

	ldxy @file
	jsr file::scratch
@err:	jsr irq::on
	jmp report_drive_error
.endproc

;******************************************************************************
; LOAD
; Loads the file from disk into the source buffer
; IN:
;  - .XY: the filename to load
; OUT:
;  - .C: set if file could not be loaded into a buffer
.export __edit_load
.proc __edit_load
@file=r9
	stxy @file

	; check if the file is already open in one of our buffers
	lda src::numbuffers
	cmp #MAX_SOURCES-1
	bcc :+
	lda #ERR_TOO_MANY_BUFFERS
	jmp report_typein_error

:	jsr src::buffer_by_name
	bcs @notfound

@switch_buff:
; buffer already loaded, switch to it
	cmp src::activebuff
	beq @ok			; buffer already active; quit

	pha
	jsr src::save		; save the current buffer's state
	pla
	jsr src::setbuff	; switch to the new buffer

	jsr refresh
	RETURN_OK

@notfound:
; buffer doesn't exist in any RAM bank, load from disk
	jsr irq::off

	ldxy @file
	jsr file::exists
	bcs @err

	; display loading...
	ldxy #strings::loading
	jsr print_info

	; load the file
	ldxy @file
	jsr file::open_r
	bcs @err		; failed to load file
	pha			; save file handle
	jsr src::new
	pla			; get the file handle
	pha
	jsr file::loadsrc	; load to SOURCE buff
	pla
	php

	jsr irq::on
	jsr file::closeall	; close all files
	plp
	bcs @err
	ldxy @file
	jsr src::name		; name the buffer

	; give the filename a debuginfo ID
	jsr src::current_filename
	jsr dbgi::setfile

	lda #$00
	sta zp::curx
	sta zp::cury		; reset cursor
	jsr src::setflags	; clear flags on the source buffer

	jsr refresh
	jsr text::clrinfo
	jsr cancel
@ok:	RETURN_OK

@err:	jsr irq::on
	jsr report_drive_error
	jsr file::closeall	; close all files
	sec
	rts
.endproc

;******************************************************************************
; NEWL
; Inserts a newline at the current cursor position
; The indentation flag is ignored (line will not be indented)
.proc newl
	jsr is_readonly
	bne @insert
	; if in readonly mode, just go down
	jmp begin_next_line

@insert:
	lda #$0d
	jsr src::insert
	jmp scroll_line
.endproc

;******************************************************************************
; LINEDONE
; Inserts a newline ($0d) and attempts to compile the line entered in
; (mem::linebuffer).
; If successful, formats the source according to the type of the assembled line
; (instruction, label, etc.) and creates a line/address mapping.
.proc linedone
@indent=zp::editortmp
	jsr is_readonly
	bne :+
	jmp begin_next_line	; if READONLY, just go down a line

:	lda #$0d
	jsr src::insert

	; save the character at the location we're terminating in case we need
	; to undo this operation
	jsr __text_char_index
	lda mem::linebuffer,y
	pha
	tya
	pha

	lda #$00
	jsr text::putch

	lda zp::curx
	beq @fmt_done	; @ column 0, skip tokenization and go to the next line
	jsr src::up
	jsr fmt_line
	php
	jsr src::down
	plp
	bcc @ok

@err:	; invalid line, back up and return
	jsr beep::short
	pla
	tay
	pla
	sta mem::linebuffer,y
	jsr print_current_line
	jmp src::backspace

@ok:
@fmt_done:
	sta @indent
	pla			; clean stack
	pla
	jsr scroll_line
	lda @indent
	jmp start_next_line
.endproc

;******************************************************************************
; FMT LINE
; Attempts to compile the line entered in (mem::linebuffer)
; If successful, formats the source according to the type of the assembled line
; (instruction, label, etc.) and creates a line/address mapping.
; OUT:
;   - .A: indent hint; 1=next line should be indented, 0=not
;   - .C: set if the line was not formatted (assembly failed)
.proc fmt_line
	lda fmt::enable
	bne :+
	RETURN_OK

:	; tokenize (1st pass) to check if the line is valid
	ldxy #mem::linebuffer
	lda #FINAL_BANK_MAIN
	jsr asm::tokenize
	tax
	php			; save assembly status (.C flag)
	bcs @done		; failed to assemble, skip formatting

; format the line based on the line's contents (in .A from tokenize)
@fmt:	lda autoindent
	beq @done		; if indent disabled, skip

	lda #$00		; init flag to NO indentation
	cpx #ASM_COMMENT	; if this is a comment, don't format
	beq @done
	txa
	jsr fmt::line
	lda #$01		; default to indent ON

@done:	pha			; save indent hint
	jsr print_current_line
	pla			; restore indent hint
	plp			; restore formatting/assembly success flag
	rts
.endproc

;******************************************************************************
; START NEXT LINE
; Begins a new line based on the given indentation hint
; IN:
;   - .A: indent hint; 1=next line should be indented, 0=not
.proc start_next_line
@indent=ra		; indent boolean (!0 = indent)
	sta @indent		; set indent flag

	; redraw the cleared status line
	jsr text::update

	; indent the new line
	lda @indent
	beq @indentdone		; skip indent if curx == 0
	lda format
	beq @indentdone

	; make sure indent won't overflow line
	jsr text::rendered_line_len
	txa
	clc
	adc #TAB_WIDTH		; full width tab
	cmp #LINESIZE
	bcs @indentdone		; can't indent, line would overflow

	jsr src::after_cursor
	cmp #$09
	beq @indentdone		; already indented, skip
	lda #$09		; TAB
	jsr src::insert
	lda #$09		; TAB
	jsr text::putch

@indentdone:
	jmp print_current_line
.endproc

;******************************************************************************
; REDRAW_TO_END_OF_LINE
; Redraws the line starting at the cursor's x position to the next $0d in the
; source
.proc redraw_to_end_of_line
	jsr text::char_index
	tya
	pha

	jsr src::pushp
	jsr src::home
	jsr src::get
	jsr draw_active_line
	jsr src::popgoto

	pla
	jsr text::index2cursor
	stx zp::curx

	; if we're on a TAB, move cursor to the end of it
	jsr src::after_cursor
	cmp #$09
	bne :+
	jsr advance_tab

:	; make sure cursor is pointing to something in the source
	; (unless line is empty)
	jsr src::end
	beq @back
	jsr src::after_cursor
	cmp #$0d
	bne @done
@back:	jmp src::left
@done:	rts
.endproc

;******************************************************************************
; SCROLL LINE
; Updates the curson and scrolls lines below the one we're on
; The linebuffer is also updated to contain the contents of the new line
; IN:
;   - zp::cury: row to draw the line at
.proc scroll_line
	; scroll lines below cursor position
	ldy zp::cury
	cpy height
	bcc @scrolld

@scrollu:
	; if we're at the bottom, scroll whole screen up
	ldx #EDITOR_ROW_START
	lda height
	jsr scrollup

	; and clear the new line
	jsr text::clrline
	lda height
	jsr scr::clrline

	dec zp::cury
	bne @setcur		; branch always

@scrolld:
	iny
	tya
	ldx height
	jsr text::scrolldown

	; shift colors below cursor down by 1
	ldx zp::cury
	ldy height
	jsr draw::scrollcolorsd1

	; and clear the color of the newly opened line
	ldx zp::cury
	jsr draw::resetline

@setcur:
	jsr src::get
	lda mem::linebuffer
	cmp #$09		; TAB
	bne @setx
	ldx #TAB_WIDTH
	lda mode
	cmp #MODE_INSERT
	beq @setx
	dex
	skw
@setx:	ldx #$00
	ldy zp::cury
	iny
	jsr cur::set
	jmp draw_active_line
.endproc

;*******************************************************************************
; CLRERROR
; Clears any error message
.proc clrerror
	lda #$00
	sta mem::statusinfo
	rts
.endproc

;******************************************************************************
; INSERT
; Adds a character at the cursor position.
.proc insert
	cmp #$14		; handle DEL
	bne :+
	jmp ccdel
:	cmp #$0d
	bne :+
	jsr clrerror		; clear error so we can report on THIS line
	jmp linedone		; handle RETURN

:	jsr key::isprinting
	bcs @done		; non-printing

	ldx text::insertmode
	bne @put
@replace:
	jsr src::replace
	bcs @done		; nothing to replace
	jmp text::putch
@put:	pha
	jsr text::putch
	pla
	bcs @done
	jmp src::insert
@done:	rts
.endproc

;*******************************************************************************
; CCUP
; Handles the up cursor key
; OUT:
;   - .C: set if the cursor could not be moved
.proc ccup
@xend=r9
@ch=ra
	; are we already on the first line of the buffer?
	jsr src::currline
	cmpw #1
	bne :+			; not first line -> continue
	;sec
	rts			; return, can't move up

:	jsr src::atcursor
	sta @ch

	lda zp::curx
	sta @xend

	lda mode
	cmp #MODE_VISUAL_LINE
	bne @chkvis
	; if we're below the start line, redraw the current line (deselect)
	jsr cmp_vis_start
	beq @up
	bcc @up
	jsr draw_active_line
	jmp @up

@chkvis:
	; if we are in VISUAL mode, highlight to the beginning of the line
	lda mode
	cmp #MODE_VISUAL
	bne @up

	jsr cmp_vis_start
	beq @sameline
@diffline:
	ldx zp::curx
	ldy #$00
	beq @rvs0

@sameline:
	; vis start line == current line, reverse based on cursor's column
	; relative to the visual start column
	ldx visual_start_x
	cpx zp::curx
	bcs :+

	; curx > visual_start_x
	ldy visual_start_x
	ldx zp::curx
	inx			; (already toggled curx off)
	lda zp::cury
	jsr scr::rvsline_part	; deselect section right of visual_start_x
	ldx visual_start_x
	inx
	ldy #$00
	beq @rvs0		; reverse 0 to (viusal_start_x-1)

:	ldy #$00
	ldx zp::curx		; reverse 0 to curx
@rvs0:	lda zp::cury
	cpx #$00
	beq @viscur
	jsr scr::rvsline_part

@viscur:
	; handle cursor state for VISUAL mode
	jsr cmp_vis_start
	bcc @up
	beq @up
@toggle:
	jsr cur::toggle	; if we're deselecting, toggle cursor off

@up:	jsr src::home
	jsr src::start
	bne @cont

	; couldn't move up, we're now at the start of the buffer
	lda @ch
	cmp #$0d
	beq @cont	; if we crossed a newline, continue
	jsr src::get	; get the contents of the line we're on now

	ldx #$00
	stx zp::cury	; row 0
	lda mem::linebuffer
	cmp #$09	; TAB
	bne :+
	jsr src::next
	ldx #TAB_WIDTH
:	ldy mode
	cmp #MODE_INSERT
	beq :+
	dex
:	stx zp::curx
	sec
	rts		; done

@cont:	jsr src::up
	jsr src::get	; read the line we're moving to into linebuffer

	ldx #$00
	stx zp::curx

@checkscroll:
	ldy zp::cury		; is cursor at row 0?
	beq @scroll		; if it is, scroll the screen down
	dec zp::cury		; if not, decrement it
	bpl @redraw

@scroll:
	lda #EDITOR_ROW_START
	ldx height
	jsr scrolldown		; cursor wasn't moved, scroll
@redraw:
	jsr print_current_line

	; if in VISUAL_LINE mode, just rvs the line and return
	lda mode
	cmp #MODE_VISUAL_LINE
	bne @movex
	jsr text::rendered_line_len
	ldy #$00
	lda zp::cury
	jsr scr::rvsline_part
	RETURN_OK

@movex: lda zp::curx
	cmp @xend
	bcs :+
	jsr src_right
	bcs :+
	jsr cur::right
	jmp @movex

:	; if we ended on a TAB, advance to the next TAB col else curx
	jsr src::after_cursor
	cmp #$09		; did we end on a TAB?
	bne ccup_highlight	; if not, continue
	lda mode
	cmp #MODE_INSERT
	beq ccup_highlight
	jsr advance_tab

; fallthrough to ccup_highlight
.endproc

;*******************************************************************************
; CCUP HIGHLIGHT
; Highlights the line that the cursor is on if the editor is in VISUAL mode
; This is called after the ccup logic
; the logic defining what to highlight is as follows:
;  if we are SELECTING (current line < visual-start-line):
;    hightlight from [cur-x, end-of-line]
;  if DESELECTING (visual-start-line < current line):
;    highlight from [0, cur-x]
;  if cur line == start line:
;    highlight from [cur-x, visual-start-x]
;
; If the editor is not in visual mode, this routine does nothing
.proc ccup_highlight
@togglecur=r7
	lda mode
	cmp #MODE_VISUAL
	bne @done

	lda #$00
	sta @togglecur

	jsr cmp_vis_start
	bcc @sel
	beq @eq

@desel:	; highlight (deselct) from [0, cur-x]
	ldy #$00
	ldx zp::curx
	beq @toggle
	inc @togglecur
	jmp @rvs

@sel:	; highlight from [cur-x, end-of-line]
	jsr text::rendered_line_len
	ldy zp::curx
	jmp @rvs

@eq:	; highlight between cur-x and visual-start-x
	ldy zp::curx
	cpy visual_start_x
	beq @toggle
	bcc :+

	; swap .X and .Y
	ldx zp::curx
	ldy visual_start_x
	inc @togglecur
	bne @rvs

:	ldx visual_start_x
	inx

@rvs:	lda zp::cury
	jsr scr::rvsline_part	; reverse line part from column .Y to .X
	lda @togglecur
	beq @done
@toggle:
	jsr cur::toggle
@done:	RETURN_OK
.endproc

;******************************************************************************
; SRC RIGHT
; Calls the appropriate src::right procedure based on the current editor mode
.proc src_right
	lda mode
	cmp #MODE_INSERT
	beq :+
	jmp src::right_rep
:	jmp src::right
.endproc

;******************************************************************************
; DELCH
; Delete the character under the cursor
; If the character can not be deleted does nothing and returns .C set.
; Will not cross line boundaries
; OUT:
;  - .C: set if no character could be deleted.
.proc delch
@tmp=r0
	jsr src::before_newl
	beq @nodel
@del:	jsr buff::putch
	lda #MODE_VISUAL
	sta selection_type
	jmp src::delete
@nodel:	sec
	rts
.endproc

;******************************************************************************
; CCLEFT
; Handles the left cursor key
; OUT:
;  - .C: set if cursor could not be moved
.proc ccleft
@tabcnt=r4
@deselect=r5
	lda zp::curx
	bne :+
	sec
	rts

:	lda mode
	cmp #MODE_VISUAL_LINE
	bne @move		; do nothing on LEFT if in VISUAL_LINE mode
@nomove:
	sec
	rts

@move:	jsr src::atcursor
	ldy mode
	cpy #MODE_INSERT
	beq :+

	jsr src::after_cursor
:	pha
	jsr src::left
	bcc @ok			; if we successfully moved left, continue
	pla
	rts

@ok:	; check if we're in VISUAL mode
	lda mode
	cmp #MODE_VISUAL
	bne @movecur
	lda #$00
	sta @deselect

	; if (cur-line > visual_start_line) we are DESELECTING: unhighlight
	jsr cmp_vis_start
	beq @eq
	bcc @movecur		; not deselecting, continue
@desel: lda #$01		; set deselect flag
	bne :+			; continue to deselect
@eq:	lda zp::curx
	cmp visual_start_x
	beq @movecur
	lda #$00
	adc #$00		; .A = 1 : (curx > visual_start_x) ? 0
:	sta @deselect		; set deselect flag (TRUE if curx > start_x)
	beq @movecur		; if not deselecting, continue
	jsr cur::toggle		; turn off (deselect) old cursor position

@movecur:
	pla
	cmp #$09
	bne @curl

	ldy mode
	cpy #MODE_INSERT
	beq @cont

	; TAB in command/replace mode
	dec zp::curx
	jsr text::char_index
	inc zp::curx
	cpy #$00
	bne @cont
	ldx #$00
	cmp #$09
	bne :+
	ldx #TAB_WIDTH-1
:	stx zp::curx
	sec
	rts

; handle TAB (repeat the MOVE LEFT logic til we're at the prev TAB col
; OR the previous character
@cont:	lda @deselect
	pha
	lda #$00
	sta @deselect		; temporarily disable deselect

@tabl:	jsr @curl
	dec zp::curx
	jsr text::char_index
	inc zp::curx
	cmp #$09
	beq @tabl

	pla
	sta @deselect		; restore deselect flag
	ldx mode
	cpx #MODE_INSERT
	beq :+
	; if in REPLACE, move left of the TAB character
	jsr text::char_index
	cmp #$09		; are we still on a TAB char?
	bne :+
	jsr @curl		; move off it if so
:	RETURN_OK

@curl:  dec zp::curx
	lda mode
	cmp #MODE_VISUAL
	bne :+
	lda @deselect
	bne :+
	jsr cur::toggle		; toggle cursor
:	RETURN_OK
.endproc

;******************************************************************************
; CCRIGHT
; Handles the right cursor key
; OUT:
;  - .C: set if cursor could not be moved
.proc ccright
@tabcnt=r4
@deselect=r5
	lda mode
	cmp #MODE_VISUAL_LINE
	beq @ret	; do nothing on RIGHT if in VISUAL_LINE mode

	cmp #MODE_INSERT
	beq @ins

@rep:	jsr src::right_rep
	bcc @ok
	rts		; can't move right

@ins:	jsr src::right
	bcs @done	; can't move right

@ok:	; turn off the old cursor if we're unhighlighting
	jsr src::atcursor
	ldy mode
	cpy #MODE_INSERT
	beq :+
	jsr src::after_cursor
	cmp #$09
	bne @curr
	jsr @curr		; if we're moving to a TAB, fool tabr_dist
	lda #$09

:	cmp #$09		; did we move over a TAB?
	bne @curr

	; handle TAB (repeat the MOVE RIGHT logic til we're at the TAB next col)
	jsr text::tabr_dist
	sta @tabcnt
	ldy zp::editor_mode
	cpy #MODE_INSERT
	beq :+
	dec @tabcnt
	beq @retok
:	jsr @curr
	dec @tabcnt
	bne :-
@retok:	clc
@ret:	rts

@curr:	lda #$00
	sta @deselect

	lda mode
	cmp #MODE_VISUAL
	bne @movecur

	jsr cmp_vis_start
	beq @eq
	bcs @movecur
@desel:	lda #$01
	bne :+

@eq:	lda visual_start_x
	cmp zp::curx
	beq @movecur
	lda #$00
	adc #$00
:	sta @deselect
	beq @movecur
	jsr cur::toggle

@movecur:
	inc zp::curx
	lda mode
	cmp #MODE_VISUAL
	bne :+
	lda @deselect
	bne :+
	jsr cur::toggle
:	clc
@done:	rts
.endproc

;******************************************************************************
; RVS CURRENT LINE
; Reverses from column 0 to the end of the line (text::rendered_line_len) at
; the current cursor Y position
.proc rvs_current_line
	jsr text::rendered_line_len
	ldy #$00
	lda zp::cury
	jmp scr::rvsline_part
.endproc

;******************************************************************************
; CCDOWN
; Handles the down cursor key
; OUT:
;  - .C: clear if the cursor was moved DOWN or screen scrolled
.proc ccdown
@xend=r9
@selecting=ra
@linelen=rb
	lda mode
	cmp #MODE_INSERT
	beq @endins
	jsr src::end_rep
	beq @ret
@endins:
	jsr src::end
	bne :+
	sec
@ret:	rts		; cursor is at end of source file, return

:	lda zp::curx
	sta @xend

	lda mode
	cmp #MODE_VISUAL_LINE
	bne @chkvis

	; if we're above the start line, redraw the current line (deselect)
	jsr cmp_vis_start
	bcs @cont
	jsr print_current_line
	jmp @cont

@chkvis:
	cmp #MODE_VISUAL	; in VISUAL mode?
	bne @cont		; if not, no need to deal with (de)highlighting

	jsr text::rendered_line_len
	stx @linelen

	jsr cmp_vis_start
	beq @sameline

@diffline:
	ldx @linelen
	ldy zp::curx
	bcc @rvs0	; if we're above the start line, continue
	iny
	cpy @linelen
	beq @viscur	; if at end of the line, nothing to reverse
	bne @rvs0	; else reverse [curx, linelen]

@sameline:
	; vis start line == current line, reverse based on cursor's column
	; relative to the visual start column
	ldx @linelen
	ldy zp::curx		; reverse curx to end of line
	cpy visual_start_x
	bcs @rvs0		; if cursor is to the right of start-x, skip

	ldx visual_start_x
	lda zp::cury
	jsr scr::rvsline_part	; reverse OFF the part before visual_start_x

	; and reverse ON the part after visual_start_x
	ldy visual_start_x
	iny
	ldx @linelen

@rvs0:	lda zp::cury
	cpx #$00		; is line empty?
	beq @viscur		; if line is empty, nothing to reverse
	jsr scr::rvsline_part

	lda zp::curx
	sta @xend

@viscur:
	; handle cursor state for VISUAL mode
	jsr cmp_vis_start
	bne @cont
	lda zp::curx
	cmp visual_start_x
	bcc @cont
@toggle:
	jsr cur::toggle	; if we're deselecting, toggle cursor off

@cont:	jsr src::down
	bcc @down

	; can't move down, move cursor to end of line
	jsr text::rendered_line_len
	dex
	stx zp::curx
	jsr src::left
	sec		; cursor could not be moved down
	rts

@down:	jsr src::get	; get the data for this in linebuffer
	inc zp::cury	; move row down
	lda #$00
	sta zp::curx

	lda zp::cury
	cmp height
	beq @redraw
	bcc @redraw	; no need to scroll

@scroll:
	jsr scrollup_whole_screen	; cursor wasn't moved, scroll
	lda height
	sta zp::cury
@redraw:
	jsr print_line

	; if in VISUAL_LINE mode, just rvs the line and return
	lda mode
	cmp #MODE_VISUAL_LINE
	bne @movex
	jsr rvs_current_line
	RETURN_OK

@movex:	lda zp::curx
	cmp @xend
	bcs @end
	jsr src_right
	bcs @end
	jsr cur::right
	jmp @movex

@end:	; if we ended on a TAB, advance to next tab col
	jsr src::after_cursor
	cmp #$09		; did we end on a TAB?
	bne ccdown_highlight	; if not, continue
	lda mode
	cmp #MODE_INSERT
	beq ccdown_highlight
	jsr advance_tab

; fall through to ccdown_highlight
.endproc

;******************************************************************************
; CCDOWN HIGHLIGHT
; Highlights the line that the cursor is on if the editor is in VISUAL mode
; This is called after the ccdown logic
; the logic defining what to highlight is as follows:
;  if we are SELECTING (current line > visual-start-line):
;    hightlight from [0, cur-x}
;  if DESELECTING (visual-start-line > current line):
;    highlight from [cur-x, end-of-line]
;  if cur line == start line:
;    highlight from [cur-x, visual-start-x]
;
; If the editor is not in visual mode, this routine does nothing
.proc ccdown_highlight
@togglecur=r7
@tmp=r8
	lda mode
	cmp #MODE_VISUAL
	bne @done

	lda #$00
	sta @togglecur

	jsr cmp_vis_start
	beq @eq
	bcs @sel

@desel:	; highlight from [cur-x, end-of-line]
	jsr text::rendered_line_len
	ldy zp::curx
	jmp @rvs

@sel:	; highlight from [0, cur-x]
	inc @togglecur
	ldx zp::curx
	beq @toggle	; col 0: just toggle the cursor
	tay		; .Y = 0
	beq @rvs	; BRAnch always

@eq:	; highlight between cur-x and visual-start-x
	ldy zp::curx
	cpy visual_start_x
	beq @toggle
	bcc :+

	; swap .X and .Y
	ldy visual_start_x
	ldx zp::curx
	inc @togglecur
	bne @rvs

:	ldx visual_start_x
	inx

@rvs:	lda zp::cury
	jsr scr::rvsline_part
	lda @togglecur
	beq @done
@toggle:
	jsr cur::toggle
@done:	RETURN_OK
.endproc

;******************************************************************************
; CCDEL
; Handles the DEL key
.proc ccdel
	jsr src::start
	bne :+
	rts			; nothing to delete

:	lda mode
	cmp #MODE_COMMAND	; handle COMMAND mode like REPLACE
	beq @del_rep

	jsr is_readonly
	beq @del_rep		; handle DEL in r/o mode the same as REPLACE
	lda text::insertmode	; INSERT or REPLACE?
	bne backspace		; if INSERT, continue to backspace

; command or replace mode
@del_rep:
	; if we're replacing (or in r/o mode), just move left if we can
	jmp ccleft
.endproc

;******************************************************************************
; BACKSPACE
; Deletes the previous character in the source and moves the cursor as needed.
; Call text::drawline (with the newly updated cursor Y position) to render the
; result of this routine.
; OUT:
;  - mem::linebuffer: the new rendering of the line
;  - zp::curx: updated
;  - zp::cury: updated
.proc backspace
@cnt=r6
@line2len=r7
	lda #$00
	jsr src::backspace
	bcs @done
	lda #$14		; delete from the text buffer
	jsr text::putch
	bcs @prevline
	lda zp::cury
	jmp print_line

@prevline:
	; get the line we're moving up to in linebuffer
	jsr src::get

	; if the current char is a newline, we're done
	jsr src::atcursor
	cmp #$0d
	beq @scrollup

	jsr text::linelen
	stx @line2len

	; get the new cursor position (new_line_len - (old_line2_len))
	jsr src::up
	jsr src::get
	jsr text::linelen
	txa
	sec
	sbc @line2len
	sta @cnt
	beq @scrollup
	dec @cnt
	bmi @scrollup
@endofline:
	jsr ccright
	dec @cnt
	bpl @endofline
@scrollup:
	ldy zp::cury
	beq :+
	dey
:	tya
	jsr print_line		; draw the line we'll move to
	jsr text::savebuff
	jsr bumpup		; scroll the screen up (also move cursor up)
	jmp text::restorebuff

@done:	rts
.endproc

;******************************************************************************
; BUMP UP
; Bumps the screen up 1 row starting at zp::cury, erasing the contents of that
; row in the process
.proc bumpup
	ldx zp::cury
	beq @noscroll	; if cursor is at row 0, nothing to scroll

	; move the cursor
	dec zp::cury

	; scroll everything up from below the line we are bumping up to
	ldx zp::cury
	inx
	cpx height
	beq @noscroll	; if cursor is at end of screen, nothing to scroll
	lda height
	jsr scrollup

@noscroll:
	; go to the bottom row and read the line that was moved up
	jsr src::pushp		; save current source pos
	lda height
	sec
	sbc zp::cury
	tax
	ldy #$00
	jsr src::downn		; move to the line that we're bringing up
	jsr src::get
	lda height
	jsr draw_src_line	; draw the new line that was scrolled up
	jmp src::popgoto	; restore source position
.endproc

;*****************************************************************************
; SCROLLUP_WHOLE_SCREEN
; Scrolls the entire editor display (EDITOR_ROW_START to height) up
.proc scrollup_whole_screen
	ldx #EDITOR_ROW_START
	lda height

	; fall through to scrollup
.endproc

;*****************************************************************************
; SCROLLUP
; scrolls everything in the given range of rows and highlights the row that
; is scrolled in (if highlight is enabled)
; IN:
;  - .X: the row to start scrolling at
;  - .A: the row to stop scrolling at
.proc scrollup
@start=r1
@stop=r2
	stx @start
	sta @stop
	; scroll everything up from below the line we deleted
	jsr text::scrollup

	; shift colors up by 1
	ldx @start
	beq :+
	dex
:	ldy @stop
	lda #$01
	jsr draw::scrollcolorsu

	jmp highlight	; handle highlight (if enabled)
.endproc

;******************************************************************************
; COMMAND_GOTOLINE
; Converts the given string to a line number and navigates to it (if possible)
; IN:
;   - .XY: the string containing the line number to navigate to
.proc command_gotoline
@line=r2
	jsr atoi		; convert (YX) to line #
	bcs @done
	stxy @line
	jsr add_jump_point	; save the current position as a jump point
	ldx @line
	bne :+			; LSB 0?
	ldy @line+1
	bne :+			; MSB 0?
	inx			; if 0, set .XY to 1
:	jsr gotoline		; go to the target line
@done:	rts
.endproc

;******************************************************************************
; DRAW SRC LINE
; Draws the linebuffer to the given row and updates the color based on
; properties of the current source line
; IN:
;  - .A: the row to draw the text at
.proc draw_src_line
	pha		; save the row

	; if there's a breakpoint on this line, draw it
	jsr __edit_current_file
	jsr brkpt::getbyline
	bcs @nobrk

	and #BREAKPOINT_ENABLED
	bne :+
	ldy #COLOR_BRKOFF
	skw
:	ldy #COLOR_BRKON
	skw
@nobrk: ldy #COLOR_NORMAL
	pla
	pha
	tax
	tya
	jsr draw::hline

	pla		; restore the row
	jmp text::drawline
.endproc

;******************************************************************************
; COMMAND_FIND
; Gets a string from the user and searches (forward) for it in the source file
.proc command_find
@str=r0
	ldxy #@prompt_find
	jsr readinput

	; copy (.XY) to the find buffer
	stxy @str
	ldy #$00
:	lda (@str),y
	sta mem::findbuff,y
	beq @cont
	iny
	bne :-

@cont:	ldxy #mem::findbuff
	jmp __edit_find

.PUSHSEG
.RODATA
@prompt_find: .byte "find",0
.POPSEG
.endproc

;******************************************************************************
; NEXT DRIVE
.proc next_drive
	lda zp::device
	cmp #$0f
	bcs :+
	inc zp::device
:	rts
.endproc

;******************************************************************************
; PREV DRIVE
.proc prev_drive
	lda zp::device
	cmp #$09
	bcc :+
	dec zp::device
:	rts
.endproc

;*******************************************************************************
; FIND NEXT
; Navigates to the next match for the last FIND command
.proc find_next
	lda #$01	; flag search FORWRAD

	skw

	; fall through to FIND LAST SEARCH (skip find_prev)
.endproc

;*******************************************************************************
; FIND PREV
; Navigates to the previous match for the last FIND command
.proc find_prev
	lda #$00	; flag search BACKWARD

	; fall through to FIND LAST SEARCH
.endproc

;*******************************************************************************
; FIND LAST SEARCH
; Searches for the next/previous occurrence of the string in mem::findbuff
.proc find_last_search
	ldxy #mem::findbuff
	skw

	; fall through to FIND
.endproc

;*******************************************************************************
; FIND
; Searches for the text given in .YX and moves the cursor to it if it's
; found
; IN:
;  - .YX: the text to find (0-terminated)
.proc __edit_find
@string=zp::str0
@seekptr=zp::str2
@target=r8
@len=ra
@cnt=rd
@forward=re
@searchbuff=$120	; buffer of bytes to search
	lda #$01	; flag search FORWARD
	sta @forward

	stxy @string
	jsr str::len
	sta @len
	bne :+
	rts		; if 0-length string, don't search

:	jsr src::pushp	; save source position

	; set the index to begin storing characters at:
	; 0 if searching forward, MAX_SEARCH_LEN-1 if searching backward
	lda @forward
	bne :+
	lda #MAX_SEARCH_LEN-1
	skw
:	lda #$00
	sta @cnt

	jsr src::next	; start search AFTER character we're on

; fill the search buffer (MAX_SEARCH_LEN bytes)
@l0:	lda @forward
	bne :+
	jsr src::prev	; backward
	jsr src::start	; are we at start of source?
	jmp @l0next

:	jsr src::next	; forward
	jsr src::end	; are we at end of source?

@l0next:
	bne :+			; if not at start/end of source, continue
	lda #$00		; at start/end of source, terminate buffer
:	ldy @cnt
	sta @searchbuff,y	; copy char to search buffer

	lda @forward
	bne @searchfwd

@searchbwd:
	dec @cnt		; have we filled buffer?
	bpl @l0			; repeat til we have

	; seekptr = searchbuff + MAX_SEARCH_LEN - len
	lda #<(@searchbuff+MAX_SEARCH_LEN-1)
	sec
	sbc @len
	tax
	sta @seekptr
	ldy #>(@searchbuff+MAX_SEARCH_LEN-1)
	sty @seekptr+1
	bne @seekloop		; enter the main seek loop (branch always)

@searchfwd:
	inc @cnt
	cpy #MAX_SEARCH_LEN-1
	bne @l0

	; seekptr = searchbuff
	ldxy #@searchbuff
	stxy @seekptr

; see if the text we're looking for is in the buffer
@seekloop:
	jsr str::comparez
	beq @found

; if no match, shift the buffer, load a new byte, and try again
@next:	lda @forward
	bne @shiftleft

@shiftright:
	ldx #MAX_SEARCH_LEN-2
:	lda @searchbuff,x
	beq @notfound
	sta @searchbuff+1,x
	dex
	bpl :-

	; read a new byte to the left of the buffer
	lda #$00
	jsr src::start		; are we out of chars?
	beq :+			; if so, just store a 0 to the buffer
	jsr src::prev
:	sta @searchbuff
	jmp @seekloop		; not start of buff, continue (branch always)

@shiftleft:
	lda @searchbuff+1
	beq @notfound	  ; if buffer starts with 0 (EOF), we're done
	ldx #$00
:	lda @searchbuff+1,x
	sta @searchbuff,x
	inx
	cpx #MAX_SEARCH_LEN-1
	bcc :-

	; get a new byte (use 0 if EOF)
	lda #$00
	jsr src::end
	beq :+
	jsr src::next
:	sta @searchbuff+MAX_SEARCH_LEN-1
	jmp @seekloop		; not EOF, keep seeking (branch always)

@notfound:
	jsr beep::short
	jmp src::popgoto

@found:	jsr src::currline	; get the line we're moving to
	stxy @target

	lda @forward
	bne @fixforward

@fixbackward:
; for every newline in the buffer BEFORE the text we're looking for
; increment our target line
	lda #MAX_SEARCH_LEN-1
	sec
	sbc @len
	tay
:	lda @searchbuff,y
	beq :+
	cmp #$0d
	bne :+
	incw @target
:	dey
	bne :--

	; move source up to 1st matching character by advancing
	; (MAX_SEARCH_LEN-len) bytes
	lda #MAX_SEARCH_LEN-1
	sec
	sbc @len
	sta @cnt
:	ldx @cnt
	lda @searchbuff,x
	beq :+
	jsr src::next	; go up to word
:	dec @cnt
	bpl :--
	jsr src::prev	; TODO: why is this needed
	jsr src::prev
	jmp @srcfixed

@fixforward:
; for every newline in the buffer AFTER the text we're looking for
; decrement 1 from our target line
	ldy @len
	dey
@l2:	lda @searchbuff,y
	cmp #$0d
	bne :+
	decw @target
:	iny
	cpy #MAX_SEARCH_LEN
	bne @l2

	; move source back to 1st matching character by retreating
	; MAX_SEARCH_LEN bytes
	lda #MAX_SEARCH_LEN-1
	sta @cnt
:	ldx @cnt
	lda @searchbuff,x
	beq :+
	jsr src::prev	; go back MAX_SEARCH_LEN bytes (back to start of buffer)
:	dec @cnt
	bpl :--

@srcfixed:
	inc @cnt	; set @cnt back to 0

	; source cursor is now at the start of the matched word
	; find the x-offset in the line by retreating til newline
	jsr src::atcursor
	cmp #$0d
	beq @move

	; count the number of characters we are from the previous newline
:	jsr src::prev
	bcs @move
	inc @cnt
	cmp #$0d
	bne :-

	; move to the line containing the search word
@move:	jsr src::popgoto	; restore old source position
	jsr add_jump_point	; add a jump point
	ldxy @target
	jsr gotoline		; go to the new line

	; go back to the start of the line if needed
:	jsr src::atcursor
	cmp #$0d
	beq :+
	jsr src::prev
	bcc :-

:	; move the cursor to the first character of the search term
	lda @cnt		; # of chars from prev newline
	jsr text::index2cursor
	stx zp::curx

	; move source cursor to the correct offset
	lda @cnt
	beq @done
:	jsr src::next
	dec @cnt
	bne :-

:				; from next_err
@done:	rts
.endproc

;*******************************************************************************
; NEXT_ERR
; Navigates the cursor to the next error from the error log
.proc next_err
	jsr errlog::next
	beq :-			; -> RTS
	jmp gotoline
.endproc

;*******************************************************************************
; GOTOLINE
; Sets the editor to the line in .YX and refreshes the screen.
; IN:
;  - .XY: the line number to go to
.export __edit_gotoline
__edit_gotoline:
.proc gotoline
@target=r6
@diff=r6		; lines to move up or down
@seekforward=r8		; 0=backwards 1=forwards
@rowsave=rb
@cnt=rc
	; clamp target to the total # of lines
	cmpw src::lines
	bcc :+
	ldxy src::lines
:	stxy @target

	cmpw src::line	; is the target forward or backward?
	bne :+
	rts		; already on target line

:	lda #$00
	rol
	sta @seekforward
	beq @beginbackward

; get the number of lines to move forwards
@beginforward:
	lda @target
	sec
	sbc src::line
	sta @diff
	tax
	lda @target+1
	sbc src::line+1
	sta @diff+1
	beq @maybeshort
	jmp @long

@maybeshort:
	lda zp::cury
	clc
	adc @diff
	bcs @long	; if carry is set, must be long
	cmp height
	bcc @short
	jmp @long

; get the number of lines to move backwards
@beginbackward:
	lda src::line
	sec
	sbc @target
	sta @diff
	lda src::line+1
	sbc @target+1
	sta @diff+1
	bne @long

	lda zp::cury	; is (cury - diff) > 0? (is the line on screen?)
	sec
	sbc @diff
	bcc @long

@short:	lda @seekforward
	bne @shortdown

; move up and move cursor
@shortup:
	jsr ccup
	dec @diff
	bne @shortup
	rts

@shortdown:
	jsr ccdown
	dec @diff
	bne @shortdown
	rts

@long:  ; get first line of source buffer to render
	; (target +/- (EDITOR_HEIGHT - cury))
	jsr src::home

	lda @diff
	sec
	sbc height
	tax
	lda @diff+1
	sbc #$00
	tay

	bpl @movesrc
	; (diff - EDITOR_HEIGHT) < 0, need to move in the opposite direction
	stxy @diff
	lda #$00
	sec
	sbc @diff
	tax
	ldy #$00		; hi byte is 0, can't be < -EDITOR_HEIGHT
	lda @seekforward
	beq :+
	jsr src::upn		; move up before we render downward
	jmp @longf_cont
:	jsr src::downn		; move down before we we render upward
	jmp @longb_cont

; move up or down through the source to get to the start line that we'll
; redraw from
@movesrc:
	lda @seekforward
	beq @longb

@longf:	jsr src::downn	; go to the first line to render
@longf_cont:
	lda #EDITOR_ROW_START
	jmp @longmove_cont

@longb:	jsr src::upn	; go to the first line to render
@longb_cont:
	lda height

@longmove_cont:
	sta zp::cury
@l0: 	jsr src::get
	jsr print_current_line
	jsr is_visual
	bne @visdone

	; if we are on the selection that the line began at,
	; only reverse the section of the line that is highlighted
	jsr cmp_vis_start
	bne @noteq

@eq:	lda @seekforward
	beq :+
; forward: reverse from (visual_start_x, end)
	jsr text::rendered_line_len
	ldy visual_start_x
	bpl @rvspart			; branch
; backward: reverse from (0, visual_start_x+1)
:	ldy #$00
	ldx visual_start_x
	inx
@rvspart:
	lda zp::cury
	jsr scr::rvsline_part
	jmp @visdone			; continue

@noteq:	; only reverse if we are below (forward selection) or behind (backward
	; selection) the visual start-line
	lda @seekforward
	beq :+

; highlight forward
	; are we below the start line?
	bcc @visdone			; not below start line, don't highlight
	bcs @rvs0

:	; are we above the start line?
	bcs @visdone			; not above start line, don't highlight
@rvs0:	jsr rvs_current_line

@visdone:
	lda @seekforward
	bne @rowdown

@rowup: lda zp::cury
	cmp #$01
	bne :+
	sta zp::cury
	jmp ccup
:	beq @renderdone ; cmp #EDITOR_ROW_START-1; bcc @..  for non-zero starts
	dec zp::cury
	jsr src::up
	bcc @l0

	; draw the last row (src::up will return .C=1 for it)
	jsr src::get
	lda #$00
	jsr print_line
	jmp @renderdone

@rowdown:
	inc zp::cury
	jsr src::down
	bcs @clrextra
	ldx zp::cury
	cpx height
	bcc @l0

	jsr src::get
	jsr print_current_line

	jsr is_visual
	bne @renderdone

	cpx #MODE_VISUAL
	beq @vis
@visline:
	; if VISUAL LINE, reverse the entire line
	jsr text::rendered_line_len
	jmp @rvs

@vis:	; reverse the line according to the current VISUAL mode
	lda mem::linebuffer
	ldx #$00
	cmp #$09		; TAB
	bne @rvs
	jsr src::right
	ldx #TAB_WIDTH
@rvs:	ldy #$00
	lda zp::cury
	jsr scr::rvsline_part
	jmp @renderdone

; if we ran out of source but we're not at the end of the screen,
; clear whatever rows are left
@clrextra:
	jsr text::clrline
	ldx zp::cury
	stx @rowsave
@clrloop:
	txa
	jsr scr::clrline
	inc zp::cury
@clrnext:
	ldx zp::cury
	dex
	cpx height
	bcc @clrloop

	lda @rowsave
	sta zp::cury

@renderdone:
	; move the cursor to the top if we searched backwards or bottom
	; if forward
	; and move to appropriate column if we ended on a TAB
	lda mem::linebuffer
	ldx #$00
	cmp #$09		; TAB
	bne :+
	jsr src::right
	ldx #TAB_WIDTH
:	stx zp::curx
	rts
.endproc

;*******************************************************************************
; HIGHLIGHT
; If the row that should be highlighted is visible, highlight it
.proc highlight
@was_visible=r4
	lda __edit_highlight_en
	beq @done
	lda highlight_status
	sta @was_visible

	; update the status (check if the highlight was scrolled out)
	; get filename (r0 = id)
	lda src::activebuff
	lda #$00
	sta highlight_status
	ldxy __edit_highlight_line
	jsr __edit_src2screen
	bcc @ok
@done:	rts

@ok:	inc highlight_status	; flag highlight as now visible
	lda @was_visible
	bne @done		; if highlight was, and still is, visible: skip
	jmp toggle_highlight	; highlight was NOT visible, but is now
.endproc

;*******************************************************************************
; SCROLLDOWN
; scrolls everything in the given range of rows and highlights the row that
; is scrolled in (if highlight is enabled)
; IN:
;  - .A: the row to start scrolling at
;  - .X: the row to stop scrolling at
.proc scrolldown
@start=r1
@stop=r2
	stx @stop
	sta @start
	jsr text::scrolldown

	; shift colors down by 1
	ldx @start
	ldy @stop
	jsr draw::scrollcolorsd1

	ldxy __edit_highlight_line
	cmpw src::line
	bne highlight
	rts
.endproc

;*******************************************************************************
; REPORT DRIVE ERROR
; Reports the error that was last read from the drive (iec::readerr)
.proc report_drive_error
	ldxy #mem::drive_err
	jsr print_info
	jmp key::waitch
.endproc

;*******************************************************************************
; REPORT TYPEIN ERROR
; Reports just the error message for the given error.
; This is used for giving the user realtime errors as they are typing in their
; program
; IN:
;  - .A: the error code
.proc report_typein_error
	jsr err::get
	jsr text::info
	jmp beep::short
.endproc

;*******************************************************************************
; SRC2SCREEN
; Takes the given source line number and returns its row position on the screen.
; IN:
;  - .XY: the line number to get the screen row of
; OUT:
;  - .A: the row that the line number resides on
;  - .C: set if the line number is not on screen
.export __edit_src2screen
.proc __edit_src2screen
@line=zp::editortmp
@startline=zp::editortmp+2
@endline=zp::editortmp+4
	stxy @line

	lda src::line
	sec
	sbc zp::cury
	sta @startline
	lda src::line+1
	sbc #$00
	sta @startline+1

	lda @startline
	sec			; +1
	adc height
	sta @endline
	lda @startline+1
	adc #$00
	sta @endline+1

	ldxy @line
	cmpw @startline
	bcc @done
	cmpw @endline
	bcs @done

	lda @line
	sec
	sbc @startline		; will be [0, BRKVIEW_START)
	RETURN_OK

@done:	sec			; line off screen
	rts
.endproc

;*******************************************************************************
; ADD JUMP POINT
; Adds a jump point at the current source position
.proc add_jump_point
@end=r0
	ldx jumpptr
	cpx #MAX_JUMPS
	bcc @cont

	; shift all existing jumps down
	ldx #$00
:	lda jumplist_hi+1,x
	sta jumplist_hi,x
	lda jumplist_lo+1,x
	sta jumplist_lo,x
	inx
	cpx jumpptr
	bcc :-

	dec jumpptr
	ldx #MAX_JUMPS-1

; add the new jump to the end of the jumplist
@cont:	lda src::line
	sta jumplist_lo,x
	lda src::line+1
	sta jumplist_hi,x

	inc jumpptr
:	rts		; <- jumpback
.endproc

;*******************************************************************************
; JUMPBACK
; Jumps back to the last source position the user has jumped from
.export jumpback
.proc jumpback
	ldx jumpptr
	beq :-		; -> rts (jumplist is empty)
	dec jumpptr

	; use -1 offset because we loaded jumpptr before DEC'ing
	ldy jumplist_hi-1,x
	lda jumplist_lo-1,x
	tax
	jmp gotoline
.endproc

;*******************************************************************************
; GOTO INDEX 0
; Moves the cursor's x-coordinate to the index of the first character
; in the linebuffer
.proc gotoindex0
	lda #$00

	; fall through to gotoindex
.endproc

;*******************************************************************************
; GOTO INDEX
; Moves the cursor's x-coordinate to the index of the given character
; IN:
;   - .A: the index of the character in the linebuffer to move the cursor to
.proc gotoindex
	jsr text::index2cursor
	stx zp::curx
	rts
.endproc

;*******************************************************************************
; ADVANCE TAB
; Advances the cursor to the next tab stop
.proc advance_tab
	jsr text::tabr_dist
	clc
	adc zp::curx
	sta zp::curx
	dec zp::curx
	rts
.endproc

;*******************************************************************************
; DRAW ACTIVE LINE
; Draws the contents of the linebuffer at the current cury position
.proc draw_active_line
	lda zp::cury
	jmp text::drawline
.endproc

;*******************************************************************************
; IS READONLY
; Returns .Z set if the buffer should not allow edits (true if readonly has
; been explictly enabled or if we are in a VISUAL editing mode)
; OUT:
;   - .Z: set if the editor is currently in readonly mode
.proc is_readonly
	ldx readonly
	beq is_visual	; not in readonly mode, check VISUAL (treat as RO)
@ro:	ldx #$00	; set .Z
	rts
.endproc

;*******************************************************************************
; CMP VIS START
; Checks if the source cursor is on the line that the visual selection began
; on.  Assumes that we are in VISUAL/(LINE) mode
; OUT:
.proc cmp_vis_start
	jsr src::currline
	cmpw visual_start_line
	rts
.endproc

;*******************************************************************************
; IS VISUAL
; Returns .Z set if the current mode is VISUAL or VISUAL_LINE
; OUT:
;  - .X: the current editor mode
;  - .Z: set if current mode is VISUAL or VISUAL_LINE
.proc is_visual
	ldx mode
	cpx #MODE_VISUAL
	beq :+
	cpx #MODE_VISUAL_LINE
:	rts
.endproc

;*******************************************************************************
; SET_HIGHLIGHT
; Sets the line to highlight and enables line-highlight
; IN:
;  - .XY: the line to highlight
.export __edit_sethighlight
.proc __edit_sethighlight
@newhighlight=zp::editortmp+6
	lda highlight_status		; is highlight active?
	beq :+

	stxy @newhighlight
	jsr toggle_highlight		; toggle off old highlight if it's on
	ldxy @newhighlight

:	stxy __edit_highlight_line
	lda #$01
	sta __edit_highlight_en		; flag highlight as ON
	sta highlight_status		; and flag highlight as on

	; fall through to toggle_highlight
.endproc

;*******************************************************************************
; TOGGLE_HIGHLIGHT
; Unhighlights the highlighted row if it's already highlighted or highlights
; if it it isn't
.proc toggle_highlight
	lda __edit_highlight_en
	beq @done		; highlight disabled

	; get filename (r0 = id)
	lda src::activebuff
	ldxy __edit_highlight_line
	jsr __edit_src2screen
	bcs @done		; off screen
	jmp draw::rvs_underline

@done:	lda #$00
	sta highlight_status
	rts
.endproc

;*******************************************************************************
; PRINT_INFO
; Updates the status line with the given info message and refreshses the status
.proc print_info
	lda status_row
	jmp text::print
.endproc

;*******************************************************************************
; CURRENT FILE ID
; Returns the debug file ID of the active source buffer as well as the current
; line we are on in that buffer
; OUT:
;  - .A:  the debug file ID of the current file
;  - .XY: the current line in the file
;  - .C: set if there is no debug file ID for the active buffer
.export __edit_current_file
.proc __edit_current_file
	lda src::activebuff
	jsr src::filename
	bcs :+			; failed to get filename -> return
	jsr dbgi::getfileid	; .A = id of the file
	jmp src::currline
:	rts
.endproc

.RODATA

;*******************************************************************************
; TOGGLE AUTOFORMAT
; Toggles auto-formatting enable/disable
.proc toggle_autoformat
	lda #$01
	eor fmt::enable
	sta fmt::enable
	rts
.endproc

;******************************************************************************
; DIRVIEW
; Enters the directory viewer
.proc dirview
	jsr krn::clall
	jsr dir::view
	jmp krn::clall
.endproc

;*******************************************************************************
; BUFFER KEY
; Adds the given character to the input "buffer" display
; This is used for, for example, numbers, 'y', and other keys that are not
; immediately handled
.proc buffer_key
	pha
	ldx #$00
	sta mem::statusline+KEYBUFFER_COL,x
	lda status_row
	jsr text::status
	inc bufferedkeys
	pla
	rts
.endproc

;******************************************************************************
commands:
rw_commands:
	.byte $49		; I (insert start of line)
	.byte $69		; i (insert)
	.byte $72		; r (replace char)
	.byte $52		; R (replace mode)
	.byte $41		; A (append to line/insert)
	.byte $61		; a (append to character)
	.byte $43		; C (change line)
	.byte $64		; d (delete)
	.byte $44		; D (delete to end)
	.byte $70		; p (paste below)
	.byte $50		; P (paste above)
	.byte $78		; x (erase char)
	.byte $4f		; O (Open line above cursor)
	.byte $6f		; o (Open line below cursor)
	.byte $4a		; J (join line)
	.byte $3b		; ; (comment out)
	.byte $76		; v (enter visual mode)
	.byte $56		; V (enter visual line mode)
	.byte $79		; y (yank)
	.byte $73		; s (substitute char)
	.byte $53		; S (substitute line)
; commands below this work will work while in "readonly" mode (debugger)
num_rw_commands=*-rw_commands
ro_commands:
	.byte K_DIR		; - (show directory)
	.byte K_SWAP_WINS	; C= + w (swap windows)
	.byte $68		; h (left)
	.byte $6c		; l (right)
	.byte $6b		; k (up)
	.byte $6a		; j (down)
	.byte $65		; e (end of word)
	.byte $62		; b (beginning of word)
	.byte $77		; w (word advance)
	.byte $30		; 0 (column 0)
	.byte $4c		; L (last line)
	.byte $48		; H (home [first] line)
	.byte $14		; DEL (back)
	.byte $20		; SPACE (right)
	.byte $47		; G (goto end)
	.byte $67		; g (goto start)
	.byte $6e		; n (go to next search result)
	.byte $4e		; N (go to previous search result)
	.byte $24		; $ (end of line)
	.byte $5b		; [ (previous empty line)
	.byte $5d		; ] (next empty line)
	.byte $0d		; RETURN (go to start of next line)
	.byte $7a		; z (move screen prefix)
	.byte K_FIND		; / (find)
	.byte K_NEXT_DRIVE	; next drive
	.byte K_PREV_DRIVE	; prev drive
	.byte K_GETCMD		; get command
	.byte K_MONITOR		; enter console
	.byte K_NEXT_ERR	; go to next error from error log
numcommands=*-commands

; command tables for COMMAND mode key commands
.linecont +
.define cmd_vecs \
	insert_start, enter_insert, replace_char, replace, \
	append_to_line, append_char, change_line, delete, delete_to_end, \
	paste_below, paste_above, delete_char, \
	open_line_above, open_line_below, join_line, comment_out, \
	enter_visual, enter_visual_line, command_yank, sub_char, sub_line, \
	dirview, gui::reenter, ccleft, ccright, ccup, ccdown, endofword, \
	beginword, word_advance, home_col, last_line, \
	home_line, ccdel, ccright, goto_end, goto_start, find_next, find_prev, \
	end_of_line, prev_empty_line, next_empty_line, begin_next_line, \
	command_move_scr, \
	command_find, next_drive, prev_drive, get_command, monitor, next_err
.linecont -

command_vecs_lo: .lobytes cmd_vecs
command_vecs_hi: .hibytes cmd_vecs
