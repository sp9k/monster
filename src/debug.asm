;*******************************************************************************
; DEBUG.ASM
; This file contains the debugger code, which is the main loop while debugging
; and assembled program.
;*******************************************************************************

.include "asm.inc"
.include "asmflags.inc"
.include "breakpoints.inc"
.include "monitor.inc"
.include "cursor.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "file.inc"
.include "flags.inc"
.include "gui.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "memview.inc"
.include "screen.inc"
.include "settings.inc"
.include "sim6502.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "target.inc"
.include "text.inc"
.include "util.inc"
.include "watches.inc"
.include "ui.inc"
.include "vmem.inc"
.include "zeropage.inc"

.include "ram.inc"
.include "runtime.inc"

.ifdef vic20
	.include "vic20/fastcopy.inc"
.endif

.import __DEBUGGER_LOAD__
.import __DEBUGGER_SIZE__

.import stop_tracing		; flag to halt a trace command
.import PROGRAM_STACK_START	; initial stack offset for user program.

;*******************************************************************************
MAX_BREAKPOINTS = 16	; max number of breakpoints that may be set

AUX_NONE = 0		; flag for no viewer enabled
AUX_MEM  = 1		; enables the memory viewer in the debug view
AUX_GUI  = 2		; enables viewers that use the standard GUI list menu

; layout constants for the register view
REG_PC_OFFSET = 0
REG_A_OFFSET  = 5
REG_X_OFFSET  = 8
REG_Y_OFFSET  = 11
REG_SP_OFFSET = 14

;*******************************************************************************
; ACTION constants
; These tell us what command the user last executed when we return to the
; debugger via a BRK or NMI
ACTION_STEP           = 1	; action for STEP command
ACTION_STEP_OVER      = 2	; action for STEP OVER command
ACTION_GO_START       = 4	; action for first instruction of GO command
ACTION_GO             = 5	; action for subsequent GO instructions
ACTION_TRACE_START    = 6
ACTION_TRACE          = 7	; action for TRACE command
ACTION_STEP_OUT_START = 8
ACTION_STEP_OUT       = 9	; action for STEP OUT command (must be last)

;*******************************************************************************
; IFACE (interface) constants
; These are the valid values for dbg::interface. This value determines the
; interface that is entered in the debug breakpoint handler
DEBUG_IFACE_GUI  = 0	; GUI interface (returns to visual debugger)
DEBUG_IFACE_TEXT = 1	; text interface (returns to TUI)

;*******************************************************************************
debugtmp = zp::debuggertmp	; scratchpad

;******************************************************************************
.ifdef ultimem
.segment "SIMRAM_INTERNAL"
.else
.segment "VSYSRAM"
.endif
; $00-$200 is stored in the main BSS segment for faster access by the simulator
.export prog00
prog00:	.res $400	; $00-$0400

; backup for low RAM when the user's values (prog00) are swapped in

.ifdef ultimem
.segment "SIMDBG_00"
.endif
dbg00:  .res $400	; $00-$400

;*******************************************************************************
; Program state variables

.segment "SHAREBSS"

.export __debug_interface
__debug_interface: .byte DEBUG_IFACE_GUI

.BSS

;*******************************************************************************
; previous values for registers etc.
.export __debug_sw_valid
__debug_sw_valid: .byte 0	; if !0, stopwatch is valid

breakpoints_active: .byte 0	; if !0 breakpoints are installed

lineset: .byte 0		; not zero if we know the line number we're on

aux_mode: .byte 0		; the active auxiliary view

; set if interrupt was triggered by BRK
.export __debug_is_brk
__debug_is_brk: .byte 0

step_out_depth: .byte 0 ; # of RTS's to wait for when "stepping out"

.segment "SHAREBSS"
;******************************************************************************
; ENABLE EXPANSION
; This flag controls how BASIC is initialized with the NEW command. Setting it
; to 0 will initialize BASIC as if a 8+K expansion is installed.
; When this flag is !0, NEW will initialize BASIC as if no expanision is
; installed
.export __debug_enable_expansion
__debug_enable_expansion: .byte 0

.export __debug_numbreakpoints
__debug_numbreakpoints: .byte 0

;******************************************************************************
; BREAKPOINTS
.export __debug_breakpointslo
.export __debug_breakpointshi
.export __debug_breakpoint_lineslo
.export __debug_breakpoint_lineshi
.export __debug_breakpoint_fileids

;******************************************************************************
; The following tables must be stored together and
; NUM_BREAKPOINT_TABLES must be set to the number of them
; We iteratively modify the values within these tables
NUM_BREAKPOINT_TABLES=7
breakpoint_data:
__debug_breakpointslo:
breakpointslo:              .res MAX_BREAKPOINTS	; LSB's of the break points
__debug_breakpointshi:
breakpointshi:              .res MAX_BREAKPOINTS	; MSB's of the break points
__debug_breakpoint_lineslo: .res MAX_BREAKPOINTS	; breakpoint line # (LSB)
__debug_breakpoint_lineshi: .res MAX_BREAKPOINTS	; breakpoint line # (MSB)
__debug_breakpoint_fileids: .res MAX_BREAKPOINTS	; breakpoint file ID's

.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS ; active state of breakpoints
breaksave:        .res MAX_BREAKPOINTS ; backup of instructions under the BRKs

.ifdef ultimem
.segment "DEBUGGER"
.else
.CODE
.endif

;******************************************************************************
; START
; Begins debugging at the given address
; Execution will continue until a BRK instruction occurs at which point the
; debugger will take over and allow for interactive debugging from the user.
; in:
;  - .XY: the address to begin debugging at
.export __debug_start
.proc __debug_start
	lda #CUR_SELECT
	sta cur::mode

	; set the simulator's initial PC value
	stxy sim::pc

	; init state
	ldx #$00
	stx aux_mode		; initialize auxiliary views
	stx watch::num		; clear watches
	stx breakpoints_active	; flag that breakpoints are not installed
	stx __debug_interface	; set interface to GUI

	dex			; .X = $ff
	txs			; initialize stack to $1ff

	; highlight message row
	ldx #DEBUG_MESSAGE_LINE
	jsr draw::hiline

	jsr reset_stopwatch

	jsr run::init

	jsr init_breakpoints

	; initialize the user program stack
	lda #<PROGRAM_STACK_START
	sta sim::reg_sp

.ifdef hard8x8
	ldx #REGISTERS_LINE
	jsr draw::hiline
	ldx #REGISTERS_LINE+2
	jsr draw::hiline
	ldx #SCREEN_HEIGHT-1
	jsr draw::resetline
.endif

	jmp return_to_debugger		; enter the debugger
.endproc

;******************************************************************************
; INIT BREAKPOINTS
; Initializes breakpoints by looking up the address at their source position
; and mapping them to it.
.proc init_breakpoints
@cnt=zp::tmp10
	ldx __debug_numbreakpoints
	beq @done
	stx @cnt

@l0:	; get the file id and line of the breakpoint
	ldx @cnt
	lda __debug_breakpoint_fileids-1,x
	pha
	ldy __debug_breakpoint_lineshi-1,x
	lda __debug_breakpoint_lineslo-1,x
	tax
	pla
	jsr dbgi::line2addr	; get address of breakpoint

	; store the address for the breakpoint
	txa
	ldx @cnt
	sta __debug_breakpointslo-1,x
	tya
	sta __debug_breakpointshi-1,x

	dec @cnt
	bne @l0

@done:	rts
.endproc

;******************************************************************************
; INSTALL_BREAKPOINTS
; Install all breakpoints from "breakpoints" EXCEPT for those at the current
; PC. This is to prevent a loop of breakpoints being repeatedly set and
; immediately hit. For a breakpoint to be effective, it must be set at least
; one instruction from the current PC.
.proc install_breakpoints
@brkaddr=r0
@cnt=r2
	ldx __debug_numbreakpoints
	beq @done
	dex
	stx @cnt

	inc breakpoints_active

@installbrks:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda breakpointslo,x
	sta @brkaddr
	lda breakpointshi,x
	sta @brkaddr+1

	; get the opcode to save before we overwrite it with a BRK
	ldxy @brkaddr
	jsr vmem::load
	ldx @cnt
	sta breaksave,x		; save the instruction under the new BRK

	; write the BRK to its address
	ldxy @brkaddr
	lda #$00		; BRK
	jsr vmem::store

@next:	dec @cnt
	bpl @installbrks
@done:	rts
.endproc

;******************************************************************************
; UNINSTALL_BREAKPOINTS
; Restores the source code by removing all breakpoints installed by the
; debugger
.proc uninstall_breakpoints
@addr=r0
@cnt=r2
	lda #$00
	sta breakpoints_active

	ldx __debug_numbreakpoints
	beq @done
	stx @cnt

@uninstall:
	ldx @cnt
	lda breakpoint_flags-1,x
	and #BREAKPOINT_ENABLED	; if breakpoint is disabled, skip
	beq @next

	lda breakpointslo-1,x
	sta @addr
	lda breakpointshi-1,x
	sta @addr+1

	; restore the opcode we replaced with a BRK
	lda breaksave-1,x
	ldxy @addr
	jsr vmem::store
@next:	dec @cnt
	bne @uninstall
@done:	rts
.endproc

;******************************************************************************
; DEBUG REENTER
; Reenters the debugger.
; This is the entrypoint that the platform specific runtime returns to when
; execution is halted by a BRK or other interruption outside of the single-step
; or trace flows.
; It saves the user program's state and other sensitive memory areas that are
; needed by the debugger for display etc, then it restores the debugger's state
; and finally transfers control to the debugger
.export __debug_reenter
.proc __debug_reenter
	SAVE_IO

	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	; TODO: save VIA timers?
	;lda $9114
	;sta sim::via1_t1
	;lda $9114+1
	;sta sim::via1_t1+1
	;lda $9118
	;sta sim::via1_t2
	;lda $9118+1
	;sta sim::via1_t2+1
	;lda $9124
	;sta sim::via2_t1
	;lda $9124+1
	;sta sim::via2_t1+1
	;lda $9128
	;sta sim::via2_t2
	;lda $9128+1
	;sta sim::via2_t2+1

	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	pla
	sta sim::reg_y
	pla
	sta sim::reg_x
	pla
	sta sim::reg_a

	pla
	sta sim::reg_p
	and #$10		; mask BRK flag
	sta __debug_is_brk

	pla
	sta sim::pc
	pla
	sta sim::pc+1

	; check if an interrupt occurred inside the interrupt handler
	; if it did, just RTI
	cmp #$80
	bcs @enter
	cmp #$7f
	bcc @enter
	tax
	lda sim::reg_p
	pha
	lda sim::pc
	pha
	txa
	pha
	ldx sim::reg_x
	ldy sim::reg_y
	lda sim::reg_a
	rti

@enter: ; disable anything that could steal control
	TRACE_OFF

	tsx
	stx sim::reg_sp

	; clear decimal in case user set it
	cld

	lda __debug_is_brk		; was interrupt triggered a BRK?
	beq @restore_debugger		; if not, skip PC decrement

@notnmi:
	decw sim::pc	; BRK pushes PC + 2, subtract 2 from PC
	decw sim::pc

@restore_debugger:
	sei

	; reinit the debugger's SP
	ldx #$ff
	txs

	; save the user's zeropage and restore the debugger's
	ldxy #@save_done	; need to pass return address
	jmp __debug_save_user_zp

@save_done:
	ldxy #@restore_debug_done
	jmp __debug_restore_debug_low

@restore_debug_done:
	jsr __debug_restore_debug_zp

	; save program state and swap the debugger state in
	jsr __debug_swap_out
        jsr irq::on		; reinstall the main IRQ

	lda breakpoints_active
	beq return_to_debugger
	jsr uninstall_breakpoints

	; fall through to return_to_debugger
.endproc

;******************************************************************************
; RETURN TO DEBUGGER
; Entrypoint to the debugger
.export return_to_debugger
.proc return_to_debugger
; we're done updating state, check which interface we should transfer
; execution to
@enter_iface:
	lda #$00
	sta lineset		; flag that line # is (yet) unknown
	lda __debug_interface
	beq @iface_gui		; if interface is GUI, continue

@debugloop_tui:
	CALL FINAL_BANK_MONITOR, mon::reenter	; re-enter monitor (get input)
	jmp @debugloop_tui

@iface_gui:
	jsr show_aux		; display the auxiliary mode

@showbrk:
	; set color for the message row
	ldx #DEBUG_MESSAGE_LINE
	jsr draw::hiline

	; get the address before the BRK and go to it
	jsr cur::off
	ldxy sim::pc
	jsr __debug_gotoaddr
	bcs @print		; if we failed to get line #, continue
	jsr edit::sethighlight
	inc lineset

@print:	jsr showstate		; show regs/BRK message
	jsr cur::on

; main (graphical) debug loop
@debugloop:
	cli
	lda __debug_interface
	bne @debugloop_tui

@debugloop_gui:
	jsr text::update
	jsr key::waitch

	pha
	jsr cur::off
	pla

	ldx zp::editor_mode
	cpx #MODE_INSERT
	beq @nocmd		; in INSERT mode, propagate all keys to editor

 ; check if the key has a debugger command associated with it
	ldx #num_commands-1
@getcmd:
	cmp commands,x
	beq @runcmd
	dex
	bpl @getcmd

; check if the key should be ignored (not sent to be handled by the editor)
	ldx #num_disabled_commands-1
@chkdisabled:
	cmp disabled_commands,x
	beq @debugloop		; if key is marked as disabled, ignore it
	dex
	bpl @chkdisabled

; propagate the key to the editor
@nocmd:	jsr edit::handlekey
	jmp @debugloop

@runcmd:
	lda #$4c		; JMP
	sta zp::jmpaddr
	lda command_vectorslo,x	; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x	; vector MSB
	sta zp::jmpvec+1

	jsr zp::jmpaddr		; call the command
	jmp @enter_iface
.endproc

;******************************************************************************
; LOAD_FILE
; Loads the file (in the editor) for the given filename
; IN:
;  - .A: the file (as returned from dbgi::addr2line)
; OUT:
;  - .C: set if the file couldn't be opened
.export __debug_load_file
.proc __debug_load_file
	jsr dbgi::get_filename
	jmp edit::load
.endproc

;******************************************************************************
; GOTO PC
; Navigates the editor to the line that corresponds to the address where the
; debugger is currently at in the user program.
.proc goto_pc
.endproc
	ldxy sim::pc

	; fall through to __debug_gotoaddr

;******************************************************************************
; GOTOADDR
; Navigates the editor to the file/line associated with the give address
; IN:
;  - .XY: the address to "goto"
; OUT:
;  - .C:  set on failure (no line/file could be matched with given address)
;  - .XY: the line that was navigated to
.export __debug_gotoaddr
.proc __debug_gotoaddr
@line=debugtmp+2
	jsr dbgi::addr2line	; get the line #
	bcs @done		; error
	sta dbgi::file
	stxy @line
	jsr __debug_load_file	; load file (if not already)
	bcs @done		; error

	ldxy @line
	jsr edit::gotoline
	ldxy @line
	clc			; ok
@done:
:	rts			; <- __debug_go
.endproc

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.export __debug_go
.proc __debug_go
	; run one step to get over breakpoint (if we're on one)
	jsr step
	bcs :-		; failed to execute next instruction -> rts

	jsr install_breakpoints

	lda #$00
	sta __debug_sw_valid
	jmp run::go
.endproc

;******************************************************************************
; JUMP
; Runs the user program at the line the cursor is currently on
.proc jump
	ldxy sim::pc
	jsr edit::currentfile	; get current line # (.XY) and file ID (.A)
	jsr dbgi::line2addr
	bcs @done		; couldn't resolve address
	stxy sim::pc
	jsr __debug_gotoaddr
	jmp edit::sethighlight
@done:	rts
.endproc

;******************************************************************************
; STEP OUT
; Runs the user program until the next RTS is executed
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_step_out
.proc __debug_step_out
	jsr irq::off

	lda #$00
	sta step_out_depth
	jsr bsp::install_tracer

	jsr print_tracing
@trace: lda stop_tracing
	bne @done
	jsr __debug_step	; run one STEP
	bcs @done		; stop tracing if STEP OUT says we should

	TRACE_ON		; enable tracing to catch user interrupt

	lda sim::op		; get opcode we just ran
	cmp #$20		; did we run a JSR?
	bne :+
	inc step_out_depth	; if we called another subroutine, inc depth
:	cmp #$60		; did we RTS?
	beq :+
	cmp #$40		; or RTI?
	bne @trace		; if not loop
:	dec step_out_depth
	bpl @trace		; continue trace until depth is negative
	clc			; ok

@done:	php
	jsr irq::on
	plp
	rts
.endproc

;******************************************************************************
; TRACE
; Puts the debugger into TRACE mode and steps to the next instruction to
; begin the trace
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_trace
.proc __debug_trace
	jsr irq::off

	jsr bsp::install_tracer

	; run one step (get over breakpoint if there is one)
	jsr __debug_step
	bcs @ret

	; install the breakpoints
	jsr install_breakpoints

	jsr print_tracing

@trace: ; step until a watch/breakpoint triggers or the user interrupts the
	; trace
	lda stop_tracing	; check if user interrupted trace
	bne @done

	jsr __debug_step	; run the next STEP
	bcs @done

	TRACE_ON		; reinstall user interrupt

	lda sim::at_brk		; check if at a BRKpoint
	beq @trace		; if not, continue tracing

@done:  jsr uninstall_breakpoints
@ret:	php
	jsr irq::on
	plp
	rts
.endproc

;******************************************************************************
; QUIT
; Prompts the user for confirmation to quit then exits the debugger upon
; receiving it
.proc quit
	ldxy #strings::debug_stop_debugging
	lda #DEBUG_MESSAGE_LINE
	jsr text::print

:	jsr key::waitch

	; 'N' or QUIT: return back to debugger, 'Y': exit debugger
	cmp #$4e		; N
	beq @done
	cmp #K_QUIT
	beq @done		; don't quit
	cmp #$59		; Y
	bne :-

@quit:	lda #$00		; clear BRK flag
	sta edit::highlight_en	; disable highlighting
	pha			; push 0 status
	plp			; clear flags (.P)

	jsr scr::clrcolor

	jmp edit::init
@done:	rts
.endproc

;******************************************************************************
; EDIT SOURCE
; Disable all views and reenable (almost) fullscreen editing
.proc edit_source
	lda #$00
	sta aux_mode
	lda #DEBUG_MESSAGE_LINE-1
	jmp edit::resize
.endproc

;******************************************************************************
; EDIT MEM
; Transfers control to the memory viewer/editor until the user exits it
.proc edit_mem
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize

	pushcur
	lda #(DEBUG_INFO_START_ROW)
	jsr scr::clrpart
	jsr showstate		; restore the state

	lda #AUX_MEM
	sta aux_mode

	jsr view::edit
	popcur
	rts
.endproc

;******************************************************************************
; EDIT BREAKPOINTS
; Transfers control to the breakpoint viewer/editor until the user exits it
.proc edit_breakpoints
	jsr showstate		; restore the state

	lda #AUX_GUI
	sta aux_mode

	jmp brkpt::edit
.endproc

;******************************************************************************
; EDIT WATCHES
; Transfers control to the watch viewer/editor until the user exits it
.export __debug_edit_watches
.proc __debug_edit_watches
	jsr showstate		; restore the state

	lda #AUX_GUI
	sta aux_mode
	jmp watch::edit
.endproc

;******************************************************************************
; SWAP USER MEM
; Command that swaps in the user program memory, waits for a keypress, and
; returns with the debugger's memory swapped back in
.export __debug_swap_user_mem
.proc __debug_swap_user_mem
	; disable coloring in the IRQ
	jsr draw::coloroff

	; bring in the visible state from the user program
	jsr bsp::save_debug_visual
	jsr bsp::restore_prog_visual

	; wait for a key to swap the state back
	jsr key::waitch

	; reenable coloring
	inc mem::coloron

	; restore debugger state
	jsr bsp::restore_debug_visual
	jmp irq::on
.endproc

;******************************************************************************
; PRINT TRACING
; Prints the "tracing" info
.proc print_tracing
	ldxy #strings::tracing
	lda __debug_interface
	beq @gui
	JUMP FINAL_BANK_MONITOR, mon::puts

@gui:	lda #REGISTERS_LINE-1
	jsr text::print			; print the TRACING message
	rts
.endproc

;******************************************************************************
; STEP OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; Unlike STEP, if the next instruction is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.export __debug_step_over
.proc __debug_step_over
	jsr irq::off
	jsr bsp::install_tracer

	jsr __debug_step	; run one STEP
	bcs @done		; stop tracing if STEP errored
	lda sim::op		; get opcode we just ran
	cmp #$20		; did we run a JSR?
	bne @done		; if not, we're done
	jsr __debug_step_out	; if we did enter a subroutine, STEP OUT
@done:	php
	jsr irq::on
	plp
	rts
.endproc

;******************************************************************************
; STEP
; Runs the step command
.proc step
	jsr irq::off

	TRACE_OFF

	jsr __debug_step

	php
	jsr irq::on
	plp
	rts
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_step
.proc __debug_step
@cnt=r0
	lda #$00
	sta sim::illegal

	ldxy sim::pc		; get address of next instruction
	lda #$01		; don't disassemble to string
	jsr asm::disassemble	; disassemble it to get its size (BRK offset)
	stx sim::op_mode
	bcc @ok
	inc sim::illegal	; flag that we couldn't disassemble the instruction

@ok:	pha				; save instruction size
	ldxy sim::pc			; address of instruction
	jsr sim::get_side_effects	; get state that will be clobbered/used

;---------------------------------------
; update watch values and mark them as no longer dirty
	ldx watch::num
	beq @step

	stx @cnt
@l0:	lda watch::flags-1,x
	and #WATCH_DIRTY
	beq :+

	; watch was dirty, store its value as the previous one
	stx @cnt
	lda watch::los-1,x
	ldy watch::his-1,x
	tax
	jsr vmem::load
	ldx @cnt
	sta watch::prevs-1,x

:	lda #$ff^WATCH_DIRTY
	and watch::flags-1,x
	sta watch::flags-1,x
	dex
	bne @l0

;---------------------------------------
; perform the step
@step:	pla			; get instruction size
	ldxy sim::pc		; and address of instruction to-be-executed

	jsr sim::step		; execute the STEP
	bcc @countcycles	; if ok, continue

	; display the error explaining why we couldn't STEP
	jsr safety_check
	sec			; flag that traces should stop
	rts

@countcycles:
	; count the number of cycles that the next instruction will take
	jsr sim::count_cycles	; get the # of cycles for the instruction
	clc
	adc sim::stopwatch
	sta sim::stopwatch
	bcc @check_watches
	inc sim::stopwatch+1
	bne @check_watches
	inc sim::stopwatch+2

@check_watches:
	lda sim::affected
	and #OP_LOAD|OP_STORE		; was there a memory read/write?
	beq @done			; if not, skip watches check
	ldxy sim::effective_addr	; if yes, mark the watch if there is one
	jsr watch::mark			; check if there's a watch at this addr
	bcc @done			; if there's no watch at addr, done

	; activate the watch window so user sees change
	jsr watch_triggered	; display a message indicating watch triggered
	sec
	rts

@done:	RETURN_OK		; return to the debugger
.endproc

;*******************************************************************************
; SAFETY CHECK
; Checks if the CPU will be jammed after executing the next instruction or if
; a critical memory location will be clobbered.
.proc safety_check
	; is the next instruction a JAM?
	lda sim::jammed
	bne jam_detected

	; does the next instruction write to memory?
	lda sim::vital_addr_clobbered
	beq @ok

	; an important memory location will be clobbered, print error
	lda sim::effective_addr
	pha
	lda sim::effective_addr+1
	pha
	ldx #<strings::vital_addr_clobber
	ldy #>strings::vital_addr_clobber
	bne print_msg

@ok:	RETURN_OK
.endproc

;*******************************************************************************
; JAM DETECTED
; This procedure is called when STEP (via step, trace, etc.) encounters a JAM
; instruction
.proc jam_detected
	ldx #<strings::jam_detected
	ldy #>strings::jam_detected
	bne print_msg
.endproc

;*******************************************************************************
; WATCH TRIGGERED
; This procedure is called when STEP (via step, trace, etc.) reads/writes to a
; memory location that is being watched
.proc watch_triggered
	; display the watch that was triggered
	jsr ui::render_watch

	; fall through to print_msg
.endproc

;*******************************************************************************
; PRINT MSG
; Prints a message for the TUI or GUI (whichever is active)
; IN:
;   - .XY: the message to print
.proc print_msg
	lda __debug_interface
	beq @gui

@tui:	lda #REGISTERS_LINE-1
	CALL FINAL_BANK_MONITOR, mon::puts
	RETURN_OK

@gui:	lda #REGISTERS_LINE-1
	jsr text::print
	jsr scr::clrcolor
	jsr irq::on
	jsr key::waitch		; wait for keypress
	sec
	rts
.endproc

;*******************************************************************************
; ILLEGAL DETECTED
; Prints a message that an illegal opcode was detected and waits for
; user input
.proc illegal_detected
	; illegal instruction detected, give a warning
	ldx #<strings::illegal_detected
	ldy #>strings::illegal_detected
	bne print_msg			 ; branch always
.endproc

;*******************************************************************************
; SWAPIN
; This routine is called _before_ returning from the BRK interrupt and executing
; the user program.
.export __debug_swap_in
.proc __debug_swap_in
	; swap entire user RAM in (needed if we don't know what memory will
	; be changed before next BRK)
	jsr bsp::save_debug_state
	jmp bsp::restore_prog_state
.endproc

;******************************************************************************
; SWAPOUT
; Swaps *out* the user memory that needs to be saved in order to restore the
; debug state.
.export __debug_swap_out
.proc __debug_swap_out
	; save the program state before we restore the debugger's
	; NOTE: c64 already saves program state (necessarily)
.ifndef c64
	jsr bsp::save_prog_state
	jmp bsp::restore_debug_state		; restore debugger state
.else
	jsr bsp::save_prog_visual
	jmp bsp::restore_debug_visual
.endif
.endproc

;*******************************************************************************
; SETBRKATLINE
; Sets a breakpoint at the given line.  During assembly the address will be
; populated.
; IN:
;  - .XY: the line number to set the breakpoint at
;  - .A:  the file ID to set the breakpoint in
; OUT:
;  - .A: the ID of the breakpoint that was added
.export __debug_setbrkatline
.proc __debug_setbrkatline
	pha				; save file ID

	; store the line #
	txa
	ldx __debug_numbreakpoints
	sta __debug_breakpoint_lineslo,x
	tya
	sta __debug_breakpoint_lineshi,x

	pla				; restore file ID
	sta __debug_breakpoint_fileids,x
	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x

	lda __debug_numbreakpoints
	inc __debug_numbreakpoints
	rts
.endproc

;*******************************************************************************
; SETBRKATADDR
; Sets a breakpoint at the given address.
; This can be used while debugging to set breakpoints at addresses directly,
; even if they don't correspond to a line or file
; IN:
;  - .XY: the address to set the breakpoint at
.export __debug_setbrkataddr
.proc __debug_setbrkataddr
	txa
	ldx __debug_numbreakpoints
	sta breakpointslo,x
	tya
	sta breakpointshi,x

	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x

	; set file ID to $ff
	lda #$ff
	sta __debug_breakpoint_fileids,x

	inc __debug_numbreakpoints
	rts
.endproc

;*******************************************************************************
; BRKSETADDR
; Sets the address for the given (existing) breakpoint.
; If no matching breakpoint is found, does nothing
; IN:
;   - .XY: the line # of the breakpoint to set the address for
;   - .A:  the file ID for the breakpoint to set
;   - r0:  the address to store for the breakpoint
; OUT:
;   - .C: set if there is no breakpoint at the given line/file
.export __debug_brksetaddr
.proc __debug_brksetaddr
@addr=r0
	jsr brkpt::getbyline	; get breakpoint # in .X
	bcs @done		; no match
@found:	; store address
	lda @addr
	sta breakpointslo,x
	lda @addr+1
	sta breakpointshi,x
@done:	rts
.endproc

;*******************************************************************************
; REMOVEBREAKPOINT
; Removes a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to remove
; OUT:
;  - .C: set if breakpoint doesn't exist
.export __debug_remove_breakpoint
__debug_remove_breakpoint:
.proc remove_breakpoint
	jsr get_breakpoint
	bcs :+			; no breakpoint to remove -> rts

	tax
	; fall through to removebreakpointbyid
.endproc

;*******************************************************************************
; REMOVEBREAKPOINTBYID
; Removes the ID with the given handle.
; IN:
;  - .X: the breakpoint to remove
; OUT:
;  - .C: set if no breakpoint was removed
.export __debug_removebreakpointbyid
.proc __debug_removebreakpointbyid
@addr=debugtmp
@data=debugtmp+2
@id=debugtmp+4
	cpx __debug_numbreakpoints
	bcs :+				; no breakpoint of given ID

	stx @id
	ldxy #breakpoint_data
	stxy @data

	ldx #NUM_BREAKPOINT_TABLES

	; shift breakpoints down
@l0:	ldy @id
	iny
@l1:	lda (@data),y
	dey
	sta (@data),y
	iny
	iny
	cpy __debug_numbreakpoints
	bcc @l1

	lda @data
	; sec
	adc #MAX_BREAKPOINTS-1
	sta @data
	bcc @next
	inc @data+1

@next:	dex
	bne @l0
	dec __debug_numbreakpoints
@done:	clc
:	rts
.endproc

;*******************************************************************************
; SHIFT BREAKPOINTS D
; Shifts the line numbers for all breakpoints on lines below the current one
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift
.export __debug_shift_breakpointsd
.proc __debug_shift_breakpointsd
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx __debug_numbreakpoints
	beq @done
	dex

@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcc @next
	lda __debug_breakpoint_lineslo,x
	adc #$00
	cmp @line
	bcc @next
	sbc #$01
	clc
	adc @offset
	sta __debug_breakpoint_lineslo,x
	bcc @next
	inc __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; SHIFT BREAKPOINTS U
; Shifts UP the line numbers for all breakpoints on lines below the current one
; given by the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift within
.export __debug_shift_breakpointsu
.proc __debug_shift_breakpointsu
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx __debug_numbreakpoints
	beq @done
	dex

@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcc @next
	lda __debug_breakpoint_lineslo,x
	cmp @line
	beq @next
	bcc @next
	sec
	sbc @offset
	sta __debug_breakpoint_lineslo,x
	bcs @next
	dec __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; GET BREAKPOINT
; Returns the ID (index) of the breakpoint corresponding to the given address
; IN:
;  - .XY: the address of the breakpoint to get the ID of
; OUT:
;  - .C: set if no breakpoint is found
;  - .A: the id of the breakpoint
.proc get_breakpoint
@addr=r0
	stxy @addr
	ldx __debug_numbreakpoints
	beq @notfound
@l0:	lda @addr
	cmp breakpointslo,x
	bne @next
	lda @addr+1
	cmp breakpointshi,x
	beq @found
@next:	dex
	bpl @l0

@notfound:
	ldxy @addr
	sec
	rts		; not found

@found: txa
	RETURN_OK
.endproc

;*******************************************************************************
; EDIT STATE
; Moves the cursor to the registers area and allows the user to modify
; their values with hex input
.proc edit_state
	pushcur
	lda #$00
	sta zp::curx
	lda #REGISTERS_LINE+1
	sta zp::cury

	jsr showstate		; fill linebuffer with register state

@edit:	jsr cur::on
	jsr key::waitch
	pha
	jsr cur::off
	pla

	jsr key::isleft
	beq @back
	cmp #K_DEL
	bne @ret
@back:	lda zp::curx
	beq @edit		; can't move LEFT
	dec zp::curx
	ldx #@numoffsets-1
@prevx:	cmp @offsets,x		; was cursor at start of offset?
	bcc @prev		; if cursor is < offset, check prev offset
	bne @ok			; if it was NOT at offset start, it is now
	lda @offsets-1,x
	adc #$00		; +1
	sta zp::curx
	jmp @refresh
@prev:	dex
	bpl @prevx
	bmi @edit

@ret:	cmp #K_RETURN
	beq @updatevals
@quit:	cmp #K_QUIT
	beq @exit
@right:	jsr key::isright
	beq @fwd

@hex:	jsr key::ishex
	bcc @refresh

	ldx zp::curx
	sta mem::linebuffer2,x

@fwd:   lda zp::curx
	cmp #REG_SP_OFFSET+1	; check offset
	bcs @refresh		; if already at last column, don't advance
	inc zp::curx		; bump up curx
	ldx #$00
; align x-position to either a value in @offsets or a value in @offests+1
@nextx:	cmp @offsets,x		; was X at the start of the offset?
	beq @refresh		; if so, incrementing it by 1 was sufficient
	bcs @next		; if X
@ok:	lda @offsets,x		; if @offsets,x <= curx, set curx to it
	sta zp::curx
	jmp @refresh
@next:	inx
	cpx #@numoffsets
	bne @nextx

@refresh:
	lda #REGISTERS_LINE+1
	ldxy #mem::linebuffer2
	jsr text::print
	jmp @edit

;--------------------------------------
; parse the linebuffer2 and update all registers
@updatevals:
@val=r0
	ldxy #mem::linebuffer2
	stxy @val

	ldx #@numoffsets-1
@l0:	ldy @offsets,x
	lda (@val),y
	jsr util::chtohex	; get MSB
	asl
	asl
	asl
	asl
	sta sim::register_state,x

	iny
	lda (@val),y		; get LSB
	jsr util::chtohex
	ora sim::register_state,x
	sta sim::register_state,x

	dex
	bpl @l0

	; swap PC LSB and MSB
	ldx sim::pc
	lda sim::pc+1
	sta sim::pc
	stx sim::pc+1

@exit:	popcur
	rts

;--------------------------------------
.PUSHSEG
.RODATA
@offsets:
.byte REG_PC_OFFSET, REG_PC_OFFSET+2, REG_A_OFFSET, REG_X_OFFSET, REG_Y_OFFSET
.byte REG_SP_OFFSET
@numoffsets=*-@offsets
.POPSEG
.endproc

;******************************************************************************
; SHOWSTATE
; Shows the current debug state (registers and BRK line)
.proc showstate
	jsr showbrk

	; fall through to showregs
.endproc

;*******************************************************************************
; SHOWREGS
; prints the contents of the registers in the format
;   PC  A  X  Y  SP NV-BDIZC ADDR
;  f59c 02 02 00 f7 00100000 1003
.proc showregs
	; display the register names
	ldxy #strings::debug_registers
	lda #REGISTERS_LINE
	jsr text::print

	jsr ui::regs_contents

.ifndef hard8x8
	lda #REGISTERS_LINE+1
	jmp text::print
.else
	lda #$00
	sta mem::linebuffer2+16	; break the register contents line
	lda #REGISTERS_LINE+1
	jsr text::print

	ldxy #strings::debug_registers2
	lda #REGISTERS_LINE+2
	jsr text::print

	jsr ui::regs_contents
	ldxy #mem::linebuffer2+17
	lda #REGISTERS_LINE+3
	jmp text::print
.endif
.endproc

;*******************************************************************************
; SHOWBRK
; Display the BRK line number or address
.proc showbrk
	lda lineset		; is the line # known?
	bne @showline		; if so, show it

@showaddr:
	; push the address we will disassemble into
	lda #$01
	sta r0+1
	pha

	lda #$00
	sta r0
	pha

	; we couldn't find the line #; display 6ke address of the BRK

	ldxy sim::pc
	lda #$00		; disassemble to string
	jsr asm::disassemble
	bcc :+

	; couldn't disassemble (illegal opcode?), just get the hex value for
	; the byte
	ldxy sim::pc
	jsr vmem::load
	jsr util::hextostr
	sty $100
	stx $101
	lda #$00
	sta $102

:	lda sim::pc
	pha
	lda sim::pc+1
	pha
	ldxy #strings::debug_brk_addr
	jmp @print

@showline:
	; display the BRK message
	lda edit::highlight_line	; push line #
	pha
	lda edit::highlight_line+1
	pha

	lda dbgi::file
	jsr dbgi::get_filename
	tya
	pha
	txa
	pha

	ldxy #strings::debug_brk_line
@print:	lda #DEBUG_MESSAGE_LINE
	jsr text::print		; break in line <line #>
:	rts
.endproc

;*******************************************************************************
; SHOW AUX
; Displays the memory viewer, breakpoint viewer, or watchpoint viewer depending
; on which is enabled
.proc show_aux
	lda aux_mode
	beq :-			; no aux mode -> RTS
	cmp #AUX_MEM
	bne @gui
@mem:	jmp view::mem		; refresh the memory viewer
@gui:	jmp gui::refresh	; refresh the active GUI
.endproc

;*******************************************************************************
; RESET STOPWATCH
; Resets the stopwatch
.proc reset_stopwatch
	ldx #$03		; 3 bytes (24-bit stopwatch)
	stx __debug_sw_valid	; stopwatch is valid
	lda #$00
:	sta sim::stopwatch-1,x
	dex
	bne :-
	rts
.endproc

;*******************************************************************************
; ACTIVATE MONITOR
; Activates the text user interface debugger (monitor)
.proc activate_monitor
	jsr scr::save
	jsr bsp::save_debug_state
	jmp edit::entermonitor
.endproc

;******************************************************************************
; SAVE USER ZP
; Saves the state of the user's zeropage
; IN:
;  - .XY: the resturn address (the stack s clobbered by this procedure)
.export __debug_save_user_zp
.proc __debug_save_user_zp
@ret=mem::sparevec
	stxy @ret

.ifdef ultimem
@BLK5_OFFSET=$a000-$2000
	; bank in the area containing prog00
	lda #SIMRAM_00_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2

	ldx #$00
:	lda $00,x
	sta prog00+@BLK5_OFFSET,x
	lda $100,x
	sta prog00+$100+@BLK5_OFFSET,x
	lda $200,x
	sta prog00+$200+@BLK5_OFFSET,x
	lda $300,x
	sta prog00+$300+@BLK5_OFFSET,x
	dex
	bne :-
	beq reset_blk5_and_ret

.else
	ldx #$00
:	lda $00,x
	sta prog00,x
	lda $100,x
	sta prog00+$100,x
	lda $200,x
	sta prog00+$200,x
	lda $300,x
	sta prog00+$300,x
	dex
	bne :-
	jmp (@ret)
.endif
.endproc

.ifdef ultimem
;******************************************************************************
; RESET BLK5 AND RET (ULTIMEM ONLY)
; Resets the BLK5 configuration to its default
.proc reset_blk5_and_ret
@ret=mem::sparevec
	; reset BLK5 RAM area
	lda #$04
	sta $9ffe
	lda #$55
	sta $9ff2
	jmp (@ret)
.endproc
.endif

;******************************************************************************
; RESTORE USER ZP
; Restores the state of the user's zeropage
; IN:
;  - .XY: the resturn address (the stack s clobbered by this procedure)
.export __debug_restore_user_zp
.proc __debug_restore_user_zp
@ret=mem::sparevec
	stxy @ret

.ifdef ultimem
@BLK5_OFFSET=$a000-$2000
	; bank in the area containing prog00
	lda #SIMRAM_00_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2

	ldx #$00
:	lda prog00+@BLK5_OFFSET,x
	sta $00,x
	lda prog00+$100+@BLK5_OFFSET,x
	sta $100,x
	lda prog00+$200+@BLK5_OFFSET,x
	sta $200,x
	lda prog00+$300+@BLK5_OFFSET,x
	sta $300,x
	dex
	bne :-

	; reset BLK5 RAM area
	beq reset_blk5_and_ret

.else
	ldx #$00
:	lda prog00,x
	sta $00,x
	lda prog00+$100,x
	sta $100,x
	lda prog00+$200,x
	sta $200,x
	lda prog00+$300,x
	sta $300,x
	dex
	bne :-
	jmp (@ret)
.endif
.endproc

;*****************************************************************************
; RESTORE DEBUG ZP
; Restores the $00-$100 values forthe debugger
.export __debug_restore_debug_zp
.proc __debug_restore_debug_zp
.ifdef ultimem
	; bank in the area containing dbg00
	lda #SIMDBG_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2
.endif
	ldx #$00
:	lda dbg00,x
	sta $00,x
	dex
	bne :-

.ifdef ultimem
	; reset BLK5 RAM area
	lda #$04
	sta $9ffe
	lda #$55
	sta $9ff2
.endif
	rts
.endproc

;******************************************************************************
; RESTORE DEBUG LOW
; Restores the state of the debugger's "low" memory
; IN:
;   - .XY: the return address (stack can't be used)
.export __debug_restore_debug_low
.proc __debug_restore_debug_low
@ret=mem::sparevec
.ifdef ultimem
	; bank in the area containing prog00
	lda #SIMDBG_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2
.endif
	stxy @ret

	ldx #$00
:	lda dbg00+$100,x
	sta $100,x
	lda dbg00+$200,x
	sta $200,x
	dex
	bne :-

	; copy around the NMI vector
	ldx #$100-$1a-1
:	lda dbg00+$31a,x
	sta $31a,x
	dex
	bne :-

	ldx #$18-1
:	lda dbg00+$300,x
	sta $300,x
	dex
	bpl :-
.ifdef ultimem
	jmp reset_blk5_and_ret
.else
	jmp (@ret)
.endif

.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
; IN:
;   - .XY: the return address (stack can't be used)
.export __debug_save_debug_zp
.proc __debug_save_debug_zp
@zp=dbg00
@ret=mem::sparevec
.ifdef ultimem
	; bank in the area containing prog00
	lda #SIMDBG_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2
.endif

	stxy @ret
	ldx #$00
@l0:	lda $00,x
	sta @zp,x
	lda $100,x
	sta @zp+$100,x
	lda $200,x
	sta @zp+$200,x
	lda $300,x
	sta @zp+$300,x
	dex
	bne @l0

.ifdef ultimem
	; reset BLK5 RAM area
	lda #$04
	sta $9ffe
	lda #$55
	sta $9ff2
.endif
	jmp (@ret)
.endproc

.RODATA
;*******************************************************************************
; COMMANDS
; This table contains the keys used to invoke the corresponding command
; within the debugger
commands:
	.byte K_QUIT_DEBUGGER
	.byte K_QUIT
	.byte K_STEP
	.byte K_STEPOVER
	.byte K_GO
	.byte K_JUMP
	.byte K_STEPOUT
	.byte K_TRACE
	.byte K_SRCVIEW
	.byte K_MEMVIEW
	.byte K_BRKVIEW
	.byte K_WATCHVIEW
	.byte K_SWAP_USERMEM
	.byte K_RESET_STOPWATCH
	.byte K_EDIT_STATE
	.byte K_GOTO_BREAK
	.byte K_MONITOR
num_commands=*-commands

.linecont +
.define command_vectors quit, edit_source, step, __debug_step_over, \
	__debug_go, jump, __debug_step_out, __debug_trace, edit_source, \
	edit_mem, edit_breakpoints, __debug_edit_watches, \
	__debug_swap_user_mem, reset_stopwatch, edit_state, \
	goto_pc, activate_monitor
.linecont -
command_vectorslo: .lobytes command_vectors
command_vectorshi: .hibytes command_vectors

;*******************************************************************************
; DISABLED COMMANDS
; the following commands are NOT propagated to the editor. they become a nop
; when handled by the debugger
disabled_commands:
	.byte K_CLOSE_BUFF
	.byte K_ASM
	.byte K_ASM_DEBUG
	.byte K_REFRESH
	.byte $76		; v (enter visual)
	.byte $56		; V (enter visual line)
num_disabled_commands=*-disabled_commands
