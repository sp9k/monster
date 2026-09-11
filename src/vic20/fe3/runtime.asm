;*******************************************************************************
; RUNTIME.ASM
; This file contains platforms specific helpers for managing the execution of
; a user's programs
;*******************************************************************************

.include "../debug.inc"
.include "../expansion.inc"
.include "../fastcopy.inc"
.include "../nmi.inc"
.include "../prefs.inc"
.include "../settings.inc"
.include "../vaddrs.inc"
.include "../../asm.inc"
.include "../../debug.inc"
.include "../../edit.inc"
.include "../../guis.inc"
.include "../../irq.inc"
.include "../../macros.inc"
.include "../../monitor.inc"
.include "../../ram.inc"
.include "../../screen.inc"
.include "../../sim6502.inc"
.include "../../text.inc"
.include "../../vmem.inc"
.include "../../zeropage.inc"

.import return_to_debugger

.import __INTS_RUN__
.import __INTS_LOAD__
.import __INTS_SIZE__
.import __INTS_MAIN_LOAD__, __INTS_MAIN_RUN__, __INTS_MAIN_SIZE__

.import PROGRAM_STACK_START

;*******************************************************************************
; BRK/NMI HANDLER ADDRESSES
; address in user program where the BRK handler will reside
; NOTE: the user program cannot use the space occupied by these handlers
;
; Only USER has the native NMI/BRK entry at $7fe0. The first instruction
; after STA $9c02 lives at the matching address in MAIN. No other bank
; needs this handler; tracing and command cancellation use shared NMIs.
INTS_DELTA = __INTS_LOAD__-__INTS_RUN__

.export TRAMPOLINE_ADDR

.segment "SHAREBSS"
save_sp: .byte 0
ret:     .word 0

save9002: .byte 0

.CODE

;*******************************************************************************
; INSTALL SIGINT
; Installs an NMI to cancel a long running command (such as FIND) by pressing
; the RESTORE key.
.export __run_install_sigint
.proc __run_install_sigint
	lda #$7f
	sta $911d		; disable/ack RESTORE key interrupts
	ldxy #nmi::default
	stxy $318
	lda #$00
	sta edit::sigint	; reset INT flag
	lda #$82
	sta $911e		; enable RESTORE key interrupts
	rts
.endproc

;*******************************************************************************
; CLR
; Initializes the user state by running the BASIC coldstart process
.export __run_clr
.proc __run_clr
@dst=r0
	sei

	; pull return address and save it
	pla
	sta ret
	pla
	sta ret+1

	tsx
	stx save_sp

	jsr __run_init

	jsr fcpy::save_debug_state

	ldxy #@save_dbg_done		; need to pass return address
	jmp dbg::save_debug_zp

@save_dbg_done:
	lda #$7f
	sta $911e			; disable NMI's

	; initalize RAM ($00-$400)
	lda #$00
	tax
:	sta $00,x
	sta $100,x
	sta $200,x
	sta $300,x
	dex
	bne :-

	jsr $fd8d	; RAM test & init RAM locations
	jsr $fd52	; restore default I/O vectors
	jsr $fdf9	; initialize I/O registers

	; the RAM test above ran with Monster's RAM config- replace its
	; pointers with the ones for the user's configuration
	jsr memcfg::set_basic_ptrs

	jsr $e55b
	jsr $e518	; initialize rest of hardware

	; save $9002 (part of screen address) and blank screen so user doesn't
	; see garbage
	lda $9002
	sta save9002
	lda #$00
	sta $9002
	sta $9003

	jsr $e45b	; init BASIC vectors
	jsr $e3a4	; init BASIC RAM locations
	jsr $e404	; print startup message and init pointers

	ldx #<PROGRAM_STACK_START
	stx sim::reg_sp

	lda #$00
	sta sim::reg_a
	sta sim::reg_x
	sta sim::reg_y
	sta sim::reg_p

	; initialize debug BRK/NMI vectors
	ldx #DBGVECS_SIZE-1
:	lda DBGVECS,x
	sta dbg::progvecs,x
	dex
	bpl :-

	ldxy #@save_done	; need to pass return address
	jmp dbg::save_user_zp

@save_done:
	sei
	lda #$7f
	sta $911e			; disable NMI's

	; restore the "debug" (Monster's) low memory
	; this has the routines (in the shared RAM space) we need to do the rest
	; of the banked program state save (save_prog_state)
	jsr dbg::restore_debug_zp

	ldxy #@restore_debug_done
	jmp dbg::restore_debug_low

@restore_debug_done:
	ldx save_sp
	txs

	; save the initialized hi RAM ($1000-$2000) and other misc locations of
	; the user program
	jsr fcpy::save_prog_state

	; restore the rest of Monster's RAM and enter the application
	jsr fcpy::restore_debug_state

	lda #0
	ldxy #$0400
@clear123:
	jsr vmem::store
	inx
	bne @clear123
	iny
	cpy #$10
	bne @clear123
	lda save9002
	ldxy #$9002
	jsr vmem::store
	lda #23<<1
	ldxy #$9003
	jsr vmem::store

	; initialize PC to warm start
	ldxy #$c474
	stxy sim::pc
	stxy asm::origin

	jsr irq::on

	lda ret+1
	pha
	lda ret+0
	pha
	rts
.endproc

;*******************************************************************************
; INIT
; Sets up the handlers needed to run user programs
.export __run_init
.proc __run_init
	lda #<dbg::reenter
	sta native_destination+1
	lda #>dbg::reenter
	sta native_destination+2
	rts
.endproc

;*******************************************************************************
; GO BASIC
.export __run_go_basic
.proc __run_go_basic
	; install the NMI handler to return to editor
	jsr install_nmi_edit

	; disable NMIs
	lda #$7f
	sta $911d
	sta $911e

	jsr irq::off
	sei

	; write jsr $fe39 (init timer) to the pre-run buffer
	lda #$20
	sta go_pre_run
	lda #$39
	sta go_pre_run+1
	lda #$fe
	sta go_pre_run+2

	; empty the keyboard buffer
	lda #$00
	ldxy #$c6
	jsr vmem::store

	; begin execution
	jmp go_trampoline
.endproc

;*******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.export __run_go
.proc __run_go
	jsr scr::blank

	; select debugger reentry for the native handler
	jsr __run_init

	; disable NMIs
	lda #$7f
	sta $911e
	sei

	; write NOP; NOP; NOP to the pre-run buffer
	lda #$ea
	sta go_pre_run
	sta go_pre_run+1
	sta go_pre_run+2

	; fall through to go_trampoline
.endproc

;*******************************************************************************
; GO TRAMPOLINE
; Saves the debugger state and begins execution at the current simulator
; PC value
.export go_trampoline
.proc go_trampoline
	; write the address to bounce to
	lda sim::pc
	sta TRAMPOLINE_ADDR
	lda sim::pc+1
	sta TRAMPOLINE_ADDR+1

	lda memcfg::reg9c03
	sta user_config
	jsr install_trampoline
	jsr dbg::swap_in

	lda #$7f
	sta $911e
	sta $911d	; ack all interrupts
	sta $912d

	ldxy #@save_dbg_done		; need to pass return address
	jmp dbg::save_debug_zp

@save_dbg_done:
	ldxy #@restore_done		; need to pass return address
	jmp dbg::restore_user_zp

@restore_done:
	sei
	; reinstall NMI
	lda #<nmi_handler
	sta $0318
	lda #>nmi_handler
	sta $0318+1
	lda #<(brk_handler)
	sta $0316
	lda #>(brk_handler)
	sta $0316+1
.endproc

	ldx sim::reg_sp
	txs			; restore user stack

	lda sim::reg_p
	pha			; save status (will pull after bank select)
	lda sim::reg_a
	pha			; save .A (to be pulled after bank select)
	lda sim::reg_x
	pha
	lda sim::reg_y
	pha

	; Save RAM123 from SIM through a temporary internal-RAM bridge. The
	; user's registers are on the stack; no shared code executes afterward.
	ldxy #save_shared
	stxy bridge_target
	ldxy #native_go_done
	stxy bridge_finish
	jmp install_bridge

;*******************************************************************************
; INSTALL NMI EDIT
.proc install_nmi_edit
	; overwrite the JMP address to go to edit handler
	lda #<nmi_edit
	sta native_destination+1
	lda #>nmi_edit
	sta native_destination+2
	rts
.endproc

;*******************************************************************************
; NMI EDIT
; This is the NMI handler for invoking BASIC from the editor.
; It simply saves the state of BASIC and jumps back to the editor main loop
.proc nmi_edit
	;lda $912e
	;sta sim::via2+$e

	lda #$7f
	sta $911e	; disable all NMI's

	pla
	sta sim::reg_y
	pla
	sta sim::reg_x
	pla
	sta sim::reg_a

	pla
	sta sim::reg_p
	and #$10	; mask BRK flag
	sta dbg::is_brk

	pla
	sta sim::pc
	pla
	sta sim::pc+1

;	; check if an interrupt occurred inside the interrupt handler
;	; if it did, just RTI
;	cmp #$80
;	bcs :+
;	cmp #$7f
;	bcc :+
;	tax
;	lda sim::reg_p
;	pha
;	lda sim::pc
;	pha
;	txa
;	pha
;	ldx sim::reg_x
;	ldy sim::reg_y
;	lda sim::reg_a
;	rti
;:	lda #$7f
;	sta $911e	; disable all NMI's
;	sta $911d

	RESTORE_IO

	tsx
	stx sim::reg_sp

	; clear decimal in case user set it
	cld

	sei

	; reinit the debugger's SP
	ldx #$ff
	txs

	; save the user's zeropage and restore the debugger's
	ldxy #@save_done	; need to pass return address
	jmp dbg::save_user_zp

@save_done:
	ldxy #@restore_debug_done
	jmp dbg::restore_debug_low

@restore_debug_done:
	jsr dbg::restore_debug_zp

	; save program state and swap the debugger state in
	jsr dbg::swap_out
        jsr irq::on		; reinstall the main IRQ

	; return to the editor or monitor (whichever is active)
	lda dbg::interface
	beq @edit

@mon:	; need to clear bank stack (will grow each time user enters monitor
	; from BASIC)
	lda #$00
	sta zp::banksp
@mon0:	CALL FINAL_BANK_MONITOR, mon::reenter
	cmp #GUI_RET_CYCLE	; no window manager here to cycle windows;
	beq @mon0		; just re-prompt

	; return to the editor or monitor (whichever is active)
	lda edit::debugging
	beq @edit		; if not debugging, reinit editor

	; edit::gets likely changed the editor mode
	lda #MODE_COMMAND
	sta zp::editor_mode

	; re-init debugger at current PC and enter it
	ldxy sim::pc
	jmp dbg::start

@edit: jmp edit::run
.endproc

;*******************************************************************************
; USER NATIVE ENTRY AND RESUME ($7fe0-$7ffc)
.segment "INTS"
nmi_handler:
	pha
	txa
	pha
	tya
	pha
brk_handler:
	lda #0
	sta $9c03
	lda #FINAL_BANK_MAIN
	sta $9c02
user_resume:
	; MAIN continues at this same address on interrupt entry. On resume,
	; enable RESTORE only once USER (and its handler) is actually mapped.
	lda #$82
	sta $911e
	pla
	tay
	pla
	tax
	pla
	plp
TRAMPOLINE_ADDR=*+1+INTS_DELTA
	jmp $f00d
.assert nmi_handler = $7fe0, lderror, "FE3 native NMI must start at $7fe0"
.assert * <= $8000, lderror, "FE3 native transitions exceed BLK3"

; Only the instructions executed in MAIN are installed here.
.segment "INTS_MAIN"
back_to_user:
	lda #0
user_config = *-1+__INTS_MAIN_LOAD__-__INTS_MAIN_RUN__
	sta $9c03
	lda #FINAL_BANK_USER
	sta $9c02
native_continue:
	jmp native_enter
.assert native_continue = user_resume, lderror, "FE3 bank-switch continuation must match"

.CODE
.proc install_trampoline
	ldx #<(__INTS_MAIN_SIZE__-1)
@main:
	lda __INTS_MAIN_LOAD__,x
	sta __INTS_MAIN_RUN__,x
	dex
	bpl @main

	ldxy #__INTS_RUN__
@user:
	lda __INTS_LOAD__-<__INTS_RUN__,x
	sta zp::bankval
	lda #FINAL_BANK_USER
	jsr ram::store
	inx
	cpx #<(__INTS_RUN__+__INTS_SIZE__)
	bne @user
	rts
.endproc

; RAM123 belongs entirely to the native program. Borrow 16 cassette-buffer
; bytes only during transitions, preserving them in MAIN before execution.
; This bridge can select SIM to copy RAM123, then return to MAIN, without
; installing executable code in SIM or depending on the user's zero page.
BRIDGE_ADDR = $033c
BRIDGE_RETURN = BRIDGE_ADDR+8
bridge_image:
	lda #FINAL_BANK_SIM
	sta $9c02
bridge_target = *+1
	jmp $ffff
	lda #FINAL_BANK_MAIN
	sta $9c02
bridge_finish = *+1
	jmp $ffff
BRIDGE_SIZE = *-bridge_image
.assert BRIDGE_SIZE = 16, error, "FE3 internal bridge size changed"

.segment "MAINBSS_NOINIT"
bridge_save: .res BRIDGE_SIZE

.CODE
.proc install_bridge
	ldx #BRIDGE_SIZE-1
@byte:
	lda BRIDGE_ADDR,x
	sta bridge_save,x
	lda bridge_image,x
	sta BRIDGE_ADDR,x
	dex
	bpl @byte
	jmp BRIDGE_ADDR
.endproc

.proc native_go_done
	ldx #BRIDGE_SIZE-1
@byte:
	lda bridge_save,x
	sta BRIDGE_ADDR,x
	dex
	bpl @byte

go_pre_run = *
	nop
	nop
	nop

	jmp back_to_user
.endproc
go_pre_run = native_go_done::go_pre_run

.proc native_enter
	; Keep the native vectors inactive while we restore the debugger.
	lda #$7f
	sta $911e
	lda $9111
	ldxy #restore_shared
	stxy bridge_target
	ldxy #native_restore_done
	stxy bridge_finish
	jmp install_bridge
.endproc

.proc native_restore_done
	ldx #BRIDGE_SIZE-1
@byte:
	lda bridge_save,x
	sta BRIDGE_ADDR,x
	dex
	bpl @byte
native_destination:
	jmp dbg::reenter	; patched to nmi_edit for BASIC
.endproc
native_destination = native_restore_done::native_destination

; The shared 3 KiB cannot remain mapped while native user code runs.
; These loops execute in SIM's BLK5, with no calls or zero-page temporaries.
.segment "FASTCOPY"
.import prog0400, dbg0400
.proc save_shared
	ldx #0
@byte:
.repeat 12, page
	lda $0400+page*$100,x
	sta dbg0400+page*$100,x
	lda prog0400+page*$100,x
	sta $0400+page*$100,x
.endrepeat
	inx
	beq :+
	jmp @byte
:	jmp BRIDGE_RETURN
.endproc
.proc restore_shared
	ldx #0
@byte:
.repeat 12, page
	lda $0400+page*$100,x
	sta prog0400+page*$100,x
	lda dbg0400+page*$100,x
	sta $0400+page*$100,x
.endrepeat
	inx
	beq :+
	jmp @byte
:	jmp BRIDGE_RETURN
.endproc
