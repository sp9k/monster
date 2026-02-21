;*******************************************************************************
; RUNTIME.ASM
; This file contains platforms specific helpers for managing the execution of
; a user's programs
;*******************************************************************************

.include "../expansion.inc"
.include "../fastcopy.inc"
.include "../prefs.inc"
.include "../settings.inc"
.include "../vaddrs.inc"
.include "../../debug.inc"
.include "../../edit.inc"
.include "../../irq.inc"
.include "../../macros.inc"
.include "../../monitor.inc"
.include "../../ram.inc"
.include "../../sim6502.inc"
.include "../../text.inc"
.include "../../vmem.inc"
.include "../../zeropage.inc"

.import return_to_debugger

.import __INTS_RUN__
.import __INTS_LOAD__
.import __INTS_SIZE__

.import PROGRAM_STACK_START

;*******************************************************************************
; BRK/NMI HANDLER ADDRESSES
; address in user program where the BRK handler will reside
; NOTE: the user program cannot use the space occupied by these handlers
;
;  |----------------------------------------------------|
;  |  TRAMPOLINE  |   STEP    |     NMI    |     BRK    | $A000
;  |----------------------------------------------------|
;

BRK_HANDLER_TOP  = $8000

.export STEP_HANDLER_ADDR
.export STEP_EXEC_BUFFER
.export TRAMPOLINE_ADDR
.export STEP_HANDLER_ADDR

STEP_HANDLER_ADDR = step_handler

.segment "SHAREBSS"
save_sp: .byte 0
ret:     .word 0

.CODE

;******************************************************************************
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

	; check expansion disable flag
	lda __debug_enable_expansion
	beq :+
	jsr $fdca	; set top of RAM to $2000 (emulate unexpanded config)

:	jsr $e55b
	jsr $e518	; initialize rest of hardware
	; blank screen so user doesn't see garbage
	lda #$00
	sta $9002
	sta $9003

	jsr $e45b	; init BASIC vectors
	jsr $e3a4	; init BASIC RAM locations
	jsr $e404	; print startup message and init pointers

	ldx #<PROGRAM_STACK_START
	stx sim::reg_sp

	ldxy #@save_done	; need to pass return address
	jmp dbg::save_user_zp

@save_done:
	sei
	lda #$7f
	sta $911e			; disable NMI's

	; restore the "debug" (Monster's) low memory
	; this has the routines (in the shared RAM space) we need to do the rest
	; of the banekd program state save (save_prog_state)
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

	; clear $400-$1000 (RAM123) in virtual memory
	lda #VMEM_RAM123_BANK
	sta $9ffe		; in BLK5
	lda #$d5		; RAM in BLK5
	sta $9ff2

	ldy #>$a000		; BLK5
	sty @dst+1
	ldy #$00
	sty @dst
	tya
	ldx #($10-$04)		; # of pages to clear
:	sta (@dst),y
	dey
	bne :-
	inc @dst+1
	dex
	bne :-

	; write the KERNAL's default values for $9002 & $9003
	lda #VMEM_IO_BANK
	sta $9ffe		; in BLK5
	lda #22|$80
	sta prog9000+$02+($a000-$2000)
	lda #23<<1
	sta prog9000+$03+($a000-$2000)

	; switch back to default config
	lda #$55
	sta $9ff2
	lda #$04
	sta $9ffe

	; initialize PC to warm start
	ldxy #$c474
	stxy sim::pc

	jsr irq::on

	lda ret+1
	pha
	lda ret+0
	pha
	rts
.endproc

;******************************************************************************
; INIT
; Sets up the handlers needed to run user programs
.export __run_init
.proc __run_init
	; copy the TRAMPOLINE handler to the user program and our RAM
	ldy #interrupts_size
@l0:	lda __INTS_LOAD__-1,y
	sta __INTS_RUN__-1,y
	dey
	bne @l0
	rts
.endproc

;******************************************************************************
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

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.export __run_go
.proc __run_go
	; install the NMI, STEP, and TRAMPOLINE handlers
	jsr __run_init

	jsr irq::off

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

;******************************************************************************
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

	jsr dbg::swap_in

	sei
	lda #<$eb15
	sta $0314
	lda #>$eb15
	sta $0314+1

	lda prefs::normal_color
	sta $900f

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

	jmp trampoline

;******************************************************************************
; INSTALL NMI EDIT
.proc install_nmi_edit
	; overwrite the JMP address to go to edit handler
	lda #<nmi_edit
	sta nmi_handler+nmi_handler_size-2
	lda #>nmi_edit
	sta nmi_handler+nmi_handler_size-1
	rts
.endproc

;******************************************************************************
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
	CALL FINAL_BANK_MONITOR, mon::reenter

	; return to the editor or monitor (whichever is active)
	lda edit::debugging
	beq @edit		; if not debugging, reinit editor

	; edit::gets likely changed the editor mode
	lda #MODE_COMMAND
	sta zp::editor_mode

	; re-init debugger at current PC and enter it
	ldxy sim::pc
	jmp dbg::start

@edit:	jmp edit::init
.endproc

;******************************************************************************
; WRITE STEP
; Writes a step to the "step buffer" for execution
; IN:
;   sim::op[0:2]: the instruction to write
; OUT:
;   STEP_EXEC_BUFFER: contains the instruction
.export write_step
.proc write_step
@write_instruction:
@sz=r2
@cnt=r3
	stx @sz

	; copy the instruction to the execution buffer, appending
	; NOPs as needed to fill the 3 byte space
	ldx #$00
@l0:	lda sim::op,x
	cpx @sz
	bcc :+
	lda #$ea		; NOP
:	sta STEP_EXEC_BUFFER,x
	inx
	cpx #$03
	bne @l0
	rts
.endproc

;******************************************************************************
; INTERRUPT HANDLERS
.segment "INTS"
interrupt_handlers:

;******************************************************************************
; NMI HANDLER
; Handles NMI interrupts by returning control to the main bank
; and continuing execution there.
nmi_handler:
	; save registers
	pha
	txa
	pha
	tya
	pha

brk_handler:
	lda #FINAL_BANK_MAIN	; start of BRK handler
	SELECT_BANK_A		; switch to DEBUGGER bank
	lda #$00
	sta $9ff4		; restore RAM123
	lda #$82
	sta $911e		; enable CA1 (RESTORE key) interrupts
	jmp dbg::reenter	; jmp nmi_edit (when installed for editor)
brk_handler_size=*-brk_handler
nmi_handler_size=*-nmi_handler

;******************************************************************************
; STEPHANDLER
; The step handler runs a single instruction and returns to the
; debugger.
.import step_done
step_handler:
	; switch to USER bank
	lda #VMEM_BLK1_BANK
	sta $9ff8		; BLK1
	lda #VMEM_BLK2_BANK
	sta $9ffa		; BLK2
	lda #VMEM_BLK3_BANK
	sta $9ffc		; BLK3
	lda #VMEM_BLK5_BANK
	sta $9ffe		; BLK5

	; set RAM123 and IO2/3 to RAM (r/w)
	lda #VMEM_RAM123_BANK
	sta $9ff4

	; TODO: write protect IO region

	; set BLK 1,2,3, and 5 to RAM (r/w)
	lda #$ff
	sta $9ff2

	pla
	sta @restore_a

	pla			; get status flags
	ora #$04		; set I flag
	pha			; push altered status

@restore_a=*+1
	lda #$00		; SMC - restore A
	plp			; restore altered status flags

	; run the instruction
STEP_EXEC_BUFFER:
	nop
	nop
	nop

	php			; save new status register
	pha			; save .A

	; switch back to DEBUGGER bank
	lda #$00
	sta $9ff4
	lda #$01
	sta $9ff8		; BLK1
	lda #$02
	sta $9ffa		; BLK2
	lda #$03
	sta $9ffc		; BLK3
	lda #$04
	sta $9ffe		; BLK5
	lda #$55
	sta $9ff2

	pla			; restore .A
	jmp step_done		; continue to finish up step

;******************************************************************************
; TRAMPOLINE
trampoline:
	lda #FINAL_BANK_USER
	SELECT_BANK_A
	lda #VMEM_RAM123_BANK	; $0000-$2000
	sta $9ff4

go_pre_run:
	; buffer for pre-run command
	nop
	nop
	nop

	lda #$82		; enable RESTORE key interrupt
	sta $911e
	pla
	tay
	pla
	tax
	pla			; restore .A
	plp			; restore status

TRAMPOLINE_ADDR=*+1
	jmp $f00d

interrupts_size=*-interrupt_handlers
