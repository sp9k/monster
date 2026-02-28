.include "bsp.inc"
.include "debug.inc"
.include "reu.inc"
.include "vaddrs.inc"
.include "../asmflags.inc"
.include "../debug.inc"
.include "../edit.inc"
.include "../irq.inc"
.include "../macros.inc"
.include "../monitor.inc"
.include "../ram.inc"
.include "../sim6502.inc"
.include "../vmem.inc"

.import __c64_dbg

.import __STEPHANDLER_RUN__
.import __STEPHANDLER_LOAD__
.import __STEPHANDLER_SIZE__

.import __TRAMPOLINE_RUN__
.import __TRAMPOLINE_LOAD__
.import __TRAMPOLINE_SIZE__

.import __NMI_HANDLER_RUN__
.import __NMI_HANDLER_LOAD__
.import __NMI_HANDLER_SIZE__

.import __STEP_EPILOGUE_RUN__
.import __STEP_EPILOGUE_LOAD__
.import __STEP_EPILOGUE_SIZE__

.export STEP_EXEC_BUFFER
.export STEP_HANDLER_ADDR

STEP_HANDLER_ADDR = __STEPHANDLER_RUN__

.import PROGRAM_STACK_START

.CODE
;*******************************************************************************
nop_handler:
	rti

;*******************************************************************************
; CLR
.export __run_clr
.proc __run_clr
	sei

	pla
	sta @ret0
	pla
	sta @ret1

	tsx
	stx @save_sp

	jsr __run_init
	jsr bsp::save_debug_state

	ldxy #@save_dbg_done		; need to pass return address
	jmp dbg::save_debug_zp

@save_dbg_done:
	jsr nmi::disable
	sei

	lda #$37
	sta $01		; expose KERNAL


	jsr $fd50	; RAMTAS
	jsr $e518	; init hardware
	jsr $fda3	; init I/O
	jsr $fd15	; restore default I/O vectors
	jsr $ff5b	; init screen
	jsr $e453	; init BASIC vectors
	jsr $e3bf	; init BASIC RAM locations
	jsr $e422	; print startup message and init pointers
	ldx #<PROGRAM_STACK_START
	stx sim::reg_sp

	lda #$34
	sta $01		; done with KERNAL

	ldxy #@save_done	; need to pass return address
	jmp reu::save_prog00

@save_done:
	sei

	; restore the debug (Monster's) low memory
	; this has the routines (in the shared RAM space) we need to do the rest
	; of the banekd program state save (save_prog_state)
	jsr dbg::restore_debug_zp

	ldxy #@restore_debug_done
	jmp dbg::restore_debug_low

@restore_debug_done:
@save_sp=*+1
	ldx #$00
	txs

	; save the initialized hi RAM ($1000-$2000) and other misc locations of
	; the user program
	jsr bsp::save_prog_state

	; restore the rest of Monster's RAM and enter the application
	jsr bsp::restore_debug_state

	; restore the virtual ZP (clobbered by restore_debug_state)
	jsr reu::restore_prog00

	lda #$2f
	sta prog00
	lda #$37
	sta prog00+1

	; initialize PC to warm start
	ldxy #$a483
	stxy sim::pc

	jsr irq::on

@ret1=*+1
	lda #$00
	pha
@ret0=*+1
	lda #$00
	pha

	rts
.endproc

;*******************************************************************************
; INIT
.export __run_init
.proc __run_init
	jsr install_nmi
	jsr install_step
	jmp install_trampoline
.endproc

;*******************************************************************************
; GO
.export __run_go
.proc __run_go
	TRACE_ON

	lda #$34	; make all RAM available
	sta $01

	ldxy sim::pc
	stxy TRAMPOLINE_PC

	; swap in the memory above $e000
	ldxy #$e000
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_ADDR
	sta reu::reuaddr+2
	ldxy #$2000-$08
	stxy reu::txlen
	jsr reu::load_delayed

	; save the debugger's ZP and bring in the user's one
	ldxy #@save_dbg_done		; need to pass return address
	jmp dbg::save_debug_zp
@save_dbg_done:
	ldxy #@restore_done		; need to pass return address

	jmp dbg::restore_user_zp
@restore_done:
	lda #$34
	sta $01

	jsr bsp::save_debug_state
	jsr bsp::restore_prog_visual

	; install the SW/HW NMI handler
	lda #<nmi_handler
	sta $fffa
	sta $0318
	lda #>nmi_handler
	sta $fffb
	sta $0319

	; bounce to the user's program
	ldx sim::reg_sp
	txs
	ldx sim::reg_x
	ldy sim::reg_y
	lda sim::reg_p
	pha
	lda sim::reg_a
	sta TRAMPOLINE_A

	lda prog00+1
	sta TRAMPOLINE_PROG01

	lda #$36			; expose REU registers
	sta $01

	; prepare REU registers to load from $800 up to the NMI handler addr
	lda #$00
	sta $df02
	sta $df04
	lda #$08
	sta $df02+1
	sta $df04+1
	lda #^REU_VMEM_ADDR
	sta $df04+2

	lda #<(__NMI_HANDLER_RUN__-$800)
	sta $df07
	lda #>(__NMI_HANDLER_RUN__-$800)
	sta $df07+1

	lda #$91			; command to load from REU
	sei
	jmp trampoline
.endproc

;*******************************************************************************
; GO BASIC
.export __run_go_basic
.proc __run_go_basic
	jsr nmi::disable
	jsr irq::off

	jsr install_edit_nmi

	; empty the keyboard buffer
	lda #$00
	ldxy #$c6
	jsr vmem::store

	; begin execution
	jmp __run_go
.endproc

;******************************************************************************
; INSTALL TRAMPOLINE
; Installs the "trampoline" code at the top of the user and debug RAM
; This code lets us switch to the user bank and begin executing code there
.proc install_trampoline
	; copy the TRAMPOLINE handler to the user program and our RAM
	ldy #<trampoline_size-1
@l0:	lda __TRAMPOLINE_LOAD__,y
	sta __TRAMPOLINE_RUN__,y
	dey
	bpl @l0
	rts
.endproc

;******************************************************************************
; INSTALL STEP
; Installs the STEP code at the top of the user and debug RAM
; This code includes a buffer that switches to the user RAM, executes an
; instruction there, switches back to the debugger RAM, and jumps back to the
; debugger's "return from step" function to capture any changes that took place.
.proc install_step
@cnt=r0
@dst=r2
	; copy the STEP handler to the user program and our RAM
	;.assert stephandler_size < $100
	ldy #<stephandler_size-1
@l0:	lda __STEPHANDLER_LOAD__,y
	sta __STEPHANDLER_RUN__,y
	dey
	bpl @l0

	; copy the STEP handler to the user program and our RAM
;.assert __STEPHANDLER_SIZE__ < $100
	ldy #<__STEP_EPILOGUE_SIZE__-1
@l1:	lda __STEP_EPILOGUE_LOAD__,y
	sta __STEP_EPILOGUE_RUN__,y
	dey
	bpl @l1
	rts
.endproc

;******************************************************************************
; INSTALL NMI
; Installs the NMI handler
.proc install_nmi
	ldy #<nmi_handler_size-1
@l0:	lda __NMI_HANDLER_LOAD__,y
	sta __NMI_HANDLER_RUN__,y
	dey
	bpl @l0
	rts
.endproc

;******************************************************************************
; INSTALL EDIT NMI
; Installs the NMI used to return from BASIC to the editor (or monitor)
.proc install_edit_nmi
	jsr install_nmi

	; overwrite the JMP address to go to edit handler
	lda #<nmi_edit
	sta __NMI_HANDLER_RUN__+nmi_handler_size-2
	lda #>nmi_edit
	sta __NMI_HANDLER_RUN__+nmi_handler_size-1
	rts
.endproc

;******************************************************************************
; NMI EDIT
; This is the NMI handler for invoking BASIC from the editor.
; It simply saves the state of BASIC and jumps back to the editor main loop
.proc nmi_edit
	;lda $912e
	;sta sim::via2+$e

	jsr nmi::disable

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

.segment "NMI_HANDLER"
;******************************************************************************
; NMI HANDLER
; Handles an NMI (RESTORE key) or BRK to return to the debugger
nmi_handler:
	pha		; NMI handler
	txa
	pha
	tya
	pha

	lda #$36
	sta $01

	; save the current state of the program
	; ($0800-$ceff)
	lda #$00
	sta $df02
	sta $df04
	lda #$08
	sta $df02+1
	sta $df04+1
	lda #^REU_VMEM_ADDR
	sta $df04+2

	lda #<(__NMI_HANDLER_RUN__-$800)
	sta $df07
	lda #>(__NMI_HANDLER_RUN__-$800)
	sta $df07+1
	lda #$90|$20	; load + autoload
	sta $df01	; C64 -> REU + autoload

	; load program back
	lda #^REU_BACKUP_ADDR
	sta $df04+2
	lda #$91
	sta $df01	; REU -> C64

	lda #$34
	sta $01
	jmp dbg::reenter
nmi_handler_size=*-nmi_handler

.segment "TRAMPOLINE"
;******************************************************************************
; TRAMPOLINE
; Swaps memory and then jumps to the simulated PC
trampoline:
	sta $df01	; load program state from REU

TRAMPOLINE_PROG01=*+1
	lda #$00
	sta $01		; set bank register to user's value

	lda #<nmi_handler
	sta $0318
	lda #>nmi_handler
	sta $0319

	; restore .A
TRAMPOLINE_A=*+1
	lda #$00

	plp
TRAMPOLINE_PC=*+1
	jmp $f00d	; jump to the user's program
trampoline_size=*-trampoline

.segment "STEPHANDLER"
.export STEP_MEMORY_VALUE
.export STEP_EFFECTIVE_ADDR

;*******************************************************************************
; STEPHANDLER
; The step handler runs a single instruction and returns to the
; debugger.
; IN:
;   - STEP_EFFECTIVE_ADDR: memory location that will be used (if any)
;   - STEP_MEMORY_VALUE:   value that should be stored at the effective addr
;   - stack:               .A, .P (top to bottom)
;   - .A, .X, .Y:          register values for step to execute
.import step_done
stephandler:
	pla
	sta STEP_RESTORE_A

	; make I/O visible
	lda #$34
	sta $01

	; store user byte if it's used
	lda sim::affected
	and #OP_LOAD|OP_STORE
	beq :+

STEP_MEMORY_VALUE=*+1
	lda #$00
STEP_EFFECTIVE_ADDR=*+1
	sta $f00d

:	pla			; get status flags
	ora #$04		; set I flag (disable IRQs)
	pha			; push altered status

STEP_RESTORE_A=*+1
	lda #$00
	plp			; restore altered status flags (no IRQs)

STEP_EXEC_BUFFER:
	; run the instruction
	nop
	nop
	nop
	php

	jmp step_done		; done -> update simulator with new state
stephandler_size=*-stephandler
