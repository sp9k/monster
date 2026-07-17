;*******************************************************************************
; RUNTIME.ASM
; This file contains platforms specific helpers for managing the execution of
; a user's programs
;*******************************************************************************

.include "../expansion.inc"
.include "../fastcopy.inc"
.include "../prefs.inc"
.include "../settings.inc"
.include "../../debug.inc"
.include "../../edit.inc"
.include "../../guis.inc"
.include "../../irq.inc"
.include "../../macros.inc"
.include "../../monitor.inc"
.include "../../ram.inc"
.include "../../sim6502.inc"
.include "../../text.inc"
.include "../../vmem.inc"
.include "../../zeropage.inc"

.import return_to_debugger

.import PROGRAM_STACK_START

;*******************************************************************************
; BRK/NMI HANDLER ADDRESSES
; address in user program where the BRK handler will reside
; NOTE: the user program cannot use the space occupied by these handlers
;
;  |----------------------------------------------------|
;  |  TRAMPOLINE  |   STEP    |     NMI    |     BRK    | $8000
;  |----------------------------------------------------|
;

BRK_HANDLER_TOP  = $8000
NMI_HANDLER_ADDR = BRK_HANDLER_TOP-brkhandler1_size
BRK_HANDLER_ADDR = BRK_HANDLER_TOP-brkhandler1_size+5-1 ; 5 = sizeof NMI portion
RTI_ADDR         = BRK_HANDLER_ADDR+3

TRAMPOLINE = NMI_HANDLER_ADDR - trampoline_size
TRAMPOLINE_ADDR = TRAMPOLINE+13
.export TRAMPOLINE_ADDR

.segment "NMI_HANDLER"
.res 17 ;brkhandler1_size

.CODE

;******************************************************************************
; CLR
; Initializes the user state by running the BASIC coldstart process
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

:	jsr $e518	; initialize hardware
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
	jsr fcpy::save_prog_state

	; restore the rest of Monster's RAM and enter the application
	jsr fcpy::restore_debug_state

	; initialize PC to warm start
	ldxy #$c474
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

;******************************************************************************
; INIT
; Sets up the handlers needed to run user programs
.export __run_init
.proc __run_init
	jsr install_brk		; install the BRK/NMI handler
	jmp install_trampoline
.endproc

;******************************************************************************
; GO BASIC
.export __run_go_basic
.proc __run_go_basic
	; install the NMI handler to return to editor
	jsr install_brk_edit

	; install trampoline code
	jsr install_trampoline

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
	sta zp::bankval
	ldxy #TRAMPOLINE_ADDR
	lda #FINAL_BANK_USER
	jsr ram::store		; LSB

	lda sim::pc+1
	sta zp::bankval
	ldxy #TRAMPOLINE_ADDR+1
	lda #FINAL_BANK_USER
	jsr ram::store		; MSB

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

	ldxy #@restore_dbg_done		; need to pass return address
	jmp dbg::save_debug_zp

@restore_dbg_done:
	ldxy #@restore_done		; need to pass return address
	jmp dbg::restore_user_zp

@restore_done:
	; reinstall NMI
	lda #<NMI_HANDLER_ADDR
	sta $0318
	lda #>NMI_HANDLER_ADDR
	sta $0318+1
	lda #<(BRK_HANDLER_ADDR+1)
	sta $0316
	lda #>(BRK_HANDLER_ADDR+1)
	sta $0316+1
.endproc

	ldx sim::reg_sp
	txs			; restore user stack

go_pre_run:
	; buffer for pre-run command
	nop
	nop
	nop

	lda sim::reg_p
	pha			; save status (will pull after bank select)
	lda sim::reg_a
	pha			; save .A (to be pulled after bank select)
	ldx sim::reg_x
	ldy sim::reg_y

	jmp TRAMPOLINE

;******************************************************************************
; INSTALL BRK EDIT
.proc install_brk_edit
	jsr install_brk

	; overwrite the JMP address to go to edit handler
	lda #<nmi_edit
	sta BRK_HANDLER_ADDR+(brkhandler2_size-2)
	lda #>nmi_edit
	sta BRK_HANDLER_ADDR+(brkhandler2_size-1)
	rts
.endproc

;******************************************************************************
; INSTALL BRK
; Installs the debugger BRK handler to the BRK and NMI vectors and copies the
; code to enter the handler to the user program's RAM at BRK_HANDLER_ADDR.
; This handler switches back to the debuggers RAM bank, where the debugger takes
; over.
; IN:
;  - .XY: the address to install the BRK handler to
.proc install_brk
@dst=r0
@cnt=r2
	ldxy #BRK_HANDLER_ADDR+1
	stxy $0316		; BRK
	ldxy #NMI_HANDLER_ADDR
	stxy @dst
	stxy $0318		; NMI

	lda #brkhandler1_size-1
	sta @cnt
; copy part 1 of the BRK handler to the user program
@l0:	ldy @cnt
	lda brkhandlers,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr ram::store_off
	dec @cnt
	bpl @l0

	ldy #brkhandler2_size-1
; copy part 2 of the BRK handler to our memory
@l1:	lda brkhandler2,y
	sta BRK_HANDLER_ADDR,y
	dey
	bpl @l1
	rts
.endproc

;******************************************************************************
; INSTALL TRAMPOLINE
; Installs the "trampoline" code at the top of the user and debug RAM
; This code lets us switch to the user bank and begin executing code there
.proc install_trampoline
@cnt=r0
@dst=r2
	ldxy #TRAMPOLINE
	stxy @dst
	lda #trampoline_size-1
	sta @cnt

; copy the STEP handler to the user program and our RAM
@l0:	ldy @cnt
	lda trampoline,y
	sta TRAMPOLINE,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr ram::store_off
	dec @cnt
	bpl @l0

	rts
.endproc

;******************************************************************************
; NMI EDIT
; This is the NMI handler for invoking BASIC from the editor.
; It simply saves the state of BASIC and jumps back to the editor main loop
.proc nmi_edit
	lda $912e
	sta sim::via2+$e

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

@edit:	jmp edit::init
.endproc

;******************************************************************************
; BRKHANDLER
; Handles the BRK interrupt by returning control to the main bank
; and continuing execution there.
; Part 1 of the handler is copied to the user program and part 2 is copied to
; the editor, where execution will pick up after switching banks
;
; This is the layout of the code in each bank:
; | User         |  Debugger                     |
; |--------------|-------------------------------|
; | PHA          |	--                       |
; | TXA          |	--                       |
; | PHA          |	--                       |
; | TYA          |	--                       |
; | PHA          |      PHA                      |
; | LDA #$80     |	LDA #$80                 |
; | STA $9C02    |	STA $9C02                |
; | LDA #$82     |      PLA                      |
; | STA $911e    |      JMP DEBUG_BRK            |
; | PLA          |	                         |
; | RTI          | 	                         |
; | -- (0 bytes) |  -- (3 bytes)                 |
;
; handler1 (the User handler) has 1 empty bytes at the end
; handler2 (the Debug handler) has 5 empty bytes at the start
.PUSHSEG
.RODATA
brkhandlers:
brkhandlernmi:
brkhandler1:
; this portion runs in the DEBUGGER bank
	pha		; NMI handler
	txa
	pha
	tya
	pha

	lda #$80	; start of BRK handler
	sta $9c02	; switch to DEBUGGER bank
;--------------------------------------
; this portion runs in the USER bank
	lda #$82
	sta $911e	; enable CA1 (RESTORE key) interrupts
	pla
	rti		; return from BRK/NMI
brkhandler1_size=*-brkhandler1
.export brkhandler1_size

brkhandler2:
; this portion runs in the user bank
	pha
	lda #$80
	sta $9c02	; switch to DEBUGGER bank
;--------------------------------------
; this portion runs in the debugger bank
	jmp dbg::reenter
	; jmp nmi_edit (when installed for editor)
brkhandler2_size=*-brkhandler2

;******************************************************************************
; TRAMPOLINE
trampoline:
	lda #FINAL_BANK_USER
	sta $9c02
	lda #$82		; enable RESTORE key interrupt
	sta $911e
	pla			; restore .A
	plp			; restore status
;TRAMPOLINE_ADDR=*+1
	jmp $f00d
trampoline_size=*-trampoline

.POPSEG
