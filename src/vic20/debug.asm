;*******************************************************************************
; DEBUG.ASM
; This file contains Vic-20 specific support routines for the debugger
;*******************************************************************************

.include "../macros.inc"
.include "../sim6502.inc"
.include "../vmem.inc"

.import STEP_EXEC_BUFFER

; stop_tracing flag- set to tell debugger to halt a trace
.export stop_tracing

; TODO: calculate the maximum value for this
.export PROGRAM_STACK_START
PROGRAM_STACK_START = $1d8

; Stop tracing state/NMI
; This NMI is installed programatically and catches the RESTORE key as a
; signal to stop a trace
; These values must be between PRORGAM_STACK_START and $100
STOP_TRACING_NMI = PROGRAM_STACK_START+1
stop_tracing     = STOP_TRACING_NMI+4	; sizeof(inc stop_tracing)+sizeof(rti)

; Max depth the debugger may reach during handling of a step during the TRACE
; command. This amount will be saved/restored by the debugger before handling
; the debugged program's next instruction.
TRACE_STACK_DEPTH = $200-PROGRAM_STACK_START

.CODE

;*******************************************************************************
; INSTALL TRACE NMI
; Installs an NMI that increments stop_tracing when the RESTORE
; key is pressed.
; This should be installed for commands that automatically STEP
; repeatedly, like TRACE and STEP OUT
.export install_trace_nmi
.proc install_trace_nmi
	lda #$00
	sta stop_tracing

	; ack/disable all interrupts
	lda #$7f
	sta $911d
	sta $911e
	sta $912d
	sta $912e

	; write the following ISR:
	;	inc stop_tracing
	;	rti
	lda #$ee		; INC abs
	sta STOP_TRACING_NMI
	lda #<stop_tracing
	sta STOP_TRACING_NMI+1
	lda #>stop_tracing
	sta STOP_TRACING_NMI+2
	lda #$40		; RTI
	sta STOP_TRACING_NMI+3
	ldxy #STOP_TRACING_NMI
	stxy $0318

	rts
.endproc
