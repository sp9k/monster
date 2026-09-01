;*******************************************************************************
; DEBUG.ASM
; This file contains Vic-20 specific support routines for the debugger
;*******************************************************************************

.include "../macros.inc"
.include "../sim6502.inc"
.include "../vmem.inc"

; stop_tracing flag- set to tell debugger to halt a trace
.export stop_tracing

.export PROGRAM_STACK_START
PROGRAM_STACK_START = $1ff

;*******************************************************************************
; STOP TRACING STATE/NMI
; This NMI is installed for the duration of a trace and catches the RESTORE key
; as a signal to stop it
.segment "INTS"
stop_tracing_nmi:
	inc stop_tracing
	rti

stop_tracing: .byte 0

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

	ldxy #stop_tracing_nmi
	stxy $0318

	rts
.endproc
