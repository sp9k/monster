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

; the NMI vector that install_trace_nmi took over, put back by
; uninstall_trace_nmi
saved_nmi: .word 0

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

	; save the NMI handler we are displacing (the monitor installs
	; its own to catch RESTORE as a SIGINT)
	lda $0318
	sta saved_nmi
	lda $0319
	sta saved_nmi+1

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

;*******************************************************************************
; UNINSTALL TRACE NMI
; Restores the NMI handler that install_trace_nmi displaced.
.export uninstall_trace_nmi
.proc uninstall_trace_nmi
	php
	pha
	lda saved_nmi
	sta $0318
	lda saved_nmi+1
	sta $0319
	pla
	plp
	rts
.endproc
