.ifndef C64_DEBUG_INC
C64_DEBUG_INC = 1

.include "nmi.inc"

;*******************************************************************************
; TRACE ON
; This macro enables the NMI (or other method) required to interrupt a "trace"
.macro TRACE_ON
	jsr nmi::enable
.endmacro

;*******************************************************************************
; TRACE OFF
; This macro disables the NMI (or other method) responsible for performing a
; "trace".
.macro TRACE_OFF
	jsr nmi::disable
.endmacro

;*******************************************************************************
; SAVE IO
; This macro saves C64 specific I/O (CIAs etc.) upon returning to the
; debugger from a "GO" command
.macro SAVE_IO
.endmacro

;*******************************************************************************
; RESTORE IO
; This macro restores the I/O configuration that the debugger and KERNAL
; depend on after the user program has run
.macro RESTORE_IO
.endmacro

.endif
