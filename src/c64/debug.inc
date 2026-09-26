.ifndef C64_DEBUG_INC
C64_DEBUG_INC = 1

.include "nmi.inc"
.global __run_save_high

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
; Finish the C64 native return: save upper user RAM and restore the simulator
; that lives beneath the KERNAL ROM.
.macro SAVE_IO
	jsr __run_save_high
.endmacro

;*******************************************************************************
; RESTORE IO
; This macro restores the I/O configuration that the debugger and KERNAL
; depend on after the user program has run
.macro RESTORE_IO
.endmacro

.endif
