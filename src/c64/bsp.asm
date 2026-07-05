;*******************************************************************************
; BSP.ASM
; This file contains C64-specific helpers for things like debugging/tracing
;*******************************************************************************

.include "../macros.inc"
.include "../sim6502.inc"
.include "nmi.inc"
.include "reu.inc"

.export stop_tracing

.DATA
; Stop tracing state/NMI
; This NMI is installed programatically and catches the RESTORE key as a
; signal to stop a trace
; These values must be between PRORGAM_STACK_START and $100
STOP_TRACING_NMI = PROGRAM_STACK_START+1
stop_tracing     = STOP_TRACING_NMI+4	; sizeof(inc stop_tracing)+sizeof(rti)

.export PROGRAM_STACK_START
PROGRAM_STACK_START = $1e0

.CODE

;*******************************************************************************
; INSTALL TRACER
; Installs a routine to catch
.export __bsp_install_tracer
.proc __bsp_install_tracer
	; disable NMIs
	jsr nmi::disable
	lda #$7f
	sta $dc0e

	lda #$00
	sta stop_tracing

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
	stxy $fffa
	stxy $0318

	rts
.endproc

;*******************************************************************************
; RESTORE DEBUG STATE
.export __bsp_restore_debug_state
.proc __bsp_restore_debug_state
	; just restore everything
	ldxy #$0800
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2

	ldxy #$10000-$800
	stxy reu::txlen
	jsr reu::load_delayed

	jsr __bsp_restore_debug_visual
	rts
.endproc

;*******************************************************************************
; SAVE DEBUG STATE
.export __bsp_save_debug_state
.proc __bsp_save_debug_state
	; just save everything
	; TODO: don't be lazy
	ldxy #$0800
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2

	ldxy #$10000-$800
	stxy reu::txlen
	jsr reu::store_delayed
	jmp __bsp_save_debug_visual
.endproc

;******************************************************************************
; RESTORE PROG STATE
.export __bsp_restore_prog_state
.proc __bsp_restore_prog_state
	; just restore everything
	; TODO: don't be lazy
	ldxy #$0800
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_ADDR
	sta reu::reuaddr+2

	ldxy #$10000-$800
	stxy reu::txlen
	jsr reu::load_delayed
	jmp __bsp_restore_prog_visual
.endproc

;******************************************************************************
; SAVE DEBUG VISUAL
.export __bsp_save_debug_visual
.proc __bsp_save_debug_visual
	; save the screen
	ldxy #$0400
	stxy reu::c64addr
	stxy reu::reuaddr
	stxy reu::txlen
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2
	jsr reu::store

	; save the VIC-II registers and color memory
	ldxy #$d000
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_BACKUP_IO
	sta reu::reuaddr+2
	ldxy #$be8
	stxy reu::txlen
	jmp reu::store
.endproc

;******************************************************************************
; RESTORE DEBUG VISUAL
.export __bsp_restore_debug_visual
.proc __bsp_restore_debug_visual
	; load the screen
	ldxy #$0400
	stxy reu::c64addr
	stxy reu::reuaddr
	stxy reu::txlen
	lda #^REU_BACKUP_ADDR
	sta reu::reuaddr+2
	jsr reu::load

	; load the VIC-II registers and color memory
	ldxy #$d000
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_BACKUP_IO
	sta reu::reuaddr+2
	ldxy #$be8
	stxy reu::txlen
	jmp reu::load
.endproc

;******************************************************************************
; SAVE PROG VISUAL
.export __bsp_save_prog_visual
.proc __bsp_save_prog_visual
	ldxy #$0400
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_ADDR
	sta reu::reuaddr+2
	ldxy #$400
	stxy reu::txlen
	jsr reu::store

	; save the VIC-II registers and color memory
	ldxy #$d000
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_IO
	sta reu::reuaddr+2
	ldxy #$be8
	stxy reu::txlen
	jmp reu::store
.endproc

;******************************************************************************
; RESTORE PROG VISUAL
.export __bsp_restore_prog_visual
.proc __bsp_restore_prog_visual
	; load the character data for the screen
	ldxy #$0400
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_ADDR
	sta reu::reuaddr+2
	ldxy #$0400
	stxy reu::txlen
	jsr reu::load

	; load the VIC-II registers and color memory
	ldxy #$d000
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_IO
	sta reu::reuaddr+2
	ldxy #$be8
	stxy reu::txlen
	jmp reu::load
.endproc

;******************************************************************************
; SAVE PROG STATE
; Saves memory clobbered by the debugger (screen, I/O registers and color)
.export __bsp_save_prog_state
.proc __bsp_save_prog_state
	ldxy #$0800
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_ADDR
	sta reu::reuaddr+2

	ldxy #$10000-$800
	stxy reu::txlen
	jsr reu::store_delayed

	jmp __bsp_save_prog_visual
.endproc
