;*******************************************************************************
; SIM6502.ASM
; This file contains the state for the simulated 6502: the virtual register
; file and the per-step flags/results produced by the simulator
;*******************************************************************************

.include "asmflags.inc"
.include "macros.inc"
.include "ram.inc"
.include "vmem.inc"
.include "zeropage.inc"

.ifdef ultimem
.include "vic20/expansion.inc"	; FINAL_BANK_SIM, FINAL_BANK_FASTCOPY
.endif

;*******************************************************************************
; VIA REGISTER OFFSETS
via_t1cl = $4	; T1 counter lo (read: ack T1 IRQ, write: set T1 latch lo)
via_t1ch = $5	; T1 counter hi (write: load counter from latch, ack T1 IRQ)
via_t1ll = $6	; T1 latch lo
via_t1lh = $7	; T1 latch hi (write: also ack T1 IRQ)
via_t2cl = $8	; T2 counter lo (read: ack T2 IRQ, write: set T2 latch lo)
via_t2ch = $9	; T2 counter hi (write: load counter, ack T2 IRQ)
via_acr  = $b	; auxiliary control register
via_ifr  = $d	; interrupt flag register
via_ier  = $e	; interrupt enable register

;*******************************************************************************
.BSS

;*******************************************************************************
; SIMUATOR REGISTER STATE
.export __sim_register_state
.export __sim_pc
.export __sim_reg_a
.export __sim_reg_x
.export __sim_reg_y
.export __sim_reg_sp
.export __sim_reg_p

__sim_register_state:
__sim_pc:     .word 0
__sim_reg_a:  .byte 0
__sim_reg_x:  .byte 0
__sim_reg_y:  .byte 0
__sim_reg_sp: .byte 0
__sim_reg_p:  .byte 0

; if !0, a relative branch will be taken next STEP
.export __sim_branch_taken
__sim_branch_taken:  .byte 0

; the next PC after the current instruction is executed
.export __sim_next_pc
__sim_next_pc: .word 0

; set if the CPU has encountered a JAM instruction
.export __sim_jammed
__sim_jammed: .byte 0

; set if a write to an unusable address occurred (e.g. the step handler)
.export __sim_vital_addr_clobbered
__sim_vital_addr_clobbered: .byte 0

; set if the CPU has encountered an "illegal" (undocumented) opcode
.export __sim_illegal
__sim_illegal: .byte 0

; set if the CPU has encountered a BRK
.export __sim_at_brk
__sim_at_brk: .byte 0

; next opcode that will be executed
.export __sim_op
__sim_op: .byte 0

; operand of next instruction that will be executed
.export __sim_operand
__sim_operand: .word 0

; address modes used by current instruction
.export __sim_op_mode
__sim_op_mode: .byte 0

; flag of what a given instruction affects, OP_LOAD, OP_STORE, OP_REG_A, etc.
.export __sim_affected
__sim_affected: .byte 0

; address that is written/loaded by a given STEP
.export __sim_effective_addr
__sim_effective_addr: .word 0

; depth of simulated (VIA) interrupt handlers currently entered
.export __sim_irq_depth
__sim_irq_depth: .byte 0

; !0 if the last RTI returned from a simulated IRQ/NMI (not a subroutine-like
; RTI); used by step-out/step-over to keep their JSR/RTS depth balanced
.export __sim_rti_irq
__sim_rti_irq: .byte 0

; stopwatch of cycles counted by simulator since last reset
.export __sim_stopwatch
__sim_stopwatch: .res 3

;*******************************************************************************
; shadow registers for the user's VIA1 ($9110) and VIA2 ($9120)
; the simulator redirects the user's loads/stores in this range to these
; shadows so that the timers can be emulated (see via_read/via_write)
; NOTE: the two blocks must be contiguous (they are indexed as one array)
.export __sim_via1
.export __sim_via2
vias:
__sim_via1: .res $10
__sim_via2: .res $10

; T2 lo-order latches; a write to T2CL is buffered here and only transferred
; to the counter when T2CH is written (index 0 = VIA1, 1 = VIA2)
via_t2_latch: .res 2

; timer "armed" flags: if !0, the corresponding timer sets its interrupt flag
; when it underflows.  T1 stays armed in free-run mode; a one-shot timer is
; disarmed when it fires until its counter hi byte is rewritten
; (index 0 = VIA1, 1 = VIA2)
via_t1_armed: .res 2
via_t2_armed: .res 2

; cycles executed by the current STEP (amount to tick the VIA timers down by)
step_cycles: .byte 0

; previous level of the virtual NMI line (VIA1); NMIs are edge-triggered
nmi_prev: .byte 0

; temp storage for via_read/via_write
viatmp: .byte 0

; if !0, we're executing a TRACE (not STEP).  The debugger sets/clears this
; flag (see debug.asm); it must only ever hold 0 or 1 (see vmem_load).
; While set, the user's memory must be swapped in (dbg::swap_in)
.export __sim_tracing
__sim_tracing:
tracing: .byte 0

.import stop_tracing		; flag to halt a trace command

;******************************************************************************
.segment "DEBUGGER"

;******************************************************************************
; INIT
; Initializes the simulator state for a new debug session.
; Copies the user's saved VIA registers to the VIA shadows and resets the
; timer state used to generate simulated IRQ/NMIs
.export __sim_init
.proc __sim_init
	lda #$00
	sta step_cycles
	sta nmi_prev
	sta __sim_irq_depth
	sta __sim_rti_irq

.ifdef vic20
	; copy the user's saved VIA registers ($9110-$912f) to the shadows
	ldx #$00
@copy:	txa
	pha
	clc
	adc #<$9110
	tax
	ldy #>$9110
	jsr vmem::load
	sta viatmp
	pla
	tax
	lda viatmp
	sta vias,x
	inx
	cpx #$20
	bne @copy

	; begin with no interrupts pending and both timers on each VIA armed
	lda #$00
	sta __sim_via1+via_ifr
	sta __sim_via2+via_ifr
	lda #$01
	sta via_t1_armed
	sta via_t1_armed+1
	sta via_t2_armed
	sta via_t2_armed+1

	; best guess for the (write-only) T2 latches: the current counter value
	lda __sim_via1+via_t2cl
	sta via_t2_latch
	lda __sim_via2+via_t2cl
	sta via_t2_latch+1
.endif
	rts
.endproc

;******************************************************************************
; FLUSH VIAS
; Writes the VIA shadow registers back to virtual memory ($9110-$912f).
.export __sim_flush_vias
.proc __sim_flush_vias
.ifdef vic20
	ldx #$00
@copy:	lda vias,x
	sta viatmp
	txa
	pha
	clc
	adc #<$9110
	tax
	ldy #>$9110
	lda viatmp
	jsr vmem::store
	pla
	tax
	inx
	cpx #$20
	bne @copy
.endif
	rts
.endproc

;******************************************************************************
; STEP
; Executes one step of the 6502 simulator
.export __sim_step
.proc __sim_step
.ifdef ultimem
	lda #$00
	sta __sim_tracing
	JUMP FINAL_BANK_SIM, step
.else
	jmp step
.endif
.endproc

;******************************************************************************
; TRACE
; Executes steps of the 6502 simulator repeatedly until interrupted (the caller
; is responsible for installing the interrupt(s) to do this).
; The caller is also responsible for setting __sim_tracing (and swapping in
; the user's memory if it is set - see dbg::swap_in)
.export __sim_trace
.proc __sim_trace
.ifdef ultimem
	lda #$01
	sta __sim_tracing
	JUMP FINAL_BANK_SIM, trace
.else
	jmp trace
.endif
.endproc

;******************************************************************************
.ifdef ultimem
.segment "SIM"
.else
.segment "DEBUGGER"
.endif

;******************************************************************************
; DISPATCH TABLES
; htab_lo[opcode] and htab_hi[opcode] hold the lo/hi bytes of each handler.
;******************************************************************************
.linecont +
.define handlers \
	h_brk,     h_ora_indx, h_jam,      h_ill, \
	h_ill,     h_ora_zp,   h_asl_zp,   h_ill, \
	h_php,     h_ora_imm,  h_asl_a,    h_ill, \
	h_ill,     h_ora_abs,  h_asl_abs,  h_ill, \
	h_bpl,     h_ora_indy, h_jam,      h_ill, \
	h_ill,     h_ora_zpx,  h_asl_zpx,  h_ill, \
	h_clc,     h_ora_absy, h_ill,      h_ill, \
	h_ill,     h_ora_absx, h_asl_absx, h_ill, \
	h_jsr,     h_and_indx, h_jam,      h_ill, \
	h_bit_zp,  h_and_zp,   h_rol_zp,   h_ill, \
	h_plp,     h_and_imm,  h_rol_a,    h_ill, \
	h_bit_abs, h_and_abs,  h_rol_abs,  h_ill, \
	h_bmi,     h_and_indy, h_jam,      h_ill, \
	h_ill,     h_and_zpx,  h_rol_zpx,  h_ill, \
	h_sec,     h_and_absy, h_ill,      h_ill, \
	h_ill,     h_and_absx, h_rol_absx, h_ill, \
	h_rti,     h_eor_indx, h_jam,      h_ill, \
	h_ill,     h_eor_zp,   h_lsr_zp,   h_ill, \
	h_pha,     h_eor_imm,  h_lsr_a,    h_ill, \
	h_jmp_abs, h_eor_abs,  h_lsr_abs,  h_ill, \
	h_bvc,     h_eor_indy, h_jam,      h_ill, \
	h_ill,     h_eor_zpx,  h_lsr_zpx,  h_ill, \
	h_cli,     h_eor_absy, h_ill,      h_ill, \
	h_ill,     h_eor_absx, h_lsr_absx, h_ill, \
	h_rts,     h_adc_indx, h_jam,      h_ill, \
	h_ill,     h_adc_zp,   h_ror_zp,   h_ill, \
	h_pla,     h_adc_imm,  h_ror_a,    h_ill, \
	h_jmp_ind, h_adc_abs,  h_ror_abs,  h_ill, \
	h_bvs,     h_adc_indy, h_jam,      h_ill, \
	h_ill,     h_adc_zpx,  h_ror_zpx,  h_ill, \
	h_sei,     h_adc_absy, h_ill,      h_ill, \
	h_ill,     h_adc_absx, h_ror_absx, h_ill, \
	h_ill,     h_sta_indx, h_ill,      h_ill, \
	h_sty_zp,  h_sta_zp,   h_stx_zp,   h_ill, \
	h_dey,     h_ill,      h_txa,      h_ill, \
	h_sty_abs, h_sta_abs,  h_stx_abs,  h_ill, \
	h_bcc,     h_sta_indy, h_jam,      h_ill, \
	h_sty_zpx, h_sta_zpx,  h_stx_zpy,  h_ill, \
	h_tya,     h_sta_absy, h_txs,      h_ill, \
	h_ill,     h_sta_absx, h_ill,      h_ill, \
	h_ldy_imm, h_lda_indx, h_ldx_imm,  h_ill, \
	h_ldy_zp,  h_lda_zp,   h_ldx_zp,   h_ill, \
	h_tay,     h_lda_imm,  h_tax,      h_ill, \
	h_ldy_abs, h_lda_abs,  h_ldx_abs,  h_ill, \
	h_bcs,     h_lda_indy, h_jam,      h_ill, \
	h_ldy_zpx, h_lda_zpx,  h_ldx_zpy,  h_ill, \
	h_clv,     h_lda_absy, h_tsx,      h_ill, \
	h_ldy_absx,h_lda_absx, h_ldx_absy, h_ill, \
	h_cpy_imm, h_cmp_indx, h_ill,      h_ill, \
	h_cpy_zp,  h_cmp_zp,   h_dec_zp,   h_ill, \
	h_iny,     h_cmp_imm,  h_dex,      h_ill, \
	h_cpy_abs, h_cmp_abs,  h_dec_abs,  h_ill, \
	h_bne,     h_cmp_indy, h_jam,      h_ill, \
	h_ill,     h_cmp_zpx,  h_dec_zpx,  h_ill, \
	h_cld,     h_cmp_absy, h_ill,      h_ill, \
	h_ill,     h_cmp_absx, h_dec_absx, h_ill, \
	h_cpx_imm, h_sbc_indx, h_ill,      h_ill, \
	h_cpx_zp,  h_sbc_zp,   h_inc_zp,   h_ill, \
	h_inx,     h_sbc_imm,  h_nop,      h_ill, \
	h_cpx_abs, h_sbc_abs,  h_inc_abs,  h_ill, \
	h_beq,     h_sbc_indy, h_jam,      h_ill, \
	h_ill,     h_sbc_zpx,  h_inc_zpx,  h_ill, \
	h_sed,     h_sbc_absy, h_ill,      h_ill, \
	h_ill,     h_sbc_absx, h_inc_absx, h_ill
.linecont -

htab_lo: .lobytes handlers
htab_hi: .hibytes handlers

;******************************************************************************
; PER-OPCODE ATTRIBUTE TABLES
; affected_tab[op]: OP_* flags stored to __sim_affected by the dispatcher
;                   before the handler runs.
; cycles_tab[op]:   base cycle count added to the stopwatch by the
;                   dispatcher.  Variable-cycle opcodes (branches, BRK,
;                   JAM) hold 0 and account for their own cycles.  The
;                   page-cross penalty (+1) is added by the handler from
;                   the carry returned by am_absx/am_absy/am_indy.
;******************************************************************************
affected_tab:
.byte $00				; $00: brk
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $01: ora_indx
.byte $00				; $02: jam
.byte $00				; $03: ill
.byte $00				; $04: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $05: ora_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $06: asl_zp
.byte $00				; $07: ill
.byte OP_STACK|OP_STORE			; $08: php
.byte OP_REG_A|OP_FLAG			; $09: ora_imm
.byte OP_REG_A|OP_FLAG			; $0a: asl_a
.byte $00				; $0b: ill
.byte $00				; $0c: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $0d: ora_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $0e: asl_abs
.byte $00				; $0f: ill
.byte OP_PC				; $10: bpl
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $11: ora_indy
.byte $00				; $12: jam
.byte $00				; $13: ill
.byte $00				; $14: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $15: ora_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $16: asl_zpx
.byte $00				; $17: ill
.byte OP_FLAG				; $18: clc
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $19: ora_absy
.byte $00				; $1a: ill
.byte $00				; $1b: ill
.byte $00				; $1c: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $1d: ora_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $1e: asl_absx
.byte $00				; $1f: ill
.byte OP_PC|OP_STACK|OP_STORE		; $20: jsr
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $21: and_indx
.byte $00				; $22: jam
.byte $00				; $23: ill
.byte OP_LOAD|OP_FLAG			; $24: bit_zp
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $25: and_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $26: rol_zp
.byte $00				; $27: ill
.byte OP_STACK|OP_LOAD|OP_FLAG		; $28: plp
.byte OP_REG_A|OP_FLAG			; $29: and_imm
.byte OP_REG_A|OP_FLAG			; $2a: rol_a
.byte $00				; $2b: ill
.byte OP_LOAD|OP_FLAG			; $2c: bit_abs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $2d: and_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $2e: rol_abs
.byte $00				; $2f: ill
.byte OP_PC				; $30: bmi
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $31: and_indy
.byte $00				; $32: jam
.byte $00				; $33: ill
.byte $00				; $34: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $35: and_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $36: rol_zpx
.byte $00				; $37: ill
.byte OP_FLAG				; $38: sec
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $39: and_absy
.byte $00				; $3a: ill
.byte $00				; $3b: ill
.byte $00				; $3c: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $3d: and_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $3e: rol_absx
.byte $00				; $3f: ill
.byte OP_STACK|OP_LOAD|OP_PC|OP_FLAG	; $40: rti
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $41: eor_indx
.byte $00				; $42: jam
.byte $00				; $43: ill
.byte $00				; $44: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $45: eor_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $46: lsr_zp
.byte $00				; $47: ill
.byte OP_STACK|OP_STORE			; $48: pha
.byte OP_REG_A|OP_FLAG			; $49: eor_imm
.byte OP_REG_A|OP_FLAG			; $4a: lsr_a
.byte $00				; $4b: ill
.byte OP_PC				; $4c: jmp_abs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $4d: eor_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $4e: lsr_abs
.byte $00				; $4f: ill
.byte OP_PC				; $50: bvc
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $51: eor_indy
.byte $00				; $52: jam
.byte $00				; $53: ill
.byte $00				; $54: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $55: eor_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $56: lsr_zpx
.byte $00				; $57: ill
.byte OP_FLAG				; $58: cli
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $59: eor_absy
.byte $00				; $5a: ill
.byte $00				; $5b: ill
.byte $00				; $5c: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $5d: eor_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $5e: lsr_absx
.byte $00				; $5f: ill
.byte OP_STACK|OP_LOAD|OP_PC		; $60: rts
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $61: adc_indx
.byte $00				; $62: jam
.byte $00				; $63: ill
.byte $00				; $64: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $65: adc_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $66: ror_zp
.byte $00				; $67: ill
.byte OP_STACK|OP_LOAD|OP_REG_A|OP_FLAG	; $68: pla
.byte OP_REG_A|OP_FLAG			; $69: adc_imm
.byte OP_REG_A|OP_FLAG			; $6a: ror_a
.byte $00				; $6b: ill
.byte OP_PC|OP_LOAD			; $6c: jmp_ind
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $6d: adc_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $6e: ror_abs
.byte $00				; $6f: ill
.byte OP_PC				; $70: bvs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $71: adc_indy
.byte $00				; $72: jam
.byte $00				; $73: ill
.byte $00				; $74: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $75: adc_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $76: ror_zpx
.byte $00				; $77: ill
.byte OP_FLAG				; $78: sei
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $79: adc_absy
.byte $00				; $7a: ill
.byte $00				; $7b: ill
.byte $00				; $7c: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $7d: adc_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $7e: ror_absx
.byte $00				; $7f: ill
.byte $00				; $80: ill
.byte OP_STORE|OP_REG_A			; $81: sta_indx
.byte $00				; $82: ill
.byte $00				; $83: ill
.byte OP_STORE|OP_REG_Y			; $84: sty_zp
.byte OP_STORE|OP_REG_A			; $85: sta_zp
.byte OP_STORE|OP_REG_X			; $86: stx_zp
.byte $00				; $87: ill
.byte OP_REG_Y|OP_FLAG			; $88: dey
.byte $00				; $89: ill
.byte OP_REG_A|OP_FLAG			; $8a: txa
.byte $00				; $8b: ill
.byte OP_STORE|OP_REG_Y			; $8c: sty_abs
.byte OP_STORE|OP_REG_A			; $8d: sta_abs
.byte OP_STORE|OP_REG_X			; $8e: stx_abs
.byte $00				; $8f: ill
.byte OP_PC				; $90: bcc
.byte OP_STORE|OP_REG_A			; $91: sta_indy
.byte $00				; $92: jam
.byte $00				; $93: ill
.byte OP_STORE|OP_REG_Y			; $94: sty_zpx
.byte OP_STORE|OP_REG_A			; $95: sta_zpx
.byte OP_STORE|OP_REG_X			; $96: stx_zpy
.byte $00				; $97: ill
.byte OP_REG_A|OP_FLAG			; $98: tya
.byte OP_STORE|OP_REG_A			; $99: sta_absy
.byte OP_REG_X				; $9a: txs
.byte $00				; $9b: ill
.byte $00				; $9c: ill
.byte OP_STORE|OP_REG_A			; $9d: sta_absx
.byte $00				; $9e: ill
.byte $00				; $9f: ill
.byte OP_REG_Y|OP_FLAG			; $a0: ldy_imm
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $a1: lda_indx
.byte OP_REG_X|OP_FLAG			; $a2: ldx_imm
.byte $00				; $a3: ill
.byte OP_LOAD|OP_REG_Y|OP_FLAG		; $a4: ldy_zp
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $a5: lda_zp
.byte OP_LOAD|OP_REG_X|OP_FLAG		; $a6: ldx_zp
.byte $00				; $a7: ill
.byte OP_REG_Y|OP_FLAG			; $a8: tay
.byte OP_REG_A|OP_FLAG			; $a9: lda_imm
.byte OP_REG_X|OP_FLAG			; $aa: tax
.byte $00				; $ab: ill
.byte OP_LOAD|OP_REG_Y|OP_FLAG		; $ac: ldy_abs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $ad: lda_abs
.byte OP_LOAD|OP_REG_X|OP_FLAG		; $ae: ldx_abs
.byte $00				; $af: ill
.byte OP_PC				; $b0: bcs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $b1: lda_indy
.byte $00				; $b2: jam
.byte $00				; $b3: ill
.byte OP_LOAD|OP_REG_Y|OP_FLAG		; $b4: ldy_zpx
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $b5: lda_zpx
.byte OP_LOAD|OP_REG_X|OP_FLAG		; $b6: ldx_zpy
.byte $00				; $b7: ill
.byte OP_FLAG				; $b8: clv
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $b9: lda_absy
.byte OP_REG_X|OP_FLAG			; $ba: tsx
.byte $00				; $bb: ill
.byte OP_LOAD|OP_REG_Y|OP_FLAG		; $bc: ldy_absx
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $bd: lda_absx
.byte OP_LOAD|OP_REG_X|OP_FLAG		; $be: ldx_absy
.byte $00				; $bf: ill
.byte OP_FLAG				; $c0: cpy_imm
.byte OP_LOAD|OP_FLAG			; $c1: cmp_indx
.byte $00				; $c2: ill
.byte $00				; $c3: ill
.byte OP_LOAD|OP_FLAG			; $c4: cpy_zp
.byte OP_LOAD|OP_FLAG			; $c5: cmp_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $c6: dec_zp
.byte $00				; $c7: ill
.byte OP_REG_Y|OP_FLAG			; $c8: iny
.byte OP_FLAG				; $c9: cmp_imm
.byte OP_REG_X|OP_FLAG			; $ca: dex
.byte $00				; $cb: ill
.byte OP_LOAD|OP_FLAG			; $cc: cpy_abs
.byte OP_LOAD|OP_FLAG			; $cd: cmp_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $ce: dec_abs
.byte $00				; $cf: ill
.byte OP_PC				; $d0: bne
.byte OP_LOAD|OP_FLAG			; $d1: cmp_indy
.byte $00				; $d2: jam
.byte $00				; $d3: ill
.byte $00				; $d4: ill
.byte OP_LOAD|OP_FLAG			; $d5: cmp_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $d6: dec_zpx
.byte $00				; $d7: ill
.byte OP_FLAG				; $d8: cld
.byte OP_LOAD|OP_FLAG			; $d9: cmp_absy
.byte $00				; $da: ill
.byte $00				; $db: ill
.byte $00				; $dc: ill
.byte OP_LOAD|OP_FLAG			; $dd: cmp_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $de: dec_absx
.byte $00				; $df: ill
.byte OP_FLAG				; $e0: cpx_imm
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $e1: sbc_indx
.byte $00				; $e2: ill
.byte $00				; $e3: ill
.byte OP_LOAD|OP_FLAG			; $e4: cpx_zp
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $e5: sbc_zp
.byte OP_LOAD|OP_STORE|OP_FLAG		; $e6: inc_zp
.byte $00				; $e7: ill
.byte OP_REG_X|OP_FLAG			; $e8: inx
.byte OP_REG_A|OP_FLAG			; $e9: sbc_imm
.byte $00				; $ea: nop
.byte $00				; $eb: ill
.byte OP_LOAD|OP_FLAG			; $ec: cpx_abs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $ed: sbc_abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $ee: inc_abs
.byte $00				; $ef: ill
.byte OP_PC				; $f0: beq
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $f1: sbc_indy
.byte $00				; $f2: jam
.byte $00				; $f3: ill
.byte $00				; $f4: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $f5: sbc_zpx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $f6: inc_zpx
.byte $00				; $f7: ill
.byte OP_FLAG				; $f8: sed
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $f9: sbc_absy
.byte $00				; $fa: ill
.byte $00				; $fb: ill
.byte $00				; $fc: ill
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $fd: sbc_absx
.byte OP_LOAD|OP_STORE|OP_FLAG		; $fe: inc_absx
.byte $00				; $ff: ill

cycles_tab:
.byte 0, 6, 0, 2	; $00: brk,ora_indx,jam,ill
.byte 2, 3, 5, 2	; $04: ill,ora_zp,asl_zp,ill
.byte 3, 2, 2, 2	; $08: php,ora_imm,asl_a,ill
.byte 2, 4, 6, 2	; $0c: ill,ora_abs,asl_abs,ill
.byte 0, 5, 0, 2	; $10: bpl,ora_indy,jam,ill
.byte 2, 4, 6, 2	; $14: ill,ora_zpx,asl_zpx,ill
.byte 2, 4, 2, 2	; $18: clc,ora_absy,ill,ill
.byte 2, 4, 7, 2	; $1c: ill,ora_absx,asl_absx,ill
.byte 6, 6, 0, 2	; $20: jsr,and_indx,jam,ill
.byte 3, 3, 5, 2	; $24: bit_zp,and_zp,rol_zp,ill
.byte 4, 2, 2, 2	; $28: plp,and_imm,rol_a,ill
.byte 4, 4, 6, 2	; $2c: bit_abs,and_abs,rol_abs,ill
.byte 0, 5, 0, 2	; $30: bmi,and_indy,jam,ill
.byte 2, 4, 6, 2	; $34: ill,and_zpx,rol_zpx,ill
.byte 2, 4, 2, 2	; $38: sec,and_absy,ill,ill
.byte 2, 4, 7, 2	; $3c: ill,and_absx,rol_absx,ill
.byte 6, 6, 0, 2	; $40: rti,eor_indx,jam,ill
.byte 2, 3, 5, 2	; $44: ill,eor_zp,lsr_zp,ill
.byte 3, 2, 2, 2	; $48: pha,eor_imm,lsr_a,ill
.byte 3, 4, 6, 2	; $4c: jmp_abs,eor_abs,lsr_abs,ill
.byte 0, 5, 0, 2	; $50: bvc,eor_indy,jam,ill
.byte 2, 4, 6, 2	; $54: ill,eor_zpx,lsr_zpx,ill
.byte 2, 4, 2, 2	; $58: cli,eor_absy,ill,ill
.byte 2, 4, 7, 2	; $5c: ill,eor_absx,lsr_absx,ill
.byte 6, 6, 0, 2	; $60: rts,adc_indx,jam,ill
.byte 2, 3, 5, 2	; $64: ill,adc_zp,ror_zp,ill
.byte 4, 2, 2, 2	; $68: pla,adc_imm,ror_a,ill
.byte 5, 4, 6, 2	; $6c: jmp_ind,adc_abs,ror_abs,ill
.byte 0, 5, 0, 2	; $70: bvs,adc_indy,jam,ill
.byte 2, 4, 6, 2	; $74: ill,adc_zpx,ror_zpx,ill
.byte 2, 4, 2, 2	; $78: sei,adc_absy,ill,ill
.byte 2, 4, 7, 2	; $7c: ill,adc_absx,ror_absx,ill
.byte 2, 6, 2, 2	; $80: ill,sta_indx,ill,ill
.byte 3, 3, 3, 2	; $84: sty_zp,sta_zp,stx_zp,ill
.byte 2, 2, 2, 2	; $88: dey,ill,txa,ill
.byte 4, 4, 4, 2	; $8c: sty_abs,sta_abs,stx_abs,ill
.byte 0, 6, 0, 2	; $90: bcc,sta_indy,jam,ill
.byte 4, 4, 4, 2	; $94: sty_zpx,sta_zpx,stx_zpy,ill
.byte 2, 5, 2, 2	; $98: tya,sta_absy,txs,ill
.byte 2, 5, 2, 2	; $9c: ill,sta_absx,ill,ill
.byte 2, 6, 2, 2	; $a0: ldy_imm,lda_indx,ldx_imm,ill
.byte 3, 3, 3, 2	; $a4: ldy_zp,lda_zp,ldx_zp,ill
.byte 2, 2, 2, 2	; $a8: tay,lda_imm,tax,ill
.byte 4, 4, 4, 2	; $ac: ldy_abs,lda_abs,ldx_abs,ill
.byte 0, 5, 0, 2	; $b0: bcs,lda_indy,jam,ill
.byte 4, 4, 4, 2	; $b4: ldy_zpx,lda_zpx,ldx_zpy,ill
.byte 2, 4, 2, 2	; $b8: clv,lda_absy,tsx,ill
.byte 4, 4, 4, 2	; $bc: ldy_absx,lda_absx,ldx_absy,ill
.byte 2, 6, 2, 2	; $c0: cpy_imm,cmp_indx,ill,ill
.byte 3, 3, 5, 2	; $c4: cpy_zp,cmp_zp,dec_zp,ill
.byte 2, 2, 2, 2	; $c8: iny,cmp_imm,dex,ill
.byte 4, 4, 6, 2	; $cc: cpy_abs,cmp_abs,dec_abs,ill
.byte 0, 5, 0, 2	; $d0: bne,cmp_indy,jam,ill
.byte 2, 4, 6, 2	; $d4: ill,cmp_zpx,dec_zpx,ill
.byte 2, 4, 2, 2	; $d8: cld,cmp_absy,ill,ill
.byte 2, 4, 7, 2	; $dc: ill,cmp_absx,dec_absx,ill
.byte 2, 6, 2, 2	; $e0: cpx_imm,sbc_indx,ill,ill
.byte 3, 3, 5, 2	; $e4: cpx_zp,sbc_zp,inc_zp,ill
.byte 2, 2, 2, 2	; $e8: inx,sbc_imm,nop,ill
.byte 4, 4, 6, 2	; $ec: cpx_abs,sbc_abs,inc_abs,ill
.byte 0, 5, 0, 2	; $f0: beq,sbc_indy,jam,ill
.byte 2, 4, 6, 2	; $f4: ill,sbc_zpx,inc_zpx,ill
.byte 2, 4, 2, 2	; $f8: sed,sbc_absy,ill,ill
.byte 2, 4, 7, 2	; $fc: ill,sbc_absx,inc_absx,ill

;******************************************************************************
; TRACE
; Repeatedly executes steps in the 6502 simulator until a JAM or BRK is
; encountered (or the caller interrupts via an NMI/IRQ)
; Checks the "stop_tracing" flag to determine if such an interrupt occurred.
.proc trace
	; map the user's $2000-$8000 banks into BLK1/2/3 so that
	; vmem_load/vmem_store can access them directly.
	lda #VMEM_BLK1_BANK
	sta $9ff8
	lda #VMEM_BLK2_BANK
	sta $9ffa
	lda #VMEM_BLK3_BANK
	sta $9ffc

:	jsr step
	lda stop_tracing
	bne @done
	bcc :-
	rts

@done:	clc
	rts
.endproc

;******************************************************************************
; STEP
; Executes one step of the 6502 simulator
.proc step
	lda #$00
	sta __sim_branch_taken
	sta __sim_jammed
	sta __sim_at_brk
	sta __sim_vital_addr_clobbered
	sta __sim_illegal
	sta step_cycles

	ldxy __sim_pc
	jsr vmem_load
	sta __sim_op
	tax
	lda affected_tab,x
	sta __sim_affected
	lda cycles_tab,x		; base cycles (0 for variable-cycle opcodes)
	jsr add_cycles			; X is preserved
	lda htab_lo,x
	sta r0
	lda htab_hi,x
	sta r1

	jsr @go				; execute the handler

	; .C set if the step failed (BRK, JAM, or illegal opcode encountered)
	lda __sim_at_brk
	ora __sim_jammed
	ora __sim_vital_addr_clobbered
	ora __sim_illegal
	cmp #$01
.ifdef vic20
	bcs @done			; step failed; skip VIA update
	jsr update_vias			; tick timers, dispatch IRQ/NMI
	clc
@done:
.endif
	rts

@go:
	jmp (r0)
.endproc

;******************************************************************************
; ADD_CYCLES
; Updates the stopwatch by the given value
; IN:
;   - .A: amount to add to the stopwatch
.proc add_cycles
	pha
	clc
	adc step_cycles
	sta step_cycles		; update the current STEP's cycle count
	pla
	clc
	adc __sim_stopwatch
	sta __sim_stopwatch
	bcc :+
	inc __sim_stopwatch+1
	bne :+
	inc __sim_stopwatch+2
:   rts
.endproc

;******************************************************************************
; UPD_NZ
; Update N and Z bits of __sim_reg_p from current hardware flags
.proc update_nz
	php
	pla
	and #$82		; N(bit7) + Z(bit1)
	sta r2
	lda __sim_reg_p
	and #$7d		; clear N and Z
	ora r2
	sta __sim_reg_p
	rts
.endproc

;******************************************************************************
; UPD_NZC
; Update N, Z, C bits of __sim_reg_p from current hardware flags
.proc update_nzc
	php
	pla
	and #$83		; N(7) + Z(1) + C(0)
	sta r2
	lda __sim_reg_p
	and #$7c
	ora r2
	sta __sim_reg_p
	rts
.endproc

;******************************************************************************
; UPD_NZVC
; Update N, Z, V, C bits of __sim_reg_p from current hardware flags
.proc update_nzvc
	php
	pla
	and #$c3		; N(7) + V(6) + Z(1) + C(0)
	sta r2
	lda __sim_reg_p
	and #$3c
	ora r2
	sta __sim_reg_p
	rts
.endproc

;******************************************************************************
; READ PC
; Read virtual memory byte at __sim_pc + .A (offset 1 or 2)
; Clobbers .X .Y (vmem_load restores .Y on return); clobbers r0
; IN:
;   - A: offset from __sim_pc to read
; OUT:
;   - A: byte read from __sim_pc+offset
.proc read_pc
	clc
	adc __sim_pc
	tax
	lda #0
	adc __sim_pc+1
	tay
	jmp vmem_load
.endproc

;******************************************************************************
; ADVANCE1/2/3
; Advances __sim_pc by 1, 2, or 3 bytes
.proc advance1
	inc __sim_pc
	bne :+
	inc __sim_pc+1
:   rts
.endproc

.proc advance2
	lda __sim_pc
	clc
	adc #2
	sta __sim_pc
	bcc :+
	inc __sim_pc+1
:   rts
.endproc

.proc advance3
	lda __sim_pc
	clc
	adc #3
	sta __sim_pc
	bcc :+
	inc __sim_pc+1
:   rts
.endproc

;******************************************************************************
; FETCH_EA - vmem_load from __sim_effective_addr, returns byte in .A
.proc fetch_ea
	ldxy __sim_effective_addr
	jmp vmem_load
.endproc

;******************************************************************************
; STORE_EA - vmem_store .A to __sim_effective_addr
.proc store_ea
	ldxy __sim_effective_addr
	jmp vmem_store
.endproc

;******************************************************************************
; RMW_DONE - after a shift/rotate with result in .A: capture N/Z/C, write back
.proc rmw_done
	pha
	jsr update_nzc
	pla
	jmp store_ea
.endproc

;******************************************************************************
; ADDRESSING MODE RESOLVERS
; Each sets __sim_effective_addr, __sim_op_mode, __sim_operand, advances PC.
; Indexed modes return .C set if a page boundary is crossed.
;******************************************************************************

;******************************************************************************
; AM IMM
; Immediate mode address resolver
.proc am_imm
	lda #1
	jsr read_pc
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	lda __sim_pc
	clc
	adc #1
	sta __sim_effective_addr
	lda __sim_pc+1
	adc #0
	sta __sim_effective_addr+1
	lda #MODE_IMMEDIATE
	sta __sim_op_mode
	jmp advance2
.endproc

;******************************************************************************
; AM ZP
; Zeropage mode address resolver
.proc am_zp
	lda #1
	jsr read_pc
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	sta __sim_effective_addr+1
	lda __sim_operand
	sta __sim_effective_addr
	lda #MODE_ZP
	sta __sim_op_mode
	jmp advance2
.endproc

;******************************************************************************
; AM ZPX
; Zeropage,X mode address resolver
.proc am_zpx
	lda #1
	jsr read_pc
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	clc
	lda __sim_operand
	adc __sim_reg_x
	sta __sim_effective_addr
	lda #0
	sta __sim_effective_addr+1
	lda #MODE_ZP|MODE_X_INDEXED
	sta __sim_op_mode
	jmp advance2
.endproc

;******************************************************************************
; AM ZPY
; Zeropage,Y mode address resolver
.proc am_zpy
	lda #1
	jsr read_pc
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	clc
	lda __sim_operand
	adc __sim_reg_y
	sta __sim_effective_addr
	lda #0
	sta __sim_effective_addr+1
	lda #MODE_ZP|MODE_Y_INDEXED
	sta __sim_op_mode
	jmp advance2
.endproc

;******************************************************************************
; AM ABS
; ABS mode address resolver
.proc am_abs
	lda #1
	jsr read_pc
	sta __sim_operand
	sta __sim_effective_addr
	lda #2
	jsr read_pc
	sta __sim_operand+1
	sta __sim_effective_addr+1
	lda #MODE_ABS
	sta __sim_op_mode
	jmp advance3
.endproc

;******************************************************************************
; AM ABS,X
; ABS,X mode address resolver
; OUT:
;   - .C: set if page boundary crossed
.proc am_absx
	lda #1
	jsr read_pc
	sta __sim_operand
	clc
	adc __sim_reg_x
	sta __sim_effective_addr
	lda #0
	rol				; A = page-cross flag (0 or 1)
	sta r3
	lda #2
	jsr read_pc
	sta __sim_operand+1
	clc
	adc r3
	sta __sim_effective_addr+1
	lda #MODE_ABS|MODE_X_INDEXED
	sta __sim_op_mode
	jsr advance3
	lsr r3				; .C = page cross
	rts
.endproc

;******************************************************************************
; AM ABS,Y
; ABS,Y mode address resolver
; OUT:
;   - .C: set if page boundary crossed
.proc am_absy
	lda #1
	jsr read_pc
	sta __sim_operand
	clc
	adc __sim_reg_y
	sta __sim_effective_addr
	lda #0
	rol
	sta r3
	lda #2
	jsr read_pc
	sta __sim_operand+1
	clc
	adc r3
	sta __sim_effective_addr+1
	lda #MODE_ABS|MODE_Y_INDEXED
	sta __sim_op_mode
	jsr advance3
	lsr r3
	rts
.endproc

;******************************************************************************
; AM INDX
; (ZP),X mode address resolver
.proc am_indx
	lda #1
	jsr read_pc			; A = ZP base byte
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	clc
	lda __sim_operand
	adc __sim_reg_x			; + X, wraps in ZP
	sta r4				; r4 = ZP index
	ldy #0				; hi byte of ZP addr = $00
	ldx r4
	jsr vmem::load			; A = virtual ZP[r4]
	sta __sim_effective_addr
	inc r4
	ldx r4				; .Y restored to 0 by vmem::load
	jsr vmem::load			; A = virtual ZP[r4+1]
	sta __sim_effective_addr+1
	lda #MODE_ZP|MODE_X_INDEXED|MODE_INDIRECT
	sta __sim_op_mode
	jmp advance2
.endproc

;******************************************************************************
; AM INDY
; (ZP),Y mode address resolver
; OUT:
;   - .C: set if page boundary crossed
.proc am_indy
	lda #1
	jsr read_pc			; A = ZP pointer byte
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	lda __sim_operand
	sta r4				; r4 = ZP pointer address
	ldy #0
	ldx r4
	jsr vmem::load			; A = virtual ZP[r4] = base addr lo
	clc
	adc __sim_reg_y
	sta __sim_effective_addr
	lda #0
	rol				; page-cross flag
	sta r3
	inc r4
	ldx r4				; .Y restored to 0 by vmem::load
	jsr vmem::load			; A = virtual ZP[r4+1] = base addr hi
	clc
	adc r3
	sta __sim_effective_addr+1
	lda #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	sta __sim_op_mode
	jsr advance2
	lsr r3				; .C = page cross
	rts
.endproc

;******************************************************************************
; VIRTUAL STACK HELPERS - use vmem::store/load so BLK5 (SIM bank) is untouched
;******************************************************************************
vpush:				; push .A onto virtual stack at $01SP, dec SP
	ldy #1				; stack page hi = $01
	ldx __sim_reg_sp
	sty __sim_effective_addr+1	; record access so watches see stack ops
	stx __sim_effective_addr	; (and don't fire on a stale address)
	jsr vmem::store			; .A preserved by vmem::store; ldy/ldx don't touch .A
	dec __sim_reg_sp
	rts

vpull:				; inc SP, pull byte from virtual stack into .A
	inc __sim_reg_sp
	ldy #1
	ldx __sim_reg_sp
	sty __sim_effective_addr+1	; record access so watches see stack ops
	stx __sim_effective_addr
	jmp vmem::load

;******************************************************************************
; BRANCH HELPER
; IN: .A = condition (0 = not taken, nonzero = taken)
;******************************************************************************
do_branch:
	beq @not_taken
	inc __sim_branch_taken
	lda __sim_pc
	clc
	adc #2
	sta __sim_next_pc
	lda __sim_pc+1
	adc #0
	sta __sim_next_pc+1
	lda #1
	jsr read_pc			; signed offset byte
	sta r2
	lda r2				; reload: vmem_done's ldy savey clobbers N flag
	bpl @pos
	lda #$ff
	bne @ext
@pos:
	lda #0
@ext:
	sta r3				; sign extension byte
	lda __sim_next_pc
	clc
	adc r2
	sta __sim_pc
	lda __sim_next_pc+1
	adc r3
	sta __sim_pc+1
	lda __sim_pc+1
	cmp __sim_next_pc+1
	beq :+
	lda #4
	jmp add_cycles
:   lda #3
	jmp add_cycles

@not_taken:
	jsr advance2
	lda #2
	jmp add_cycles

;******************************************************************************
; HANDLERS
;******************************************************************************

;-------------------------------------------------------------------------------
h_brk:
	inc __sim_at_brk
	rts

h_jam:
	inc __sim_illegal
	inc __sim_jammed
	rts

h_ill:
	inc __sim_illegal
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_nop:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

;******************************************************************************
; ORA
;******************************************************************************
h_ora_indx:
	jsr am_indx
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_zp:
	jsr am_zp
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_imm:
	jsr am_imm
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_abs:
	jsr am_abs
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_zpx:
	jsr am_zpx
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

;******************************************************************************
; AND
;******************************************************************************
h_and_indx:
	jsr am_indx
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_zp:
	jsr am_zp
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_imm:
	jsr am_imm
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_abs:
	jsr am_abs
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_zpx:
	jsr am_zpx
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

;******************************************************************************
; EOR
;******************************************************************************
h_eor_indx:
	jsr am_indx
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_zp:
	jsr am_zp
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_imm:
	jsr am_imm
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_abs:
	jsr am_abs
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_zpx:
	jsr am_zpx
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

;******************************************************************************
; ADC
;******************************************************************************
h_adc_indx:
	jsr am_indx
	jsr fetch_ea
	jmp do_adc

h_adc_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_adc

h_adc_imm:
	jsr am_imm
	jsr fetch_ea
	jmp do_adc

h_adc_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_adc

h_adc_zpx:
	jsr am_zpx
	jsr fetch_ea
	jmp do_adc

h_adc_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

h_adc_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

h_adc_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

do_adc:
	sta r4
	lda __sim_reg_p
	ora #$24			; force I=1 before plp
	pha
	lda __sim_reg_a
	plp
	adc r4
	cld			; don't leak virtual D flag to the host
	pha
	jsr update_nzvc
	pla
	sta __sim_reg_a
	rts

;******************************************************************************
; SBC
;******************************************************************************
h_sbc_indx:
	jsr am_indx
	jsr fetch_ea
	jmp do_sbc

h_sbc_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_sbc

h_sbc_imm:
	jsr am_imm
	jsr fetch_ea
	jmp do_sbc

h_sbc_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_sbc

h_sbc_zpx:
	jsr am_zpx
	jsr fetch_ea
	jmp do_sbc

h_sbc_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_sbc

h_sbc_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_sbc

h_sbc_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_sbc

do_sbc:
	sta r4
	lda __sim_reg_p
	ora #$24
	pha
	lda __sim_reg_a
	plp
	sbc r4
	cld			; don't leak virtual D flag to the host
	pha
	jsr update_nzvc
	pla
	sta __sim_reg_a
	rts

;******************************************************************************
; CMP
;******************************************************************************
h_cmp_indx:
	jsr am_indx
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_imm:
	jsr am_imm
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_zpx:
	jsr am_zpx
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_cmp_a

do_cmp_a:
	sta r4
	lda __sim_reg_a
	cmp r4
	jmp update_nzc

;******************************************************************************
; CPX
;******************************************************************************
h_cpx_imm:
	jsr am_imm
	jsr fetch_ea
	jmp do_cmp_x

h_cpx_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_cmp_x

h_cpx_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_cmp_x

do_cmp_x:
	sta r4
	lda __sim_reg_x
	cmp r4
	jmp update_nzc

;******************************************************************************
; CPY
;******************************************************************************
h_cpy_imm:
	jsr am_imm
	jsr fetch_ea
	jmp do_cmp_y

h_cpy_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_cmp_y

h_cpy_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_cmp_y

do_cmp_y:
	sta r4
	lda __sim_reg_y
	cmp r4
	jmp update_nzc

;******************************************************************************
; LDA
;******************************************************************************
h_lda_indx:
	jsr am_indx
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_zp:
	jsr am_zp
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_imm:
	jsr am_imm
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_abs:
	jsr am_abs
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_zpx:
	jsr am_zpx
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_indy:
	jsr am_indy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

;******************************************************************************
; LDX
;******************************************************************************
h_ldx_imm:
	jsr am_imm
	jsr fetch_ea
	sta __sim_reg_x
	jmp update_nz

h_ldx_zp:
	jsr am_zp
	jsr fetch_ea
	sta __sim_reg_x
	jmp update_nz

h_ldx_abs:
	jsr am_abs
	jsr fetch_ea
	sta __sim_reg_x
	jmp update_nz

h_ldx_zpy:
	jsr am_zpy
	jsr fetch_ea
	sta __sim_reg_x
	jmp update_nz

h_ldx_absy:
	jsr am_absy
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_x
	jmp update_nz

;******************************************************************************
; LDY
;******************************************************************************
h_ldy_imm:
	jsr am_imm
	jsr fetch_ea
	sta __sim_reg_y
	jmp update_nz

h_ldy_zp:
	jsr am_zp
	jsr fetch_ea
	sta __sim_reg_y
	jmp update_nz

h_ldy_abs:
	jsr am_abs
	jsr fetch_ea
	sta __sim_reg_y
	jmp update_nz

h_ldy_zpx:
	jsr am_zpx
	jsr fetch_ea
	sta __sim_reg_y
	jmp update_nz

h_ldy_absx:
	jsr am_absx
	lda #0
	adc #0				; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_y
	jmp update_nz

;******************************************************************************
; STA
;******************************************************************************
h_sta_indx:
	jsr am_indx
	lda __sim_reg_a
	jmp store_ea

h_sta_zp:
	jsr am_zp
	lda __sim_reg_a
	jmp store_ea

h_sta_abs:
	jsr am_abs
	lda __sim_reg_a
	jmp store_ea

h_sta_zpx:
	jsr am_zpx
	lda __sim_reg_a
	jmp store_ea

h_sta_absy:
	jsr am_absy
	lda __sim_reg_a
	jmp store_ea

h_sta_absx:
	jsr am_absx
	lda __sim_reg_a
	jmp store_ea

h_sta_indy:
	jsr am_indy
	lda __sim_reg_a
	jmp store_ea

;******************************************************************************
; STX
;******************************************************************************
h_stx_zp:
	jsr am_zp
	lda __sim_reg_x
	jmp store_ea

h_stx_abs:
	jsr am_abs
	lda __sim_reg_x
	jmp store_ea

h_stx_zpy:
	jsr am_zpy
	lda __sim_reg_x
	jmp store_ea

;******************************************************************************
; STY
;******************************************************************************
h_sty_zp:
	jsr am_zp
	lda __sim_reg_y
	jmp store_ea

h_sty_abs:
	jsr am_abs
	lda __sim_reg_y
	jmp store_ea

h_sty_zpx:
	jsr am_zpx
	lda __sim_reg_y
	jmp store_ea

;******************************************************************************
; ASL - Arithmetic Shift Left; N, Z, C updated (C = old bit 7)
;******************************************************************************
h_asl_a:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_a
	asl
	pha
	jsr update_nzc
	pla
	sta __sim_reg_a
	jmp advance1

h_asl_zp:
	jsr am_zp
	jsr fetch_ea
	asl
	jmp rmw_done

h_asl_abs:
	jsr am_abs
	jsr fetch_ea
	asl
	jmp rmw_done

h_asl_zpx:
	jsr am_zpx
	jsr fetch_ea
	asl
	jmp rmw_done

h_asl_absx:
	jsr am_absx
	jsr fetch_ea
	asl
	jmp rmw_done

;******************************************************************************
; LSR - Logical Shift Right; N=0, Z, C updated (C = old bit 0)
;******************************************************************************
h_lsr_a:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_a
	lsr
	pha
	jsr update_nzc
	pla
	sta __sim_reg_a
	jmp advance1

h_lsr_zp:
	jsr am_zp
	jsr fetch_ea
	lsr
	jmp rmw_done

h_lsr_abs:
	jsr am_abs
	jsr fetch_ea
	lsr
	jmp rmw_done

h_lsr_zpx:
	jsr am_zpx
	jsr fetch_ea
	lsr
	jmp rmw_done

h_lsr_absx:
	jsr am_absx
	jsr fetch_ea
	lsr
	jmp rmw_done

;******************************************************************************
; ROL - Rotate Left through Carry; plp sets carry from virtual P
;******************************************************************************
h_rol_a:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_p
	ora #$24
	pha
	lda __sim_reg_a
	plp
	rol
	cld			; don't leak virtual D flag to the host
	pha
	jsr update_nzc
	pla
	sta __sim_reg_a
	jmp advance1

h_rol_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_rol_mem

h_rol_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_rol_mem

h_rol_zpx:
	jsr am_zpx
	jsr fetch_ea
	jmp do_rol_mem

h_rol_absx:
	jsr am_absx
	jsr fetch_ea
	jmp do_rol_mem

do_rol_mem:
	sta r4
	lda __sim_reg_p
	ora #$24
	pha
	lda r4
	plp
	rol
	cld			; don't leak virtual D flag to the host
	jsr rmw_done
	rts

;******************************************************************************
; ROR - Rotate Right through Carry
;******************************************************************************
h_ror_a:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_p
	ora #$24
	pha
	lda __sim_reg_a
	plp
	ror
	cld			; don't leak virtual D flag to the host
	pha
	jsr update_nzc
	pla
	sta __sim_reg_a
	jmp advance1

h_ror_zp:
	jsr am_zp
	jsr fetch_ea
	jmp do_ror_mem

h_ror_abs:
	jsr am_abs
	jsr fetch_ea
	jmp do_ror_mem

h_ror_zpx:
	jsr am_zpx
	jsr fetch_ea
	jmp do_ror_mem

h_ror_absx:
	jsr am_absx
	jsr fetch_ea
	jmp do_ror_mem

do_ror_mem:
	sta r4
	lda __sim_reg_p
	ora #$24
	pha
	lda r4
	plp
	ror
	cld			; don't leak virtual D flag to the host
	jsr rmw_done
	rts

;******************************************************************************
; INC - Increment memory; updates N, Z only
;******************************************************************************
h_inc_zp:
	jsr am_zp
	jsr fetch_ea
	clc
	adc #1
	jmp do_inc_done

h_inc_abs:
	jsr am_abs
	jsr fetch_ea
	clc
	adc #1
	jmp do_inc_done

h_inc_zpx:
	jsr am_zpx
	jsr fetch_ea
	clc
	adc #1
	jmp do_inc_done

h_inc_absx:
	jsr am_absx
	jsr fetch_ea
	clc
	adc #1
	jmp do_inc_done

do_inc_done:
	pha
	jsr update_nz
	pla
	jmp store_ea

;******************************************************************************
; DEC - Decrement memory; updates N, Z only
;******************************************************************************
h_dec_zp:
	jsr am_zp
	jsr fetch_ea
	sec
	sbc #1
	jmp do_inc_done

h_dec_abs:
	jsr am_abs
	jsr fetch_ea
	sec
	sbc #1
	jmp do_inc_done

h_dec_zpx:
	jsr am_zpx
	jsr fetch_ea
	sec
	sbc #1
	jmp do_inc_done

h_dec_absx:
	jsr am_absx
	jsr fetch_ea
	sec
	sbc #1
	jmp do_inc_done

;******************************************************************************
; INX, INY, DEX, DEY
;******************************************************************************
h_inx:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	inc __sim_reg_x
	lda __sim_reg_x
	jsr update_nz
	jmp advance1

h_iny:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	inc __sim_reg_y
	lda __sim_reg_y
	jsr update_nz
	jmp advance1

h_dex:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	dec __sim_reg_x
	lda __sim_reg_x
	jsr update_nz
	jmp advance1

h_dey:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	dec __sim_reg_y
	lda __sim_reg_y
	jsr update_nz
	jmp advance1

;******************************************************************************
; BIT - N=mem[7], V=mem[6], Z=(A AND mem)==0
; Uses "bit r4" trick: store memory byte in r4=$f4, then hardware BIT r4.
;******************************************************************************
h_bit_zp:
	jsr am_zp
	jsr fetch_ea
	sta r4
	lda __sim_reg_a
	bit r4				; BIT $f4 - reads ZP[$f4]=r4, sets N,V,Z
	php
	pla
	and #$c2			; N(7) + V(6) + Z(1)
	sta r2
	lda __sim_reg_p
	and #$3d
	ora r2
	sta __sim_reg_p
	rts

h_bit_abs:
	jsr am_abs
	jsr fetch_ea
	sta r4
	lda __sim_reg_a
	bit r4
	php
	pla
	and #$c2
	sta r2
	lda __sim_reg_p
	and #$3d
	ora r2
	sta __sim_reg_p
	rts

;******************************************************************************
; REGISTER TRANSFERS
;******************************************************************************
h_tax:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_a
	sta __sim_reg_x
	jsr update_nz
	jmp advance1

h_tay:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_a
	sta __sim_reg_y
	jsr update_nz
	jmp advance1

h_txa:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_x
	sta __sim_reg_a
	jsr update_nz
	jmp advance1

h_tya:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_y
	sta __sim_reg_a
	jsr update_nz
	jmp advance1

h_txs:				; does NOT affect flags
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_x
	sta __sim_reg_sp
	jmp advance1

h_tsx:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_sp
	sta __sim_reg_x
	jsr update_nz
	jmp advance1

;******************************************************************************
; FLAG OPERATIONS
;******************************************************************************
h_clc:
	lda __sim_reg_p
	and #$fe
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_sec:
	lda __sim_reg_p
	ora #$01
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_clv:
	lda __sim_reg_p
	and #$bf
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_cld:
	lda __sim_reg_p
	and #$f7
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_sed:
	lda __sim_reg_p
	ora #$08
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_cli:
	lda __sim_reg_p
	and #$fb
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

h_sei:
	lda __sim_reg_p
	ora #$04
	sta __sim_reg_p
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jmp advance1

;******************************************************************************
; STACK OPERATIONS
;******************************************************************************
h_pha:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_a
	jsr vpush
	jmp advance1

h_pla:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jsr vpull
	sta __sim_reg_a
	jsr update_nz
	jmp advance1

h_php:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	lda __sim_reg_p
	ora #$30			; set B and unused bits on push
	jsr vpush
	jmp advance1

h_plp:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jsr vpull
	ora #$30			; bit5 (UNUSED) always 1; bit4 (BREAK) reads as 1
	sta __sim_reg_p
	jmp advance1

;******************************************************************************
; BRANCHES
;******************************************************************************
h_bpl:
	lda __sim_reg_p
	and #$80
	eor #$80			; nonzero when N=0 (branch taken)
	jmp do_branch

h_bmi:
	lda __sim_reg_p
	and #$80
	jmp do_branch

h_bvc:
	lda __sim_reg_p
	and #$40
	eor #$40
	jmp do_branch

h_bvs:
	lda __sim_reg_p
	and #$40
	jmp do_branch

h_bcc:
	lda __sim_reg_p
	and #$01
	eor #$01
	jmp do_branch

h_bcs:
	lda __sim_reg_p
	and #$01
	jmp do_branch

h_bne:
	lda __sim_reg_p
	and #$02
	eor #$02
	jmp do_branch

h_beq:
	lda __sim_reg_p
	and #$02
	jmp do_branch

;******************************************************************************
; JUMPS AND CALLS
;******************************************************************************
h_jmp_abs:
	lda #MODE_ABS
	sta __sim_op_mode
	lda #1
	jsr read_pc		; read lo byte while __sim_pc still intact
	sta __sim_operand
	sta r4			; stash lo in r4 - do NOT touch __sim_pc yet
	lda #2
	jsr read_pc		; read hi byte (uses original __sim_pc + 2)
	sta __sim_operand+1
	sta __sim_pc+1
	lda r4
	sta __sim_pc
	rts

h_jmp_ind:			; 6502 page-boundary bug emulated
	lda #1
	jsr read_pc
	sta __sim_operand
	sta r4
	lda #2
	jsr read_pc
	sta __sim_operand+1
	sta r5
	ldxy r4
	jsr vmem_load
	sta __sim_pc
	inc r4			; wraps to $00 if r4 was $FF (hardware bug)
	ldxy r4
	jsr vmem_load
	sta __sim_pc+1
	lda #MODE_ABS|MODE_INDIRECT
	sta __sim_op_mode
	rts

h_jsr:
	lda #MODE_ABS
	sta __sim_op_mode
	lda #1
	jsr read_pc
	sta r4				; target lo
	lda #2
	jsr read_pc
	sta r5				; target hi
	; push return address (PC+2) - hi byte first
	lda __sim_pc
	clc
	adc #2
	sta r2				; (PC+2) lo
	lda __sim_pc+1
	adc #0
	sta r3				; (PC+2) hi
	lda r3
	jsr vpush
	lda r2
	jsr vpush
	lda r4
	sta __sim_operand
	lda r5
	sta __sim_operand+1
	sta __sim_pc+1
	lda r4
	sta __sim_pc
	rts

h_rts:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jsr vpull			; lo byte
	sta r2
	jsr vpull			; hi byte
	sta r3
	lda r2
	clc
	adc #1
	sta __sim_pc
	lda r3
	adc #0
	sta __sim_pc+1
	rts

h_rti:
	lda #MODE_IMPLIED
	sta __sim_op_mode

	; if we are in a simulated interrupt handler, flag that this RTI
	; returns from it (step-out/step-over must not count it as an RTS)
	ldx #$00
	lda __sim_irq_depth
	beq :+
	dec __sim_irq_depth
	inx
:	stx __sim_rti_irq

	jsr vpull			; P
	ora #$30			; bit5 (UNUSED) always 1; bit4 (BRK) == 1
	sta __sim_reg_p
	jsr vpull			; PC lo
	sta __sim_pc
	jsr vpull			; PC hi
	sta __sim_pc+1
	rts

.ifdef vic20
;******************************************************************************
; VIA EMULATION
; The VIA registers ($9110-$912f) are shadowed by the simulator so that the
; timers can be emulated.  Each STEP ticks the timers down by the number of
; cycles that the executed instruction took.  When an enabled timer interrupt
; is flagged, the simulator dispatches an NMI (VIA1) or IRQ (VIA2) exactly as
; the 6502 would.
;******************************************************************************

;******************************************************************************
; VIA READ
; Reads one of the shadowed VIA registers, applying any side effect defined
; by the 6522 for the read (e.g. acknowledging a timer interrupt)
; IN:
;   - .X: LSB of the register address ($10-$2f)
;   - .Y: MSB of the register address ($91)
; OUT:
;   - .A: the register value
;   - .X, .Y: preserved
.proc via_read
	txa
	pha			; save address LSB
	and #$1f
	tax			; .X = offset into the shadow registers
	and #$0f		; .A = register number
	cmp #via_t1cl
	beq @t1cl
	cmp #via_t2cl
	beq @t2cl
	cmp #via_ifr
	beq @ifr
	cmp #via_ier
	beq @ier
	lda vias,x		; no side effects; return the shadow value
	jmp @done

@t1cl:	; reading the T1 counter lo byte acknowledges the T1 interrupt
	lda vias+(via_ifr-via_t1cl),x
	and #$ff-$40
	sta vias+(via_ifr-via_t1cl),x
	lda vias,x
	jmp @done

@t2cl:	; reading the T2 counter lo byte acknowledges the T2 interrupt
	lda vias+(via_ifr-via_t2cl),x
	and #$ff-$20
	sta vias+(via_ifr-via_t2cl),x
	lda vias,x
	jmp @done

@ier:	lda vias,x
	ora #$80		; IER bit 7 always reads back as set
	jmp @done

@ifr:	; IFR bit 7 reads as set if any enabled interrupt is flagged
	lda vias+(via_ier-via_ifr),x
	and vias,x
	and #$7f
	beq :+
	lda vias,x
	ora #$80
	bne @done		; branch always
:	lda vias,x
	and #$7f

@done:	sta viatmp
	pla
	tax			; restore address LSB
	ldy #>$9100		; restore address MSB
	lda viatmp
	rts
.endproc

;******************************************************************************
; VIA WRITE
; Applies a write to one of the shadowed VIA registers, handling the side
; effects defined by the 6522 (timer loads, interrupt acks, etc.)
; The caller must also perform the store to virtual memory (the shadows are
; only authoritative while the simulator is running)
; IN:
;   - .A: the value to write
;   - .X: LSB of the register address ($10-$2f)
;   - .Y: MSB of the register address ($91)
; OUT:
;   - .A, .X, .Y: preserved
.proc via_write
	sta viatmp		; save the value to write
	txa
	pha			; save address LSB
	and #$1f
	tax			; .X = offset into the shadow registers
	and #$0f		; .A = register number
	cmp #via_t1cl
	beq @t1cl
	cmp #via_t1ch
	beq @t1ch
	cmp #via_t1ll
	beq @t1ll
	cmp #via_t1lh
	beq @t1lh
	cmp #via_t2cl
	bne :+
	jmp @t2cl
:	cmp #via_t2ch
	bne :+
	jmp @t2ch
:	cmp #via_ifr
	bne :+
	jmp @ifr
:	cmp #via_ier
	bne :+
	jmp @ier

:	; ordinary register: just store the value to the shadow
	lda viatmp
	sta vias,x
	jmp @done

@t1cl:	; a write to the T1 counter lo byte is redirected to the lo latch
	lda viatmp
	sta vias+(via_t1ll-via_t1cl),x
	jmp @done

@t1ll:	lda viatmp
	sta vias,x
	jmp @done

@t1lh:	; writing the T1 latch hi byte also acknowledges the T1 interrupt
	lda viatmp
	sta vias,x
	lda vias+(via_ifr-via_t1lh),x
	and #$ff-$40
	sta vias+(via_ifr-via_t1lh),x
	jmp @done

@t1ch:	; write the hi latch, transfer the latch to the counter, acknowledge
	; the T1 interrupt, and (re)arm the timer
	lda viatmp
	sta vias+(via_t1lh-via_t1ch),x
	sta vias,x
	lda vias+(via_t1ll-via_t1ch),x
	sta vias+(via_t1cl-via_t1ch),x
	lda vias+(via_ifr-via_t1ch),x
	and #$ff-$40
	sta vias+(via_ifr-via_t1ch),x
	jsr via_idx
	lda #$01
	sta via_t1_armed,y
	jmp @done

@t2cl:	; a write to the T2 counter lo byte is buffered in the T2 latch
	jsr via_idx
	lda viatmp
	sta via_t2_latch,y
	jmp @done

@t2ch:	; load the counter, acknowledge the T2 interrupt, and arm the timer
	lda viatmp
	sta vias,x
	jsr via_idx
	lda via_t2_latch,y
	sta vias+(via_t2cl-via_t2ch),x
	lda #$01
	sta via_t2_armed,y
	lda vias+(via_ifr-via_t2ch),x
	and #$ff-$20
	sta vias+(via_ifr-via_t2ch),x
	jmp @done

@ifr:	; writing a 1 to an interrupt flag clears it
	lda viatmp
	and #$7f
	eor #$ff
	and vias,x
	sta vias,x
	jmp @done

@ier:	; bit 7 selects whether the written bits are set (1) or cleared (0)
	bit viatmp
	bpl @ierclr
	lda viatmp
	and #$7f
	ora vias,x
	sta vias,x
	jmp @done
@ierclr:
	lda viatmp
	and #$7f
	eor #$ff
	and vias,x
	sta vias,x

@done:	pla
	tax			; restore address LSB
	ldy #>$9100		; restore address MSB
	lda viatmp		; restore the stored value
	rts
.endproc

;******************************************************************************
; VIA IDX
; Returns the index for the per-VIA state arrays (via_t1_armed etc.)
; IN:
;   - .X: offset into the shadow registers ($00-$1f)
; OUT:
;   - .Y: 0 for VIA1, 1 for VIA2
.proc via_idx
	txa
	and #$10
	beq :+
	lda #$01
:	tay
	rts
.endproc

;*******************************************************************************
; UPDATE VIAS
; Ticks both VIAs' timers down by the number of cycles that the instruction
; just executed took and, if any enabled interrupt is flagged, dispatches an
; NMI (VIA1) or IRQ (VIA2)
.proc update_vias
	ldx #$00		; VIA1
	jsr tick_via
	ldx #$10		; VIA2
	jsr tick_via

	; check for an NMI (VIA1): edge-triggered on the line going active
	lda __sim_via1+via_ifr
	and __sim_via1+via_ier
	and #$7f
	beq @nonmi
	lda nmi_prev
	bne @chkirq		; NMI line was already active; no new edge
	inc nmi_prev
	ldxy #$fffa		; NMI vector
	jmp do_interrupt	; dispatch the NMI (any IRQ remains pending)

@nonmi:	lda #$00
	sta nmi_prev

@chkirq:
	; check for an IRQ (VIA2): level-triggered, masked by the I flag
	lda __sim_reg_p
	and #$04
	bne @done		; I flag set; IRQs are masked
	lda __sim_via2+via_ifr
	and __sim_via2+via_ier
	and #$7f
	beq @done
	ldxy #$fffe		; IRQ vector
	jmp do_interrupt
@done:	rts
.endproc

;*******************************************************************************
; TICK VIA
; Counts the given VIA's timers down by the current STEP's cycle count and
; flags any timer interrupt that occurs
; IN:
;   - .X: offset of the VIA to update ($00 = VIA1, $10 = VIA2)
.proc tick_via
	jsr via_idx		; .Y = index for the per-VIA state arrays

	; count T1 down by the number of cycles the instruction took
	lda vias+via_t1cl,x
	sec
	sbc step_cycles
	sta vias+via_t1cl,x
	lda vias+via_t1ch,x
	sbc #$00
	sta vias+via_t1ch,x
	bcs @t2			; no underflow

	; T1 underflowed
	lda vias+via_acr,x
	and #$40		; free-run mode?
	beq @oneshot

	; free-run: reload the counter from the latch and flag the interrupt
	lda vias+via_t1cl,x
	clc
	adc vias+via_t1ll,x
	sta vias+via_t1cl,x
	lda vias+via_t1ch,x
	adc vias+via_t1lh,x
	sta vias+via_t1ch,x
	jmp @t1flag

@oneshot:
	; one-shot: only flag the interrupt the first time the timer expires
	lda via_t1_armed,y
	beq @t2
	lda #$00
	sta via_t1_armed,y

@t1flag:
	lda vias+via_ifr,x
	ora #$40
	sta vias+via_ifr,x

@t2:	; T2 only counts cycles in one-shot mode (ACR bit 5 clear)
	lda vias+via_acr,x
	and #$20
	bne @done		; T2 counts PB6 pulses; not emulated

	lda vias+via_t2cl,x
	sec
	sbc step_cycles
	sta vias+via_t2cl,x
	lda vias+via_t2ch,x
	sbc #$00
	sta vias+via_t2ch,x
	bcs @done		; no underflow

	; T2 underflowed: flag the interrupt if it hasn't fired yet
	lda via_t2_armed,y
	beq @done
	lda #$00
	sta via_t2_armed,y
	lda vias+via_ifr,x
	ora #$20
	sta vias+via_ifr,x
@done:	rts
.endproc

;*******************************************************************************
; DO INTERRUPT
; Performs the 6502's interrupt sequence: pushes the PC and status (with the
; BREAK flag clear), sets the I flag, and loads the PC from the given vector.
; Takes 7 cycles, which are added to the stopwatch.
; IN:
;   - .XY: address of the vector to jump through ($fffa or $fffe)
.proc do_interrupt
@vec=r2
	stxy @vec

	inc __sim_irq_depth	; track handler depth (see h_rti)

	; push the return PC and the status
	lda __sim_pc+1
	jsr vpush
	lda __sim_pc
	jsr vpush
	lda __sim_reg_p
	and #$ef		; BRK flag is pushed clear
	ora #$20		; unused bit is pushed set
	jsr vpush

	; mask interrupts while in the handler
	lda __sim_reg_p
	ora #$04
	sta __sim_reg_p

	; load the PC from the interrupt vector
	ldxy @vec
	jsr vmem_load
	sta __sim_pc
	incw @vec
	ldxy @vec
	jsr vmem_load
	sta __sim_pc+1

	lda #7			; the interrupt sequence takes 7 cycles
	jmp add_cycles
.endproc
.endif	; vic20

;*******************************************************************************
; VMEM LOAD
; Loads a byte from virtual memory.  If we are tracing, this may be the
; physical address
; IN:
;   - .XY: (virtual) address to load from
; OUT:
;   - .A: byte loaded from the requested address
.proc vmem_load
@target=r0
.ifdef vic20
	; redirect reads of the VIA registers ($9110-$912f) to their shadows
	cpy #>$9100
	bne @notvia
	cpx #$10
	bcc @notvia
	cpx #$30
	bcs @notvia
	jmp via_read
@notvia:
.endif
.ifdef ultimem
	lsr tracing
	bcc @v			; not tracing, always use virtual mem
	rol tracing		; reset tracing flag

	; check if address is in $1000-$8000 or $9400-$9800, load directly if
	; so (this range is not virtualized during tracing so that the user
	; can see what's happening and for a bit of a speed bonus)
	cpy #>$1000
	bcc @v
	cpy #>$9000
	bcc @phy		; [$1000, $7fff]
	bne :+

	; check VIC range ($9000-$9010)
	cpx #<$9010
	bcc @phy

:	cpy #>$9400
	bcc @v
	cpy #>$9800
	bcs @v

@phy:	; [$1000, $7fff] or [$9400, $9800]
	stxy @target
	ldy #$00
	lda (@target),y
	pha
	ldy @target+1
	pla			; restore .A to set flags
	rts
.endif

@v:	jmp vmem::load ; not in the visible range, load from virtual memory
.endproc

;*******************************************************************************
; VMEM STORE
; Stores a byte from virtual memory.  If we are tracing, this may be the
; physical address
; IN:
;   - .XY: (virtual) address to store to
;   - .A:  byte to store
.proc vmem_store
@target=r0
.ifdef ultimem
	; mirror writes to the VIA registers ($9110-$912f) into their shadows
	; to update the simulated timer state; the write then falls through to
	; virtual memory as usual so the stored value is also visible there
	cpy #>$9100
	bne @notvia
	cpx #$10
	bcc @notvia
	cpx #$30
	bcs @notvia
	jsr via_write		; .A, .X, .Y are preserved

@notvia:
	; check if target address is ok
	; writes to IO2/3 ($9800-$9fff) and $316-$319 are not allowed
	cpy #$98
	bcc :+
	cpy #$a0
	bcc @err

:	; check the LSB for $316-$319
	cpy #$03
	bne @ok
	cpx #$16
	bcc @ok
	cpx #$1a	; $31a+ (KERNAL vectors) may be legitimately revectored
	bcs @ok

@err:	; an important memory location will be clobbered
	inc __sim_vital_addr_clobbered
	sec
	rts

;-------------------------------------------------------------------------------
@ok:	lsr tracing
	bcc @v			; not tracing, always use virtual mem
	rol tracing		; reset tracing flag

	; check if address is in $1000-$8000, $9000-$9010, $9400-$9800,
	; store directly if so (faster and change is visible on screen)
	cpy #>$1000
	bcc @v
	cpy #>$9000
	bcc @phy		; [$1000, $7fff]
	bne :+

	; check VIC range ($9000-$9010)
	cpx #<$9010
	bcc @phy

:	cpy #>$9400
	bcc @v
	cpy #>$9800
	bcs @v

@phy:	; [$1000, $7fff] or [$9400, $9800]
	stxy @target
	ldy #$00
	sta (@target),y
	ldy @target+1
	rts
.endif

@v:	jmp vmem::store	; not in the visible range, store to virtual memory
.endproc
