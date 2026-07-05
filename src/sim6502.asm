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
.include "vic20/expansion.inc"  ; FINAL_BANK_SIM, FINAL_BANK_FASTCOPY
.endif

.BSS

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

; stopwatch of cycles counted by simulator since last reset
.export __sim_stopwatch
__sim_stopwatch: .res 3

.export __sim_via2
__sim_via2: .res $10

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

	ldxy __sim_pc
	jsr vmem_load
	sta __sim_op
	tax
	lda affected_tab,x
	sta __sim_affected
	lda cycles_tab,x            ; base cycles (0 for variable-cycle opcodes)
	jsr add_cycles              ; X is preserved
	lda htab_lo,x
	sta r0
	lda htab_hi,x
	sta r1

	jsr @go                     ; execute the handler

	; .C set if the step failed (BRK or JAM encountered)
	lda __sim_at_brk
	ora __sim_jammed
	ora __sim_vital_addr_clobbered
	cmp #$01
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
	and #$82                ; N(bit7) + Z(bit1)
	sta r2
	lda __sim_reg_p
	and #$7d                ; clear N and Z
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
	and #$83                ; N(7) + Z(1) + C(0)
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
	and #$c3                ; N(7) + V(6) + Z(1) + C(0)
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
	jsr store_ea
	rts
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
	rol                         ; A = page-cross flag (0 or 1)
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
	lsr r3                      ; .C = page cross
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
	jsr read_pc                    ; A = ZP base byte
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	clc
	lda __sim_operand
	adc __sim_reg_x             ; + X, wraps in ZP
	sta r4                      ; r4 = ZP index
	ldy #0                      ; hi byte of ZP addr = $00
	ldx r4
	jsr vmem::load              ; A = virtual ZP[r4]
	sta __sim_effective_addr
	inc r4
	ldx r4                      ; .Y restored to 0 by vmem::load
	jsr vmem::load              ; A = virtual ZP[r4+1]
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
	jsr read_pc                    ; A = ZP pointer byte
	sta __sim_operand
	lda #0
	sta __sim_operand+1
	lda __sim_operand
	sta r4                      ; r4 = ZP pointer address
	ldy #0
	ldx r4
	jsr vmem::load              ; A = virtual ZP[r4] = base addr lo
	clc
	adc __sim_reg_y
	sta __sim_effective_addr
	lda #0
	rol                         ; page-cross flag
	sta r3
	inc r4
	ldx r4                      ; .Y restored to 0 by vmem::load
	jsr vmem::load              ; A = virtual ZP[r4+1] = base addr hi
	clc
	adc r3
	sta __sim_effective_addr+1
	lda #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	sta __sim_op_mode
	jsr advance2
	lsr r3                      ; .C = page cross
	rts
.endproc

;******************************************************************************
; VIRTUAL STACK HELPERS - use vmem::store/load so BLK5 (SIM bank) is untouched
;******************************************************************************
vpush:                          ; push .A onto virtual stack at $01SP, dec SP
	ldy #1                      ; stack page hi = $01
	ldx __sim_reg_sp
	jsr vmem::store             ; .A preserved by vmem::store; ldy/ldx don't touch .A
	dec __sim_reg_sp
	rts

vpull:                          ; inc SP, pull byte from virtual stack into .A
	inc __sim_reg_sp
	ldy #1
	ldx __sim_reg_sp
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
	jsr read_pc                    ; signed offset byte
	sta r2
	lda r2                      ; reload: vmem_done's ldy savey clobbers N flag
	bpl @pos
	lda #$ff
	bne @ext
@pos:
	lda #0
@ext:
	sta r3                      ; sign extension byte
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	ora __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_ora_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	and __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_and_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	eor __sim_reg_a
	sta __sim_reg_a
	jmp update_nz

h_eor_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

h_adc_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

h_adc_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_adc

do_adc:
	sta r4
	lda __sim_reg_p
	ora #$24                    ; force I=1 before plp
	pha
	lda __sim_reg_a
	plp
	adc r4
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_sbc

h_sbc_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_sbc

h_sbc_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	jmp do_cmp_a

h_cmp_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_absy:
	jsr am_absy
	lda #0
	adc #0                      ; +1 cycle if page crossed
	jsr add_cycles
	jsr fetch_ea
	sta __sim_reg_a
	jmp update_nz

h_lda_indy:
	jsr am_indy
	lda #0
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
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
	adc #0                      ; +1 cycle if page crossed
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
	jsr store_ea
	rts

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
	bit r4                      ; BIT $f4 - reads ZP[$f4]=r4, sets N,V,Z
	php
	pla
	and #$c2                    ; N(7) + V(6) + Z(1)
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

h_txs:                          ; does NOT affect flags
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
	ora #$30                    ; set B and unused bits on push
	jsr vpush
	jmp advance1

h_plp:
	lda #MODE_IMPLIED
	sta __sim_op_mode
	jsr vpull
	ora #$30                    ; bit5 (UNUSED) always 1; bit4 (BREAK) reads as 1
	sta __sim_reg_p
	jmp advance1

;******************************************************************************
; BRANCHES
;******************************************************************************
h_bpl:
	lda __sim_reg_p
	and #$80
	eor #$80                    ; nonzero when N=0 (branch taken)
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
	jsr vpull                   ; lo byte
	sta r2
	jsr vpull                   ; hi byte
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
	jsr vpull			; P
	ora #$30			; bit5 (UNUSED) always 1; bit4 (BRK) == 1
	sta __sim_reg_p
	jsr vpull			; PC lo
	sta __sim_pc
	jsr vpull			; PC hi
	sta __sim_pc+1
	rts

;******************************************************************************
; VMEM LOAD
; Loads a byte from virtual memory.  If we are tracing, this may be the
; physical address
; IN:
;   - .XY: (virtual) address to load from
; OUT:
;   - .A: byte loaded from the requested address
.proc vmem_load
@target=r0
.ifdef ultimem
	lsr tracing
	bcc @v			; not tracing, always use virtual mem
	rol tracing		; reset tracing flag

	; check if address is in $1000-$8000 or $9400-$9800, load directly if
	; so (this range is not virtualized during tracing so that the user
	; can see what's happening and for a bit of a speed bonus)
	cpy #>$1000
	bcc @v
	cpy #>$8000
	bcc @phy		; [$1000, $7fff]
	cpy #>$9400
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

;******************************************************************************
; VMEM STORE
; Stores a byte from virtual memory.  If we are tracing, this may be the
; physical address
; IN:
;   - .XY: (virtual) address to store to
;   - .A:  byte to store
.proc vmem_store
@target=r0
.ifdef ultimem
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
	cpx #$20
	bcs @ok

@err:	; an important memory location will be clobbered
	inc __sim_vital_addr_clobbered
	sec
	rts

;-------------------------------------------------------------------------------
@ok:	lsr tracing
	bcc @v			; not tracing, always use virtual mem
	rol tracing		; reset tracing flag

	; check if address is in $1000-$8000 or $9400-$9800, store directly if
	; so (so that the change is visible on screen and for speed)
	cpy #>$1000
	bcc @v
	cpy #>$8000
	bcc @phy		; [$1000, $7fff]
	cpy #>$9400
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
