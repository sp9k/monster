;*******************************************************************************
; SIM6502.ASM
; This file contains the code to simulate the 6502. This is used to
; determine what memory/registers are affected by an instruction, count the
; number of cycles that instructions execute, and to determine what the
; next instruction that will be executed as.
;*******************************************************************************

.include "asmflags.inc"
.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "target.inc"
.include "vmem.inc"
.include "zeropage.inc"
.include "vic20/vaddrs.inc"

.import STEP_HANDLER_ADDR

.import is_internal_address
.import write_step

;*******************************************************************************
; SIMBSS
; This segment must not be clobbered by the simulator when stepping.
; The effective address and "msave" value are used to save/restore internal
; memory state.
; The following platforms handle this in various ways:
;  - vic 20 (FE3): this segment is in the "MAIN" bank and is banked out when
;                  executing a step
;  - vic 20 (Ultimem): this segment is in the I/O 2/3 2/3 space, which is
;                      considered "dangerous" and the simulator will prohibit
;                      clobbering.
;  - c64: this segment is DMA swapped by the REU for each step
;         (TODO: no it's not)
;.segment "SIMBSS"
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

save_sp: .byte 0

; the debugger's value for the byte that was swapped in for a step (if any)
msave: .byte 0

.segment "STEP_EPILOGUE"
;*******************************************************************************
; STEP DONE
; Return point for instruction execution via STEP
.export step_done
.proc step_done
	; update registers state
	sta __sim_reg_a
	stx __sim_reg_x
	sty __sim_reg_y

	lda __sim_reg_p
	and #$04		; get .I flag (and clear all other flags)
	sta __sim_reg_p
	pla			; get status register
	ora __sim_reg_p		; OR real .I value (always clear during step)
	sta __sim_reg_p

	tsx
	stx __sim_reg_sp

	ldx save_sp
	txs

	cld

; if the effective address was INTERNAL, restore the debugger's byte at
; that address
	lda __sim_affected
	and #OP_LOAD|OP_STORE
	beq :+			; if memory wansn't used -> skip

	ldxy __sim_effective_addr
	jsr is_internal_address
	bne :+			; if address wasn't "swapped in" -> skip

.ifdef ultimem
	jsr exp::get_byte
	ldxy __sim_effective_addr
.else
	stxy @msave_src
@msave_src=*+1
	lda $f00d		; get the affected user byte
.endif
	jsr vmem::store		; save it to its buffer

	; restore the byte that was clobbered at the effective address
	ldxy __sim_effective_addr
	stxy r0
	ldy #$00
	lda msave
	sta (r0),y

:	ldx save_sp
	txs

.ifdef ultimem
	jsr ultim::reset_blk5
.endif

	cli
	RETURN_OK
.endproc

.segment "DEBUGGER"

;*******************************************************************************
; STEP
; Runs the next instruction
; IN:
;  - .XY: the address of the current instruction
;  - .A:  the size of the current instruction
; OUT:
;  - __sim_op:     the opcode that was executed
;  - __sim_jammed: set if the opcode is a JAM
; TODO: handle interrupts (VIA timers)
.export __sim_step
.proc __sim_step
@op=r0
@sz=r2
@cnt=r3
@y=r3
@msb=r4
@opcode=r5
@operand=r6
@backup=r8
	sta @sz
	stxy @op

.ifdef ultimem
	jsr ultim::select_prog00
.endif
	jsr @getnextpc
	bcc @ok

.ifdef ultimem
	jsr ultim::reset_blk5
.endif
	rts

@ok:	; check if opcode uses indexed/indrect addressing
	stxy __sim_next_pc

	lda __sim_op_mode
	cmp #(MODE_ZP | MODE_X_INDEXED | MODE_INDIRECT)	; x, ind?
	beq @translate_ind
	cmp #(MODE_ZP | MODE_Y_INDEXED | MODE_INDIRECT)	; y, ind?
	bne @write_instruction

@translate_ind:
	; rewrite the opcode as an absolute one (change the bbb bits to 011)
	lda __sim_op
	and #%11100011	; clear bbb
	ora #%00001100	; set to %011 (absolute)
	sta __sim_op

	; rewrite the operand with the effective address
	lda __sim_effective_addr
	sta __sim_operand
	lda __sim_effective_addr+1
	sta __sim_operand+1
	lda #$03
	sta @sz

@write_instruction:
	; copy the instruction to the execution buffer, appending
	; NOPs as needed to fill the 3 byte space
	ldx @sz
	jsr write_step

@execute:
	sei
	TRACE_OFF

	lda __sim_affected
	and #OP_LOAD|OP_STORE
	beq @memdone

.if .defined(c64)
.import STEP_MEMORY_VALUE
.import STEP_EFFECTIVE_ADDR
	ldxy __sim_effective_addr
	stxy STEP_EFFECTIVE_ADDR
	jsr is_internal_address
	bne @memdone		; not internal, no need to swap address
	stxy @backup
	jsr vmem::load
	sta STEP_MEMORY_VALUE
	ldy #$00
	lda (@backup),y
	sta msave		; save debugger byte
.elseif .defined(vic20)
	ldxy __sim_effective_addr
	jsr is_internal_address
	bne @memdone		; not internal, no need to swap address

	; swap the debugger/user bytes
	; save the value at the effective address and replace it with the user
	; byte
	stxy @backup
	jsr vmem::load
	tax
	ldy #$00
	lda (@backup),y
	sta msave		; save debugger byte
	txa
	sta (@backup),y		; store user byte
.endif

@memdone:
	; restore registers for instruction execution
	tsx
	stx save_sp

	ldx __sim_reg_sp
	txs			; restore user stack pointer
	lda __sim_reg_p
	pha			; push status flags
	lda __sim_reg_a
	ldx __sim_reg_x
	ldy __sim_reg_y

	; execute the instruction
	pha
	jmp STEP_HANDLER_ADDR

;---------------------------------------
; GETNEXTPC
; Returns the next PC value given the current state of the simulator
; Also updates simulator flags based on that next instruction
@getnextpc:
	lda #$00
	sta __sim_branch_taken		; clear branch taken flag
	sta __sim_jammed		; clear JAM'd flag
	sta __sim_at_brk		; clear BRK flag
	sta __sim_vital_addr_clobbered	; clear dangerous write flag

	; get the opcode
	ldxy @op
	jsr vmem::load
	sta @opcode
	sta __sim_op

	; get the operand (LSB)
	ldxy @op
	lda #$01
	jsr vmem::load_off
	sta @operand
	sta __sim_operand

	; operand operand MSB
	ldxy @op
	lda #$02
	jsr vmem::load_off
	sta @operand+1
	sta __sim_operand+1

;------------------
; if we're writing, check if it is safe to do so to the effective addr
@chkaddr:
	lda __sim_affected
	and #OP_STORE
	beq @chkjam
	jsr is_write_safe
	bcc @chkjam
	ldxy __sim_pc		; return original PC (processor jammed)
	sec			; flag error
	rts

;------------------
; check if opcode to execute will JAM the CPU
@chkjam:
	lda @opcode
	jsr @isjam
	bne @nojam
@jam:	inc __sim_jammed
	ldxy __sim_pc		; return original PC (processor jammed)
	sec			; flag error
	rts

@nojam:	cmp #$20		; JSR?
	bne :+
@jsr:	; fully simulate JSR
	; "push" the return address-1 and then set the PC to the operand
	ldy __sim_reg_sp
	lda __sim_pc
	clc
	adc #$02
	sta prog00+$100-1,y	; store LSB of return address
	lda __sim_pc+1
	adc #$00
	sta prog00+$100,y	; store MSB of return address
	decw __sim_reg_sp	; SP -= 2
	decw __sim_reg_sp
	lda @operand
	sta __sim_pc
	lda @operand+1
	sta __sim_pc+1
	jmp @step_handled

:	cmp #$4c
	bne :+
@jmp:	; fully simulate JMP
	; set the PC to the operand
	lda __sim_operand
	sta __sim_pc
	lda __sim_operand+1
	sta __sim_pc+1
	jmp @step_handled

:	cmp #$6c	; JMP (ind)?
	bne :+
@jmpind:
	; fully simulate JMP (ind):
	; load the value at the operand, and set the PC to it
	ldxy @operand
	jsr vmem::load		; get the LSB
	sta __sim_pc
	incw @operand
	ldxy @operand
	jsr vmem::load		; get the MSB
	sta __sim_pc+1
	jmp @step_handled

:	cmp #$60
	bne :+

@rts:   ; fully simulate RTS:
	; pull the return address, add 1 to it, and set it as the new
	; PC. Then add 2 to the stack pointer
	ldy __sim_reg_sp
	lda prog00+$100+1,y
	clc
	adc #$01
	sta __sim_pc
	lda prog00+$100+2,y
	adc #$00
	sta __sim_pc+1
	; SP += 2
	inc __sim_reg_sp
	inc __sim_reg_sp
	jmp @step_handled

:	cmp #$40
	bne :+

@rti:	; fully simulate RTI:
	; "pull" status then return address
	ldy __sim_reg_sp
	lda prog00+$100+1,y
	sta __sim_reg_p
	lda prog00+$100+2,y
	sta __sim_pc
	lda prog00+$100+3,y
	sta __sim_pc+1
	lda __sim_reg_sp
	clc
	adc #$03
	sta __sim_reg_sp
	jmp @step_handled

:	; check if opcode is CLI/SEI
	; we allow these to set the virtual .I flag, but this flag is always
	; SET during simulation to keep processor control
	cmp #$58		; CLI
	bne :+
	lda __sim_reg_p
	and #$04^$ff
	sta __sim_reg_p
	incw __sim_pc
	jmp @step_handled

:	cmp #$78		; SEI
	bne :+
	lda __sim_reg_p
	ora #$04
	sta __sim_reg_p
	incw __sim_pc
	jmp @step_handled

:	cmp #$00		; BRK?
	bne :+

	; BRK is never executed/simulated, we use this to return to the debugger
	inc __sim_at_brk
	;sec
	rts

:	; check if opcode is a relative branch
	and #$1f
	cmp #$10
	beq @branch

; not a simulated instruction: just add the size of the instruction to the PC
; we will execute the instruction in an execution buffer directly
@regular:
	lda __sim_pc
	clc
	adc @sz
	sta __sim_pc
	bcc :+
	inc __sim_pc+1
	clc
:	rts

; Simulate branches. branch is taken if corresponding flag for top two bits
; of branch opcode equal bit 5 of the opcode
; e.g. 11010000 will branch if the Z (zero) flag, which is the flag represented
; by bits 6 & 7 (11) is clear- the 0 in bit 5 in this example.
@branch:
	; get top 2 bits (xx) of branch to get type of branch
	lda @opcode
	asl
	rol
	rol
	and #$03
	tax

	; get the y (bit 5) in the same bit position as the flag we're testing
	lda @opcode
	and #$20
	beq :+			; if bit y is 0, use $00
	lda branch_masks,x	; if bit y is 1, use the mask
:	sta @y

	lda branch_masks,x
	and __sim_reg_p		; isolate the bit we're interested in
	eor @y			; if y != .P[xx], no branch
	beq @takebranch

; branch isn't taken, just add 2 to the PC
@nobranch:
	lda __sim_pc
	clc
	adc #$02
	sta __sim_pc
	bcc :+
	inc __sim_pc+1
:	jmp @step_handled

; branch is taken, add the operand (offset) + 2
@takebranch:
	inc __sim_branch_taken
	lda @operand
	bpl :+
	lda #$ff
	skw
:	lda #$00
	sta @msb

	lda __sim_pc
	clc
	adc @operand
	tax

	lda __sim_pc+1
	adc @msb
	tay

	; add 2 to the encoded offset
	txa
	clc
	adc #$02
	sta __sim_pc
	tya
	adc #$00
	sta __sim_pc+1

;--------------------------------------
@step_handled:
	; eat return address
	pla
	pla

.ifdef ultimem
	jsr ultim::reset_blk5
.endif

	clc
	rts

;--------------------------------------
; IN:  - .A: the opcode to check if JAM
; OUT: - .Z: set if the instruction is a JAM
@isjam:
	ldx #@numjams-1
:	cmp @jamops,x
	beq @done	; if JAM, return (.Z is set)
	dex
	bpl :-
	; .X=$ff (.Z=0)
@done:	rts

.ifndef ultimem
.PUSHSEG
.RODATA
.endif
@jamops:
.byte $02, $12, $22, $32, $42, $52, $62, $72, $92, $B2, $D2, $F2
@numjams=*-@jamops
.ifndef ultimem
.POPSEG
.endif
.endproc

;*******************************************************************************
; GET SIDE EFFECTS
; Checks if the given instruction requires any RAM state and handles the
; creation of any state needed to handle them.
; This essentially involves checking if the instruction accesses any RAM and,
; if it does, settting __sim_effective_addr to the address that will be affected
; IN:
;  - .XY:         address of the binary instruction
;  - .A:          the size of the instruction
;  - sim::opmode: the address modes for the instruction (see asm::disassemble)
; OUT:
;  - __sim_effective_addr: holds the address of the byte that will be
;                          loaded/stored
;  - __sim_affected:       stores the flags with the CPU/mem state the operation
;                          affects
.export __sim_get_side_effects
.proc __sim_get_side_effects
@instruction=r2
@opsz=r4
@target=r5
@opcode=r7
@offset=zp::bankval
	stxy @instruction
	sta @opsz
	pha

	ldxy @instruction
	jsr vmem::load			; opcode
	sta @opcode

	; if 0 byte opcode (no operand), either BRK, PHA, PHP or
	; instruction doesn't write to RAM
	; We don't emulate BRK because it is used by the debugger to
	; maintain control of the program (it is implicitly
	; treated as an instruction written for and by the debugger)
	pla			; restore instruction size
	cmp #$02		; if there's an operand
	bcs @cont		; continue

@check_pha_php:
	lda @opcode
	cmp #$48		; PHA?
	beq @stack_ea
	cmp #$08		; PHP?
	beq @stack_ea
@check_pla_plp:
	cmp #$28		; PLP
	beq @stack_pop
	cmp #$68		; PLA
	bne @get_affected	; no memory affected
@stack_pop:
	ldx __sim_reg_sp
	inx
	stx __sim_effective_addr
	lda #$01
	sta __sim_effective_addr+1
	lda #OP_STACK|OP_LOAD
	sta __sim_affected
	rts

@stack_ea:
	lda __sim_reg_sp
	sta __sim_effective_addr
	lda #$01
	sta __sim_effective_addr+1
	lda #OP_STACK|OP_STORE
	sta __sim_affected
	rts			; done

	; save the debugger's contents at the @instruction address
@cont:	; get the instruction opcode/operand at the @instruction address
	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (1st byte)
	sta @target

	lda #$00
	ldx @opsz
	cpx #$03
	bcc :+
	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (2nd byte)
:	sta @target+1

@get_affected:
	; get the side-effects for this operation
	ldx @opcode
	lda side_effects_tab,x
	sta __sim_affected		; save the side-effects for aux uses

	and #OP_STORE | OP_LOAD		; is operation a load or store?
	beq @done

	; get effective target address of this instruction and save it
	; we will save/restore state before/after a BRK using this
.ifdef ultimem
	jsr ultim::select_prog00
.endif
	jsr get_effective_addr
.ifdef ultimem
	jsr ultim::reset_blk5
.endif
	stxy __sim_effective_addr

@done:	rts
.endproc

;*******************************************************************************
; GET EFFECTIVE ADDR
; Returns the effective address for the given opcode/operand. This may be the
; same as the operand (if the opcode represents a zeropage or absolute address
; mode) or not (indirect or indexed address modes)
; IN:
;  - .XY: the operand
;  - .A:  the opcode
;  - r0:  the address mode of the instruction
; OUT
;  - .XY: the effective address of the instruction
.proc get_effective_addr
@target=r5
	lda __sim_op_mode
	cmp #MODE_X_INDEXED|MODE_ZP|MODE_INDIRECT	; x,ind?
	bne @check_ind_y
@ind_x:
	; add .X register to ZP target to get target address (wrapping is fine)
	; THEN load the address at this target
	ldx __sim_reg_x
	ldy @target
	lda prog00,y
	sta @target
	iny
	lda prog00,x
	sta @target+1
	jmp @get_ind

;--------------------------------------
@check_ind_y:
	lda __sim_op_mode
	cmp #MODE_Y_INDEXED|MODE_ZP|MODE_INDIRECT	; y,ind?
	bne @check_rel_y
	; get the value of the ZP location in the *user* ZP
	ldy @target
	lda prog00,y
	sta @target
	iny
	lda prog00,y
	sta @target+1
	; add the .Y register value to the address from the ZP
	lda __sim_reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

;--------------------------------------
@check_rel_y:
	lda __sim_op_mode
	and #MODE_Y_INDEXED		; y indexed?
	beq @check_rel_x
	; add the value of .Y to the target get the target address
	lda __sim_reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	lda __sim_op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,y clear the MSB of target
	sta @target+1
	beq @done

;--------------------------------------
@check_rel_x:
	lda __sim_op_mode
	and #MODE_X_INDEXED		; x indexed?
	beq @check_ind
	; add the value of .X to get the target address
	lda __sim_reg_x
	clc
	adc @target
	sta @target
	bcc @done
	inc @target+1
	lda __sim_op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,x clear the MSB of target
	sta @target+1
	beq @done

@check_ind:
	lda __sim_op_mode
	cmp #MODE_INDIRECT|MODE_ABS	; JMP (ind) ?
	bne @abs_or_zp
	; read the target of the indirect JMP and set @target to it
@get_ind:
	ldxy @target
	jsr vmem::load
	pha
	incw @target
	ldxy @target
	jsr vmem::load
	sta @target+1
	pla
	sta @target

@abs_or_zp:
@done:	ldxy @target
	RETURN_OK
.endproc

;*******************************************************************************
; COUNT CYCLES
; Counts the number of cycles that the instruction last run via STEP
; used.
; OUT:
;  - .A: the number of cycles the instruction will use
.export __sim_count_cycles
.proc __sim_count_cycles
@cycles=r0
	lda __sim_op
	lsr
	tax
	bcc @l
@r:	lda #$0f
	and timings,x
	bne @fix

@err:	lda #$ff
	sec
	rts

@l:	lda timings,x
	lsr
	lsr
	lsr
	lsr
	beq @err

@fix:	sta @cycles

@chkbra:
	lda __sim_branch_taken
	beq @chkpagecross
	inc @cycles		; +1 cycle if branch taken
	lda __sim_next_pc+1	; get target MSB
	cmp __sim_pc+1		; is the target on the same page?
	bne @penalty		; different page -> 1 MORE cycle penalty
	beq @done

;abs,x abs,y and (zp),y need an extra cycle if indexing crosses page boundary
@chkpagecross:
	; (zp),y
	lda #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	and __sim_op_mode
	cmp #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	beq @chkpage

	; abs,x and abs,y
	lda #MODE_ABS|MODE_X_INDEXED|MODE_Y_INDEXED
	and __sim_op_mode
	beq @done
	and #MODE_ABS
	beq @done
@chkpage:
	lda __sim_effective_addr
	cmp #$ff		; is effective address LSB $ff?
	bne @done		; no penalty if not (no page boundary crossed)
@penalty:
	inc @cycles

@done:	lda @cycles
	rts
.endproc

;*******************************************************************************
; IS WRITE SAFE
; Checks if the simulator's effective address is safe to write
; IN:
;   - .XY: the address to check
; OUT:
;   - .C: set if the address is not safe to write to
.proc is_write_safe
.ifdef vic20
	; on C64, disallow writes to IO2/3 ($9800-$9fff)
	cpy #$98
	bcc :+
	cpy #$a0
	bcc @err

:
.endif

	ldx #@num_safe_addrs-1
@l0:	lda __sim_effective_addr
	cmp @safeaddrs_lo,x
	bne :+
	lda __sim_effective_addr+1
	cmp @safeaddrs_hi,x
	beq @err
:	dex
	bpl @l0

@ok:	RETURN_OK

@err:	; an important memory location will be clobbered
	inc __sim_vital_addr_clobbered
	;sec
	rts

.ifndef ultimem
.PUSHSEG
.RODATA
.endif
.if .defined(vic20)
	.define safety_addrs $0316, $0317, $0318, $0319
.elseif .defined(c64)
	.define safety_addrs $0316, $0317, $0318, $0319, $0000, $0001
.endif
	@safeaddrs_lo: .lobytes safety_addrs
	@safeaddrs_hi: .hibytes safety_addrs
	@num_safe_addrs=*-@safeaddrs_hi
.ifndef ultimem
.POPSEG
.endif
.endproc

.ifndef ultimem
.RODATA
.endif
;*******************************************************************************
; OERATION SIDE EFFECTS TABLE
; This table contains all opcodes and what state they affect.
; This is used to display changes made by a given instruction as well as
; determine what (internal) state the debugger needs to preserve and restore
; between steps.
; If a value in internal memory is accessed (OP_LOAD/OP_STORE) the user
; value for that memory must be swapped in before executing the instruction
side_effects_tab:
; $0-
.byte $00			; $00 BRK
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $01 ORA x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $05 ORA zpg
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $06 ASL zpg
.byte $00			; ---
.byte OP_STACK|OP_STORE		; $08 PHP
.byte OP_REG_A|OP_FLAG		; $09 ORA #
.byte OP_REG_A|OP_FLAG		; ASL A
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $0d ORA abs
.byte OP_LOAD|OP_STORE|OP_FLAG	; $0e ASL abs
.byte $00			; ---

; $1-
.byte OP_PC			; $10 BPL rel
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $11 ORA ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $15 ORA zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $16 ASL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $18 CLC
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $19 ORA abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $1d ORA abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $1e ASL abs,x
.byte $00			; ---

; $2-
.byte OP_PC|OP_STACK|OP_STORE	; $20 JSR abs
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $21 AND x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $24 BIT zpg
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $25 AND zpg
.byte OP_LOAD|OP_STORE|OP_FLAG	; $26 ROL zpg
.byte $00			; ---
.byte OP_FLAG|OP_STACK|OP_LOAD	; $28 PLP
.byte OP_REG_A|OP_FLAG   	; $29 AND #
.byte OP_REG_A|OP_FLAG		; $2a ROL A
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $2c BIT abs
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $2d AND abs
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $2e ROL abs
.byte $00			; ---

; $3-
.byte OP_PC			; $30 BMI rel
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $31 AND ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $35 AND zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $36 ROL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $38 SEC
.byte OP_REG_A|OP_LOAD|OP_FLAG 	; $39 AND abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $3d AND abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $3e ROL abs,x
.byte $00			; ---

; $4-
.byte OP_STACK|OP_PC|OP_LOAD|OP_FLAG	; $40 RTI
.byte OP_REG_A|OP_LOAD|OP_FLAG		; $41 EOR x, ind
.byte $00				; ---
.byte $00				; ---
.byte $00				; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG		; $45 EOR zpg
.byte OP_LOAD|OP_STORE|OP_FLAG 		; $46 LSR zpg
.byte $00				; ---
.byte OP_STACK|OP_STORE			; $48 PHA
.byte OP_REG_A				; $49 EOR #
.byte OP_REG_A                  	; $4a LSR A
.byte $00				; ---
.byte OP_PC				; $4c JMP abs
.byte OP_LOAD|OP_REG_A|OP_FLAG		; $4d EOR abs
.byte OP_LOAD|OP_STORE|OP_FLAG		; $4e LSR abs
.byte $00				; ---

; $5-
.byte OP_PC			; $50 BVC rel
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $51 EOR ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $55 EOR zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $56 LSR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $58 CLI
.byte OP_REG_A|OP_LOAD|OP_FLAG  ; $59 EOR abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $5d EOR abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $5e LSR abs,x
.byte $00			; ---

; $6-
.byte OP_STACK|OP_PC|OP_LOAD	; $60 RTS
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $61 ADC x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $65 ADC zpg
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $66 ROR zpg
.byte $00			; ---
.byte OP_STACK|OP_LOAD|OP_REG_A|OP_FLAG	; $68 PLA
.byte OP_REG_A|OP_FLAG		; $69 ADC #
.byte OP_REG_A|OP_FLAG          ; $6a ROR A
.byte $00			; ---
.byte OP_PC|OP_LOAD		; $4c JMP (ind)
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $4d ADC abs
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $4e ROR abs
.byte $00			; ---

; $7-
.byte OP_PC			; $70 BVS rel
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $71 ADC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $75 ADC zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $76 ROR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $78 SEI
.byte OP_REG_A|OP_LOAD|OP_FLAG 	; $79 ADC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $7d ADC abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG 	; $7e ROR abs,x
.byte $00			; ---

; $8-
.byte $00			; ---
.byte OP_STORE			; $81 STA x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $84 STY zpg
.byte OP_STORE   		; $85 STA zpg
.byte OP_STORE 		        ; $86 STX zpg
.byte $00			; ---
.byte OP_REG_Y|OP_FLAG         	; $88 DEY
.byte $00			; ---
.byte OP_REG_A|OP_FLAG          ; $8a TXA
.byte $00			; ---
.byte OP_STORE			; $8c STY abs
.byte OP_STORE			; $8d STA abs
.byte OP_STORE			; $8e STX abs
.byte $00			; ---

; $9-
.byte OP_PC			; $90 BCC rel
.byte OP_STORE			; $91 STA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $94 STY zpg,x
.byte OP_STORE   		; $95 STA zpg,x
.byte OP_STORE 		        ; $96 STX zpg,y
.byte $00			; ---
.byte OP_REG_A|OP_FLAG         	; $98 TYA
.byte OP_STORE                	; $99 STA abs,Y
.byte OP_STACK			; $9a TXS
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $9d STA abs,x
.byte $00			; ---
.byte $00			; ---

; $A-
.byte OP_REG_Y|OP_FLAG		; $a0 LDY #
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $a1 LDA x,ind
.byte OP_REG_X|OP_FLAG		; $a2 LDX #
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y|OP_FLAG	; $a4 LDY zpg
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $a5 LDA zpg
.byte OP_LOAD|OP_REG_X|OP_FLAG  ; $a6 LDX zpg
.byte $00			; ---
.byte OP_REG_Y|OP_FLAG        	; $a8 TAY
.byte OP_REG_A|OP_FLAG		; $a9 LDA #
.byte OP_REG_X|OP_FLAG          ; $aa TAX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y|OP_FLAG	; $ac LDY abs
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $ad LDA abs
.byte OP_LOAD|OP_REG_X|OP_FLAG	; $ae LDX abs
.byte $00			; ---

; $B-
.byte OP_PC                     ; $b0 BCS rel
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $b1 LDA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y|OP_FLAG	; $b4 LDY zpg,x
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $b5 LDA zpg,x
.byte OP_LOAD|OP_REG_X|OP_FLAG  ; $b6 LDX zpg,y
.byte $00			; ---
.byte OP_FLAG               	; $b8 CLV
.byte OP_REG_A|OP_LOAD|OP_FLAG	; $b9 LDA abs,y
.byte OP_REG_X|OP_FLAG          ; $ba TSX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y|OP_FLAG	; $bc LDY abs,y
.byte OP_LOAD|OP_REG_A|OP_FLAG	; $bd LDA abs,x
.byte OP_LOAD|OP_REG_X|OP_FLAG	; $be LDX abs,y
.byte $00			; ---

; $C-
.byte OP_FLAG                   ; $c0 CPY #
.byte OP_LOAD|OP_FLAG           ; $c1 CMP x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $c4 CPY zpg
.byte OP_LOAD|OP_FLAG 		; $c5 CMP zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $c6 DEC zpg
.byte $00			; ---
.byte OP_REG_Y|OP_FLAG          ; $c8 INY
.byte OP_FLAG                   ; $c9 CMP #
.byte OP_REG_X|OP_FLAG          ; $ca DEX
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $cc CPY abs
.byte OP_FLAG|OP_LOAD     	; $cd CMP abs
.byte OP_LOAD|OP_STORE|OP_FLAG	; $ce DEC abs
.byte $00			; ---

; $D-
.byte OP_PC                     ; $d0 BNE rel
.byte OP_LOAD|OP_FLAG 		; $d1 CMP ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG  		; $d5 CMP zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $d6 DEC zpg,x
.byte $00			; ---
.byte OP_FLAG               	; $d8 CLD
.byte OP_REG_A|OP_STORE|OP_FLAG	; $d9 CMP abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG		; $dd CMP abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $de DEC abs,y
.byte $00			; ---

; $E-
.byte OP_FLAG                   ; $e0 CPX #
.byte OP_LOAD|OP_FLAG|OP_REG_A  ; $e1 SBC x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $e4 CPX zpg
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $e5 SBC zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $e6 INC zpg
.byte $00			; ---
.byte OP_REG_X|OP_FLAG          ; $e8 INX
.byte OP_REG_A|OP_FLAG	        ; $e9 SBC #
.byte $00			; $ea NOP
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $ec CPX abs
.byte OP_FLAG|OP_LOAD|OP_REG_A 	; $ed SBC abs
.byte OP_LOAD|OP_STORE|OP_FLAG	; $ee INC abs
.byte $00			; ---

; $F-
.byte OP_PC                     ; $f0 BEQ rel
.byte OP_LOAD|OP_FLAG|OP_FLAG	; $f1 SBC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A 	; $f5 SBC zpg,x
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $f6 INC zpg,x
.byte $00			; ---
.byte OP_FLAG               	; $f8 SED
.byte OP_REG_A|OP_FLAG|OP_LOAD  ; $f9 SBC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $fd SBC abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $fe INC abs,x
.byte $00			; ---

;*******************************************************************************
; corresponding masks for top 2 bits of opcode to flags in the status register
branch_masks:
.byte $80	; negative
.byte $40	; overflow
.byte $01	; carry
.byte $02	; zero

;*******************************************************************************
; Instruction timing table
; Each byte contains the timing for two instructions beginning at opcodes $00
; and $01 and ending at opcodes $fe and $ff.
; The MSB represents the even opcode (0, 2, etc.) and the LSB the odd one.
; For example, BRK and ORA x,ind (opcodes 0 and 1) are stored as:
;   (BRK cycles << 4) | (ORA x, ind cycles)
timings:
.byte $70|$06	; BRK       | ORA x,ind
.byte $00
.byte $00|$03	; --        | ORA zpg
.byte $50|$00	; ASL zpg   | --
.byte $30|$02	; PHP       | ORA #
.byte $20|$00	; ASL       | --
.byte $00|$04	; --        | ORA abs
.byte $60|$00	; ASL abs   | --

; $10
.byte $20|$05   ; BPL       | ORA ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | ORA zpg,x
.byte $60|$00   ; ASL zpg,x | --
.byte $20|$04   ; CLC       | ORA abs,y
.byte $00	; --        | --
.byte $20|$04   ; CLC       | ORA abs,x
.byte $70|$00   ; ASL abs,x | --

; $20
.byte $60|$06	; JSR       | AND x,ind
.byte $00	; --        | --
.byte $30|$03	; BIT zpg   | AND zpg
.byte $50|$00	; ROL zpg   | --
.byte $40|$02	; PLP       | AND #
.byte $20|$00	; ROL       | --
.byte $40|$04	; BIT abs   | AND abs
.byte $60|$00	; ROL abs   | --

; $30
.byte $20|$05	; BMI       | AND ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | AND zpg,x
.byte $60|$00   ; ROL zpg,x | --
.byte $20|$04	; SEC       | AND abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | AND abs,x
.byte $70|$00   ; ROL abs,x | --

; $40
.byte $60|$06	; RTI       | EOR x,ind
.byte $00	; --        | --
.byte $00|$03	; --        | EOR zpg
.byte $50|$00	; LSR zpg   | --
.byte $30|$02	; PHA       | EOR #
.byte $20|$00	; LSR       | --
.byte $30|$04	; JMP abs   | EOR abs
.byte $60|$00	; LSR abs   |

; $50
.byte $20|$05	; BVC       | EOR ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | EOR zpg,x
.byte $60|$00	; LSR zpg,x | --
.byte $20|$04	; CLI       | EOR abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | EOR abs,x
.byte $70|$00	; LSR abs,x | --

; $60
.byte $60|$06   ; RTS       | ADC x,ind
.byte $00	; --        | --
.byte $00|$03	; --        | ADC zpg
.byte $50|$00	; ROR zpg   | --
.byte $40|$02	; PLA       | ADC #
.byte $20|$00	; ROR       | --
.byte $50|$04	; JMP ind   | ADC abs
.byte $60|$00	; ROR abs   | --

; $70
.byte $20|$05	; BVS       | ADC ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | ADC zpg,x
.byte $60|$00	; ROR zpg,x | --
.byte $20|$04	; SEI       | ADC abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | ADC abs,x
.byte $70|$00	; ROR abs,x | --

; $80
.byte $00|$06	; --        | STA x,ind
.byte $00	; --        | --
.byte $30|$03	; STY zpg   | STA zpg
.byte $30|$00	; STX zpg   | --
.byte $20|$00	; DEY       | --
.byte $20|$00	; TXA       | --
.byte $40|$04	; STY abs   | STA abs
.byte $40|$00	; STX abs   | --

; $90
.byte $20|$06	; BCC       | STA ind,y
.byte $00	; --        | --
.byte $40|$04	; STY zpg,x | STA zpg,x
.byte $40|$00	; STX zpg,y | --
.byte $20|$05	; TYA       | STA abs,y
.byte $20|$00	; TXS       | --
.byte $00|$05	; --        | STA abs,x
.byte $00	; --        | --

; $A0
.byte $20|$06	; LDY #     | LDA X,ind
.byte $20|$00	; LDX #     | --
.byte $30|$03	; LDY zpg   | LDA zpg
.byte $30|$00	; LDX zpg   | --
.byte $20|$02	; TAY       | LDA #
.byte $20|$00	; TAX       | --
.byte $40|$04	; LDY abs   | LDA abs
.byte $40|$00	; LDX abs

; $B0
.byte $20|$05	; BCS       | LDA ind,y
.byte $00	; --        | --
.byte $40|$04   ; LDY zpg,x | LDA zpg,x
.byte $40|$00	; LDX zpg,y | --
.byte $20|$04	; CLV       | LDA abs,y
.byte $20|$00	; TSX       | --
.byte $40|$04	; LDY abs,x | LDA abs,x
.byte $40|$00	; LDX abs,y | --

; $C0
.byte $20|$06	; CPY #     | CMP x,ind
.byte $00	; --        | --
.byte $30|$03	; CPY zpg   | CMP zpg
.byte $50|$00	; DEC zpg   | --
.byte $20|$02	; INY       | CMP #
.byte $20|$00	; DEX       | --
.byte $40|$04	; CPY abs   | CMP abs
.byte $60|$00	; DEC abs   | --

; $D0
.byte $20|$05	; BNE       | CMP ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | CMP zpg,x
.byte $60|$00	; DEC zpg,x | --
.byte $20|$04	; CLD       | CMP abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | CMP abs,x
.byte $70|$00	; DEC abs,x | --

; $E0
.byte $20|$06	; CPX #     | SBC x,ind
.byte $00	; --        | --
.byte $30|$03	; CPX zpg   | SBC zpg
.byte $50|$00	; INC zpg   | --
.byte $20|$02	; INX       | SBC #
.byte $20|$00	; NOP       | --
.byte $40|$04	; CPX abs   | SBC abs
.byte $60|$00	; INC abs

; $F0
.byte $20|$05	; BEQ       | SBC ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | SBC zpg,x
.byte $60|$00	; INC zpg,x | --
.byte $20|$04	; SED       | SBC abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | SBC abs,x
.byte $70|$00	; INC abs,x | --
