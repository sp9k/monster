;*******************************************************************************
; MATH.ASM
; This file contains math-related procedures.  These are primarily used by the
; expression parser.
;*******************************************************************************

.include "macros.inc"
.include "zeropage.inc"

;*******************************************************************************
.exportzp __math_arg
__math_arg = zp::expr

;*******************************************************************************
.exportzp __math_dividend, __math_divisor, __math_remainder
__math_dividend = zp::expr+2
__math_divisor = zp::expr+5
__math_remainder = zp::expr+8

; must be in same segment as expr.asm
.CODE
.segment "EXPR"

;*******************************************************************************
; MUL24
; Multiplies two unsigned 24-bit integers modulo $1000000
; IN:
;   - zp::expr+2: multiplier
;   - zp::expr+5: multiplicand
; OUT:
;   - zp::expr+2: product
;   - .C:         clear
.export __math_mul24
.proc __math_mul24
@lhs=zp::expr+2
@rhs=zp::expr+5
@work=r0
	lda #$00
	sta @work
	sta @work+1
	sta @work+2
	ldx #$18
@bit:	lda @lhs
	lsr
	lda @work+2
	bcc @rotate
	clc
	lda @work
	adc @rhs
	sta @work
	lda @work+1
	adc @rhs+1
	sta @work+1
	lda @work+2
	adc @rhs+2
@rotate:
	ror
	sta @work+2
	ror @work+1
	ror @work
	ror @lhs+2
	ror @lhs+1
	ror @lhs
	dex
	bne @bit
	clc
	rts
.endproc

;*******************************************************************************
; DIV24
; Divides two unsigned 24-bit integers.
; IN:
;   - zp::expr+2: dividend
;   - zp::expr+5: divisor
; OUT:
;   - zp::expr+2: quotient
;   - C:          set on division by zero
.export __math_div24
.proc __math_div24
@lhs=zp::expr+2
@rhs=zp::expr+5
@work=zp::expr+8
@count=zp::expr+11
	lda @rhs
	ora @rhs+1
	ora @rhs+2
	bne :+
	sec
	rts
:	lda #$00
	sta @work
	sta @work+1
	sta @work+2
	lda #$18
	sta @count
@divbit:
	asl @lhs
	rol @lhs+1
	rol @lhs+2
	rol @work
	rol @work+1
	rol @work+2
	lda @work
	sec
	sbc @rhs
	tax
	lda @work+1
	sbc @rhs+1
	tay
	lda @work+2
	sbc @rhs+2
	bcc @shiftdiv
	sta @work+2
	sty @work+1
	stx @work
	inc @lhs
@shiftdiv:
	dec @count
	bne @divbit
	clc
	rts
.endproc

;*******************************************************************************
; DIV16
; Divides the given 16-bit dividend by the given 16-bit divisor
; IN:
;  - __math_divisor: the divisor
;  - __math_dividend: the dividend
; OUT:
;  - __math_remainder: the remainder
;  - __math_dividend: the quotient
;  - .C: set on division by zero, clear on success
; PRESERVES:
;  - r0-rf
.export __math_div16
.proc __math_div16
	lda #$00
	sta __math_dividend+2
	sta __math_divisor+2
	jmp __math_div24
.endproc

;*******************************************************************************
; ALIGN UP
; Rounds a value up to the next multiple of an alignment
; IN:
;  - .XY:        the value to round
;  - __math_arg: the alignment to round it to (0 or 1 rounds nothing)
; OUT:
;  - .XY: the value, rounded up to the next multiple of the alignment
;  - .C:  set if the rounded value does not fit in 16 bits
; PRESERVES:
;  - r0-rd
; CLOBBERS:
;  - re/rf, __math_dividend, __math_divisor, __math_remainder
.export __math_align_up
.proc __math_align_up
@dividend  = __math_dividend
@divisor   = __math_divisor
@remainder = __math_remainder
@value = re
	stxy @value

	; an alignment of 0 or 1 is "aligned" by every address
	lda __math_arg+1
	bne @round
	lda __math_arg
	cmp #$02
	bcc @done

@round:	ldxy @value
	stxy @dividend
	ldxy __math_arg
	stxy @divisor
	jsr __math_div16
	lda @remainder		; the remainder; DIV16 returned carry clear
	ora @remainder+1
	beq @return		; already on a boundary

	; value += alignment-remainder
	lda __math_arg
	sec
	sbc @remainder
	sta @remainder
	lda __math_arg+1
	sbc @remainder+1
	sta @remainder+1

	lda @value
	clc
	adc @remainder
	sta @value
	lda @value+1
	adc @remainder+1
	sta @value+1

@return:
	ldxy @value
	rts

@done:	ldxy @value
	clc
	rts
.endproc
