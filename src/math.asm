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
__math_divisor = zp::expr+4
__math_remainder = zp::expr+8

; must be in same segment as expr.asm
.CODE
.segment "EXPR"

;*******************************************************************************
; MUL16
; Multiplies the two given 16-bit numbers and returns a 32-bit product
; From 6502.org
; IN:
;  - r0: the multiplier
;  - r2: the multiplicand
; OUT:
;  - ra: the product
.export __math_mul16
.proc __math_mul16
@multiplier	= r0
@multiplicand	= r2
@product	= ra
	lda	#$00
	sta	@product+2	; clear upper bits of product
	sta	@product+3
	ldx	#$10		; set binary count to 16
@shift_r:
	lsr	@multiplier+1	; divide multiplier by 2
	ror	@multiplier
	bcc	@rotate_r
	lda	@product+2	; get upper half of product and add multiplicand
	clc
	adc	@multiplicand
	sta	@product+2
	lda	@product+3
	adc	@multiplicand+1
@rotate_r:
	ror			; rotate partial product
	sta	@product+3
	ror	@product+2
	ror	@product+1
	ror	@product
	dex
	bne	@shift_r
	ldx @product
	ldy @product+1
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
@divisor = __math_divisor
@dividend = __math_dividend
@remainder = __math_remainder
@result = @dividend		; return quotient in dividend's place
	; division by zero is undefined; return with .C set
	lda @divisor
	ora @divisor+1
	bne @start
	sec			; divide-by-zero -> error
	rts

@start:	lda #0			; preset remainder to 0
	sta @remainder
	sta @remainder+1
	ldx #16			; repeat for each bit: ...

@divloop:
	asl @dividend		; dividend lb & hb*2, msb -> Carry
	rol @dividend+1
	rol @remainder		; remainder lb & hb * 2 + msb from carry
	rol @remainder+1
	lda @remainder
	sec
	sbc @divisor		; substract divisor to see if it fits in
	tay			; lb result -> Y, for we may need it later
	lda @remainder+1
	sbc @divisor+1
	bcc @skip		; if carry=0 then divisor didn't fit in yet

	sta @remainder+1	; else save substraction result as new remainder
	sty @remainder
	inc @result		; and INCrement result (divisor fit in 1 time)

@skip:	dex
	bne @divloop
	clc			; ok
	rts
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
