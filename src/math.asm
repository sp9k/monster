;*******************************************************************************
; MATH.ASM
; This file contains math-related procedures.  These are primarily used by the
; expression parser.
;*******************************************************************************

.include "zeropage.inc"

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
; Divides the given 16-bit divisor by the given 16-bit dividend
; IN:
;  - r2: the divisor
;  - r0: the dividend
; OUT:
;  - r4: the remainder
;  - r0: the quotient
.export __math_div16
.proc __math_div16
@divisor = r2
@dividend = r0
@remainder = r4
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
