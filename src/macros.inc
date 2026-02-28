.include "zeropage.inc"

;******************************************************************************
; INCW
; incw increments the 16-bit value at the address w.
.macro incw w
.local @skip
	inc w
	bne @skip
	inc w+1
@skip:
.endmacro

;******************************************************************************
; DECW
; decrements the 16-bit value at the address w.
; clobbers .A
; .Z is set if LSB of w if 0 after decrementing
; .A is the LSB of w before decrementing
.macro decw w
.local @skip
	lda w
	bne @skip
	dec w+1
@skip:	dec w
.endmacro

;******************************************************************************
; CMPW
; Compares .XY to the 16-bit argument. The flags are set as they would be
; from a normal cmp instruction
; OUT:
;  - .Z: set if the two are equal
;  - .C: set if .XY is >= to the argument
.macro cmpw arg
.local @skip
	.if .match( .left( 1, {arg} ), # )
		cpy #>(.right( .tcount( {arg} )-1, {arg} ))
		bne @skip
		cpx #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		cpy arg+1
		bne @skip
		cpx arg
	.endif
@skip:
.endmacro

;******************************************************************************
; LDXY
; loads .X with the LSB of arg and .Y with the MSB.
.macro ldxy arg
	.if .match( .left( 1, {arg} ), # )
		ldx #<(.right( .tcount( {arg} )-1, {arg} ))
		ldy #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		ldx arg
		ldy arg+1
	.endif
.endmacro

;******************************************************************************
; STXY
; Stores the contents of the 16-bit value in .XY to the given address
.macro stxy arg
	stx arg
	sty arg+1
.endmacro

;******************************************************************************
; ADD16
.macro add16 arg
	txa
	clc
	.if .match( .left( 1, {arg} ), # )
		adc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		adc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg+1
	.endif
	tay
.endmacro

;******************************************************************************
; SUB16
.macro sub16 arg
	txa
	sec
	.if .match( .left( 1, {arg} ), # )
		sbc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		sbc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg+1
	.endif
	tay
.endmacro

;******************************************************************************
; SKB
; Skips the next byte
.macro skb
	.byte $24
.endmacro

;******************************************************************************
; SKW
; Skips execution of the next word (equivalent to .byte $2C (bit ABS))
.macro skw
	.byte $2c
.endmacro

;******************************************************************************
; ISZERO
; Sets the .Z flag if the 16-bit operand is 0. If the .Z flag is set, .A will
; also be 0
; .A is destroyed (0 if .Z is set)
.macro iszero arg
	lda arg
	ora arg+1
.endmacro
