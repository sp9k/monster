.org $1000
	lda #$00
	beq ++
:	asl
:	lda #$ff
	bne --
	nop
	nop
	nop

