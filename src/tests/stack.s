.org $1000
	lda #$ff
	ldx #$00
:	sta $100,x
	dex
	bne -
	asl
	asl
	asl
	jmp *
