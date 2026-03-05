	.org $1200
.mac m a, b
	lda #a
	beq +
	lda #b
:
.endmac

	m 2, 3
	m 4, 5
:
