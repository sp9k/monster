.org $7700
.mac base
.endmac

.mac ldxy val
	ldx #<val
	ldy #>val
.endmac

.mac stxy val
	stx val
	sty val+1
.endmac

.mac set src, dst
	ldxy src
	stxy dst
.endmac
	set 100, 200

	ldxy $1234
	stxy $1000
