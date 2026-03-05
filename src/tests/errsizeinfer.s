; size inference test
; If ZP, label's size must be known
; before their first use
.org $1000
	lda l
	nop
:	nop ; err l inferred wrong
	nop
	jmp -
	nop
.eq l $10
