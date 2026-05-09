.include "memory.inc"
.include "zeropage.inc"

.CODE

;*******************************************************************************
; SHL
; Shifts the linebuffer characters at the given index
; to the left by the given number of characters
; IN:
;  - .X: the index to start shifting down
;  - .Y: the last index to shift
; OUT:
;  - .Z: set (unless start > stop)
.export __linebuffer_shl
.proc __linebuffer_shl
@stop=r0
	sty @stop
	cpx @stop
	bcs @done
:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx @stop
	bne :-
@done:	rts
.endproc
