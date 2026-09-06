.org $7a00

.eq four 4

start
	; addition
	lda #1
	lda #1+1
	lda #1+1+1
	lda #1-1
	;lda #1-1-1

	lda #>$100+<$102
	lda #>($1e00+2+5)

	lda #*-start
	lda #(*-start)/2

	; subtraction
	lda #9-(3+3)

	; multiplication
	lda #2*3
	lda #2*2*2

	; division
	lda #100/5

	; bitwise ops
	lda #$a0.$07
	lda #$77&$33
	lda #$f0^$f3

	; order of operations
	lda #1+2*4
	lda #(1+2)*4

	; misc
	lda *+3

	lda #>$10f0
	lda #<$f010

	lda #>$110+$210
	lda #<($08*$100)

	lda <($08*$100-10*5)

	; comparisons
	; each yields 1 (true) or 0 (false)
	lda #1==1	; 1
	lda #1==2	; 0
	lda #1!=2	; 1
	lda #2!=2	; 0
	lda #1<2	; 1
	lda #2<1	; 0
	lda #2>1	; 1
	lda #1>2	; 0
	lda #2<=2	; 1
	lda #3<=2	; 0
	lda #2>=2	; 1
	lda #1>=2	; 0

	; the operands are unsigned words
	lda #$8000>$7fff	; 1
	lda #$ff00<$ff01	; 1
	lda #$ffff==$ffff	; 1

	; comparisons bind last, so the
	; arithmetic on either side runs
	; first
	lda #1+1==2	; 1
	lda #1==1+1	; 0
	lda #2*3>5	; 1

	; ...and they are left to right,
	; like the other binary operators
	lda #1<2==1	; 1

	; parentheses group them as usual
	lda #(1==1)+(2==2)	; 2
	lda #(1<2)*3		; 3

	; named constants and the PC work
	; on either side
	lda #four==4		; 1
	lda #four*2>=8		; 1
	lda #*>start		; 1

	; a prefix '<'/'>' still picks a
	; byte, and it does so before the
	; comparison
	lda #>$1234==$12	; 1
	lda #<$1234==$34	; 1
	lda #<$1234>$33		; 1 ($34 > $33)

	jmp *

; ---------------- ERRORS
; Uncomment one at a time; each must
; be rejected.
;
; a single '=' is not an operator, and
; '!' has to be followed by '=':
;
;	lda #1=1
;	lda #1!1
;
; everything here is in the ABS segment
; because of the .org.  In relocatable
; code a symbol has no value until link
; time and cannot be compared at all.
