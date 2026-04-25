.seg "code"
main:
	;jsr $e5b5	; INIT VIC
	;jsr $e55f	; CLRSCR
	;lda #$f0
	;sta $9005
	;lda #$96
	;sta $9002
	ldx #0
loop:
	lda msg,x
	beq done
	jsr $ffd2
	inx
	bne loop
done:
	jmp *
msg:
	.db "hello world!",0
