;*******************************************************************************
; BINARY CONTEXT CAPTURE
; Packs lexical tokens without binding symbols or evaluating expressions.
.include "asm.inc"
.include "config.inc"
.include "ctx.inc"
.include "context_tokens.inc"
.include "errors.inc"
.include "lexer.inc"
.include "memory.inc"
.include "macros.inc"
.include "ram.inc"
.include "target.inc"
.macpack longbranch

BANKED_SEG "CTX", FINAL_BANK_CTX

;*******************************************************************************
; ENCODE
; Encodes a source view; optional iterator reduction matches complete WORD tokens.
; Invalid quoted syntax is retained as RAW so disabled branches remain inert.
; IN: .XY normalized source view, .A nonzero to freeze CTX_ITER_NAME to iterator
; OUT: CTX_TOKEN_BUFFER record, .A byte size, .C clear; .C set on overflow
.export __ctx_encode
.proc __ctx_encode
@out=r0
@kind=r1
@span=r2
@replace=r3
@lo=r6
@hi=r7
	sta @replace
	lda zp::line
	pha
	lda zp::line+1
	pha
	stxy zp::line
	lda #$03
	sta @out
	lda asm::linenum
	sta CTX_TOKEN_BUFFER+1
	lda asm::linenum+1
	sta CTX_TOKEN_BUFFER+2
@next:
	CALL LEX_BANK, lex::peek
	jcs @raw_tail
	cmp #LEX_END
	jeq @end
	cmp #';'
	jeq @end
	stx @span
	sta @kind
	cmp #LEX_INTEGER
	beq @cached_value
	cmp #LEX_WORD
	bne @ordinary
	lda @replace
	beq @ordinary
	ldy #$00
@match:
	lda (zp::line),y
	cmp CTX_ITER_NAME,y
	bne @ordinary
	iny
	cpy @span
	bcc @match
	lda CTX_ITER_NAME,y
	bne @ordinary
	lda zp::ctx+repctx::iter
	sta @lo
	lda zp::ctx+repctx::iter+1
	sta @hi
	jmp @integer
@cached_value:
	lda zp::line
	sec
	sbc #<mem::asmbuffer
	tax
	CALL LEX_BANK, lex::value_at
	jcs @bad
	stx @lo
	sty @hi
@integer:
	lda #LEX_INTEGER
	jsr @put
	jcs @bad
	lda @lo
	jsr @put
	jcs @bad
	lda @hi
	jsr @put
	jcs @bad
	jmp @advance
@ordinary:
	lda @kind
	cmp #LEX_SPACE
	beq @single
	cmp #LEX_EQ
	bcc @payload_test
	cmp #LEX_GE+1
	bcc @single
@payload_test:
	cmp #LEX_WORD
	bcc @single
	cmp #LEX_FLOAT+1
	bcc @payload
	lda #LEX_RAW
@payload:
	jsr @put
	jcs @bad
	lda @span
	jsr @put
	jcs @bad
	ldy #$00
@copy:
	lda (zp::line),y
	jsr @put
	jcs @bad
	iny
	cpy @span
	bcc @copy
	jmp @advance
@single:
	jsr @put
	bcs @bad
@advance:
	ldx @span
	CALL LEX_BANK, lex::advance
	jmp @next
@raw_tail:
	; Preserve deferred errors as an opaque remainder, including quote bytes.
	ldy #$00
@length:
	lda (zp::line),y
	beq @raw_size
	iny
	cpy #MAX_LINE_LEN+1
	bcc @length
	bcs @bad
@raw_size:
	sty @span
	lda #LEX_RAW
	sta @kind
	jmp @payload
@end:
	lda #LEX_END
	jsr @put
	bcs @bad
	lda @out
	sta CTX_TOKEN_BUFFER
	clc
	bcc @restore
@bad:
	lda #ERR_LINE_TOO_LONG
	sec
@restore:
	tax
	pla
	sta zp::line+1
	pla
	sta zp::line
	txa
	rts
@put:
	ldx @out
	cpx #CTX_TOKEN_LIMIT
	bcs @ret
	sta CTX_TOKEN_BUFFER,x
	inc @out
	clc
@ret:	rts
.endproc
