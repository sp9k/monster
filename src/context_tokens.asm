;*******************************************************************************
; CONTEXT_TOKENS.ASM
; This file contains the code to encode a line of source as a context record
; (see context_tokens.inc).
;*******************************************************************************

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
; Encodes the given line as a record in CTX_TOKEN_BUFFER.
; If the rest of the line can't be split into tokens (e.g. a missing closing
; quote) it is stored as one RAW token.
; IN:
;  - .XY:          line to encode
;  - .A:           if nonzero, replace words matching CTX_ITER_NAME with
;                  iterator's current value
;  - asm::linenum: line number to store in the record
; OUT:
;  - .A:               size of the record (or error if .C set)
;  - .C:               set if the record doesn't fit or contains a macro
;                      argument
;  - CTX_TOKEN_BUFFER: encoded record
; CLOBBERS:
;  - r0-r3, r6-r7
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

@next:	CALL LEX_BANK, lex::peek
	jcs @raw_tail
	cmp #LEX_END
	jeq @end
	cmp #';'
	jeq @end
	stx @span
	sta @kind
	cmp #LEX_INTEGER
	beq @cached_value
	cmp #LEX_ARG
	beq @cached_value
	cmp #LEX_IMMARG
	beq @cached_value
	cmp #LEX_WORD
	bne @ordinary
	lda @replace
	beq @ordinary

	ldy #$00
@match: lda (zp::line),y
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
	sta @kind
	cmp #LEX_INTEGER
	beq @valued
	; arguments can't be stored in a context, which can outlive the
	; invocation that they belong to
	lda #ERR_INVALID_MACRO_ARGS
	sec
	jmp @restore

@integer:
	lda #LEX_INTEGER
	sta @kind
@valued:
	lda @kind
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
	cmp #' '
	bcc @raw		; store control characters as is
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
@raw:	lda #LEX_RAW

@payload:
	jsr @put
	jcs @bad
	lda @span
	jsr @put
	jcs @bad
	ldy #$00
@copy:	lda (zp::line),y
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
	; store the rest of the line as is (as a RAW token)
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

@bad:	lda #ERR_LINE_TOO_LONG
	sec
@restore:
	tax
	pla
	sta zp::line+1
	pla
	sta zp::line
	txa
	rts

@put:	ldx @out
	cpx #CTX_TOKEN_LIMIT
	bcs @ret
	sta CTX_TOKEN_BUFFER,x
	inc @out
	clc
@ret:	rts
.endproc
