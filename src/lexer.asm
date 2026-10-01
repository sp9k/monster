;*******************************************************************************
; LEXER
; Reads lexical tokens from shared source without evaluating expressions.
; The cursor is zp::line; each token is a kind and a byte span at that cursor.
; Binary replay supplies cached spans and typed integer values for asmbuffer.
; Every new source entry or assembly pass invalidates the previous view.
; Syntax markers (#, parentheses, commas) remain separate from expression RPN.
; This source reader does not bind names, capture contexts or select opcodes.
;*******************************************************************************

.include "config.inc"
.include "errors.inc"
.include "macros.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.include "lexer_tokens.inc"
.include "context_tokens.inc"
.include "macro.inc"
.include "memory.inc"
.macpack longbranch

.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "BSS_NOINIT"
.endif
.export __lex_cached
__lex_cached: .byte $00
; The upper spare page belongs to the active assembly source view. Nested
; source entries invalidate it; record staging occupies the preceding page.
token_kinds = mem::spare+$300
token_spans = token_kinds+MAX_LINE_LEN+1
token_lows = token_spans ; INTEGER always spans one byte, freeing its length slot
token_highs = token_spans+MAX_LINE_LEN+1
token_length = token_highs+MAX_LINE_LEN+1
.assert token_length+1 <= mem::spare+$400, error, "token view exceeds spare page"

BANKED_SEG "EXPR", FINAL_BANK_EXPR

;*******************************************************************************
; PEEK
; Recognizes one token, including whitespace and comments. Names and numbers
; retain their spelling; their validity and values are checked by consumers.
; IN:
;   - zp::line: points to 0-terminated source in shared memory
; OUT:
;   - .A: kind (or error if .C set)
;   - .X: span length
;   - .Y  zero
;   - .C: set on error
.export __lex_peek
.proc __lex_peek
	lda __lex_cached
	beq source
	lda zp::line
	sec
	sbc #<mem::asmbuffer
	tax
	lda zp::line+1
	sbc #>mem::asmbuffer
	bne source
	cpx token_length
	bcs source
	lda token_kinds,x
	beq source
	cmp #LEX_INTEGER
	beq @integer_span
	pha
	lda token_spans,x
	tax
	pla
	bne @cached_return
@integer_span:
	ldx #$01
@cached_return:
	ldy #$00
	clc
	rts
source:
	ldy #$00
	lda (zp::line),y
classify:
	jeq @end
	cmp #' '
	beq @spaces
	bcs @printing
	jsr whitespace
	beq @spaces
@printing:
	cmp #';'
	jeq @comment
	cmp #'"'
	jeq @string
	cmp #$27
	jeq @character
	cmp #'$'
	jeq @hex
	jsr digit
	jcc @number
	cmp #'.'
	bne :+
	iny
	lda (zp::line),y
	jsr digit
	dey
	jcc @number
	lda #'.'
:	cmp #':'
	bne @name
	iny
	lda (zp::line),y
	dey
	cmp #':'
	bne :+
	iny
	jmp @word
:
	lda #':'
	jmp @single
@name:
	jsr namechar
	bcc @word
	jmp @punctuation

@spaces:
	ldx #LEX_SPACE
@ws:	iny
	jeq @long
	lda (zp::line),y
	jsr whitespace
	beq @ws
	jmp @span

@word:	ldx #LEX_WORD
@wordnext:
	iny
	jeq @long
	lda (zp::line),y
	jsr namechar
	bcc @wordnext
	jsr digit
	bcc @wordnext
	cmp #':'
	bne @span
	iny
	jeq @long
	lda (zp::line),y
	cmp #':'
	beq @wordnext
	dey
	jmp @span

@comment:
	ldx #';'
@commentnext:
	iny
	jeq @long
	lda (zp::line),y
	bne @commentnext
	beq @span

@string:
	ldx #LEX_STRING
@stringnext:
	iny
	jeq @long
	lda (zp::line),y
	jeq @bad
	cmp #'"'
	bne @stringnext
	iny
	jeq @long
	bne @span

@character:
	ldy #$01
	lda (zp::line),y
	jeq @bad
	iny
	lda (zp::line),y
	cmp #$27
	jne @bad
	iny
	ldx #LEX_CHAR
	bne @span

@end:	ldx #$00
	clc
	rts

@single:
	tax
	ldy #$01
@span:	txa
	pha
	tya
	tax
	pla
	ldy #$00
	clc
	rts

@hex:	ldx #LEX_NUMBER
@hexnext:
	iny
	jeq @long
	lda (zp::line),y
	jsr digit
	bcc @hexnext
	and #$df
	cmp #$41
	bcc @span
	cmp #$47
	bcc @hexnext
	bcs @span

@number:
	ldx #LEX_NUMBER
	lda (zp::line),y
@digits:
	jsr digit
	bcs @fraction
	iny
	jeq @long
	lda (zp::line),y
	jmp @digits
@fraction:
	cmp #'.'
	bne @exponent
	iny
	jeq @long
	lda (zp::line),y
	jsr digit
	bcc @fractiondigits
	dey
	jmp @span
@fractiondigits:
	ldx #LEX_FLOAT
	iny
	jeq @long
	lda (zp::line),y
	jsr digit
	bcc @fractiondigits
@exponent:
	and #$df
	cmp #$45
	jne @span
	; Only absorb an exponent when its optional sign is followed by a digit.
	tya
	pha
	iny
	jeq @long_pop
	lda (zp::line),y
	cmp #'+'
	beq @sign
	cmp #'-'
	bne @expdigit
@sign:
	iny
	jeq @long_pop
	lda (zp::line),y
@expdigit:
	jsr digit
	bcs @notexp
	pla
	ldx #LEX_FLOAT
@expnext:
	iny
	jeq @long
	lda (zp::line),y
	jsr digit
	bcc @expnext
	jmp @span
@notexp:
	pla
	tay
	jmp @span

@punctuation:
	cmp #$80
	bcc :+
	lda #LEX_RAW
	jmp @single
:	ldx #LEX_EQ
	cmp #'='
	beq @pair
	ldx #LEX_NE
	cmp #'!'
	beq @pair
	ldx #LEX_LE
	cmp #'<'
	beq @pair
	ldx #LEX_GE
	cmp #'>'
	jne @single
@pair:
	pha
	ldy #$01
	lda (zp::line),y
	cmp #'='
	bne @unpaired
	pla
	iny
	jmp @span
@unpaired:
	pla
	jmp @single

@long_pop:
	pla
@long:	RETURN_ERR ERR_LINE_TOO_LONG
@bad:	RETURN_ERR ERR_UNEXPECTED_CHAR
.endproc

;*******************************************************************************
; ADVANCE
; Consumes a previously inspected span.
; IN: zp::line cursor, .X span length
; OUT: cursor advanced by .X; .A/.X preserved, .Y zero, .C clear
.export __lex_advance
.proc __lex_advance
	pha
	txa
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	pla
	ldy #$00
	clc
	rts
.endproc

;*******************************************************************************
; NEXT
; Returns and consumes one token. END has a zero-length span.
; IN: zp::line cursor
; OUT: same result as PEEK; cursor advanced on success, unchanged on error
.export __lex_next
.proc __lex_next
	jsr __lex_peek
	bcs @done
	jsr __lex_advance
@done:	rts
.endproc

;*******************************************************************************
; SIGNIFICANT
; Skips assembler whitespace/control bytes and peeks at the next token.
; IN: zp::line cursor
; OUT: same result as PEEK; cursor at first non-whitespace token
.export __lex_significant
.proc __lex_significant
@controls:
	ldy #$00
	lda (zp::line),y
	bpl @next
	ldx #$01		; retain the source reader's high-bit control-byte handling
	jsr __lex_advance
	jmp @controls
@next:
	jsr __lex_peek
	bcs @ret
	cmp #LEX_SPACE
	bne @done
	jsr __lex_advance
	jmp @controls
@done:	clc
	ora #$00
@ret:	rts
.endproc

;*******************************************************************************
; INDIRECT
; Checks whether the leading parenthesized expression is an indirect operand.
; Parentheses and separators inside literals are opaque lexical tokens.
; IN: zp::line operand, beginning with '('
; OUT: .Z set if indirect; .C set on unbalanced parentheses or bad literals;
;      cursor restored, .A $00 if indirect or $ff otherwise; r0 volatile
.export __lex_indirect
.proc __lex_indirect
@depth=r0
	lda zp::line
	pha
	lda zp::line+1
	pha
	lda #$00
	sta @depth

@next:	jsr __lex_peek
	bcs @bad
	cmp #LEX_END
	beq @bad
	cmp #';'
	beq @bad
	cmp #':'
	beq @bad
	cmp #'('
	bne :+
	inc @depth
:	cmp #')'
	bne @advance
	dec @depth
	beq @closed
@advance:
	jsr __lex_advance
	jmp @next

@closed:
	jsr __lex_advance
	jsr __lex_significant
	bcs @bad
	cmp #LEX_END
	beq @yes
	cmp #';'
	beq @yes
	cmp #':'
	beq @yes
	cmp #','
	beq @yes
	lda #$ff
	clc
	bcc @restore
@yes:	lda #$00
	clc
	bcc @restore
@bad:	lda #$ff
	sec

@restore:
	tax
	pla
	sta zp::line+1
	pla
	sta zp::line
	txa
	rts
.endproc

;*******************************************************************************
; WHITESPACE
; Tests if the given character is a whitespace char
; IN:
;   - .A: character to test
; OUT:
;   - .Z set if whitespace
.proc whitespace
	.include "inline/is_ws.asm"
.endproc

;*******************************************************************************
; DIGIT
; Tests if the given character is a decimal
; IN:
;   - .A: character to test
; OUT:
;   - .C: clear if character is a decimal
.proc digit
	cmp #$30
	bcc @no
	cmp #$3a
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; NAME CHARACTER
; Tests if the given character is valid as a sybmol name
; IN:
;   - .A character to test
; OUT:
;   - .C: clear for letters, '.', '@', '_'
.proc namechar
	cmp #'.'
	beq @yes
	cmp #'@'
	beq @yes
	cmp #$5f
	beq @yes
	cmp #$41
	bcc @no
	cmp #$5b
	bcc @yes
	cmp #$61
	bcc @no
	cmp #$7b
	bcc @yes
	cmp #$c1
	bcc @no
	cmp #$db
	bcc @yes
@no:	sec
	rts
@yes:	clc
	rts
.endproc

;*******************************************************************************
; VALUE AT
; Reads a captured integer at an offset in the current source view.
; IN: .X byte offset in mem::asmbuffer
; OUT: .XY integer, .C clear if present; .C set otherwise
.export __lex_value_at
.proc __lex_value_at
	lda __lex_cached
	beq @no
	cpx token_length
	bcs @no
	lda token_kinds,x
	cmp #LEX_INTEGER
	bne @no
	lda token_lows,x
	ldy token_highs,x
	tax
	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; DECODE
; Builds a source view and lexical metadata from a framed binary context line.
; Integer placeholders carry their value in metadata, never in their spelling.
; IN: CTX_TOKEN_BUFFER contains a complete record (size includes its header)
; OUT: .A source length, .XY mem::asmbuffer, .C clear; .C set on invalid encoding
.export __lex_decode
.proc __lex_decode
@read=r0
@write=r1
@kind=r2
@count=r3
@start=r4
	lda #$00
	sta __lex_cached
	sta @write
	lda CTX_TOKEN_BUFFER
	jeq @empty
	cmp #$04
	jcc @bad
	lda #$03
	sta @read
@next:
	jsr @get
	jcs @bad
	jeq @end
	sta @kind
	ldx @write
	stx @start
	cmp #LEX_INTEGER
	beq @integer
	cmp #LEX_SPACE
	jeq @space
	cmp #LEX_EQ
	bcc @spelling_test
	cmp #LEX_GE+1
	jcc @comparison
@spelling_test:
	cmp #LEX_WORD
	jcc @punctuation
	cmp #LEX_RAW+1
	bcs @copy_bad
	jsr @get
	bcs @copy_bad
	beq @copy_bad
	sta @count
	lda @kind
	cmp #LEX_RAW
	bne @copy
	lda #$00
	sta @kind
@copy:
	; Check the entire spelling once, then copy without per-byte subcalls.
	lda @read
	clc
	adc @count
	bcs @copy_bad
	cmp CTX_TOKEN_BUFFER
	bcs @copy_bad
	lda @write
	clc
	adc @count
	bcs @copy_bad
	cmp #MAX_LINE_LEN+1
	bcs @copy_bad
	ldx @read
	ldy @write
@bytes:
	lda CTX_TOKEN_BUFFER,x
	beq @copy_bad
	sta mem::asmbuffer,y
	sta mac::source,y
	lda #$00
	sta token_kinds,y
	inx
	iny
	dec @count
	bne @bytes
	stx @read
	sty @write
	jmp @span
@copy_bad:
	jmp @bad
@integer:
	lda #$80
	sta __lex_cached
	jsr @get
	jcs @bad
	ldx @write
	sta token_lows,x
	jsr @get
	jcs @bad
	ldx @write
	sta token_highs,x
	lda #'0'
	bne @one
@space:
	lda #' '
	bne @one
@comparison:
	sec
	sbc #LEX_EQ
	tax
	lda @pairs,x
	jsr @put
	bcs @bad
	lda #'='
	bne @one
@punctuation:
	cmp #$20
	bcc @bad
	cmp #$80
	bcs @bad
@one:
	jsr @put
	bcs @bad
@span:
	ldx @start
	lda @kind
	sta token_kinds,x
	cmp #LEX_INTEGER
	jeq @next
	lda @write
	sec
	sbc @start
	sta token_spans,x
	jmp @next
@end:
	lda @read
	cmp CTX_TOKEN_BUFFER
	bne @bad
	lda __lex_cached
	ora #$01
	sta __lex_cached
@empty:
	ldx @write
	stx token_length
	lda #$00
	sta mem::asmbuffer,x
	sta mac::source,x
	txa
	ldxy #mem::asmbuffer
	clc
	rts
@bad:
	lda #$00
	sta __lex_cached
	RETURN_ERR ERR_SYNTAX_ERROR
@get:
	ldx @read
	cpx CTX_TOKEN_BUFFER
	bcs @getret
	lda CTX_TOKEN_BUFFER,x
	inc @read
	ora #$00
	clc
@getret:
	rts
@put:
	ldx @write
	cpx #MAX_LINE_LEN
	bcs @putret
	sta mem::asmbuffer,x
	sta mac::source,x
	lda #$00
	sta token_kinds,x
	inc @write
	clc
@putret:
	rts
@pairs: .byte '=','!','<','>'
.endproc
