;*******************************************************************************
; LEXER.ASM
; This file contains the code to split a line of source into tokens.
; Each token is a kind and the number of bytes it spans at zp::line.
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
__lex_cached: .byte $00	; $01: line came from decode, $80: it has values

; token info for the line built by DECODE (indexed by offset in asmbuffer)
; this shares a page with lbl::namebuffer, which is not used while a line
; is being assembled
token_kinds = mem::spare+$200
token_spans = token_kinds+MAX_LINE_LEN+1
token_lows = token_spans	; values are 1 byte long so they don't need a span
token_highs = token_spans+MAX_LINE_LEN+1
token_length = token_highs+MAX_LINE_LEN+1
.assert token_length+1 <= mem::spare+$300, error, "token info too big"

BANKED_SEG "EXPR", FINAL_BANK_EXPR

;*******************************************************************************
; PEEK
; Returns the token at zp::line without moving past it.
; If the line was built by DECODE, the token info saved for it is used.
; IN:
;  - zp::line: 0-terminated source line
; OUT:
;  - .A: the token kind (or error if .C set)
;  - .X: the number of bytes in the token
;  - .Y: 0
;  - .C: set on error
.export __lex_peek
.proc __lex_peek
	lda __lex_cached
	beq @source
	lda zp::line
	sec
	sbc #<mem::asmbuffer
	tax
	lda zp::line+1
	sbc #>mem::asmbuffer
	bne @source		; not in asmbuffer
	cpx token_length
	bcs @source

	lda token_kinds,x
	beq @source		; not the start of a saved token
	cmp #LEX_INTEGER
	bcs @integer_span	; values are a 1-byte placeholder
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

@source:
	ldy #$00
	lda (zp::line),y
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
	jcc @number		; . followed by digit -> number
	lda #'.'		; restore '.'
:	cmp #':'
	bne @name
	iny
	lda (zp::line),y
	dey
	cmp #':'
	bne :+
	iny
	jmp @word

:	lda #':'
	jmp @single

@name:	jsr namechar
	bcc @word
	jmp @punctuation

@spaces:
	ldx #LEX_SPACE
@ws:	iny
	beq @early_long
	lda (zp::line),y
	jsr whitespace
	beq @ws
	jmp @span

@word:	ldx #LEX_WORD
@wordnext:
	iny
	beq @early_long
	lda (zp::line),y
	jsr namechar
	bcc @wordnext
	jsr digit
	bcc @wordnext

	cmp #':'
	bne @span
	iny
	beq @early_long
	lda (zp::line),y
	cmp #':'
	beq @wordnext
	dey
	jmp @span

@comment:
	ldx #';'
@commentnext:
	iny
	beq @early_long
	lda (zp::line),y
	bne @commentnext
	beq @span

@string:
	ldx #LEX_STRING
@stringnext:
	iny
	beq @early_long
	lda (zp::line),y
	beq @bad
	cmp #'"'
	bne @stringnext
	iny
	beq @early_long
	bne @span

@character:
	ldy #$01
	lda (zp::line),y
	beq @bad
	iny
	lda (zp::line),y
	cmp #$27
	bne @bad
	iny
	ldx #LEX_CHAR
	bne @span

; Nearby error exits keep the string/word scan branches short.
@early_long:	RETURN_ERR ERR_LINE_TOO_LONG
@bad:	RETURN_ERR ERR_UNEXPECTED_CHAR

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
	beq @long
	lda (zp::line),y
	jsr digit
	bcc @fractiondigits
@exponent:
	and #$df
	cmp #$45
	bne @span
	; only treat 'E' as an exponent if a digit follows it (or its sign)
	tya
	pha
	iny
	beq @long_pop
	lda (zp::line),y
	cmp #'+'
	beq @sign
	cmp #'-'
	bne @expdigit

@sign:	iny
	beq @long_pop
	lda (zp::line),y
@expdigit:
	jsr digit
	bcs @notexp
	pla
	ldx #LEX_FLOAT
@expnext:
	iny
	beq @long
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
.endproc

;*******************************************************************************
; ADVANCE
; Moves zp::line past a token returned by PEEK.
; IN:
;  - .X:       the number of bytes to move
;  - zp::line: the source line
; OUT:
;  - zp::line: moved forward by .X bytes
;  - .A, .X:   preserved
;  - .Y:       0
;  - .C:       clear
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
; SIGNIFICANT
; Skips whitespace and control bytes, then peeks at the next token.
; IN:
;  - zp::line: the source line
; OUT:
;  - .A, .X, .Y, .C: same as PEEK
;  - .Z:             set if at the end of the line
;  - zp::line:       moved to the first token that isn't whitespace
.export __lex_eatws
.proc __lex_eatws
@controls:
	ldy #$00
	lda (zp::line),y
	bpl @next
	ldx #$01		; skip bytes with bit 7 set (like line::process_ws)
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
; Checks if the operand at zp::line is indirect, e.g. "(addr)" or "(zp),y".
; It is if the closing ')' is followed by a comma or the end of the operand.
; IN:
;  - zp::line: the operand (beginning with '(')
; OUT:
;  - .A: $00 if indirect, $ff if not
;  - .Z: set if indirect
;  - .C: set if the parentheses don't match or a token is bad
; CLOBBERS:
;  - r0
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
	jsr __lex_eatws
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
; Checks if the given character is whitespace
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if whitespace
.proc whitespace
	.include "inline/is_ws.asm"
.endproc

;*******************************************************************************
; DIGIT
; Checks if the given character is a decimal digit
; IN:
;  - .A: the character to test
; OUT:
;  - .C: clear if the character is a digit
.proc digit
	cmp #$30
	bcc @no
	cmp #$3a
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; NAMECHAR
; Checks if the given character is valid in a symbol name
; IN:
;  - .A: the character to test
; OUT:
;  - .C: clear for letters, '.', '@', and '_'
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
; Returns the value saved by DECODE for an INTEGER, ARG, or IMMARG token.
; IN:
;  - .X: offset of the token in mem::asmbuffer
; OUT:
;  - .A:  the token kind
;  - .XY: the value (for ARG/IMMARG: .X=frame depth, .Y=argument index)
;  - .C:  set if there is no value at the given offset
.export __lex_value_at
.proc __lex_value_at
	lda __lex_cached
	beq @no
	cpx token_length
	bcs @no
	lda token_kinds,x
	cmp #LEX_INTEGER
	bcc @no
	pha
	lda token_lows,x
	ldy token_highs,x
	tax
	pla
	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; DECODE
; Rebuilds a line of source in mem::asmbuffer (and mac::source) from the
; record in CTX_TOKEN_BUFFER (see context_tokens.inc).
; Each value is written as a 1-byte placeholder ('0' or '?'); its value and
; the kind of each token are saved for PEEK and VALUE AT.
; IN:
;  - CTX_TOKEN_BUFFER: the record to decode
; OUT:
;  - .A:  the length of the line (or error if .C set)
;  - .XY: mem::asmbuffer
;  - .C:  set if the record is bad or the line is too long
; CLOBBERS:
;  - r0-r4
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
	cmp #LEX_ARG
	beq @argument
	cmp #LEX_IMMARG
	beq @argument
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
	; make sure the whole spelling fits before copying it
	lda @read
	clc
	adc @count
	bcs @copy_bad
	cmp CTX_TOKEN_BUFFER
	bcs @copy_bad
	lda @write
	clc
	adc @count
	jcs @long
	cmp #MAX_LINE_LEN+1
	jcs @long
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
	lda #'0'
	bne @valued
@argument:
	lda #ARG_PLACEHOLDER
@valued:
	sta @count
	lda #$80
	sta __lex_cached
	jsr @get
	bcs @bad
	ldx @write
	sta token_lows,x
	jsr @get
	bcs @bad
	ldx @write
	sta token_highs,x
	lda @count
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
	bcs @long
	lda #'='
	bne @one
@punctuation:
	cmp #$20
	bcc @bad
	cmp #$80
	bcs @bad
@one:
	jsr @put
	bcs @long
@span:
	ldx @start
	lda @kind
	sta token_kinds,x
	cmp #LEX_INTEGER
	jcs @next		; values are stored where the span would be
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
@long:
	lda #ERR_LINE_TOO_LONG	; e.g. local names or .IDENT made it longer
	skw
@bad:
	lda #ERR_SYNTAX_ERROR
	ldx #$00
	stx __lex_cached
	sec
	rts
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
