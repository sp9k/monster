;*******************************************************************************
; DEFERRED.ASM
; This file contains procedures for compiling/evaluating integer expressions
; stored in relocation records. Used by the linker to resolve things like:
;
; .import A
; .imprt B
; A + B
;*******************************************************************************

.include "asm.inc"
.include "deferred.inc"
.include "errors.inc"
.include "expr.inc"
.include "fp.inc"
.include "kernal.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "macros.inc"
.include "ram.inc"
.include "rpn.inc"
.import __obj_get_import_address, __obj_get_fragment_run_base
.macpack longbranch

.segment "OBJVARS"

.export __deferred_code
__deferred_code: .res MAX_DEFERRED_LEN

.export __deferred_length
__deferred_length: .byte $00

index:      .byte $00
dependent:  .byte $00
normalized: .byte $00
number:     .res 3
symbol:     .word $0000
segment:    .byte $00
error:      .byte $00
token:      .byte $00

BANKED_SEG "LINKER_AUX", FINAL_BANK_LINKER_AUX

;*******************************************************************************
; COMPILE
; Converts a parsed integer expression to relocation bytecode.  Used to defer
; expressions that cannot be reduced at assembly time for the linker to handle.
; IN:
;   - .A:               original evaluation error
;   - expr::rpnlist:    parsed expression
;   - expr::rpnlistlen: parsed expression length
; OUT:
;   - .A:               result width, or an error code
;   - .XY:              0 (placeholder for deferred result)
;   - expr::kind:       VAL_DEFERRED on success
;   - deferred::code:   serialized expression bytecode
;   - deferred::length: serialized expression length
;   - .C:               set if expression cannot be deferred
.export __deferred_compile
.proc __deferred_compile
	sta error
	lda asm::mode
	jeq @original
	lda zp::verify
	jne @original

	CALL FINAL_BANK_EXPR, expr::verify
	jcs @original

	lda #$00
	sta index
	sta dependent
	sta normalized
	sta __deferred_length

;------------------------------------------------------------------------------
; read the next token and select the code that handles it
@next:	lda #$00
	sta number+2
	ldx index
	cpx expr::rpnlistlen
	jeq @done
	jcs @original
	lda expr::rpnlist,x
	sta token
	inc index
	cmp #TOK_UNARY_OP
	jeq @unary
	cmp #TOK_BINARY_OP
	jeq @binary
	cmp #TOK_PC
	beq @pc
	cmp #TOK_FLOAT
	jeq @original		; floating-point relocations are unsupported
	cmp #TOK_VALUE
	bcc @symbol
	beq @literal
	cmp #TOK_WIDE
	jne @original
@literal:
	jsr read_word
	jcs @original
	lda token
	cmp #TOK_WIDE
	jne @value
	jsr read_operator
	jcs @original
	sta number+2
	jmp @value

;------------------------------------------------------------------------------
; capture current program counter as an absolute value or fragment offset
@pc:	ldxy zp::virtualpc
	stxy number
	lda asm::segment
	sta segment
	cmp #SEG_ABS
	jeq @value
	jmp @fragment

;------------------------------------------------------------------------------
; read symbol ID (use a zero placeholder if it is unresolved on pass 1)
@symbol:
	jsr read_word
	jcs @original
	ldxy number
	stxy symbol
	cmpw #SYM_UNRESOLVED
	bne @lookup
	lda zp::pass
	cmp #$02
	jeq @original
	inc dependent		; pass 1 placeholder; no record is emitted
	ldxy #$0000
	stxy number
	jmp @value

;------------------------------------------------------------------------------
; look up symbol's segment and value to determine how to encode it
@lookup:
	CALLMAIN lbl::getsegment
	sta segment
	cmp #SEG_FLOAT
	jeq @original
	cmp #SEG_UNDEF
	beq @import
	ldxy symbol
	CALLMAIN lbl::getaddr
	stxy number
	lda segment
	cmp #SEG_ABS
	beq @value

;------------------------------------------------------------------------------
; write fragment token and ID so the linker can resolve its address
@fragment:
	inc dependent
	lda #TOK_PC		; fragment ID followed by a 16-bit offset
	jsr emit
	jcs @ret
	lda segment
	jsr emit
	jcs @ret
	jmp @word

;------------------------------------------------------------------------------
; write a token identifying the operand as an imported symbol or constant value
@import:
	inc dependent
	lda #TOK_SYMBOL		; assembler ID, mapped to import ID on export
	bne @operand
@value:	lda number+2
	beq :+
	lda #TOK_WIDE
	bne @operand
:	lda #TOK_VALUE		; resolved value
@operand:
	jsr emit
	jcs @ret

;------------------------------------------------------------------------------
; write operand's two bytes and count its size in the resolved expression
@word:	lda number
	jsr emit
	jcs @ret
	lda number+1
	jsr emit
	jcs @ret
	lda number+2
	beq :+
	jsr emit
	jcs @ret
	lda #$04
	bne @normalized
:	lda #$03		; ordinary operands occupy three RPN bytes
	bne @normalized

;------------------------------------------------------------------------------
; read the unary operator and check that deferred expressions support it
@unary: jsr read_operator
	jcs @original
	jsr unary_integer
	jcs @original
	jmp @operator

;------------------------------------------------------------------------------
; read the binary operator and check that deferred expressions support it
@binary:
	jsr read_operator
	jcs @original
	jsr binary_integer
	jcs @original

;------------------------------------------------------------------------------
; write the operator's token and code to the bytecode buffer
@operator:
	sta number
	lda token
	jsr emit
	bcs @ret
	lda number
	jsr emit
	bcs @ret
	lda #$02

;------------------------------------------------------------------------------
; update resolved expression's size and ensure its terminator will fit
@normalized:
	clc
	adc normalized
	cmp #MAX_RPN_LEN		; reserve a byte for TOK_END
	bcs @full
	sta normalized
	jmp @next

;-------------------------------------------------------------------------------
; finish the deferred expression and select its result width
@done:	lda dependent
	beq @original
	lda #TOK_END
	jsr emit			; terminate the bytecode
	bcs @ret

	lda #VAL_DEFERRED
	sta expr::kind
	lda #SEG_UNDEF
	sta expr::segment

	ldxy #$0000
	stxy expr::value
	stx expr::postproc
	stx expr::value+2

	lda #$02			; default to word-sized address
	ldx __deferred_length
	cpx #$03
	bcc @width

	; check if expression has a byte-selector operator ('<' or '>')
	ldy __deferred_code-3,x
	cpy #TOK_UNARY_OP
	bne @width
	ldy __deferred_code-2,x
	cpy #'<'
	beq @byte
	cpy #'>'
	bne @width
@byte:	lda #$01			; flag byte-sized address (> or < used)
@width:	ldxy #$0000
	RETURN_OK

;-------------------------------------------------------------------------------
; return the original evaluation error when the expression cannot be deferred
@original:
	lda error			; restore original error
	sec
@ret:	rts

;-------------------------------------------------------------------------------
@full:	RETURN_ERR ERR_EXPRESSION_TOO_COMPLEX
.endproc

;*******************************************************************************
; READ WORD
; Reads a two-byte operand from the parsed assembly expression.
; IN:
;   - index: position after the token in expr::rpnlist
; OUT:
;   - number: operand value
;   - index:  next token position
;   - .C:     set if the operand exceeds the parsed expression
.proc read_word
	lda index
	clc
	adc #$02
	cmp expr::rpnlistlen
	bcc @read
	beq @read
	;sec
	rts

@read:	ldx index
	lda expr::rpnlist,x
	sta number
	lda expr::rpnlist+1,x
	sta number+1
	inc index
	inc index
	RETURN_OK
.endproc

;*******************************************************************************
; READ OPERATOR
; Reads an operator from the parsed assembly expression.
; IN:
;   - index: operator position in expr::rpnlist
; OUT:
;   - .A: operator
;   - index: next token position
;   - .C: set if the operator is missing
.proc read_operator
	ldx index
	cpx expr::rpnlistlen
	bcs @ret
	lda expr::rpnlist,x
	inc index
@ret:	rts
.endproc

;*******************************************************************************
; EMIT
; Appends one byte to the portable expression buffer.
; IN:
;   - .A: byte
; OUT:
;   - deferred::length: advanced on success
;   - .C: set and .A = error code if the buffer is full
.proc emit
	ldx __deferred_length
	cpx #MAX_DEFERRED_LEN
	bcs @full
	sta __deferred_code,x
	inc __deferred_length
	RETURN_OK

@full:	lda #ERR_EXPRESSION_TOO_COMPLEX
	;sec
	rts
.endproc

;*******************************************************************************
; UNARY INTEGER
; Checks whether an operator can be evaluated as a deferred integer operation.
; IN:
;   - .A: operator
; OUT:
;   - .A: unchanged
;   - .C: set if unsupported
.proc unary_integer
	cmp #'<'
	beq @ok
	cmp #'>'
	beq @ok
	cmp #FP_NEG
	beq @ok
	cmp #FP_POS
	beq @ok
	sec
	rts

@ok:	clc
	rts
.endproc

;*******************************************************************************
; BINARY INTEGER
; Checks arithmetic, bitwise and comparison operators.
; IN:
;   - .A: operator
; OUT:
;   - .A: unchanged
;   - .C: set if unsupported
.proc binary_integer
	cmp #FP_EQ
	bcc @scan
	cmp #FP_GE+$01
	bcc @ok

@scan:	ldx #$06
@next:	cmp binary_operators,x
	beq @ok
	dex
	bpl @next
	sec
	rts
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; EVALUATE
; Reads a serialized integer expression, resolves its imports and fragment
; addresses, then evaluates the resulting constant RPN list.
; IN:
;   - .A: bytecode length, including TOK_END
;   - current input channel: expression bytes
; OUT:
;   - .XY, expr::value: final integer value
;   - .C: set and .A = error code for malformed or invalid expressions
.export __deferred_evaluate
.proc __deferred_evaluate
	cmp #MAX_DEFERRED_LEN+$01
	jcs @bad
	cmp #$01
	jcc @bad
	sta __deferred_length
	lda #$00
	sta index
@read:	jsr krn::chrin
	ldx index
	sta __deferred_code,x
	inc index
	lda index
	cmp __deferred_length
	bcc @read
	lda #$00
	sta index
	sta expr::rpnlistlen

@next:	lda #$00
	sta number+2
	jsr take
	jcs @ret
	cmp #TOK_END
	jeq @done
	sta token
	cmp #TOK_UNARY_OP
	jeq @unary
	cmp #TOK_BINARY_OP
	jeq @binary
	cmp #TOK_PC
	beq @fragment
	cmp #TOK_SYMBOL
	beq @word
	cmp #TOK_VALUE
	beq @word
	cmp #TOK_WIDE
	jne @bad
@word:	jsr take_word
	jcs @ret
	lda token
	cmp #TOK_WIDE
	bne :+
	jsr take
	jcs @ret
	sta number+2
	jmp @value
:	cmp #TOK_SYMBOL
	bne @value
	ldxy number
	CALL FINAL_BANK_LINKER, __obj_get_import_address
	jcs @bad
	stxy number
	jmp @value

@fragment:
	jsr take
	jcs @ret
	CALL FINAL_BANK_LINKER, __obj_get_fragment_run_base
	jcs @bad
	stxy symbol
	jsr take_word
	jcs @ret
	clc
	lda number
	adc symbol
	sta number
	lda number+1
	adc symbol+1
	sta number+1

@value:	lda number+2
	beq :+
	lda #TOK_WIDE
	bne :++
:	lda #TOK_VALUE
:	jsr append
	bcs @ret
	lda number
	jsr append
	bcs @ret
	lda number+1
	jsr append
	bcs @ret
	lda number+2
	beq :+
	jsr append
	bcs @ret
:	jmp @next

@unary:
	jsr take
	bcs @ret
	jsr unary_integer
	bcs @bad
	jmp @operator
@binary:
	jsr take
	bcs @ret
	jsr binary_integer
	bcs @bad
@operator:
	sta number
	lda token
	jsr append
	bcs @ret
	lda number
	jsr append
	bcs @ret
	jmp @next

@done:	lda index
	cmp __deferred_length
	bne @bad		; TOK_END must be the final byte
	ldx expr::rpnlistlen
	lda #TOK_END
	sta expr::rpnlist,x
	CALL FINAL_BANK_EXPR, expr::verify
	bcs @ret
	CALL FINAL_BANK_EXPR, expr::eval_list
@ret:	rts
@bad:	RETURN_ERR ERR_INVALID_EXPRESSION
.endproc

;*******************************************************************************
; TAKE
; Reads the next byte from the serialized expression.
; IN:
;   - index: byte position
; OUT:
;   - .A: byte
;   - index: advanced on success
;   - .C: set and .A = error code at the end of the buffer
.proc take
	ldx index
	cpx __deferred_length
	bcs @bad

	lda __deferred_code,x
	inc index
	RETURN_OK

@bad:	lda #ERR_INVALID_EXPRESSION
	;sec
	rts
.endproc

;*******************************************************************************
; TAKE WORD
; Reads a little-endian operand from the serialized expression.
; IN:
;   - index: operand position
; OUT:
;   - number: operand value
;   - index:  advanced on success
;   - .C:     set on error
;   - .A:     error code if the operand is truncated
.proc take_word
	jsr take
	bcs @ret
	sta number
	jsr take
	bcs @ret
	sta number+1
	;clc
@ret:	rts
.endproc

;*******************************************************************************
; APPEND
; Appends a resolved token byte to the evaluator's RPN buffer.
; IN:
;   - .A: token byte
; OUT:
;   - expr::rpnlistlen: advanced on success
;   - .C:               set
;   - .A:               error code when the expression is too large
.proc append
	ldx expr::rpnlistlen
	cpx #MAX_RPN_LEN-$01
	bcs @full
	sta expr::rpnlist,x
	inc expr::rpnlistlen
	RETURN_OK

@full:	lda #ERR_EXPRESSION_TOO_COMPLEX
	;sec
	rts
.endproc

;*******************************************************************************
binary_operators: .byte '+', '-', '*', '/', '&', '^', K_PIPE
