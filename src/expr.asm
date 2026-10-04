;*******************************************************************************
; EXPR.ASM
; This file contains code to evaluate expressions. This is used, among other
; things, to resolve operand values during assembly.
; Expression parsing involves the creation of an "RPN list", an array that
; represents the expression as a tokenized list of operations and operands
; in RPN format.
; Expression evaluation involves walking this list and producing a result
; that is either a) an absolute value or b) a relocation entry.
; For the sake of evaluation outside the context of assembly, the caller should
; error out if the result is the latter (it must be an absolute value only)
;*******************************************************************************

.include "asm.inc"
.include "errors.inc"
.include "keycodes.inc"
.include "fp.inc"
.include "labels.inc"
.include "limits.inc"
.include "kernal.inc"
.include "macros.inc"
.include "macro.inc"
.include "math.inc"
.include "object.inc"
.include "ram.inc"
.include "target.inc"
.include "util.inc"
.include "lexer.inc"
.include "rpn.inc"
.include "deferred.inc"
.import __mem_asmbuffer

.macpack longbranch

;*******************************************************************************
; CONSTANTS
MAX_OPERATORS = $10
MAX_OPERANDS  = MAX_OPERATORS/2

PC_SYMBOL_ID   = $ffff	; magic value for '*' (in eval result)

; These flags tell the expression evaluator what "kind" an operand
; is: REL means relocatable (symbol-based) and ABS means absolute (fixed value)
; FLOAT operands store CBM float values alongside the unused integer value;
; they are always absolute and are coerced back to an integer at the end of the
; evaluation
VAL_ABS   = 0
VAL_REL   = 1
VAL_FLOAT = 2
VAL_DIFF  = 3	; base - base + constant
VAL_DEFERRED = 4	; integer RPN expression evaluated by the linker

;*******************************************************************************
; How the evaluator finishes a result.  By default an expression must reduce to
; an integer. Exceptions are:
;  - .DF (always float)
;  - .EQ (whichever result type evals to)
FLOAT_MODE_COERCE = 0	; float result -> integer (error if not integral)
FLOAT_MODE_KEEP   = 1	; leave a float result alone (.EQ)
FLOAT_MODE_FORCE  = 2	; always return a float, promoting integers (.DF)
FLOAT_MODE_MONITOR = 3	; keep the result type; never defer monitor errors

;*******************************************************************************
; number of named float constants (.EQ) that may be defined
MAX_FLOAT_CONSTS = MAX_LABELS

; These flags are for "post-processing", which may be applied to a VAL_REL
; (relocatable) expression. In these cases, it must be applied as the final
; step in the evaluation to be legal
;  lda <label + 3	; ok - result is LSB of (label+3) at link time
;  lda 1 + >label	; not ok - post-processing can only be applied at end
POSTPROC_NONE = 0
POSTPROC_LSB  = 1
POSTPROC_MSB  = 2

.BSS

;*******************************************************************************
; END ON WHITESPACE
; If !0, expr::eval will terminate parsing when whitespace is encountered.
; If 0, whitespace is ignored
end_on_whitespace: .byte 0

;*******************************************************************************
; FLOAT MODE
; One of the FLOAT_MODE_* values; selects how a result is finished.
float_mode: .byte 0

;*******************************************************************************
; SYNTAX ONLY
; Nonzero while the assembler checks a source line without assembling it.
; Monitor expression evaluation retains normal symbol values.
.pushseg
.DATA
.export __expr_syntax_only
__expr_syntax_only: .byte $00
.popseg

;*******************************************************************************
; COMPILING
; If !0, '*' is the macro invocation's address rather than the current PC.
compiling: .byte 0

.segment "SHAREBSS"

.pushseg
.DATA

.export __expr_wide_mode
__expr_wide_mode: .byte $00	; !0 = monitor result; 0 = checked assembly word
.popseg

.export __expr_rpnlist
__expr_rpnlist: .res $20

.export __expr_rpnlistlen
__expr_rpnlistlen: .byte 0

.export __expr_kind
__expr_kind: .byte 0

.export __expr_segment
__expr_segment: .byte 0

.export __expr_symbol
__expr_symbol: .word 0

; negative base for VAL_DIFF: fragment ID, or label ID with bit 15 set.
.export __expr_negative
__expr_negative: .word 0

.export __expr_postproc
__expr_postproc: .byte 0

.export __expr_value
__expr_value: .res 3	; full 24-bit result of the last evaluation

;*******************************************************************************
; FLOAT VALUE
; The packed result of an evaluation whose kind is VAL_FLOAT.  This lives in
; shared RAM (not the EXPR bank) so that the assembler can read it back after a
; banked call to expr::eval_float.
.export __expr_floatval
__expr_floatval: .res FP_SIZE
.export __expr_floatstr
__expr_floatstr: .res 24

.segment "EXPR_BSS"
literal_high = r2		; get_val accumulator high byte, consumed by appendval

;*******************************************************************************
; OPERANDS
; The operand stack used by the evaluator.  Each entry is OPERAND_SIZE bytes:
;   +0 value LSB   +1 value MSB
;   +2 kind        +3 segment ID (ABS: signed extension for narrowing)
;   +4 symbol LSB  +5 symbol MSB
;   +6 postproc
;   +7 negative base LSB  +8 negative base MSB (VAL_DIFF only)
;   +9 integer high byte
;   +10 float value (only if kind is VAL_FLOAT)
.if FP_SUPPORTED
OPERAND_SIZE = 10+FP_SIZE
.else
OPERAND_SIZE = 10
.endif
operands: .res $100


.if FP_SUPPORTED
;*******************************************************************************
; FLITERALS
; Pool of float literals found while parsing.  TOK_FLOAT tokens in the RPN list
; contain the byte offset of its value here.
fliterals:  .res MAX_OPERANDS*FP_SIZE
fliteralsz: .byte 0	; bytes of fliterals in use

;*******************************************************************************
; FBUF
; The packed float belonging to the operand most recently pushed or popped.
fbuff: .res FP_SIZE

;*******************************************************************************
; FCONSTS
; Values of the named float constants defined with .EQ.  A float constant's
; symbol carries SEG_FLOAT and holds its index here in place of an address.
; The pool has to outlive pass 1 (where .EQ defines things) into pass 2, so it
; is cleared alongside the symbol table in asm::reset, not per expression.
fconsts:  .res MAX_FLOAT_CONSTS*FP_SIZE
nfconsts: .word 0	; bytes of fconsts in use (handles are byte offsets)
.endif

.CODE

;*******************************************************************************
; EVAL
; Calls the evaluation procedure
.export __expr_eval
.proc __expr_eval
	JUMP FINAL_BANK_EXPR, eval
.endproc

.segment "EXPR"

;*******************************************************************************
; END_ON_SPACE
; Configures the evaluation behavior when whitespace is encountered.
; IN:
;   - .A: if set to 0, future calls to eval will ignore whitespace
;         if set to 1, future calls to eval will treat whitespace as end of expr
.export __expr_end_on_whitespace
.proc __expr_end_on_whitespace
	sta end_on_whitespace
:	rts			; return point for following proc's
.endproc

;*******************************************************************************
; EVAL
; Resolves the contents of the given zp::line and returns its evaluated value
; Object operands that cannot use a compact relocation are compiled into an
; integer expression for the linker. Floats must resolve during assembly.
;
; IN:
;  - zp::line: pointer to the expression to evaluate
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;              $ff means "unknown"
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
;  - zp::line: updated to point beyond the parsed expression
.proc eval
	jsr __expr_parse	; parse the RPN list
	bcs :-			; -> rts

	jsr __expr_eval_list
	bcc @done
	ldx float_mode
	cpx #FLOAT_MODE_FORCE
	bcs @done		; .DF and monitor evaluations cannot defer errors
	JUMP FINAL_BANK_LINKER_AUX, deferred::compile
@done:	rts
.endproc

;*******************************************************************************
; EVAL WIDE
; Evaluates monitor expressions with absolute symbols and a 24-bit result
; IN:
;   - zp::line: expression text
; OUT:
;   - expr::value:    24-bit integer
;   - expr::floatval: float result (if applicable)
;   - expr::kind:     result type
;   - .C:             set on error
;   - .A:             error code on failure
.export __expr_eval_wide
.proc __expr_eval_wide
	lda #FLOAT_MODE_MONITOR
	sta __expr_wide_mode
	sta float_mode
	jsr eval
	php
	pha
	lda #$00
	sta __expr_wide_mode
	sta float_mode
	pla
	plp
	rts
.endproc

;*******************************************************************************
; EVAL LIST
; Evaluates the provided RPN list of tokens and returns the result
; Stack operands use the following structure to help determine the
; relocatability of the expression:
;    kind:       0=ABS, 1=RELOCATE
;    symbol_id:  symbol id (RELCOATE only)
;    segment_id: segment ID of symbol (RELOCATE only)
; The addend to a relocatable result (if any) will be the final value of the
; expression.
; IN:
;  - expr::rpnlist: list of tokens to evaluate (as produced by expr::parse)
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
.export __expr_eval_list
.proc __expr_eval_list
@i           = zp::expr
@sp          = zp::expr+1
@val1        = zp::expr+2
@val2        = zp::expr+5
@negative    = zp::tmp15
@negative2   = zp::expr+8
@kind1       = r4
@segment1    = r5
@symbol1     = r6		; 2 bytes
@postproc1   = r8
@kind2       = r9
@segment2    = ra
@symbol2     = rb		; 2 bytes
@postproc2   = rd
@operator    = re
@kind        = zp::tmp10
@segment     = zp::tmp11
@symbol      = zp::tmp12	; 2 bytes
@postproc    = zp::tmp14
@product_hi  = r0		; high three bytes returned by mul24
@operands    = operands	    ; operand stack (grows up from here)
	lda __expr_syntax_only
	beq @evaluate
	jsr __expr_verify
	jcs @ret
	beq @evaluate

	; Symbolic checks return a byte-sized placeholder without evaluating it.
	ldxy #$0000
	stxy @val1
	stx __expr_kind
	stx @segment
	stx @val1+2
	jmp @abs_result

@evaluate:
	ldx #$00
	stx @i
	stx @sp

	cpx __expr_rpnlistlen
	bne @evalloop
	RETURN_ERR ERR_VALUE_EXPECTED

@evalloop:
	lda #$00
	sta @val1+2
	ldx @i
	lda __expr_rpnlist,x
	jpl @cont

@done:	jsr @popval		; read result (should be only value on stack)
	jcs @ret

	stxy @val1
.if FP_SUPPORTED
	jsr @finish_result	; apply the active float mode
	jcs @ret
.endif

	; make sure stack is empty
	lda @sp
	beq :+
	RETURN_ERR ERR_INVALID_EXPRESSION

:	; set the kind of result, symbol, segment, and post-processing
	; for the result
	lda @segment
	sta __expr_segment
	lda @kind
	sta __expr_kind
.if FP_SUPPORTED
	cmp #VAL_FLOAT
	jeq @float_result
.endif
	cmp #VAL_ABS
	bne @rel_result
@abs_result:
	lda #POSTPROC_NONE
	sta __expr_postproc
	ldx __expr_wide_mode
	bne @wide_result
	lda @segment
	beq @positive
	cmp #$ff
	bne @overflow
	lda @val1+2
	cmp #$ff
	bne @overflow
	bit @val1+1
	bmi @word_result
	bpl @overflow
@positive:
	lda @val1+2
	beq @word_result
@overflow:
	RETURN_ERR ERR_OVERSIZED_OPERAND

@wide_result:
	lda @val1+2
	beq @word_result
	lda #$03
	bne @ok
@word_result:
	lda @val1+1		; is MSB of result 0?
	bne :+
	lda #$01
	skw
:	lda #$02
	jmp @ok			; -> done

@rel_result:
	lda @val1+2
	bne @overflow		; wide addends require deferred evaluation
	ldxy @symbol
	stxy __expr_symbol
	lda @kind
	cmp #VAL_DIFF
	bne :+
	ldxy @negative
	stxy __expr_negative
	lda #$02
	jmp @rel_done
:
	lda @segment
	cmp asm::segment	; is the result in a different segment?
	bne @sym_result		; if so, need a symbol-relative answer

@seg_result:
	; same segment, no need to lookup symbol
	lda asm::segtype	; get the size of segment for result size
	jsr type_to_mode
	clc
	adc #$01		; add 1 to get # of bytes
	bcc @rel_done		; and continue to finish up building result

@sym_result:
	ldxy @symbol

	lda @segment
	cmp #SEG_UNDEF		; is segment undefined?
	beq :+			; if so, assume 2 bytes

	cmpw #PC_SYMBOL_ID
	beq :+			; if '*' assume 2 bytes

	; get the address mode of the symbol
	CALLMAIN lbl::addrmode

	cmp #$00		; zeropage?
	bne :+			; if not zeropage, need 2 bytes
	lda @val1
	ora @val1+1		; is there an addend?
	bne :+			; if so, we need 2 bytes to be safe
	lda #$01		; ZP label with no addend -> 1 byte result
	skw
:	lda #$02

@rel_done:
	ldx @postproc
	stx __expr_postproc
	beq @ok			; no postproc -> continue with current size
	lda #$01		; force 1 byte size if we are taking '>' or '<'
	bne @ok			; branch always

.if FP_SUPPORTED
@float_result:
	lda #POSTPROC_NONE	; no postproc for FP results
	sta __expr_postproc
	lda #FP_SIZE		; floats are 5 bytes
.endif

@ok:	pha
	lda @val1+2
	sta __expr_value+2
	pla
	ldxy @val1
	stxy __expr_value	; save full result (e.g. for relocation addend)
	clc			; ok
@ret:	rts

@cont:	inx			; move past token index
	cmp #TOK_UNARY_OP
	bne :+
	jsr @eval_unary		; evaluate the unary operator
	bcs @ret		; propagate error (e.g. missing operand)
	jmp @evalloop		; and continue

:	cmp #TOK_BINARY_OP
	bne :+
	jsr @eval_binary	; evaluate the binary operator
	bcs @ret
	jmp @evalloop		; and continue

:	cmp #TOK_PC
	bne @getoperand
	inc @i
	lda __expr_wide_mode
	bne @abs_pc
	lda asm::mode		; check if we are in DIRECT mode
	bne :+			; continue to push relocation value if not

	; direct mode or ABS segment -> just push the current PC as VAL_ABS
@abs_pc:
	ldxy zp::virtualpc
	jmp @const

:	lda asm::segment	; current segment at assembly time
	cmp #SEG_ABS
	beq @abs_pc		; if * used in an ABS segment, treat as constant

	sta @segment
	ldxy #PC_SYMBOL_ID	; use the magic value for PC as the symbol ID
	stxy @symbol
	ldxy zp::virtualpc	; offset from SECTION base
	stxy @val1
	jmp @pushrel		; finish by pushing this as a VAL_REL

@getoperand:
	; not operator, get the operand
	pha			; save TOKEN type
	lda __expr_rpnlist,x	; get LSB
	inx
	ldy __expr_rpnlist,x	; and MSB
	inx
	stx @i
	tax
	pla			; restore TOKEN type

	cmp #TOK_WIDE
	bne :+
@wide:	stx @val1
	ldx @i
	lda __expr_rpnlist,x
	sta @val1+2		; set MSB of 24 bit vlaue
	inc @i
	ldx @val1
	jmp @const
:
.if FP_SUPPORTED
	cmp #TOK_FLOAT
	bne :++

	; FP tokens operands are offsets (in .X) into the FP pool, not a value
	ldy #$00
:	lda fliterals,x
	sta fbuff,y
	inx
	iny
	cpy #FP_SIZE
	bcc :-
	lda #VAL_FLOAT
	sta @kind
	ldxy #$0000		; the integer half of a float operand is unused
	jmp @valdone
:
.endif
	cmp #TOK_SYMBOL_ZP
	beq @sym
	cmp #TOK_SYMBOL
	bne @const

@sym:	; resolve symbol and push its value/metadata
	stxy @symbol
	cmpw #SYM_UNRESOLVED	; check magic "unresolved" value
	beq @unresolved

	lda __expr_wide_mode
	beq :+
	CALLMAIN lbl::getaddr
	jmp @const
:	CALLMAIN lbl::addr_and_mode
	stxy @val1		; addend

	; get the segment ID
	ldxy @symbol
	CALLMAIN lbl::getsegment

	sta @segment		; store segment ID
	ldxy @val1		; restore label address/addend
	cmp #SEG_ABS		; is segment "ABSOLUTE"?
	beq @const		; if so, treat as constant value

@pushrel:
	lda #VAL_REL
	sta @kind		; set "kind" to RELOCATE
	bne @valdone		; branch always - continue to store

@unresolved:
	ldx __expr_wide_mode
	bne @undefined		; monitor expressions require defined symbols
	; if we're here, the expression is unresolved (so far)
	; while verifying or in pass 1, that's fine - return and assume we will
	; figure it out
	ldx zp::verify
	bne @dummy
	ldx zp::pass
	cpx #$02
	bne @dummy		; pass 1 -> proceed with dummy

	; if in pass 2 and haven't seen the label, return error
@undefined:
	RETURN_ERR ERR_UNRESOLVABLE_LABEL

@dummy:	; return dummy
	lda #SEG_UNDEF
	sta @segment		; mark segment as undefined
	ldxy #$00		; dummy value
	lda #VAL_REL
	bne @setkind

@const: lda #VAL_ABS
	sta @segment		; absolute literals have no negative extension
@setkind:
	sta @kind		; set "kind" to constant

@valdone:
	jsr @pushval		; store value and metadata
	jmp @evalloop		; continue processing

;-------------------------------------------------------------------------------
; handle unary operator
@eval_unary:
	lda __expr_rpnlist,x	; get the operator
	pha			; and save it
	inx			; move index past the operator
	stx @i			; update list index

	; get the operand for the unary operation
	jsr @popval
	bcc @unary_ok
	tax			; save error code
	pla			; clean up saved operator
	txa			; restore error code
	jmp @ret

@unary_ok:
	stxy @val1
	pla
	sta @operator
	cmp #FP_POS
	jeq @pushval_with_postproc
	cmp #FP_NEG
	beq @negate
.if FP_SUPPORTED
	cmp #FP_FLOAT
	bcs @function
.endif
	jmp @byteop

@negate:
	lda @kind
.if FP_SUPPORTED
	cmp #VAL_FLOAT
	bne :+
	lda fbuff
	beq @negfloat
	lda fbuff+1
	eor #$80
	sta fbuff+1
@negfloat:
	ldxy #$0000
	jmp @pushval
:
.endif
	cmp #VAL_ABS
	jne @invalid_unary
	lda #$00
	sec
	sbc @val1
	tax
	lda #$00
	sbc @val1+1
	tay
	lda #$00
	sbc @val1+2
	sta @val1+2
	lda #$00
	sbc @segment
	sta @segment
	jmp @pushval

.if FP_SUPPORTED
@function:
	lda @kind
	cmp #VAL_FLOAT
	beq @function_float
	cmp #VAL_ABS
	jne @invalid_unary
	ldxy @val1
	lda @val1+2
	jsr fp::fromwide
	jcs @ret
	jmp @function_apply
@function_float:
	jsr @load_float
@function_apply:
	lda @operator
	cmp #FP_INT
	bne :+
	jsr fp::towide
	jcs @ret
	sta @val1+2
	lda #VAL_ABS
	sta @segment
	sta @kind
	jmp @pushval
:	cmp #FP_FLOAT
	beq @function_result
	jsr fp::unary
	jcs @ret
@function_result:
	ldx #FP_SIZE-1
:	lda fp::val,x
	sta fbuff,x
	dex
	bpl :-
	lda #VAL_FLOAT
	sta @kind
	ldxy #$0000
	jmp @pushval
.endif

@byteop:
.if FP_SUPPORTED
	; '<' and '>' select a byte of an integer, so coerce a float first
	lda @kind
	cmp #VAL_FLOAT
	bne :++
	jsr @float_to_int
	bcc :+
	rts

:	lda #VAL_ABS
	sta @kind
:
.endif
	; if VAL_REL, this must be the last operator
	lda @operator
@lsb:	cmp #'<'
	bne @msb
	lda @kind
	bne :+
	sta @segment
	beq :++
:	lda #POSTPROC_LSB
	sta @postproc
:	lda #$00
	sta @val1+2
	tay
	jmp @pushval_with_postproc

@msb:	cmp #'>'
	bne @invalid_unary
	lda @kind
	cmp #VAL_ABS
	beq @msb_abs		; if ABS, apply the '>' now

	; for a relocatable operand the '>' is applied at link time (post-
	; processing).  Keep the FULL 16-bit addend: the linker must compute
	; MSB(segment base + addend), so truncating the addend here would
	; drop the carry out of the LSB sum (and the addend's own MSB)
	lda #POSTPROC_MSB
	sta @postproc
	jmp @pushval_with_postproc

@msb_abs:
	lda #$00
	sta @segment
	sta @val1+2
	tya
	tax
	ldy #$00
	beq @pushval+4	; branch always

@invalid_unary:
	; unrecognized unary operator byte in the RPN list
	RETURN_ERR ERR_INVALID_EXPRESSION

;--------------------------------------
@pushval:
	lda #POSTPROC_NONE
	sta @postproc

@pushval_with_postproc:
	txa
	ldx @sp
	sta @operands,x		; store LSB of addend
	tya
	sta @operands+1,x	; store MSB of addend
	ldy #$00
:	lda @kind,y
	sta @operands+2,x
	inx
	iny
	cpy #$07
	bcc :-
	ldx @sp
	lda @val1+2
	sta @operands+9,x

.if FP_SUPPORTED
	; store the float value (only meaningful for VAL_FLOAT operands)
	ldy #$00
:	lda fbuff,y
	sta @operands+10,x
	inx
	iny
	cpy #FP_SIZE
	bcc :-
.endif

	; update stack pointer
	lda @sp
	clc
	adc #OPERAND_SIZE
	sta @sp
	;clc
	rts

;-------------------------------------------------------------------------------
; handle binary operator
@eval_binary:
	lda __expr_rpnlist,x	; get the operator
	sta @operator		; and save it
	inx			; move index past the operator
	stx @i			; update list index

	; get the operands for the binary operation
	jsr @popval
	bcs @err
	stxy @val2
	lda @val1+2
	sta @val2+2
	ldxy @negative
	stxy @negative2
.if FP_SUPPORTED
	ldy #FP_SIZE-1
:	lda fbuff,y
	sta fp::arg2,y
	dey
	bpl :-
.endif
	ldx #$04
:	lda @kind,x
	sta @kind2,x
	dex
	bpl :-
	lda @postproc
	beq @getval1
	; post-processing ('<'/'>') applies to the final result of the
	; expression, so it must begin the expression (documented); a
	; post-processed value as the RIGHT operand (e.g. "1 + >label")
	; is not representable
	RETURN_ERR ERR_INVALID_EXPRESSION

@getval1:
	jsr @popval
	bcs @err
	stxy @val1
.if FP_SUPPORTED
	ldy #FP_SIZE-1
:	lda fbuff,y
	sta fp::arg1,y
	dey
	bpl :-
.endif
	ldx #$04
:	lda @kind,x
	sta @kind1,x
	dex
	bpl :-
	; NOTE: if val1 carries post-processing (e.g. "<label + 3"), it is
	; NOT applied here: the full 16-bit addend takes part in the
	; arithmetic and the marker (still in the shared @postproc) is
	; carried onto the result for +/- via @pushval_with_postproc, to be
	; applied at link time
	jmp @cont_eval

@err:	rts

@cont_eval:
.if FP_SUPPORTED
@check_float:
	; if either operand is a float, operation is a float operation
	lda @kind1
	cmp #VAL_FLOAT
	beq @float_op
	lda @kind2
	cmp #VAL_FLOAT
	bne @int_op

@float_op:
	jsr @promote		; bring both operands up to floats
	bcs @err
	lda @operator
	cmp #'+'
	beq @fmath
	cmp #'-'
	beq @fmath
	cmp #'*'
	beq @fmath
	cmp #'/'
	beq @fmath
	cmp #FP_EQ
	bcc :+
	cmp #FP_GE+$01
	bcc @fcompare
:

	; bitwise operators have no meaning for floats; demote to integers
	jsr @demote
	bcs @err
	jmp @int_op

@fcompare:
	jsr fp::binop
	bcs @err
	jsr fp::towide
	bcs @err
	sta @val1+2
	lda #VAL_ABS
	sta @segment
	sta @kind
	jmp @pushval

@fmath:	jsr fp::binop
	bcs @err
	jmp @function_result
.endif

@int_op:
	lda @operator		; restore operator

	; Relational operators on integer operands.  Integers are unsigned here
	; and in the FP package, so both paths agree.  The result is the
	; integer 0 (false) or 1 (true).
	cmp #FP_EQ
	bcc @chkadd
	cmp #FP_GE+1
	jcc @relational
@chkadd:
	cmp #'+'
	bne @chksub

@add:	jsr @reduce_operation_addition
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @val1
	clc
	adc @val2
	tax
	lda @val1+1
	adc @val2+1
	tay
	lda @val1+2
	adc @val2+2
	sta @val1+2
	lda @kind
	bne :+
	lda @segment1
	adc @segment2
	sta @segment
:	jmp @pushval_with_postproc	; keep val1's postproc (if any)

@chksub:
	cmp #'-'
	bne @chkmul

@sub:	jsr @reduce_operation_subtraction
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @val1
	sec
	sbc @val2
	tax
	lda @val1+1
	sbc @val2+1
	tay
	lda @val1+2
	sbc @val2+2
	sta @val1+2
	lda @kind
	bne :+
	lda @segment1
	sbc @segment2
	sta @segment
:	jmp @pushval_with_postproc	; keep val1's postproc (if any)

@chkmul:
	; Multiplication, division and bitwise operations all require constants.
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @operator
	cmp #'*'	; MULTIPLY
	bne @chkdiv
	lda @segment1
	eor @segment2
	sta @segment
	jsr m::mul24
	lda @segment1
	ora @segment2
	bne @int_result
	lda @product_hi		; upper half of the unsigned product
	ora @product_hi+1
	ora @product_hi+2
	beq @int_result
	lda #$01
	sta @segment		; positive overflow, rejected when narrowing
	jmp @int_result

@chkdiv:
	cmp #'/'
	bne @bits_start
	jsr m::div24
	bcc @int_result
	RETURN_ERR ERR_DIVIDE_BY_ZERO

@bits_start:
	lda #$00
	sta @segment
	ldx #$02
@bits:	lda @operator
	cmp #'&'
	beq @and
	cmp #K_PIPE
	beq @or
	cmp #'^'
	bne @unknownop
	lda @val1,x
	eor @val2,x
	jmp @bitbyte
@and:	lda @val1,x
	and @val2,x
	jmp @bitbyte
@or:	lda @val1,x
	ora @val2,x
@bitbyte:
	sta @val1,x
	dex
	bpl @bits
@int_result:
	ldxy @val1
	jmp @pushval
@unknownop:
	; unrecognized operator byte in the RPN list (e.g. from a
	; malformed/unbalanced expression like "1[2]")
	RETURN_ERR ERR_INVALID_EXPRESSION

;-------------------------------------------------------------------------------
; evaluates a relational operator on two integer operands
@relational:
	jsr @reduce_operation_other	; both operands must be ABSolute
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

	; build a code for the relation that actually holds between the
	; operands: bit 0 = less than, bit 1 = equal, bit 2 = greater than
:	lda @val1+2
	cmp @val2+2
	bne :+
	lda @val1+1
	cmp @val2+1
	bne :+
	lda @val1
	cmp @val2
	beq @releq
:	lda #%00000001		; val1 < val2
	bcc :+
	lda #%00000100		; val1 > val2
	skw
@releq:	lda #%00000010		; val1 = val2

	; true if the relation that holds is one this operator accepts
:	ldx #$00
	stx @segment
	stx @val1+2
	ldx @operator
	and @relations-FP_EQ,x
	beq :+
	lda #$01
:	tax
	ldy #$00
	jmp @pushval

; the relations each operator is true for, indexed by operator-FP_EQ
@relations:
	.byte %00000010		; FP_EQ: equal
	.byte %00000101		; FP_NE: less than or greater than
	.byte %00000001		; FP_LT: less than
	.byte %00000011		; FP_LE: less than or equal
	.byte %00000100		; FP_GT: greater than
	.byte %00000110		; FP_GE: greater than or equal

;--------------------------------------
@popval:
	; sp -= OPERAND_SIZE
	lda @sp
	sec
	bne :+
	; if stack is empty, error out
	RETURN_ERR ERR_VALUE_EXPECTED
:	sbc #OPERAND_SIZE
	sta @sp

.if FP_SUPPORTED
	; recover the packed float that travelled with this entry
	ldx @sp
	ldy #$00
:	lda @operands+10,x
	sta fbuff,y
	inx
	iny
	cpy #FP_SIZE
	bcc :-
.endif

	ldx @sp
	ldy #$00
:	lda @operands+2,x
	sta @kind,y
	inx
	iny
	cpy #$07
	bcc :-
	ldx @sp
	lda @operands+9,x
	sta @val1+2
	ldy @operands+1,x	; get MSB
	lda @operands,x		; and LSB
	tax
	clc			; ok
	rts

;-------------------------------------------------------------------------------
; REDUCE OPERATION ADDITION
; Determines the @segment and @symbol for the two active operands
; Also validates that the combination of ABS/REL modes is valid
@reduce_operation_addition:
	lda @kind1
	bne @add_a_rel

@add_a_abs:
	lda @kind2
	bne @add_a_abs_b_rel

	; A=ABS, B=ABS keeps the absolute kind from the last pop.
	RETURN_OK

@add_a_abs_b_rel:
	ldxy @negative2
	stxy @negative

	; A=ABS, B=REL, result is REL with b's symbol and segment
	; (the shared vars hold A's metadata from the last pop, so ALL of
	; kind/segment/symbol must be replaced with B's)
	ldx #$03
:	lda @kind2,x
	sta @kind,x
	dex
	bpl :-
	RETURN_OK

@add_a_rel:
	lda @kind2
	cmp #VAL_ABS+$01		; only ABS may be added to a relocatable value
	rts

;-------------------------------------------------------------------------------
; REDUCE OPERATION SUBTRACTION
; Validates/reduces the segment/symbol/and kind for a subtraction operation
; Also validates that the combination of ABS/REL modes is valid
@reduce_operation_subtraction:
	lda @kind2
	bne @sub_a_rel_b_rel
	RETURN_OK		; subtraction of an absolute value keeps the left kind

@sub_a_rel_b_rel:
	lda @kind1
	cmp #VAL_REL
	bne @bad_difference
	lda @kind2
	cmp #VAL_REL
	bne @bad_difference
	; a post-processed ('<'/'>') value cannot take part in a symbol
	; difference
	lda @postproc
	bne @bad_difference

@chksegs:
	; if either operand is not yet resolved (pass 1 forward reference),
	; the segments can't be validated yet; assume the difference will
	; reduce (pass 2 revalidates with both symbols resolved)
	lda @segment1
	cmp #SEG_UNDEF
	beq @symbol_difference
	lda @segment2
	cmp #SEG_UNDEF
	beq @symbol_difference

	; the difference of two symbols reduces to a constant if and only if
	; they are in the same segment (the segment base cancels out)
	lda @segment1
	cmp @segment2
	beq :+		; same segment -> reduce to ABS constant

	; both bases are known fragments. Their offsets are already in the
	; numeric addend; only the bases remain for the linker to subtract.
	jmp @defer_difference

:	lda #VAL_ABS
	sta @kind	; same fragment: the bases cancel
	RETURN_OK

@symbol_difference:
	; make sure both symbols are resolved (will only succeed in pass 2)
	lda @segment1
	bne :+
	ldxy @symbol1
	cmpw #SYM_UNRESOLVED
	beq @unresolved_difference	; symbol 1 unresolved -> can't defer

:	lda @segment2
	bne @defer_difference
	ldxy @symbol2
	cmpw #SYM_UNRESOLVED
	beq @unresolved_difference	; both symbols unresolved -> can't defer

@defer_difference:
	; the left operand's segment and symbol remain from the last pop
	lda @segment2
	cmp #SEG_UNDEF
	beq @negative_symbol

	; subtracting local FRAGMENT
	sta @negative			; @negative=fragment ID
	lda #$00
	sta @negative+1			; clear bit 15 (negative FRAGMENT)
	beq @difference_done

@negative_symbol:
	lda @symbol2
	sta @negative			; @negative=symbol ID
	lda @symbol2+1
	ora #$80			; set bit 15 (negative imported SYMBOL)
	sta @negative+1

@difference_done:
	lda #VAL_DIFF
	sta @kind
	RETURN_OK

@unresolved_difference:
	lda zp::pass
	cmp #$01
	bne @bad_difference
	lda #VAL_ABS
	sta @kind	; pass 1 forward reference; revalidate on pass 2
	RETURN_OK
@bad_difference:
	sec
	rts

;--------------------------------------
; REDUCE OPERATION OTHER
; For operators that need to resolve to constant (pretty much anything but
; addition and subtraction)
@reduce_operation_other:
	lda @kind1
	ora @kind2
	sta @kind
	cmp #VAL_ABS+$01		; carry clear only when both operands are absolute
	rts

.if FP_SUPPORTED
;-------------------------------------------------------------------------------
; PROMOTE
; Makes sure fp::arg1 and fp::arg2 both hold the current operands as floats.
; The buffers already carry whatever float travelled with each operand, so only
; the integer operands need converting.  A relocatable operand cannot be
; converted at all: its value is not known until link time and there is no way
; to defer float arithmetic to the linker.
@promote:
	lda @kind1
	cmp #VAL_FLOAT
	beq @promote2
	cmp #VAL_ABS
	bne @promote_err
	ldxy @val1
	lda @val1+2
	jsr fp::fromwide
	bcs @promote_ret
	ldx #FP_SIZE-1
:	lda fp::val,x
	sta fp::arg1,x
	dex
	bpl :-

@promote2:
	lda @kind2
	cmp #VAL_FLOAT
	beq @promote_ok
	cmp #VAL_ABS
	bne @promote_err
	ldxy @val2
	lda @val2+2
	jsr fp::fromwide
	bcs @promote_ret
	ldx #FP_SIZE-1
:	lda fp::val,x
	sta fp::arg2,x
	dex
	bpl :-

@promote_ok:
	RETURN_OK

@promote_err:
	RETURN_ERR ERR_INVALID_EXPRESSION

@promote_ret:
	rts

;-------------------------------------------------------------------------------
; DEMOTE
; Coerces both float operands back down to integers.  Both must be EXACTLY
; integral. This means "1.5 & 3" is an error rather than a silent 1 & 3.
@demote:
	ldx #FP_SIZE-1
:	lda fp::arg1,x
	sta fp::val,x
	dex
	bpl :-
	jsr fp::towide
	bcs @demote_ret
	sta @val1+2
	stxy @val1

	ldx #FP_SIZE-1
:	lda fp::arg2,x
	sta fp::val,x
	dex
	bpl :-
	jsr fp::towide
	bcs @demote_ret
	sta @val2+2
	stxy @val2

	lda #VAL_ABS
	sta @kind1
	sta @kind2
	sta @segment1
	sta @segment2
	sta @postproc1
	sta @postproc2
	sta @postproc		; post-processing not used for floats
	RETURN_OK

@demote_ret:
	rts

;*******************************************************************************
; LOAD FLOAT
; Copies the current packed operand to the floating-point accumulator buffer.
; IN:
;   - fbuff: the packed operand
; OUT:
;   - fp::val: the packed operand
;   - .X: $ff
@load_float:
	ldx #FP_SIZE-1
:	lda fbuff,x
	sta fp::val,x
	dex
	bpl :-
	rts

;------------------------------------------------------------------------------
; FLOAT TO INT
; Coerces the float in fbuff to an unsigned 24-bit integer.
; IN:
;   - fbuff: packed floating-point value
; OUT:
;   - .XY, @val1+2: integer value
;   - .C: set and .A: error code for a fractional or out-of-range value
@float_to_int:
	jsr @load_float
	lda #$00
	sta @segment
	jsr fp::towide
	sta @val1+2
	rts

;-------------------------------------------------------------------------------
; FINISH RESULT
; Resolves the final result according to the active float mode.
; IN:
;   - @kind, @val1, fbuff: the popped result
; OUT:
;   - @kind:           may change between VAL_ABS and VAL_FLOAT
;   - @val1:           the integer result (integer results only)
;   - expr::floatval:  the packed result (float results only)
;   - .C:              set on error
@finish_result:
	lda @kind
	cmp #VAL_FLOAT
	beq @fr_float

	; an integer (or relocatable) result
	lda float_mode
	cmp #FLOAT_MODE_FORCE
	bne @fr_ok		; nothing to do

	; .DF expects a float; only an absolute value can become one
	lda @kind
	bne @fr_err
	ldxy @val1
	lda @val1+2
	jsr fp::fromwide		; int -> float
	bcs @fr_ret
	jmp @fr_publish

@fr_float:
	lda float_mode
	bne @fr_keep		; KEEP and FORCE both leave it a float

	; default: an expression must reduce to an integer, since that is
	; all the assembler can emit
	jsr @float_to_int
	bcs @fr_ret
	stxy @val1
	lda #VAL_ABS
	sta @kind
@fr_ok:	clc
	rts

@fr_keep:
	jsr @load_float

@fr_publish:
	; copy to buffer expr::floatval in shared RAM
	ldx #FP_SIZE-1
:	lda fp::val,x
	sta __expr_floatval,x
	dex
	bpl :-
	lda #VAL_FLOAT
	sta @kind
	ldxy #$0000
	stxy @val1		; no meaningful 16-bit value
	clc
@fr_ret:
	rts

@fr_err:
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

;*******************************************************************************
; VERIFY RPN
; Checks operand counts and operators without resolving symbolic values.
; IN:
;   - expr::rpnlist: the parsed expression
; OUT:
;   - .A: nonzero if a symbol or current-PC operand needs deferred evaluation
;         error code on failure
;   - .Z: set for a valid literal-only expression
;   - .C: set on malformed expression
.export __expr_verify
.proc __expr_verify
@symbolic=zp::expr+6
@index=zp::expr+7
	ldx #$00
	ldy #$00		; operand stack depth
	stx @symbolic
@next:	lda __expr_rpnlist,x
	bmi @end
	inx
	cmp #TOK_UNARY_OP
	beq @unary
	cmp #TOK_BINARY_OP
	beq @binary

	iny			; push an operand
	cmp #TOK_PC
	beq @pc
	cmp #TOK_VALUE
	bcs @value		; integer or float literal
@pc:	inc @symbolic
	cmp #TOK_PC
	beq @next
@value:	cmp #TOK_WIDE
	bne :+
	inx
:	inx
	inx
	bne @next

@binary:
	dey			; binary operators consume one extra operand
	bmi @missing
	lda __expr_rpnlist,x
	cmp #FP_EQ
	bcc @arithmetic
	cmp #FP_GE+$01
	bcc @operator

@arithmetic:
	stx @index
	ldx #$06
@match:	cmp ::isoperator::opchars,x
	beq @matched
	dex
	bpl @match
	bmi @invalid
@matched:
	ldx @index
	bne @operator

@unary:
	lda __expr_rpnlist,x
	cmp #'<'
	beq @operator
	cmp #'>'
	beq @operator
	cmp #FP_NEG
	bcc @invalid
.if FP_SUPPORTED
	cmp #FP_EXP+$01
.else
	cmp #FP_POS+$01
.endif
	bcs @invalid

@operator:
	cpy #$01		; a unary operand, or the binary operator's left side
	bcc @missing
	inx
	bne @next

@end:	cpy #$01
	bne @invalid
	lda @symbolic
	clc
	rts
@missing:
	RETURN_ERR ERR_VALUE_EXPECTED
@invalid:
	RETURN_ERR ERR_INVALID_EXPRESSION
.endproc

;*******************************************************************************
; FLOAT ENTRY POINTS
; The routines below are the float half of the evaluator's interface.  They are
; exported without a thunk in .CODE (ROM1 has no room to spare), so callers in
; another bank reach them with an explicit CALL/JUMP to FINAL_BANK_EXPR, the
; same way they reach expr::end_on_ws.
.if FP_SUPPORTED

;*******************************************************************************
; EVAL FLOAT / EVAL KEEP
; Run eval under a non-default float mode, restoring the default afterwards so
; that a failed evaluation cannot leave the mode set for the next caller.
;   eval_float: always return a float, promoting an integer result (.DF)
;   eval_keep:  return a float result as a float, an integer as an integer (.EQ)
; OUT:
;  - as expr::eval, with expr::kind saying which of the two came back
.export __expr_eval_float
__expr_eval_float:
	lda #FLOAT_MODE_FORCE
	skw			; skip the mode below

.export __expr_eval_keep
__expr_eval_keep:
	lda #FLOAT_MODE_KEEP

	sta float_mode
	jsr eval
	php			; hold the result across the reset
	pha
	lda #FLOAT_MODE_COERCE
	sta float_mode
	pla
	plp
	rts

;*******************************************************************************
; EVAL BOOL
; Evaluates for .IF: a float result is reduced to 0 (false) or 1 (true) rather
; than rejected for not being integral.
.export __expr_eval_bool
__expr_eval_bool:
.proc eval_bool
	jsr __expr_eval_keep
	bcs @ret
	pha
	lda __expr_kind
	cmp #VAL_FLOAT
	bne @integer
	lda #VAL_ABS
	sta __expr_kind
	ldx __expr_floatval	; only the exponent matters, including signed zero
	ldy #$00
	stxy __expr_value
@integer:
	pla
	clc
@ret:
	rts
.endproc

;*******************************************************************************
; FLOAT FORMAT
; Renders expr::floatval into expr::floatstr.
.export __expr_float_format
__expr_float_format:
format_float:
	ldx #FP_SIZE-1
:	lda __expr_floatval,x
	sta fp::val,x
	dex
	bpl :-
	jmp fp::format

;*******************************************************************************
; FCONST WRITE
; Writes the five bytes of the constant named by the .XY handle to the current
; output file.
.export __expr_fconst_write
__expr_fconst_write:
.proc write_const
	jsr get_const
	bcs @ret
	ldy #$00
:	lda __expr_floatval,y
	jsr krn::chrout
	iny
	cpy #FP_SIZE
	bcc :-
	clc
@ret:	rts
.endproc

;*******************************************************************************
; FCONST READ
; Reads five bytes from the current input file into a new pool entry.
; OUT:
;  - .XY: the handle of the new constant
.export __expr_fconst_read
__expr_fconst_read:
.proc read_const
	ldy #$00
:	jsr krn::readst
	beq :+
	RETURN_ERR ERR_IO_ERROR
:	jsr krn::chrin
	sta __expr_floatval,y
	iny
	cpy #FP_SIZE
	bcc :--
	jmp add_const
.endproc

;*******************************************************************************
; ADD CONST
; Copies expr::floatval into the named-constant pool.
; OUT:
;  - .XY: the index of the stored constant
;  - .C:  set if the pool is full
.export __expr_fconst_add
__expr_fconst_add:
.proc add_const
@ptr=zp::expr+8
	ldxy nfconsts
	cmpw #MAX_FLOAT_CONSTS*FP_SIZE
	bcs @full
	jsr const_ptr
	ldy #$00
:	lda __expr_floatval,y
	sta (@ptr),y
	iny
	cpy #FP_SIZE
	bcc :-
	ldxy nfconsts
	lda nfconsts
	clc
	adc #FP_SIZE
	sta nfconsts
	bcc :+
	inc nfconsts+1
:
	RETURN_OK

@full:	RETURN_ERR ERR_TOO_MANY_LABELS
.endproc

;*******************************************************************************
; FCONST GET
; Loads the constant named by the .XY handle into expr::floatval.
; Shared RAM is the only interface to values owned by this bank.
.export __expr_fconst_get
__expr_fconst_get:
.proc get_const
@ptr=zp::expr+8
	cmpw nfconsts
	bcc :+
	RETURN_ERR ERR_INVALID_EXPRESSION
:	jsr const_ptr
	ldy #FP_SIZE-1
:	lda (@ptr),y
	sta __expr_floatval,y
	dey
	bpl :-
	RETURN_OK
.endproc

.proc const_ptr
@ptr=zp::expr+8
	txa
	clc
	adc #<fconsts
	sta @ptr
	tya
	adc #>fconsts
	sta @ptr+1
	rts
.endproc

;*******************************************************************************
; FCONST CLR
; Forgets every named float constant.
.export __expr_fconst_clr
__expr_fconst_clr:
clr_consts:
	lda #$00
	sta nfconsts
	sta nfconsts+1
	clc
	rts
.else

; Without the FP package only these two do anything: .EQ still evaluates (as an
; integer), and clearing an empty pool is a no-op.
.export __expr_eval_keep
__expr_eval_keep = eval

.export __expr_fconst_clr
__expr_fconst_clr:
	clc
	rts

; The rest are reachable only from code that is itself gated on FP_SUPPORTED.
; They are defined so that this file still links, and report a bad expression
; if one ever does get called.
.export __expr_eval_float
.export __expr_eval_bool
.export __expr_fconst_add
.export __expr_fconst_get
.export __expr_fconst_read
.export __expr_fconst_write
.export __expr_float_format
__expr_eval_float:
__expr_eval_bool:
__expr_fconst_add:
__expr_fconst_get:
__expr_fconst_read:
__expr_fconst_write:
__expr_float_format:
	RETURN_ERR ERR_INVALID_EXPRESSION

.endif

;*******************************************************************************
; PARSE
; Parses the expression into a RPN list of tokens evaluatable by
; expr::eval_token_list
; IN:
;   - zp::line: pointer to the expression to parse
; OUT:
;   - .C:            set on error
;   - expr::rpnlist: a list of tokens for evaluation (in RPN format)
.export __expr_parse
.proc __expr_parse
@i=zp::expr+2
@num_operators=zp::expr+3
@may_be_unary=zp::expr+4
@operators=$128+1
@priorities=@operators+(MAX_OPERATORS*2)
	ldy #$00
	sty @num_operators
	sty @i
	sty __expr_rpnlistlen
.if FP_SUPPORTED
	sty fliteralsz		; the float literal pool is per-expression
.endif

	lda (zp::line),y
	bne :+

	; no expression
	RETURN_ERR ERR_VALUE_EXPECTED

:	; by default flag that operator might be unary
	iny
	sty @may_be_unary

@l0:	ldy #$00
	lda (zp::line),y
	jsr is_whitespace	; eat whitespace
	bne :+

	; check whitespace behavior, finish if configured as terminator
	lda end_on_whitespace
	jne @done
	jsr inc_line
	bne @l0		; branch always

:	lda (zp::line),y
	jsr @isterminator
	jeq @done

@rparen:
	cmp #'('
	bne @lparen
	inc @may_be_unary
	jsr @pushop
	jcs @ret	; operator stack full
	jsr inc_line
	bne @l0		; branch always

@lparen:
	cmp #')'
	bne @checkop
	ldx #$00
	stx @may_be_unary

@paren_eval:
	ldx @num_operators
	jeq @err	; no parentheses found
	lda @operators-1,x
	cmp #'('
	bne :+
	jsr @popop	; pop the parentheses
	jsr inc_line
	bne @l0		; branch always - done evaluating this () block

:	jsr @eval	; append the top operation/operand(s)
	jcs @ret	; RPN list full
	jmp @paren_eval

@checkop:
	;ldy #$00
	lda (zp::line),y
	cmp #'*'		; '*' can be a value or operator
	bne :+
	ldx @may_be_unary	; if unary logic applies, treat as value (PC)
	bne @getoperand

:	ldx @may_be_unary
	beq @binaryop
	cmp #'<'
	beq @prefix
	cmp #'>'
	beq @prefix
.if FP_SUPPORTED
	cmp #'.'
	beq @getoperand		; leading-dot literal is unambiguous here
.endif
	cmp #'-'
	bne :+
	lda #FP_NEG
	bne @prefix
:	cmp #'+'
	bne @binaryop
	lda #FP_POS
@prefix:
	jsr @pushop		; prefix operators associate right-to-left
	jcs @ret
	jsr inc_line
	jmp @l0

@binaryop:
	ldx @may_be_unary
	bne :+
	jsr get_comparison
	bcc @operator_found
:	jsr isoperator
	bne @getoperand
@operator_found:
	pha			; save the operator
	jsr @priority		; get the priority of this operator

@process_ops:
	ldx @num_operators	; any operators to the left?
	beq @process_ops_done
	dex

	; the relational operators have priority 0, the same as the '(' sentinel,
	; so stop here rather than letting @eval consume the parenthesis
	ldy @operators,x
	cpy #'('
	beq @process_ops_done

	; if the operator to the left has >= priority, append it to result
	cmp @priorities,x
	beq :+
	bcs @process_ops_done
:	pha			; save priority
	jsr @eval		; append operation to the stack
	bcs @poperr		; RPN list full
	pla			; get priority
	jmp @process_ops	; continue til op on left has lower priority

@poperr:
	tax			; save error code
	pla			; clean up saved priority
	pla			; clean up the saved operator (pushed before
				; @process_ops) - without this the rts at @ret
				; consumes it as part of the return address
	txa			; restore error code
	jmp @ret

@process_ops_done:
	pla			; restore operator of operator
	jsr @pushop		; push it to operator stack
	jcs @ret		; operator stack full
	jsr inc_line
	inc @may_be_unary
	jmp @l0

@getoperand:
	; when checking a macro body line, accept .isimm(), .present() and
	; .value() as a dummy value
	lda zp::verify
	beq @ordinary_operand
	ldy #$00
	lda (zp::line),y
	cmp #'.'
	bne @ordinary_operand

	CALL FINAL_BANK_MACROS, mac::verify_property
	bcs @ordinary_operand
	cmp #$04		; .ident and .text are not values
	bcs @err

	CALL FINAL_BANK_MACROS, mac::verify_name
	bcs @ret

	ldxy #SYM_UNRESOLVED
	lda #TOK_SYMBOL
	jmp @operand

@ordinary_operand:
	jsr lex::peek
	bcs @ret
	cmp #LEX_ARG
	beq @argument
	cmp #LEX_IMMARG
	bne @function

@argument:			; macro parameter: append its compiled expression
	jsr splice
	bcs @ret
	lda #$00
	sta @may_be_unary
	jmp @l0

@function:
.if FP_SUPPORTED
	jsr get_function
	bcs :+
	jsr @pushop
	bcs @ret
	jmp @l0			; the following '(' opens the function argument
:
.endif
	jsr get_operand		; have we found a valid operand?
	bcs @ret		; no

@operand:
	jsr @appendval
	bcs @ret		; RPN list full
	lda #$00
	sta @may_be_unary
	jmp @l0

@done:	ldx @num_operators	; if there are still ops on stack
	beq @end		; no operators: terminate the RPN list
	lda @operators-1,x
	cmp #'('
	bne @evalrem

	; Unclosed '(' is legal for (zp,x) addressing- Assume this is the
	; case if the paren is the last operator left and we stopped on a ','
	; Anything else is a missing ')' or comma in a function
	cpx #$01
	bne @noclose
	ldy #$00
	lda (zp::line),y
	cmp #','
	beq @end		; "(zp,x)" -> drop the paren and terminate

@noclose:
	RETURN_ERR ERR_INVALID_EXPRESSION ; missing ')' or comma in a function

@evalrem:
	jsr @eval		; evaluate each remaining operator
	bcs @ret		; RPN list full
	jmp @done

@end:	; TODO: validate
@terminate:
	lda #TOK_END
	ldx @i
	stx __expr_rpnlistlen	; store the length
	sta __expr_rpnlist,x	; terminate RPN list
	RETURN_OK

@err:	; check if this is parentheses (could be indirect addressing)
	ldy #$00
	lda (zp::line),y
	cmp #')'
	beq @done
	lda #ERR_UNEXPECTED_CHAR	; unexpected operands still on stack
	sec
@ret:	rts

;-------------------------------------------------------------------------------
; isterminator returns .Z set if the character in .A is
; one that should end the evaluation of the expression
@isterminator:
	cmp #$00
	beq :+
	cmp #';'
	beq :+
	cmp #':'
	beq :+
	cmp #','
:	rts

;-------------------------------------------------------------------------------
@popop:
	dec @num_operators
	ldx @num_operators
	lda @operators,x
	rts

;-------------------------------------------------------------------------------
@pushop:
	ldx @num_operators
	cpx #MAX_OPERATORS
	bcs @rpnfull		; operator stack exhausted
	sta @operators,x
	pha
	jsr @priority
	sta @priorities,x
	pla
	inc @num_operators
	clc			; ok
	rts

;-------------------------------------------------------------------------------
@priority:
	cmp #FP_NEG
	bcc :+
	lda #$06
	rts
:
	ldy #@num_prios
:	cmp @priochars-1,y
	beq @prio_found
	dey
	bne :-
	tya			; .A=0
	rts			; not found
@prio_found:
	lda @prios-1,y
	rts

@priochars: .byte '+', '-', '*', '/', '&', '^', K_PIPE, '<', '>'
@prios:	    .byte  1,   1,   2,   2,   3,   4,   5,   3,   3
@num_prios=*-@prios

;-------------------------------------------------------------------------------
; expression is too complex to represent (RPN list or operator stack full)
@rpnfull_pla:
	pla			; clean up saved token/operator
@rpnfull:
	lda #ERR_EXPRESSION_TOO_COMPLEX
	sec
	rts

;-------------------------------------------------------------------------------
; appends the operands involved in the evaluation followed by the operation
; to the RPN result
; returns the evaluation of the operator in .A on the operands @val1 and @val2
@eval:	jsr @popop
	cmp #'('		; ignore opening paren sentinel
	beq @evaldone

	; check if operator is unary
	pha			; save operator
	cmp #FP_NEG
	bcs @unary
	cmp #'<'
	beq @unary
	cmp #'>'
	beq @unary
@binary:
	; not unary, append a second argument
	lda #TOK_BINARY_OP
	skw			; skip next instruction
@unary: ; append the operator token
	lda #TOK_UNARY_OP
	ldx @i
	cpx #MAX_RPN_LEN-2	; room for token, operator, and terminator?
	bcs @rpnfull_pla
	sta __expr_rpnlist,x	; write TOKEN type
	pla			; get operator
	sta __expr_rpnlist+1,x	; write operator
	inx
	inx
	stx @i
@evaldone:
	clc
	rts

;-------------------------------------------------------------------------------
; append .XY (token type in .A) to the RPN list result
@appendval:
	cmp #TOK_PC
	bne :+
	; TOK_PC only takes 1 byte
	ldx @i
	cpx #MAX_RPN_LEN-1	; room for token and terminator?
	bcs @rpnfull
	sta __expr_rpnlist,x
	bcc @val_end		; branch always

:	pha			; save the token type
	lda @i
	cmp #MAX_RPN_LEN-3	; room for token, value, and terminator?
	bcs @rpnfull_pla
	txa			; .A = value LSB
	ldx @i
	sta __expr_rpnlist+1,x	; LSB
	tya
	sta __expr_rpnlist+2,x	; MSB
	pla
	sta __expr_rpnlist,x	; TOKEN type
	cmp #TOK_WIDE
	bne :+
	cpx #MAX_RPN_LEN-4
	bcs @rpnfull
	lda literal_high
	sta __expr_rpnlist+3,x
	inx
:	inx
	inx
@val_end:
	inx
	stx @i
	rts
.endproc

;*******************************************************************************
; SPLICE
; Appends a macro argument's compiled expression to the RPN list as a single
; operand.
; IN:
;  - zp::line: an argument placeholder in the source view
; OUT:
;  - zp::line: updated to point past the placeholder
;  - .C:       set on error
.proc splice
	lda zp::line
	sec
	sbc #<__mem_asmbuffer
	tax
	jsr lex::value_at
	bcs @ret
.if FP_SUPPORTED
	lda fliteralsz
.else
	lda #$00
.endif
	CALL FINAL_BANK_MACROS, mac::splice_arg
	bcs @ret
.if FP_SUPPORTED
	lda mac::argrecord	; size of the argument's float literals
	beq @done
	clc
	adc fliteralsz
	cmp #MAX_OPERANDS*FP_SIZE+1
	bcs @full
	ldy #$00
	ldx fliteralsz
:	lda mac::argrecord+1,y
	sta fliterals,x
	inx
	iny
	cpy mac::argrecord
	bne :-
	stx fliteralsz
.endif
@done:	jsr inc_line
	clc
	rts
.if FP_SUPPORTED
@full:	lda #ERR_EXPRESSION_TOO_COMPLEX
	sec
.endif
@ret:	rts
.endproc

;*******************************************************************************
; COMPILE
; Parses a macro argument into a record that SPLICE can append to later
; expressions. The record is the RPN size, the RPN tokens, the size of the
; float literals, then the float literals.
; '*' becomes a symbol for the address of the invocation.
; IN:
;  - zp::line: the expression to parse
;  - .XY:      where to write the record
; OUT:
;  - zp::line: updated to point past the expression
;  - .C:       set on error
.export __expr_compile
.proc __expr_compile
@dst=zp::expr+6
	lda #$01
	sta compiling
	txa
	pha
	tya
	pha

	jsr __expr_parse
	tax
	pla
	sta @dst+1
	pla
	sta @dst
	lda #$00
	sta compiling
	txa
	bcs @ret
	ldy #$00
	lda __expr_rpnlistlen
	sta (@dst),y
	tax
	beq @rpndone
:	lda __expr_rpnlist,y
	iny
	sta (@dst),y
	dex
	bne :-

@rpndone:
	iny
.if FP_SUPPORTED
	lda fliteralsz
	sta (@dst),y
	beq @ok
:	lda fliterals,x
	iny
	sta (@dst),y
	inx
	cpx fliteralsz
	bne :-
.else
	lda #$00
	sta (@dst),y
.endif
@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; GET COMPARISON
; Recognize binary relations. Consume only the first byte of two-byte tokens;
; the parser's usual inc_line consumes the final byte.
.proc get_comparison
@peek:
	jsr lex::peek
	bcs @ret
	cmp #'<'
	beq @less
	cmp #'>'
	beq @greater
	cmp #LEX_EQ
	bcc @no
	cmp #LEX_GE+1
	bcs @no
	sec
	sbc #LEX_EQ		; ==, !=, <= or >=
	tax
	lda @operators,x
	pha
	jsr inc_line
	pla
	clc
@ret:	rts
@less:	lda #FP_LT
	clc
	rts
@greater:
	lda #FP_GT
	clc
	rts
@no:	sec
	rts
@operators: .byte FP_EQ, FP_NE, FP_LE, FP_GE
.endproc

;*******************************************************************************
; GET FUNCTION
; Recognizes NAME( without reserving bare names. On success advances only
; over NAME and returns the unary opcode, leaving '(' for the parser.
.if FP_SUPPORTED
.proc get_function
@entry=zp::expr+6
	; sanity check that first character is a letter- if not, not a func
	ldy #$00
	lda (zp::line),y
	cmp #'a'
	bcc @not_function
	cmp #'z'+1
	bcc @scan
@not_function:
	sec
	rts

@scan:	ldx #$00
@next:	stx @entry
	ldy #$00
@match:	lda fnames,x
	beq @endname
	cmp (zp::line),y
	bne @skip
	inx
	iny
	bne @match
@endname:
	lda (zp::line),y
	cmp #'('
	beq @found
@skip:	ldx @entry
:	lda fnames,x
	inx
	cmp #$00
	bne :-
	inx			; skip the opcode
	lda fnames,x
	bne @next
	sec
	rts
@found:
	lda fnames+1,x
	sty @entry
	ldx @entry
	jmp lex::advance
fnames:
	.byte "float",0,FP_FLOAT,"int",0,FP_INT
	.byte "trunc",0,FP_TRUNC,"round",0,FP_ROUND
	.byte "floor",0,FP_FLOOR,"ceil",0,FP_CEIL
	.byte "abs",0,FP_ABS,"sqrt",0,FP_SQRT
	.byte "sin",0,FP_SIN,"cos",0,FP_COS
	.byte "log",0,FP_LOG,"exp",0,FP_EXP,0
.endproc
.endif

;*******************************************************************************
; GETLABEL
; Reads the given label and returns the address of it if there
; is one
; IN:
;  - zp::line: the label to get the address of
;  - .X:       the length of the label
; OUT:
;  - zp::line: updated to point past the label parsed
;  - .C        is set if no label is found
;  - .A:       the size of the label's address
;  - .XY:      the ID for the label
.proc get_label
@id=zp::expr
@mode=r0
	txa
	pha			; save the label's length
	ldxy zp::line
	CALLMAIN lbl::isvalid 	; if verifying, let this pass if label is valid
	bcs @done

	; Syntax checks validate spelling without consulting assembly-time values.
	; Unresolved pass-1 names use the same placeholder ID.
	ldxy #SYM_UNRESOLVED
	stxy @id
	lda __expr_syntax_only
	bne @dummy

@get_id:
	; if not verifying (e.g. in pass 2), label ID is final; try to get it
	ldxy zp::line
	CALLMAIN lbl::find
	bcc :+

	; failed to lookup symbol ID, check if verifying or pass 1
	ldx zp::verify
	bne @dummy
	ldx zp::pass
	cpx #$02
	bcs @done

@dummy:	ldx #$01
	stx @mode	; default to ABS mode
	bne @updateline	; proceed with dummy ID

:	stxy @id
	CALLMAIN lbl::addrmode
	sta @mode

@updateline:
	pla
	tax
	jsr lex::advance
	lda @mode	; restore mode
@ok:	ldxy @id	; get label
	clc		; ok
	rts
@done:	tax
	pla			; discard the saved length
	txa
	rts
.endproc

;*******************************************************************************
; GET OPERAND
; Attempts to get an operand from an expression (constant value or label)
; and returns the token for it if one was found
; IN:
;   - zp::line: the text to parse
; OUT:
;   - .A:  the token type
;   - .XY: the value for the operand (symbol-id or absolute value)
;   - .C:  set if no operand was able to be parsed
.proc get_operand
@lbl=zp::expr
	jsr lex::peek
	jcs @ret
	cmp #LEX_INTEGER	; a captured .REP iterator value?
	bne @lexical
	lda zp::line
	sec
	sbc #<__mem_asmbuffer
	tax
	jsr lex::value_at
	jcs @ret
	jsr inc_line
	lda #TOK_VALUE
	RETURN_OK
@lexical:
.if FP_SUPPORTED
	cmp #LEX_FLOAT
	bne @notfloat

	; a float literal is parked in the literal pool and the token carries
	; its offset, so that TOK_FLOAT stays the same width as every other
	; operand token in the RPN list
	lda fliteralsz
	cmp #MAX_OPERANDS*FP_SIZE
	bcs @poolfull

	jsr fp::parse
	jcs @ret

	ldx fliteralsz
	txa
	pha			; remember the offset of this literal
	ldy #$00
:	lda fp::val,y
	sta fliterals,x
	inx
	iny
	cpy #FP_SIZE
	bcc :-
	stx fliteralsz
	pla
	tax			; .X = offset of the literal
	ldy #$00		; .Y = 0: the pool is smaller than a page
	lda #TOK_FLOAT
	RETURN_OK

@poolfull:
	RETURN_ERR ERR_EXPRESSION_TOO_COMPLEX

@notfloat:
.endif
	cmp #LEX_WORD
	beq @label
	cmp #LEX_CHAR
	beq @value
	cmp #'*'
	beq @star
	cmp #LEX_NUMBER
	bne @badvalue

	; the number must end at a separator, and '$' alone is not a number
	txa
	tay
	lda (zp::line),y
	jsr isseparator
	bne @badvalue
	cpx #$01
	bne @value
	ldy #$00
	lda (zp::line),y
	cmp #'$'
	bne @value
@badvalue:
	RETURN_ERR ERR_UNEXPECTED_CHAR

@star:
	jsr inc_line		; move past the '*'
	lda compiling
	bne @invocation
	lda #TOK_PC		; if '*' just return the token for current PC
	RETURN_OK

@invocation:
	CALL FINAL_BANK_MACROS, mac::bind_pc
	bcs @ret
	stxy @lbl
	CALLMAIN lbl::addrmode
	ldxy @lbl
	jmp @resolved

@value:	jsr get_val
	bcs @ret
	lda literal_high
	beq :+
	lda #TOK_WIDE
	RETURN_OK
:	lda #TOK_VALUE
@ret:	rts

@label:	jsr get_label
	bcs @ret

@resolved:
	cmpw #SYM_UNRESOLVED	; is label undefined (id == $ffff)?
	beq @abs		; if so, just return placeholder token

	pha			; save address mode
	stxy @lbl
	CALLMAIN lbl::getsegment
	ldxy @lbl
.if FP_SUPPORTED
	cmp #SEG_FLOAT
	beq @floatconst
.endif
	cmp #SEG_ABS
	bne @chkmode

@val:	; if segment == SEG_ABS, label is constant, return its value
	pla			; cleanup
	CALLMAIN lbl::getaddr
	lda #TOK_VALUE
	RETURN_OK

@chkmode:
	pla			; restore mode
	bne @abs		; !0 -> absolute addressing
	lda #TOK_SYMBOL_ZP
	skw
@abs:	lda #TOK_SYMBOL
	RETURN_OK

.if FP_SUPPORTED
@floatconst:
	; a named float constant: its "address" is an index into fconsts.  Copy
	; the value into the per-expression literal pool so that it is handled
	; exactly like a float literal from here on.
	pla			; cleanup saved mode
	lda fliteralsz
	cmp #MAX_OPERANDS*FP_SIZE
	jcs @poolfull
	CALLMAIN lbl::getaddr	; .XY = index into fconsts
	jsr get_const
	bcs @ret
	ldx fliteralsz
	txa
	pha			; remember the offset of this literal
	ldy #$00
:	lda __expr_floatval,y
	sta fliterals,x
	inx
	iny
	cpy #FP_SIZE
	bcc :-
	stx fliteralsz
	pla
	tax			; .X = offset of the literal
	ldy #$00
	lda #TOK_FLOAT
	RETURN_OK
.endif
.endproc

;*******************************************************************************
; GETVAL
; Parses a 24-bit decimal or hexadecimal value.
; It may also parse a character in the format 'x'.  This must be a 1 byte
; value.
; IN:
;  - zp::line: the text to parse a value from
;  - .X: token length validated by the lexer
; OUT:
;  - zp::line: updated to point past the value that was parsed
;  -.XY: low word; literal_high: high byte of a wide literal
;  -.C: set on error and clear if a value was extracted.
.proc get_val
@val=r0			; three-byte accumulator
@twice=r3		; accumulator multiplied by two
@hex=r6			; nonzero for hexadecimal
@end=r7			; lexer-validated token length
	stx @end
	ldy #$00
	sty @val
	sty @val+1
	sty @val+2
	sty @hex
	lda (zp::line),y
	cmp #$27
	beq @char
	cmp #'$'
	bne @digit
	inc @hex
	iny

;-------------------------------------------------------------------------------
@digit:	lda (zp::line),y
	cmp #'9'+1
	bcc :+
	sbc #'a'-'9'-1
:	and #$0f
	pha
	jsr @shift
	bcs @overflow_pop
	ldx #$02
@copy:	lda @val,x
	sta @twice,x
	dex
	bpl @copy
	ldx #$02
	lda @hex
	beq @scale
	inx
@scale:	jsr @shift
	bcs @overflow_pop
	dex
	bne @scale
	lda @hex
	bne @add
	clc
@times10:
	lda @val,x
	adc @twice,x
	sta @val,x
	inx
	txa
	eor #$03		; test the index without changing the addition carry
	bne @times10
	bcs @overflow_pop
@add:	pla
	clc
	adc @val
	sta @val
	bcc @next
	inc @val+1
	bne @next
	inc @val+2
	beq @overflow
@next:	iny
	cpy @end
	bcc @digit


@advance:
	tya
	tax
	jsr lex::advance
	ldx @val
	ldy @val+1
	clc
	rts

;-------------------------------------------------------------------------------
@overflow_pop:
	pla
@overflow:
	RETURN_ERR ERR_OVERSIZED_OPERAND

@shift:	asl @val
	rol @val+1
	rol @val+2
	rts

;-------------------------------------------------------------------------------
@char:	iny			; the lexer already checked both quotes
	lda (zp::line),y
	sta @val
	ldy #$03
	jmp @advance
.endproc

;*******************************************************************************
; IS WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.proc is_whitespace
	.include "inline/is_ws.asm"
.endproc

;*******************************************************************************
; is_null_space_comma_closingparen
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is: 0,$0d,' ', ',', or ')'
.proc is_null_return_space_comma_closingparen_newline
	cmp #$00
	beq @done
	jsr is_whitespace
	beq @done
	cmp #','
	beq @done
	cmp #')'
@done:	rts
.endproc

;*******************************************************************************
; IS SEPARATOR
; Checks if the given byte represents a "separator". A separator is any of:
;  0, $0d, ' ', ',', ')', or any operator character
; IN:
;   - .A: the byte to check
; OUT:
;   - .Z: set if the given byte represents a "separator"
.proc isseparator
	cmp #':'
	beq @yes
	jsr is_null_return_space_comma_closingparen_newline
	bne isoperator
@yes:	rts
.endproc

;*******************************************************************************
; IS OPERATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is an operator ('+', '-', etc.)
;  - .AXY: unchanged
.proc isoperator
@xsave=zp::util+2
@scan:	stx @xsave
	ldx #numchars-$01
@next:	cmp opchars,x
	beq @found
	dex
	bpl @next
@found:	php
	ldx @xsave
	plp
@no:	rts
; The first seven entries are the binary arithmetic operators for verify_rpn.
opchars:	.byte '+', '-', '*', '/', '&', '^', K_PIPE
	.byte '!', '(', ')', '<', '=', '>', '[', ']'
numchars=*-opchars
.endproc

;*******************************************************************************
; TYPE TO MODE
; Returns the label address mode that corresponds to the given TYPE
; IN:
;   - .A: the segment TYPE to get the address mode for (e.g. TYPE_BSS)
; OUT:
;   - .A: the corresponding MODE (*ZP=0, others=1)
.export type_to_mode
.proc type_to_mode
	cmp #TYPE_SEGZP
	beq @zp
	cmp #TYPE_BSSZP
	beq @zp
@abs:	lda #$01
	rts
@zp:	lda #$00
	rts
.endproc

;*******************************************************************************
; INC LINE
; Increments the line pointer, which points to the current character being read
; during assembly or other parsing (e.g. expression evaluation)
; OUT:
;   - .Z: effectively always clear (unless line wrapped to 0)
.proc inc_line
	incw zp::line
	rts
.endproc
