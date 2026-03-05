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
.include "labels.inc"
.include "macros.inc"
.include "math.inc"
.include "ram.inc"
.include "target.inc"
.include "util.inc"

;*******************************************************************************
; CONSTANTS
MAX_OPERATORS = $10
MAX_OPERANDS  = MAX_OPERATORS/2

TOK_SYMBOL    = 1	; symbol e.g. "label"
TOK_SYMBOL_ZP = 2	; zeropage symbol e.g. "tmp"
TOK_VALUE     = 3	; constant value e.g. 123
TOK_PC        = 4	; current PC e.g. '*'
TOK_BINARY_OP = 5	; binary operator e.g. '+' or '-'
TOK_UNARY_OP  = 6	; unary operator e.g. '<'
TOK_END       = $ff	; end of expression marker

PC_SYMBOL_ID   = $ffff	; magic value for '*' (in eval result)

; These flags tell the expression evaluator what "kind" an operand
; is: REL means relocatable (symbol-based) and ABS means absolute (fixed value)
VAL_ABS = 0
VAL_REL = 1

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

.segment "SHAREBSS"

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

.export __expr_postproc
__expr_postproc: .byte 0

.segment "EXPR_BSS"
operands: .res $100

.CODE

;*******************************************************************************
; EVAL
; Calls the evaluation procedure
.export __expr_eval
.proc __expr_eval
	JUMP FINAL_BANK_UDGEDIT, eval
.endproc

; expression code stored in UDG bank
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
; If evaluating an expression that contains an external (imported) symbol,
; there is some slightly stricter syntax required.
; That symbol must be the first in the expression and its operation must be
; immediately after that
; e.g. `GLOBAL + (rest of expr)`
; If post-processing is to be used, it must be the FIRST character of the
; expression. Do not use parentheses.
; e.g. `<GLOBAL+3`
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

	; fall through to __expr_eval_list
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
@val2        = zp::expr+4
@result_size = zp::expr+6
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
@tmp         = zp::tmp15
@operands    = operands	    ; operand stack (grows up from here)
	ldx #$00
	stx @i
	stx @sp

@evalloop:
	ldx @i
	lda __expr_rpnlist,x
	bpl @cont

@done:	jsr @popval		; read result (should be only value on stack)
	stxy @val1

	; set the kind of result, symbol, segment, and post-processing
	; for the result
	lda @segment
	sta __expr_segment
	lda @kind
	sta __expr_kind
	cmp #VAL_ABS
	bne @rel_result
@abs_result:
	lda @val1+1		; is MSB of result 0?
	bne :+
	lda #$01
	skw
:	lda #$02
	jmp @ok			; -> done

@rel_result:
	lda @segment
	sta __expr_segment
	cmp asm::segment	; is the result in a different segment?
	bne @sym_result		; if so, need a symbol-relative answer

@seg_result:
	; same segment, no need to lookup symbol
	lda asm::segmode	; get the size of segment for result size
	clc
	adc #$01		; add 1 to get # of bytes
	bcc @rel_done		; and continue to finish up building result

@sym_result:
	ldx @symbol
	stx __expr_symbol
	ldy @symbol+1
	sty __expr_symbol+1

	lda @segment
	cmp #SEG_UNDEF		; is segment undefined?
	beq :+			; if so, assume 2 bytes

	cmpw #PC_SYMBOL_ID
	bne  :+			; if '*' assume 2 bytes

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
@ok:	ldxy @val1
	clc			; ok
@ret:	rts

@cont:	inx			; move past token index
	cmp #TOK_UNARY_OP
	bne :+
	jsr @eval_unary
	jmp @evalloop

:	cmp #TOK_BINARY_OP
	bne :+
	jsr @eval_binary
	bcs @ret
	jmp @evalloop

:	cmp #TOK_PC
	bne @getoperand
	inc @i
	lda asm::mode		; check if we are in DIRECT mode
	bne :+			; continue to push relocation value if not

	; direct mode -> just push the current PC as VAL_ABS
	ldxy zp::virtualpc
	jmp @const

:	lda asm::segment	; current segment at assembly time
	sta @segment
	ldxy #PC_SYMBOL_ID	; use the magic value for PC as the symbol ID
	stxy @symbol
	ldxy zp::virtualpc	; offset from SECTION base
	stxy @val1
	bpl @pushrel		; finish by pushing this as a VAL_REL

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

	cmp #TOK_SYMBOL_ZP
	beq @sym
	cmp #TOK_SYMBOL
	bne @const

@sym:	; resolve symbol and push its value/metadata
	stxy @symbol
	cmpw #SYM_UNRESOLVED	; check magic "unresolved" value
	beq @unresolved

	CALLMAIN lbl::addr_and_mode
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
	; if we're here, the expression is unresolved (so far)
	; in pass 1, that's fine - return and assume we will figure it out
	ldx zp::pass
	cpx #$02
	bne @dummy		; pass 1 -> proceed with dummy

	; if in pass 2 and haven't seen the label, return error
	RETURN_ERR ERR_UNRESOLVABLE_LABEL

@dummy:	; return dummy
	lda #SEG_UNDEF
	sta @segment		; mark segment as undefined
	ldxy #$00		; dummy value
	lda #VAL_REL
	skw

@const: lda #VAL_ABS
	sta @kind		; set "kind" to constant

@valdone:
	jsr @pushval		; store value and metadata
	jmp @evalloop		; continue processing

;--------------------------------------
; handle unary operator
@eval_unary:
	lda __expr_rpnlist,x	; get the operator
	pha			; and save it
	inx			; move index past the operator
	stx @i			; update list index

	; get the operand for the unary operation
	jsr @popval

	; if VAL_REL, this must be the last operator
	pla				; restore operator
@lsb:	cmp #'<'
	bne @msb
	lda @kind
	cmp #VAL_ABS
	beq :+				; if ABS, no need for post-processing
	lda #POSTPROC_LSB
	sta @postproc
:	ldy #$00
	beq @pushval_with_postproc	; branch always

@msb:	cmp #'>'
	bne @invalid_unary
	lda @kind
	cmp #VAL_ABS
	beq :+			; if ABS, no need for post-processing
	lda #POSTPROC_MSB
	sta @postproc
:	tya
	tax
	ldy #$00
	beq @pushval+4	; branch always

@invalid_unary:
	brk		; TODO: should be impossible

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
	lda @kind
	sta @operands+2,x	; store kind (RELOCATE or ABSOLUTE)
	lda @segment
	sta @operands+3,x	; store segment ID
	lda @symbol
	sta @operands+4,x	; store symbol ID LSB
	lda @symbol+1
	sta @operands+5,x	; store symbol ID MSB
	lda @postproc
	sta @operands+6,x	; store postproc

	; update stack pointer (sp += 7)
	lda @sp
	clc
	adc #$07
	sta @sp
	;clc
	rts

;--------------------------------------
; handle binary operator
@eval_binary:
	lda __expr_rpnlist,x	; get the operator
	sta @operator		; and save it
	inx			; move index past the operator
	stx @i			; update list index

	; get the operands for the binary operation
	jsr @popval
	stxy @val2
	lda @kind
	sta @kind2
	lda @segment
	sta @segment2
	lda @symbol
	sta @symbol2
	lda @symbol+1
	sta @symbol2+1
	lda @postproc
	sta @postproc2
	cmp #POSTPROC_NONE
	beq @getval1
	jsr @handle_postproc
	bcs @err
	stxy @val2

@getval1:
	jsr @popval
	stxy @val1
	lda @kind
	sta @kind1
	lda @segment
	sta @segment1
	lda @symbol
	sta @symbol1
	lda @symbol+1
	sta @symbol1+1
	lda @postproc
	sta @postproc1
	cmp #POSTPROC_NONE
	beq @cont_eval
	jsr @handle_postproc
	bcc :+
	; TODO: make error code for this

@err:	rts
:	stxy @val1

@cont_eval:
	lda @operator		; restore operator
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
	jmp @pushval

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
	jmp @pushval

@chkmul:
	cmp #'*'	; MULTIPLY
	bne @chkdiv
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	; get the product TODO: 32-bit precision expressions?
	ldxy @val1
	stxy r0
	ldxy @val2
	stxy r2
	jsr m::mul16
	ldxy ra	; get product
	jmp @pushval

@chkdiv:
	cmp #'/'	; DIVIDE
	bne @chkand
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	ldxy @val2
	stxy r2
	ldxy @val1
	stxy r0
	jsr m::div16
	ldxy r0
	jmp @pushval

@chkand:
	cmp #'&'	; AND
	bne @chkor
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @val1
	and @val2
	tax
	lda @val1+1
	and @val2+1
	tay
	jmp @pushval

@chkor: cmp #'.'	; OR
	bne @chkeor
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @val1
	ora @val2
	tax
	lda @val1+1
	ora @val2+1
	tay
	jmp @pushval

@chkeor:
	cmp #'^'	; EOR
	bne @unknownop
	jsr @reduce_operation_other
	bcc :+
	RETURN_ERR ERR_CANNOT_REDUCE

:	lda @val1
	eor @val2
	tax
	lda @val1+1
	eor @val2+1
	tay
	jmp @pushval
@unknownop:
	brk		; TODO: should be impossible

;--------------------------------------
@popval:
	; sp -= 7
	lda @sp
	sec
	sbc #$07
	sta @sp

	ldx @sp
	lda @operands+2,x	; get "kind"
	sta @kind
	lda @operands+3,x	; get segment ID (for kind==RELOCATE)
	sta @segment
	lda @operands+4,x	; get symbol ID (for kind==RELOCATE)
	sta @symbol
	lda @operands+5,x	; get symbol ID (for kind==RELOCATE)
	sta @symbol+1
	lda @operands+6,x	; get post processing
	sta @postproc
	ldy @operands+1,x	; get MSB
	lda @operands,x		; and LSB
	tax
	rts

;--------------------------------------
; REDUCE OPERATION ADDITION
; Determines the @segment and @symbol for the two active operands
; Also validates that the combination of ABS/REL modes is valid
@reduce_operation_addition:
	lda @kind1
	cmp #VAL_ABS
	bne @add_a_rel

@add_a_abs:
	lda @kind2
	cmp #VAL_ABS
	bne @add_a_abs_b_rel

	; A=ABS, B=ABS, no need to worry about segment/symbol
	sta @kind	; set @kind to ABS
	RETURN_OK

@add_a_abs_b_rel:
	; A=ABS, B=REL, use b's symbol and segment
	lda @segment2
	sta @segment
	lda @symbol2
	sta @symbol
	RETURN_OK

@add_a_rel:
	lda @kind2
	cmp #VAL_ABS
	beq :+		; -> ok
	; A=REL, B=REL is illegal, return err
	sec
	rts

@add_a_rel_b_abs:
	; A=REL, B=ABS, use a's symbol and segment
	lda @segment1
	sta @segment
	lda @symbol1
	sta @symbol
:	RETURN_OK

;--------------------------------------
; REDUCE OPERATION SUBTRACTION
; Validates/reduces the segment/symbol/and kind for a subtraction operation
; Also validates that the combination of ABS/REL modes is valid
@reduce_operation_subtraction:
	lda @kind1
	cmp #VAL_ABS
	bne @sub_a_rel

@sub_a_abs:
	lda @kind2
	cmp #VAL_REL
	bne :+
	; if val1 is ABS, val2 must be too
	sec
	rts			; return err

:	lda #VAL_ABS
	sta @kind
	RETURN_OK

@sub_a_rel:
	lda @kind2
	cmp #VAL_REL
	beq @sub_a_rel_b_rel

	; A=REL, B=ABS, result is REL with A's symbol/segment
	sta @kind		; kind = REL
	lda @segment1
	sta @segment
	lda @symbol1
	sta @symbol
	RETURN_OK

@sub_a_rel_b_rel:
	; this case can be reduced to an ABS entry if val1 and val2 share
	; a segment
	lda @segment1
	cmp @segment2
	bne :+
	sec
	rts		; different segments -> err

:	lda #VAL_ABS
	sta @kind	; set kind to absolute
	RETURN_OK	; and we're done

;--------------------------------------
; REDUCE OPERATION OTHER
; For operators that need to resolve to constant (pretty much anything but
; addition and subtraction)
@reduce_operation_other:
	; validate that both values are ABSolute
	lda @kind1
	cmp @kind2
	beq :+
@reduce_err:
	sec
	rts		; invalid

:	cmp #VAL_ABS
	bne @reduce_err
	sta @kind
	RETURN_OK

;--------------------------------------
; HANDLE POSTPROC
; Validates a binary operand's post-processing
; Post-processing is allowed if symbol is in the same segment as the
; expression (asm::segment)
@handle_postproc:
	lda @postproc
	beq :+++	; -> ok

	; check if symbol is in same segment
	; (can't do postproc on inter-segment symbol)
	CALLMAIN lbl::getsegment
	cmp asm::segment
	beq :+
	sec
	rts
:	ldxy @symbol		; restore symbol ID

	; get the address (addend) of the label within the segment
	CALLMAIN lbl::by_id

	lda @postproc
	cmp #POSTPROC_MSB
	beq :+
	; set value to just the LSB of label address
	ldy #$00		; clear MSB
	rts
:	; set value to MSB (move MSB to LSB)
	tya
	tax
:	RETURN_OK
.endproc

;*******************************************************************************
; PARSE
; Parses the expression into a RPN list of tokens evaluatable by
; expr::eval_token_list
; IN:
;   - zp::line: pointer to the expression to parse
; OUT:
;   - expr::rpnlist a list of tokens for evaluation (in RPN format)
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

	lda (zp::line),y
	bne :+
	sec
	rts			; no expression

:	; by default flag that operator might be unary
	iny
	sty @may_be_unary

@l0:	ldy #$00
	lda (zp::line),y
	jsr is_whitespace	; eat whitespace
	bne :+

	; check whitespace behavior, finish if configured as terminator
	lda end_on_whitespace
	bne @done
	jsr inc_line
	bne @l0		; branch always

:	lda (zp::line),y
	jsr @isterminator
	beq @done

@rparen:
	cmp #'('
	bne @lparen
	inc @may_be_unary
	jsr @pushop
	jsr inc_line
	bne @l0		; branch always

@lparen:
	cmp #')'
	bne @checkop
	ldx #$00
	stx @may_be_unary

@paren_eval:
	ldx @num_operators
	beq @err	; no parentheses found
	lda @operators-1,x
	cmp #'('
	bne :+
	jsr @popop	; pop the parentheses
	jsr inc_line
	bne @l0		; branch always - done evaluating this () block

:	jsr @eval	; append the top operation/operand(s)
	jmp @paren_eval

@checkop:
	;ldy #$00
	lda (zp::line),y
	cmp #'*'		; '*' can be a value or operator
	bne :+
	ldx @may_be_unary	; if unary logic applies, treat as value (PC)
	bne @getoperand

:	jsr isoperator
	bne @getoperand
	pha			; save the operator
	jsr @priority		; get the priority of this operator

@process_ops:
	ldx @num_operators	; any operators to the left?
	beq @process_ops_done
	dex

	; if the operator to the left has >= priority, append it to result
	cmp @priorities,x
	beq :+
	bcs @process_ops_done
:	pha			; save priority
	jsr @eval		; append operation to the stack
	pla			; get priority
	jmp @process_ops	; continue til op on left has lower priority

@process_ops_done:
	pla
	jsr @pushop
	jsr inc_line
	inc @may_be_unary
	bne @l0			; branch always

@getoperand:
	jsr get_operand		; have we found a valid operand?
	bcs @ret		; no

@operand:
	jsr @appendval
	lda #$00
	sta @may_be_unary
	jmp @l0

@done:	ldx @num_operators	; if there are still ops on stack
	beq @end		; no operators: terminate the RPN list
	jsr @eval		; evaluate each remaining operator
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

;------------------
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

;------------------
@popop:
	dec @num_operators
	ldx @num_operators
	lda @operators,x
	rts

;------------------
@pushop:
	ldx @num_operators
	cpx #MAX_OPERATORS
	bcc :+
:	sta @operators,x
	pha
	jsr @priority
	sta @priorities,x
	pla
	inc @num_operators
	rts

;------------------
@priority:
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

@priochars: .byte '+', '-', '*', '/', '&', '^', '.', '<', '>'
@prios:	    .byte  1,   1,   2,   2,   3,   4,   5,   3,   3
@num_prios=*-@prios

;------------------
; appends the operands involved in the evaluation followed by the operation
; to the RPN result
; returns the evaluation of the operator in .A on the operands @val1 and @val2
@eval:	jsr @popop
	cmp #'('		; ignore opening paren sentinel
	beq @evaldone

	; check if operator is unary
	pha			; save operator
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
	sta __expr_rpnlist,x	; write TOKEN type
	pla			; get operator
	sta __expr_rpnlist+1,x	; write operator
	inx
	inx
	stx @i
@evaldone:
	rts

;--------------------------------------
; append .XY (token type in .A) to the RPN list result
@appendval:
	cmp #TOK_PC
	bne :+
	; TOK_PC only takes 1 byte
	ldx @i
	sta __expr_rpnlist,x
	bpl :++			; branch always

:	pha
	txa
	ldx @i
	sta __expr_rpnlist+1,x	; LSB
	tya
	sta __expr_rpnlist+2,x	; MSB
	pla
	sta __expr_rpnlist,x	; TOKEN type
	inx
	inx
:	inx
	stx @i
	rts
.endproc

;*******************************************************************************
; GETLABEL
; Reads the given label and returns the address of it if there
; is one
; IN:
;  - .XY: pointer to the label to get the address of
; OUT:
;  - zp::line: updated to point past the label parsed
;  - .C        is set if no label is found
;  - .A:       the size of the label's address
;  - .XY:      the ID for the label
.proc get_label
@id=zp::expr
@mode=r0
	CALLMAIN lbl::isvalid 	; if verifying, let this pass if label is valid
	bcs @done

	; if we are only verifying (e.g. in pass 1 of assembly), label
	; ID is not final, proceed with dummy id
	ldxy #SYM_UNRESOLVED
	stxy @id

@get_id:
	; if not verifying (e.g. in pass 2), label ID is final; try to get it
	ldxy zp::line
	CALLMAIN lbl::find
	bcc :+

	; failed to lookup symbol ID, check if pass 1
	ldx zp::pass
	cpx #$02
	bcs @done

	ldx #$01
	stx @mode	; default to ABS mode
	bcc @updateline	; proceed with dummy ID

:	stxy @id
	CALLMAIN lbl::addrmode
	sta @mode

@updateline:
	; move the line pointer to the separator
	ldy #$00
@l0:	lda (zp::line),y
	jsr isseparator
	beq :+
	jsr inc_line
	bne @l0
:
	lda @mode	; restore mode
@ok:	ldxy @id	; get label
	clc		; ok
@done:	rts
.endproc

;*******************************************************************************
; ISVAL
; Checks if the word in zp::line is a value or not
; OUT:
;  - .C: clear if the string is a hex or decimal value
.proc isval
	; allow first char to be '$' or '*'
	ldx #$00	; 0 if decimal, 1 if hex
	lda (zp::line),y

	; check for '*' (current PC)
	cmp #'*'
	bne :+
	ldy #$01
	lda (zp::line),y
	jsr isseparator
	beq @done
	bne @err
:	cmp #$27	; single quote
	bne :+
	RETURN_OK	; if single quote, this is either a value or nothing

:	cmp #'$'
	bne @cont
	inx		; flag hex
	iny

@cont:	lda (zp::line),y
	jsr isseparator
	beq @done

	cpx #$00
	beq @dec

	; check hex
	cmp #$66+1	; 'f'+1
	bcs @err
	cmp #$61	; 'a'
	bcs @ok
	cmp #$46+1	; 'F'+1
	bcs @err
	cmp #$41	; 'A'
	bcs @ok

@dec:	cmp #'0'
	bcc @err
	cmp #'9'+1
	bcs @err

@ok:	iny
	bne @cont

@done:	RETURN_OK
@err:	sec
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
	jsr isval
	bcs @label		; not a literal value, try label

@star:	; check for '*'
	ldy #$00
	lda (zp::line),y
	cmp #'*'
	bne :+			; if not '*', check label
	jsr inc_line		; move past the '*'
	lda #TOK_PC		; if '*' just return the token for current PC
	RETURN_OK

:	jsr get_val		; is this a value?
	bcs @ret
	lda #TOK_VALUE
@ret:	rts

@label: ldxy zp::line
	jsr get_label		; is it a label?
	bcs @ret

	cmpw #SYM_UNRESOLVED	; is label undefined (id == $ffff)?
	beq @abs		; if so, just return placeholder token

	pha			; save address mode
	stxy @lbl
	CALLMAIN lbl::getsegment
	ldxy @lbl
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
.endproc

;*******************************************************************************
; GETVAL
; Parses zp::line for a decimal or hexadecimal value up to 16 bits in size.
; It may also parse a character in the format 'x'.  This must be a 1 byte
; value.
;  - zp::line: the text to parse a value from
; OUT:
;  - zp::line: updated to point past the value that was parsed
;  -.XY: the value of the string in zp::line
;  -.C: set on error and clear if a value was extracted.
.proc get_val
@val=zp::expr
	ldy #$00
	sty @val
	sty @val+1
	lda (zp::line),y
	cmp #'$'
	beq @hex

	cmp #$27		; single quote
	bne @decimal

;------------------
@char:
	jsr inc_line
	lda (zp::line),y	; read the character value
	tax			; put it into .X
	jsr inc_line
	lda (zp::line),y	; make sure there is a terminating quote
	cmp #$27		; single quote
	bne @badchar
	jsr inc_line
	lda #$01		; result is 1 byte in size
	clc			; ok
@ret:	rts

;------------------
@decimal:
	ldxy zp::line
	CALLMAIN atoi	; convert to binary
	bcs @ret			; return err
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	stxy @val
	jmp @success

;------------------
@hex:
	iny
@l0:	lda (zp::line),y
	jsr isseparator
	beq @done

	jsr chtohex
	bcc @next

@badchar:
	RETURN_ERR ERR_UNEXPECTED_CHAR

@next:	asl
	asl
	asl
	asl
	asl
	rol @val
	rol @val+1
	bcs @err
	asl
	rol @val
	rol @val+1
	bcs @err
	asl
	rol @val
	rol @val+1
	bcs @err
	asl
	rol @val
	rol @val+1
	bcs @err	; oversized value
	iny
	bne @l0

@done: 	tya
	clc
	adc zp::line
	sta zp::line
	bcc @success
	inc zp::line+1
@success:
	ldx @val
	ldy @val+1
	beq :+
	lda #$02	; 2 bytes
	skw
:	lda #$01	; 1 byte
	RETURN_OK

@err:	RETURN_ERR ERR_OVERSIZED_OPERAND
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
.proc isoperator
@xsave=zp::util+2
	stx @xsave
	ldx #@numops-1
:	cmp @ops,x
	beq @end
	dex
	bpl :-
@end:	php
	ldx @xsave
	plp
	rts
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.', '<', '>'
@numops = *-@ops
.endproc

;*******************************************************************************
; CHTOHEX
.proc chtohex
	JUMPMAIN util::chtohex
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
