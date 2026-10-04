.ifndef RPN_INC
RPN_INC = 1

MAX_RPN_LEN   = $20	; size of the RPN token list (__expr_rpnlist)

TOK_SYMBOL    = 1	; symbol e.g. "label"
TOK_SYMBOL_ZP = 2	; zeropage symbol e.g. "tmp"
TOK_VALUE     = 3	; constant value e.g. 123
TOK_PC        = 4	; current PC e.g. '*'
TOK_BINARY_OP = 5	; binary operator e.g. '+' or '-'
TOK_UNARY_OP  = 6	; unary operator e.g. '<'
TOK_FLOAT     = 7	; float literal e.g. 1.5 (value is an index into
			; fliterals, not the number itself)
TOK_WIDE      = 8	; integer literal followed by three value bytes
TOK_END       = $ff	; end of expression marker

.endif
