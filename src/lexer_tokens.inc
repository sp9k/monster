.ifndef LEXER_TOKENS_INC
LEXER_TOKENS_INC = 1

; token kinds returned by the lexer (lex::peek)
; punctuation (#, (, ), comma, colon, *, etc.) uses same character as its KIND
LEX_END		= $00
LEX_SPACE	= $80
LEX_WORD	= $81
LEX_NUMBER	= $82
LEX_STRING	= $83
LEX_CHAR	= $84
LEX_EQ		= $85	; ==
LEX_NE		= $86	; !=
LEX_LE		= $87	; <=
LEX_GE		= $88	; >=
LEX_FLOAT	= $89
LEX_INTEGER	= $8a	; 16-bit value (e.g. a .REP iterator)
LEX_RAW		= $8b	; bad syntax (reported only if the line is assembled)
LEX_ARG		= $8c	; macro argument: frame depth, argument index
LEX_IMMARG	= $8d	; macro argument that was given with a '#'

ARG_PLACEHOLDER	= '?'	; character written in place of an argument

LEX_BANK	= FINAL_BANK_EXPR
LEX_DECODE_BANK	= FINAL_BANK_EXPR

.endif
