.ifndef LEXER_INC
LEXER_INC = 1

.include "lexer_tokens.inc"

.import __lex_peek
.import __lex_advance
.import __lex_eatws
.import __lex_indirect
.import __lex_cached
.import __lex_decode
.import __lex_value_at
.scope lex
	peek        = __lex_peek
	advance     = __lex_advance
	eatws       = __lex_eatws
	indirect    = __lex_indirect
	cached      = __lex_cached
	decode      = __lex_decode
	value_at    = __lex_value_at
.endscope

.endif
