.ifndef LEXER_INC
LEXER_INC = 1

.include "lexer_tokens.inc"

; Source cursor API (LEX_BANK): peek/next return A=kind, X=span, Y=0, C=error.
; peek leaves zp::line alone; next consumes the span; advance consumes X bytes.
; significant skips source whitespace/control bytes, then peeks without consuming.
; Raw spelling remains in the source buffer. Binary replay uses a source view
; with cached spans and integer payloads; raw source entry clears lex::cached.
.import __lex_peek, __lex_next, __lex_advance, __lex_significant
.import __lex_indirect
.import __lex_cached, __lex_decode, __lex_value_at
.scope lex
	peek        = __lex_peek
	next        = __lex_next
	advance     = __lex_advance
	significant = __lex_significant
	indirect    = __lex_indirect
	cached      = __lex_cached
	decode      = __lex_decode
	value_at    = __lex_value_at
.endscope

.endif
