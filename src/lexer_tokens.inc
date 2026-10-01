.ifndef LEXER_TOKENS_INC
LEXER_TOKENS_INC = 1

; Lexical tokens are separate from expr's evaluated RPN tokens. Single-byte
; punctuation uses its byte value, including #, (, ), comma, colon and *.
; Spans refer to the original source: no case folding or symbol lookup occurs.
LEX_END    = $00
LEX_SPACE  = $80
LEX_WORD   = $81
LEX_NUMBER = $82
LEX_STRING = $83
LEX_CHAR   = $84
LEX_EQ     = $85
LEX_NE     = $86
LEX_LE     = $87
LEX_GE     = $88
LEX_FLOAT  = $89
LEX_INTEGER = $8a ; captured iterator: little-endian word, no spelling payload
LEX_RAW     = $8b ; deferred malformed syntax; checked only when executed
LEX_BANK   = FINAL_BANK_EXPR

.endif
