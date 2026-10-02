.ifndef CONTEXT_TOKENS_INC
CONTEXT_TOKENS_INC = 1
.include "lexer_tokens.inc"
.import __mem_spare

;*******************************************************************************
; Context record format (.MAC/.REP body):
;  - 1 byte:  size of the record (including this header)
;  - 2 bytes: source line number
;  - tokens, terminating with LEX_END
;
; A record with size=0 marks the end of the body
; Tokens are stored as:
;   ----------------------------------------|-----------------------------------
;   |              Token type               |          Format                  |
;   |---------------------------------------|----------------------------------|
;   | WORD, NUMBER, STRING, CHAR, FLOAT, RAW| kind, length, the characters     |
;   | SPACE, comparisons, and punctuation:  | just the kind                    |
;   | INTEGER                               | kind, 16-bit value               |
;   | ARG, IMMARG                           | kind, frame depth, argument index|
;   ----------------------------------------|-----------------------------------
;*******************************************************************************

CTX_TOKEN_BUFFER = __mem_spare+$300	; shared with lbl's scope buffer
CTX_TOKEN_LIMIT  = $ff
CTX_ITER_NAME    = __mem_spare+$180	; name of the .REP iterator

.endif
