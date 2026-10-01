.ifndef CONTEXT_TOKENS_INC
CONTEXT_TOKENS_INC = 1
.include "lexer_tokens.inc"
.import __mem_spare

; Internal context format, rebuilt each assembly pass (never an object format).
; Record: byte total size, word source line, tokens, LEX_END.
; Zero size marks the end of a body. Payload zero bytes are ordinary data.
; WORD/NUMBER/STRING/CHAR/FLOAT/RAW: kind, byte spelling size, spelling bytes.
; SPACE: one canonical space. Comparisons and punctuation: one token byte.
; INTEGER: kind, word value; source view uses one placeholder backed by metadata.
CTX_TOKEN_BUFFER = __mem_spare+$200
CTX_TOKEN_LIMIT = $ff
CTX_ITER_NAME = __mem_spare+$180
.endif
