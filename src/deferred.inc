.ifndef DEFERRED_INC
DEFERRED_INC = 1

.global __deferred_code
.global __deferred_compile
.global __deferred_evaluate
.global __deferred_length

RELOC_EXPRESSION = $40
MAX_DEFERRED_LEN = $28	; 32-byte RPN list plus fragment-ID bytes

.scope deferred
	compile  = __deferred_compile
	evaluate = __deferred_evaluate
	code     = __deferred_code
	length   = __deferred_length
.endscope
.endif
