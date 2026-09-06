.ifndef FP_INC
FP_INC = 1

;*******************************************************************************
; FP
; Floating point support built on the Commodore BASIC ROM's FP package.
; Currently VIC-20 only
.ifdef vic20
.define FP_SUPPORTED 1
.else
.define FP_SUPPORTED 0
.endif

; size of a CBM float (exponent + 4 mantissa bytes)
FP_SIZE = 5

; max number of significant digits accepted in a float literal
FP_MAX_DIGITS = 16

; Internal relational opcodes; results are unsigned integers 0 or 1.
FP_EQ = $70
FP_NE = $71
FP_LT = $72
FP_LE = $73
FP_GT = $74
FP_GE = $75

; Internal unary opcodes. Prefix signs/functions bind above binary operators.
FP_NEG   = $80
FP_POS   = $81
FP_FLOAT = $82
FP_INT   = $83	; checked unsigned word conversion (no rounding)
FP_TRUNC = $84
FP_ROUND = $85	; nearest integral float, ties away from zero
FP_FLOOR = $86
FP_CEIL  = $87
FP_ABS   = $88
FP_SQRT  = $89
FP_SIN   = $8a
FP_COS   = $8b
FP_LOG   = $8c
FP_EXP   = $8d

.ifndef FP_IMPL

.import __fp_arg1
.import __fp_arg2
.import __fp_val
.import __fp_binop
.import __fp_fromint
.import __fp_isfloat
.import __fp_parse
.import __fp_toint
.import __fp_unary
.import __fp_format

.scope fp
	arg1 = __fp_arg1
	arg2 = __fp_arg2
	val  = __fp_val

	binop   = __fp_binop
	fromint = __fp_fromint
	isfloat = __fp_isfloat
	parse   = __fp_parse
	toint   = __fp_toint
	unary   = __fp_unary
	format  = __fp_format
.endscope

.endif
.endif
