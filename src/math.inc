.import __math_mul24
.import __math_div24
.import __math_div16
.import __math_align_up

.importzp __math_arg
.importzp __math_dividend
.importzp __math_divisor
.importzp __math_remainder

.scope m
	mul24     = __math_mul24
	div24     = __math_div24
	div16     = __math_div16
	align_up  = __math_align_up
	arg       = __math_arg
	dividend  = __math_dividend
	divisor   = __math_divisor
	remainder = __math_remainder
.endscope
