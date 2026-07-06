.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_eval_list
.import __expr_kind
.import __expr_parse
.import __expr_postproc
.import __expr_rpnlist
.import __expr_rpnlistlen
.import __expr_segment
.import __expr_symbol
.import __expr_value

VAL_ABS = 0
VAL_REL = 1

POSTPROC_NONE = 0
POSTPROC_LSB  = 1
POSTPROC_MSB  = 2

.scope expr
	end_on_ws  = __expr_end_on_whitespace
	eval       = __expr_eval
	eval_list  = __expr_eval_list
	kind       = __expr_kind
	parse      = __expr_parse
	rpnlist    = __expr_rpnlist
	rpnlistlen = __expr_rpnlistlen
	segment    = __expr_segment
	symbol     = __expr_symbol
	postproc   = __expr_postproc
	value      = __expr_value
.endscope
