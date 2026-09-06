.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_floatval
.import __expr_floatstr
.import __expr_eval_bool
.import __expr_eval_float
.import __expr_eval_keep
.import __expr_fconst_add
.import __expr_fconst_clr
.import __expr_fconst_get
.import __expr_fconst_read
.import __expr_fconst_write
.import __expr_float_format
.import __expr_eval_list
.import __expr_kind
.import __expr_parse
.import __expr_postproc
.import __expr_rpnlist
.import __expr_rpnlistlen
.import __expr_segment
.import __expr_symbol
.import __expr_value

VAL_ABS   = 0
VAL_REL   = 1
VAL_FLOAT = 2

POSTPROC_NONE = 0
POSTPROC_LSB  = 1
POSTPROC_MSB  = 2

.scope expr
	end_on_ws  = __expr_end_on_whitespace
	eval       = __expr_eval
	floatval   = __expr_floatval
	floatstr   = __expr_floatstr
	eval_bool  = __expr_eval_bool
	eval_float = __expr_eval_float
	eval_keep  = __expr_eval_keep
	fconst_add = __expr_fconst_add
	fconst_clr = __expr_fconst_clr
	fconst_get = __expr_fconst_get
	fconst_read  = __expr_fconst_read
	fconst_write = __expr_fconst_write
	float_format = __expr_float_format
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
