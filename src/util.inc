.include "zeropage.inc"

.import atoi

.import __util_chtohex
.import __util_hextostr
.import __util_is_whitespace
.import __util_is_null_return_space_comma_closingparen_newline
.import __util_parsehex
.import __util_todec
.import __util_todec8
.import __util_todec24
.import __util_div24_16
.import __util_isoperator
.import __util_is_separator
.import __util_is_alphanum
.import __util_parse_enquoted_line
.import __util_parse_enquoted_string

.scope util
	chtohex               = __util_chtohex
	hextostr              = __util_hextostr
	isalphanum            = __util_is_alphanum
	is_null_return_space_comma_closingparen_newline = __util_is_null_return_space_comma_closingparen_newline
	isoperator            = __util_isoperator
	isseparator           = __util_is_separator
	is_whitespace         = __util_is_whitespace
	parse_enquoted_line   = __util_parse_enquoted_line
	parse_enquoted_string = __util_parse_enquoted_string
	parsehex	      = __util_parsehex
	todec                 = __util_todec
	todec8		      = __util_todec8
	todec24		      = __util_todec24
	div24_16	      = __util_div24_16
.endscope
