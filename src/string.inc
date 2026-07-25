.import __str_cat
.import __str_compare
.import __str_comparez
.import __str_len
.import __str_toupper
.import __str_uncompress
.import __str_breakpoints_line_noname

.scope str
	cat        = __str_cat
	compare    = __str_compare
	comparez   = __str_comparez
	len        = __str_len
	toupper    = __str_toupper
	uncompress = __str_uncompress
.endscope

;*******************************************************************************
; STREQ
; Compares the the string in (YX) to the string s.
.macro streq s, len
	stx zp::str0
	sty zp::str0+1

	lda #<s
	sta zp::str2
	lda #>s
	sta zp::str2+1
	lda #len
	jsr __str_compare
.endmacro
