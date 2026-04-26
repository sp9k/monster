.ifndef __limits_inc
.define __limits_inc

.ifdef vic20
	.define MAX_LABELS 728		; max number of named labels
	.define MAX_ANON   1776		; max number of anonymous labels
	.define MAX_FILES  64		; max files debug info can be generated for
	.define MAX_BREAKPOINTS 16	; max number of breakpoints that may be set
	.define MAX_WATCHPOINTS 8	; max number of watchpoints that may be set
	.define MAX_MACROS 128
.elseif .defined(c64)
	.define MAX_LABELS 728		; max number of named labels
	.define MAX_ANON   1776		; max number of anonymous labels
	.define MAX_FILES  64		; max files debug info can be generated for
	.define MAX_BREAKPOINTS 16	; max number of breakpoints that may be set
	.define MAX_WATCHPOINTS 8	; max number of watchpoints that may be set
	.define MAX_MACROS 128
.endif

.endif
