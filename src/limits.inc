.ifndef __limits_inc
.define __limits_inc

;*******************************************************************************
; LINKER
.ifdef vic20
.define MAX_LINK_SECTIONS $40
.define MAX_OBJS 124		; max number of object files in a link
.else
.define MAX_LINK_SECTIONS $08
.define MAX_OBJS 16
.endif

; Caps the fragment table for the full link and the fragments in each object.
.ifdef vic20
.define MAX_LINK_FRAGMENTS 512
.define MAX_FRAGMENTS 124
.else
.define MAX_LINK_FRAGMENTS 124
.define MAX_FRAGMENTS 32
.endif

.ifdef vic20
	.define MAX_LABELS 4096		; max number of named labels
	.define MAX_ANON   1776		; max number of anonymous labels
	.define MAX_FILES  128		; max files debug info can be generated for
	.define MAX_BREAKPOINTS 16	; max number of breakpoints that may be set
	.define MAX_WATCHPOINTS 8	; max number of watchpoints that may be set
	.define MAX_MACROS 128

; LINKER / OBJECT FILES
	.define MAX_SEGMENTS 128
	.define MAX_IMPORTS  512	; referenced imports; unused declarations are pruned
	.define MAX_EXPORTS  64
.elseif .defined(c64)
	.define MAX_LABELS 4096		; max number of named labels
	.define MAX_ANON   1776		; max number of anonymous labels
	.define MAX_FILES  64		; max files debug info can be generated for
	.define MAX_BREAKPOINTS 16	; max number of breakpoints that may be set
	.define MAX_WATCHPOINTS 8	; max number of watchpoints that may be set
	.define MAX_MACROS 128

; LINKER / OBJECT FILES
	.define MAX_SEGMENTS 8
	.define MAX_IMPORTS  512	; referenced imports; unused declarations are pruned
	.define MAX_EXPORTS  64
.endif

.endif
