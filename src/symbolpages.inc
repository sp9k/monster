.include "symbolpagesconst.inc"

.import __sympage_load
.import __sympage_store
.import __sympage_copy_record
.import __sympage_copy_name
.import __sympage_name_buffer

.scope sympage
	load        = __sympage_load
	store       = __sympage_store
	copy_record = __sympage_copy_record
	copy_name   = __sympage_copy_name
	name_buffer = __sympage_name_buffer
.endscope

;*******************************************************************************
; PAGE LOAD
; Reads one byte from a symbol pool. Names use word offsets; other pools use
; byte offsets. The Y displacement is added in bytes after decoding the offset.
; ARGS:
;  - ptr:  zero-page address of the 16-bit pool offset (unchanged by the call)
;  - pool: SYM_RECORDS, SYM_NODES, SYM_INDEXES or SYM_NAMES
; IN:
;  - .Y: byte displacement from the decoded pool offset
; OUT:
;  - .A: byte read; N/Z reflect this byte
; PRESERVES:
;  - X/Y, all flags except N/Z, and the caller's memory mapping
; CLOBBERS:
;  - zp::bankaddr0/1, private page scratch, C64 REU transfer registers
.macro PAGE_LOAD ptr, pool
	jsr sympage::load
	.byte ptr, pool
.endmacro

;*******************************************************************************
; PAGE STORE
; Writes one byte using the same pool-offset encoding as PAGE_LOAD.
; ARGS:
;  - ptr:  zero-page address of the 16-bit pool offset (unchanged by the call)
;  - pool: SYM_RECORDS, SYM_NODES, SYM_INDEXES or SYM_NAMES
; IN:
;  - .A: byte to write
;  - .Y: byte displacement from the decoded pool offset
; PRESERVES:
;  - A/X/Y, all flags, and the caller's memory mapping
; CLOBBERS:
;  - zp::bankaddr0/1, private page scratch, C64 REU transfer registers
.macro PAGE_STORE ptr, pool
	jsr sympage::store
	.byte ptr, pool
.endmacro

;*******************************************************************************
; INDEX SWAP
; Swaps one byte at displacement Y between two entries in SYM_INDEXES.
; ARGS:
;  - left, right: zero-page addresses of the entries' byte offsets
; IN:
;  - .Y: byte displacement within each entry
; PRESERVES:
;  - X/Y, all flags except N/Z, both offsets, and the caller's memory mapping
; CLOBBERS:
;  - A, N/Z, zp::bankaddr0/1, private page scratch, C64 REU transfer registers
.macro INDEX_SWAP left, right
	PAGE_LOAD left, SYM_INDEXES
	pha
	PAGE_LOAD right, SYM_INDEXES
	PAGE_STORE left, SYM_INDEXES
	pla
	PAGE_STORE right, SYM_INDEXES
.endmacro
