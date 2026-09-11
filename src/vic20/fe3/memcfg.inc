.ifndef VIC20_FE3_MEMCFG_INC
VIC20_FE3_MEMCFG_INC = 1

; MEMCFG_* (the bits of memcfg::blocks) are declared alongside the rest of the
; FE3 memory map
.include "banks.inc"

.import __memcfg_blocks
.import __memcfg_reg9c03
.import __memcfg_apply
.import __memcfg_set_basic_ptrs
.import __memcfg_edit

.scope memcfg
	; bitmask of the blocks that are enabled (MEMCFG_*)
	blocks       = __memcfg_blocks

	; the FE3 disable bits corresponding to `blocks`; kept in sync with
	; it by memcfg::apply
	reg9c03      = __memcfg_reg9c03

	apply        = __memcfg_apply
	set_basic_ptrs = __memcfg_set_basic_ptrs
	edit         = __memcfg_edit
.endscope

.endif
