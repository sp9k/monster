.ifndef VIC20_ULTIMEM_MEMCFG_INC
VIC20_ULTIMEM_MEMCFG_INC = 1

; MEMCFG_* (the bits of memcfg::blocks) are declared alongside the rest of the
; UltiMem's memory map
.include "banks.inc"

.import __memcfg_blocks
.import __memcfg_reg9ff1
.import __memcfg_reg9ff2
.import __memcfg_apply
.import __memcfg_set_basic_ptrs
.import __memcfg_edit

.scope memcfg
	; bitmask of the blocks that are enabled (MEMCFG_*)
	blocks       = __memcfg_blocks

	; the UltiMem register values that realize `blocks`; kept in sync with
	; it by memcfg::apply
	reg9ff1      = __memcfg_reg9ff1
	reg9ff2      = __memcfg_reg9ff2

	apply        = __memcfg_apply
	set_basic_ptrs = __memcfg_set_basic_ptrs
	edit         = __memcfg_edit
.endscope

.endif
