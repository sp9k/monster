.ifndef MEMCFG_INC
MEMCFG_INC = 1

; The memory configuration system belongs to the expansion that provides the
; banking, so the real header lives with it.  Targets without one have no
; blocks to configure.
.if .defined(vic20) .and .defined(ultimem)
	.include "vic20/ultimem/memcfg.inc"
.endif

.endif
