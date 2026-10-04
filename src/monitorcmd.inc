; monitor address spaces
MON_MODE_VIRTUAL = $00
MON_MODE_IMAGE   = $01
MON_MODE_REAL    = $02	; reserved

.global __dbgcmd_regs
.global __dbgcmd_run
.global __dbgcmd_memory_mode
.global __dbgcmd_default_addr

.scope moncmd
	mode         = __dbgcmd_memory_mode
	default_addr = __dbgcmd_default_addr
	regs         = __dbgcmd_regs
	run          = __dbgcmd_run
.endscope
