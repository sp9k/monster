.import __dbgcmd_regs
.import __dbgcmd_run
.import __dbgcmd_default_addr

.scope moncmd
	default_addr = __dbgcmd_default_addr
	regs         = __dbgcmd_regs
	run          = __dbgcmd_run
.endscope
