.import __ui_machine_state
.import __ui_regs_contents
.import __ui_render_breakpoint
.import __ui_update_statusline
.import __ui_memline
.import __ui_render_watch

.scope ui
	machine_state     = __ui_machine_state
	regs_contents     = __ui_regs_contents
	update_statusline = __ui_update_statusline

	render_breakpoint = __ui_render_breakpoint
	render_watch      = __ui_render_watch
	memline           = __ui_memline
.endscope
