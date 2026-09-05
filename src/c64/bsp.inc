.ifndef C64_BSP_INC
C64_BSP_INC = 1

.import __bsp_install_tracer
.import __bsp_uninstall_tracer
.import __bsp_restore_debug_state
.import __bsp_restore_debug_visual
.import __bsp_restore_prog_visual
.import __bsp_restore_prog_state
.import __bsp_save_debug_state
.import __bsp_save_debug_visual
.import __bsp_save_prog_state
.import __bsp_save_prog_visual

.scope bsp
	install_tracer       = __bsp_install_tracer
	uninstall_tracer     = __bsp_uninstall_tracer
	restore_debug_state  = __bsp_restore_debug_state
	restore_prog_state   = __bsp_restore_prog_state
	restore_prog_visual  = __bsp_restore_prog_visual
	restore_debug_visual = __bsp_restore_debug_visual
	save_debug_state     = __bsp_save_debug_state
	save_debug_visual    = __bsp_save_debug_visual
	save_prog_state      = __bsp_save_prog_state
	save_prog_visual     = __bsp_save_prog_visual
.endscope

.endif
