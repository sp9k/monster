.import __debug_breakpointslo
.import __debug_breakpointshi
.import __debug_breakpoint_lineslo
.import __debug_breakpoint_lineshi
.import __debug_breakpoint_fileids
.import __debug_breakpoint_flags
.import __debug_brksetaddr
.import __debug_setbrkatline
.import __debug_edit_watches
.import __debug_enable_expansion
.import __debug_gotoaddr
.import __debug_load_file
.import __debug_go
.import __debug_go_basic
.import __debug_interface
.import __debug_is_brk
.import __debug_numbreakpoints
.import __debug_reenter
.import __debug_remove_breakpoint
.import __debug_removebreakpointbyid
.import __debug_restore_debug_low
.import __debug_restore_debug_zp
.import __debug_restore_user_zp
.import __debug_save_debug_zp
.import __debug_save_user_zp
.import __debug_setbreakpoint
.import __debug_save_prog_state
.import __debug_shift_breakpointsd
.import __debug_shift_breakpointsu
.import __debug_start
.import __debug_step
.import __debug_step_out
.import __debug_step_over
.import __debug_sw_valid
.import __debug_swap_in
.import __debug_swap_out
.import __debug_trace
.import __debug_swap_user_mem
.import __debug_setbrkataddr
.import __debug_enable_expansion

.scope dbg
	breakpointslo        = __debug_breakpointslo
	breakpointshi        = __debug_breakpointshi
	breakpoint_lineslo   = __debug_breakpoint_lineslo
	breakpoint_lineshi   = __debug_breakpoint_lineshi
	breakpoint_fileids   = __debug_breakpoint_fileids
	breakpoint_flags     = __debug_breakpoint_flags
	brksetaddr           = __debug_brksetaddr
	setbrkatline         = __debug_setbrkatline
	setbrkataddr         = __debug_setbrkataddr
	edit_watches         = __debug_edit_watches
	enable_expansion     = __debug_enable_expansion
	gotoaddr             = __debug_gotoaddr
	loadfile             = __debug_load_file
	is_brk               = __debug_is_brk
	interface            = __debug_interface
	numbreakpoints       = __debug_numbreakpoints
	reenter              = __debug_reenter
	removebreakpoint     = __debug_remove_breakpoint
	removebreakpointbyid = __debug_removebreakpointbyid
	restore_debug_low    = __debug_restore_debug_low
	restore_debug_zp     = __debug_restore_debug_zp
	restore_user_zp      = __debug_restore_user_zp
	save_debug_zp        = __debug_save_debug_zp
	save_user_zp         = __debug_save_user_zp
	shift_breakpointsd   = __debug_shift_breakpointsd
	shift_breakpointsu   = __debug_shift_breakpointsu
	start                = __debug_start
	step                 = __debug_step
	step_out             = __debug_step_out
	step_over            = __debug_step_over
	sw_valid             = __debug_sw_valid
	swap_in              = __debug_swap_in
	swap_out             = __debug_swap_out
	swapusermem          = __debug_swap_user_mem
	trace                = __debug_trace
.endscope
