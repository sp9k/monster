.importzp __edit_height

.import __edit_assemble_file
.import __edit_binary_flag
.import __edit_clear
.import __edit_current_file
.import __edit_debugging
.import __edit_enter_monitor
.import __edit_gets
.import __edit_gotoline
.import __edit_handle_key
.import __edit_highlight_en
.import __edit_highlight_line
.import __edit_init
.import __edit_load
.import __edit_refresh
.import __edit_resize
.import __edit_run
.import __edit_set_breakpoint
.import __edit_sethighlight
.import __edit_src2screen

.scope edit
	assemble_file  = __edit_assemble_file
	binary_flag    = __edit_binary_flag
	clear          = __edit_clear
	currentfile    = __edit_current_file
	debugging      = __edit_debugging
	entermonitor   = __edit_enter_monitor
	gets           = __edit_gets
	gotoline       = __edit_gotoline
	handlekey      = __edit_handle_key
	height         = __edit_height
	highlight_en   = __edit_highlight_en
	highlight_line = __edit_highlight_line
	init           = __edit_init
	load           = __edit_load
	refresh        = __edit_refresh
	resize         = __edit_resize
	run            = __edit_run
	setbreakpoint  = __edit_set_breakpoint
	sethighlight   = __edit_sethighlight
	src2screen     = __edit_src2screen
.endscope

;******************************************************************************
; CONSTANTS
MODE_COMMAND = 1
MODE_INSERT  = 2
