.import __monitor_clear
.import __monitor_getch
.import __monitor_init
.import __monitor_int
.import __monitor_line
.import __monitor_log
.import __monitor_outfile
.import __monitor_puts
.import __monitor_reenter
.import __monitor_quit
.import __monitor_update_pc_view
.import __monitor_window
.import __monitor_windowed

.scope mon
	clear     = __monitor_clear
	getch     = __monitor_getch
	init	  = __monitor_init
	int	  = __monitor_int
	line      = __monitor_line
	log       = __monitor_log
	outfile   = __monitor_outfile
	puts      = __monitor_puts
	reenter   = __monitor_reenter
	quit           = __monitor_quit
	update_pc_view = __monitor_update_pc_view
	window         = __monitor_window
	windowed       = __monitor_windowed
.endscope
