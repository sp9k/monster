.import __monitor_clear
.import __monitor_draw_border
.import __monitor_enter
.import __monitor_enter_win
.import __monitor_getch
.import __monitor_init
.import __monitor_int
.import __monitor_line
.import __monitor_outfile
.import __monitor_puts
.import __monitor_reenter
.import __monitor_quit
.import __monitor_update_pc_view
.import __monitor_win_grow
.import __monitor_win_resync
.import __monitor_wintop

.scope mon
	clear       = __monitor_clear
	draw_border = __monitor_draw_border
	enter       = __monitor_enter
	enter_win = __monitor_enter_win
	getch     = __monitor_getch
	init	  = __monitor_init
	int	  = __monitor_int
	line      = __monitor_line
	outfile   = __monitor_outfile
	puts      = __monitor_puts
	reenter   = __monitor_reenter
	quit           = __monitor_quit
	update_pc_view = __monitor_update_pc_view
	win_grow       = __monitor_win_grow
	win_resync     = __monitor_win_resync
	wintop         = __monitor_wintop
.endscope
