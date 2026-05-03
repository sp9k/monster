.import __draw_hline
.import __draw_hiline
.import __draw_resetline
.import __draw_rvs_underline
.import __draw_scrollcolorsu
.import __draw_scrollcolorsd
.import __draw_coloroff
.import __draw_scrollcolorsd1
.import __draw_refresh_colors

COLOR_TEXT    = 0
COLOR_NORMAL  = 1
COLOR_RVS     = 2
COLOR_BRKON   = 3
COLOR_BRKOFF  = 4
COLOR_SUCCESS = 5
COLOR_SELECT  = 6
COLOR_FAILURE = 3

.scope draw
	hline          = __draw_hline
	hiline         = __draw_hiline
	resetline      = __draw_resetline
	rvs_underline  = __draw_rvs_underline
	scrollcolorsu  = __draw_scrollcolorsu
	scrollcolorsd  = __draw_scrollcolorsd
	scrollcolorsd1 = __draw_scrollcolorsd1
	coloroff       = __draw_coloroff
	refresh_colors = __draw_refresh_colors
.endscope
