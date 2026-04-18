.ifndef SCREEN_INC
SCREEN_INC = 1

.import __screen_rvsline_part
.import __screen_clr
.import __screen_clrcolor
.import __screen_clrline
.import __screen_clr_part
.import __screen_init
.import __screen_save
.import __screen_restore

.import __scr_shl
.import __scr_shr
.import __scr_reset
.import __scr_restore

.scope scr
	rvsline_part = __screen_rvsline_part
	clr          = __screen_clr
	clrcolor     = __screen_clrcolor
	clrline      = __screen_clrline
	clrpart      = __screen_clr_part
	init         = __screen_init
	save         = __screen_save
	restore      = __screen_restore

	shl     = __scr_shl
	shr     = __scr_shr
.endscope

.endif
