.ifndef SCREEN_INC
SCREEN_INC = 1

.import __screen_blank
.import __screen_clr
.import __screen_clrcolor
.import __screen_clrline
.import __screen_init
.import __screen_save
.import __screen_restore
.import __screen_rvsline_part
.import __screen_unblank

.import __scr_shl
.import __scr_shr
.import __scr_reset
.import __scr_restore

.scope scr
	blank        = __screen_blank
	rvsline_part = __screen_rvsline_part
	clr          = __screen_clr
	clrcolor     = __screen_clrcolor
	clrline      = __screen_clrline
	init         = __screen_init
	save         = __screen_save
	restore      = __screen_restore
	unblank      = __screen_unblank

	shl     = __scr_shl
	shr     = __scr_shr
.endscope

.endif
