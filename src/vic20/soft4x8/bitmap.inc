.ifndef VIC20_BITMAP_INC
.define VIC20_BITMAP_INC 1

BITMAP_ADDR   = $1100
COLMEM_ADDR   = $9400
BITMAP_HEIGHT = 192
BITMAP_WIDTH  = 160

.import __screen_init
.import __screen_char_addr
.import __screen_clr
.import __screen_clrcolor
.import __screen_clr_part
.import __screen_clrline
.import __screen_columnslo
.import __screen_columnshi
.import __screen_rvsline_part

.scope bm
	init         = __screen_init
	charaddr     = __screen_char_addr
	clr          = __screen_clr
	clrcolor     = __screen_clrcolor
	clrline      = __screen_clrline
	clrpart      = __screen_clr_part
	columnslo    = __screen_columnslo
	columnshi    = __screen_columnshi
	rvsline_part = __screen_rvsline_part
.endscope
.endif
