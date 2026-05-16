.import __text_char_index
.import __text_buffer
.import __text_bufferon
.import __text_bufferoff
.import __text_clrinfo
.import __text_clrline
.import __text_drawline
.import __text_insertmode
.import __text_index2cursor
.import __text_len
.import __text_linelen
.import __text_print
.import __text_info
.import __text_putch
.import __text_puterr
.import __text_render
.import __text_render_indirect
.import __text_rendered_line_len
.import __text_restorebuff
.import __text_savebuff
.import __text_scrolldown
.import __text_scrolldownn
.import __text_scrollup
.import __text_scrollupn
.import __text_show_ws
.import __text_status
.import __text_status_mode
.import __text_status_fmt
.import __text_tabl_dist
.import __text_tabr_dist
.import __text_update

; ESCAPE CODES
ESCAPE_STRING    = $ff	; 2 byte: address of string
ESCAPE_VALUE     = $fe	; 2 byte: value to print in hex
ESCAPE_VALUE_DEC = $fd	; 2 byte: value to print in decimal
ESCAPE_SPACING   = $fc	; 1 byte: number of spaces to insert
ESCAPE_BYTE      = $fb  ; 1 byte: the value to print in hex
ESCAPE_CHAR      = $fa	; 1 byte: the literal char to print
ESCAPE_GOTO      = $f8  ; 1 byte: number of spaces to insert

TEXT_INSERT  = 1
TEXT_REPLACE = 0

RVS_ON  = $ff
RVS_OFF = $00

BREAKPOINT_CHAR     = 133
BREAKPOINT_OFF_CHAR = 134
CH_R_ARROW	    = 135

.scope text
	buffer           = __text_buffer
	bufferon         = __text_bufferon
	bufferoff        = __text_bufferoff
	char_index	 = __text_char_index
	clrinfo          = __text_clrinfo
	clrline          = __text_clrline
	drawline         = __text_drawline
	info             = __text_info
	insertmode       = __text_insertmode
	index2cursor     = __text_index2cursor
	len              = __text_len
	linelen          = __text_linelen
	print            = __text_print
	putch            = __text_putch
	render           = __text_render
	render_ind       = __text_render_indirect
	rendered_line_len= __text_rendered_line_len
	savebuff         = __text_savebuff
	scrolldown       = __text_scrolldown
	scrolldownn      = __text_scrolldownn
	scrollup         = __text_scrollup
	scrollupn        = __text_scrollupn
	show_ws          = __text_show_ws
	statusfmt	 = __text_status_fmt
	statusmode       = __text_status_mode
	status           = __text_status
	restorebuff      = __text_restorebuff
	tabl_dist        = __text_tabl_dist
	tabr_dist        = __text_tabr_dist
	update           = __text_update
.endscope
