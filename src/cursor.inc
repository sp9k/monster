.import __cur_mode
.import __cur_move
.import __cur_on
.import __cur_off
.import __cur_maxx
.import __cur_maxy
.import __cur_minx
.import __cur_miny
.import __cur_pop
.import __cur_push
.import __cur_right
.import __cur_set
.import __cur_setpos
.import __cur_setmin
.import __cur_setmax
.import __cur_status
.import __cur_toggle
.import __cur_unlimit

CUR_NORMAL = 0
CUR_SELECT = 1

CUR_OFF = 0
CUR_ON  = 1

.scope cur
	on      = __cur_on
	off     = __cur_off
	maxx    = __cur_maxx
	minx    = __cur_minx
	maxy    = __cur_maxy
	miny    = __cur_miny
	mode    = __cur_mode
	move    = __cur_move
	set     = __cur_set
	setpos  = __cur_setpos
	pop     = __cur_pop
	push    = __cur_push
	right   = __cur_right
	toggle  = __cur_toggle
	setmin  = __cur_setmin
	setmax  = __cur_setmax
	status  = __cur_status
	unlimit = __cur_unlimit
.endscope

; NOTE: must be used in same bank as the cur::push routine
.macro pushcur
	jsr __cur_push
.endmacro

; NOTE: must be used in same bank as the cur::pop routine
.macro popcur
	jsr __cur_pop
.endmacro
