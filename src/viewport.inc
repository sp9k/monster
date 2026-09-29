;*******************************************************************************
; VIEWPORT.INC
; Entry points for the 80-column source viewport in the editor.
;*******************************************************************************

.ifndef VIEWPORT_INC
VIEWPORT_INC = 1
.import __view_chars
.import __view_clear
.import __view_command
.import __view_draw
.import __view_follow
.import __view_init
.import __view_invalidate
.import __view_join
.import __view_pan
.import __view_physical_x
.import __view_reverse
.import __view_scroll_down
.import __view_scroll_up
.import __view_slots
.import __view_valid
.import __view_x

.scope viewport
	chars       = __view_chars
	clear       = __view_clear
	command     = __view_command
	draw        = __view_draw
	follow      = __view_follow
	init        = __view_init
	invalidate  = __view_invalidate
	join        = __view_join
	pan         = __view_pan
	physical_x  = __view_physical_x
	reverse     = __view_reverse
	scroll_down = __view_scroll_down
	scroll_up   = __view_scroll_up
	slots       = __view_slots
	valid       = __view_valid
	offset      = __view_x
.endscope
.endif
