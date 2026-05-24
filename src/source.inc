.importzp __src_line
.importzp __src_lines

.import __src_activebuff
.import __src_after_cursor
.import __src_anydirty
.import __src_atcursor
.import __src_backspace
.import __src_before_end
.import __src_before_newl
.import __src_buffer_by_name
.import __src_close
.import __src_current_filename
.import __src_currline
.import __src_delete
.import __src_down
.import __src_downn
.import __src_end
.import __src_end_rep
.import __src_force_set
.import __src_get
.import __src_getin
.import __src_getflags
.import __src_goto
.import __src_get_filename
.import __src_home
.import __src_insert_on_load
.import __src_insert
.import __src_insertline
.import __src_init
.import __src_isdirty
.import __src_left
.import __src_line_end
.import __src_next
.import __src_name
.import __src_names
.import __src_new
.import __src_new_log
.import __src_numbuffers
.import __src_on_last_line
.import __src_up
.import __src_upn
.import __src_popp
.import __src_popgoto
.import __src_pos
.import __src_prev
.import __src_pushp
.import __src_replace
.import __src_right
.import __src_right_rep
.import __src_rewind
.import __src_readline
.import __src_save
.import __src_set
.import __src_setflags
.import __src_start

;******************************************************************************
; CONSTANTS
MAX_SOURCES = 8			; maximum number of source banks/buffers
LOG_BUFFER  = MAX_SOURCES 	; special buffer for logging

FLAG_DIRTY = 1	; flag that source buffer has been changed since last save

GAPSIZE = 20	; size of gap in gap buffer

.scope src
	activebuff     = __src_activebuff
	after_cursor   = __src_after_cursor
	anydirty       = __src_anydirty
	atcursor       = __src_atcursor
	backspace      = __src_backspace
	before_end     = __src_before_end
	before_newl    = __src_before_newl
	buffer_by_name = __src_buffer_by_name
	close          = __src_close
	current_filename = __src_current_filename
	currline         = __src_currline
	delete         = __src_delete
	down           = __src_down
	downn          = __src_downn
	names          = __src_names
	new_log        = __src_new_log
	numbuffers     = __src_numbuffers
	end            = __src_end
	end_rep        = __src_end_rep
	filename       = __src_get_filename
	forceset       = __src_force_set
	get            = __src_get
	getin          = __src_getin
	getflags       = __src_getflags
	goto           = __src_goto
	home           = __src_home
	insert         = __src_insert
	insertline     = __src_insertline
	insert_on_load = __src_insert_on_load
	init           = __src_init
	isdirty        = __src_isdirty
	left           = __src_left
	line           = __src_line
	lineend        = __src_line_end
	lines          = __src_lines
	name           = __src_name
	new            = __src_new
	next           = __src_next
	on_last_line   = __src_on_last_line
	replace        = __src_replace
	popp           = __src_popp
	popgoto        = __src_popgoto
	pos            = __src_pos
	pushp          = __src_pushp
	prev           = __src_prev
	rewind         = __src_rewind
	readline       = __src_readline
	right          = __src_right
	right_rep      = __src_right_rep
	save           = __src_save
	setbuff        = __src_set
	setflags       = __src_setflags
	start          = __src_start
	up             = __src_up
	upn            = __src_upn
.endscope
