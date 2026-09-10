.include "errlogconst.inc"

.import __errlog_activate
.import __errlog_check_line
.import __errlog_clear
.import __errlog_reset
.import __errlog_asmerrors
.import __errlog_close_buffer
.import __errlog_deleted
.import __errlog_dismiss
.import __errlog_undismiss
.import __errlog_getbyline
.import __errlog_get_curent
.import __errlog_inserted
.import __errlog_insertion_mode
.import __errlog_log
.import __errlog_next
.import __errlog_numerrs
.import __errlog_refresh
.import __errlog_set_live
.import __errlog_shift_errorsd
.import __errlog_shift_errorsu
.import __errlog_show

.import __errlog_before_key
.import __errlog_after_key
.import __errlog_navpending

.scope errlog
	SPLIT_LINE          = ERRLOG_SPLIT_LINE
	BLANK_ABOVE         = ERRLOG_BLANK_ABOVE
	BLANK_BELOW         = ERRLOG_BLANK_BELOW
	NAV_NONE            = ERRLOG_NAV_NONE
	NAV_CHECK_ON_LEAVE  = ERRLOG_NAV_CHECK_ON_LEAVE
	NAV_CHECK_ON_RETURN = ERRLOG_NAV_CHECK_ON_RETURN

	before_key     = __errlog_before_key
	after_key      = __errlog_after_key
	navpending     = __errlog_navpending
	activate       = __errlog_activate
	check_line     = __errlog_check_line
	clear          = __errlog_clear
	reset          = __errlog_reset
	asmerrors      = __errlog_asmerrors
	close_buffer   = __errlog_close_buffer
	deleted        = __errlog_deleted
	dismiss        = __errlog_dismiss
	undismiss      = __errlog_undismiss
	getbyline      = __errlog_getbyline
	get_curent     = __errlog_get_curent
	inserted       = __errlog_inserted
	insertion_mode = __errlog_insertion_mode
	log            = __errlog_log
	next           = __errlog_next
	numerrs        = __errlog_numerrs
	refresh        = __errlog_refresh
	set_live        = __errlog_set_live
	shift_errorsd  = __errlog_shift_errorsd
	shift_errorsu  = __errlog_shift_errorsu
	show           = __errlog_show
.endscope
