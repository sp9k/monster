.import __errlog_next
.import __errlog_numerrs
.import __errlog_activate
.import __errlog_clear
.import __errlog_log
.import __errlog_shift_errorsd
.import __errlog_shift_errorsu
.import __errlog_getbyline

.scope errlog
	activate      = __errlog_activate
	clear         = __errlog_clear
	log           = __errlog_log
	numerrs       = __errlog_numerrs
	next          = __errlog_next
	shift_errorsd = __errlog_shift_errorsd
	shift_errorsu = __errlog_shift_errorsu
	getbyline     = __errlog_getbyline
.endscope
