.import __watches_add
.import __watches_init
.import __watches_edit
.import __watches_getdata
.import __watches_flags
.import __watches_mark
.import __watches_num
.import __watches_remove
.import __watches_update
.import __watches_watch_flags
.import __watches_watch_prevs
.import __watches_watcheslo
.import __watches_watcheshi

.scope watch
	add      = __watches_add
	edit     = __watches_edit
	flags    = __watches_watch_flags
	getdata  = __watches_getdata
	his      = __watches_watcheshi
	init     = __watches_init
	los      = __watches_watcheslo
	mark     = __watches_mark
	num	 = __watches_num
	remove   = __watches_remove
	update   = __watches_update
	prevs    = __watches_watch_prevs
.endscope
