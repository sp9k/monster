.import __dir_get_by_type
.import __dir_view

.scope dir
	view        = __dir_view
	get_by_type = __dir_get_by_type
.endscope

.if .defined(c64) .and .defined(CART)
.import __dir_sid
.endif
