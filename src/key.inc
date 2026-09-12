.import __key_flush
.import __key_getch
.import __key_getui
.import __key_waitui
.import __key_restore_pending
.import __key_restore_nmi
.import __key_raw
.import __key_gethex
.import __key_isdown
.import __key_ishex
.import __key_isdec
.import __key_isleft
.import __key_isright
.import __key_isup
.import __key_is_printing
.import __key_waitch

.scope key
	flush           = __key_flush
	getch           = __key_getch
	getui           = __key_getui
	waitui          = __key_waitui
	restore_pending = __key_restore_pending
	restore_nmi     = __key_restore_nmi
	raw             = __key_raw
	gethex          = __key_gethex
	isdown          = __key_isdown
	isdec           = __key_isdec
	isleft          = __key_isleft
	ishex           = __key_ishex
	isright         = __key_isright
	isup            = __key_isup
	isprinting      = __key_is_printing
	waitch          = __key_waitch
.endscope
