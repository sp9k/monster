.ifndef VIC20_FLASHCHECK_INC
VIC20_FLASHCHECK_INC = 1

.import __flashcheck_init
.import __flashcheck_printz
.import __flashcheck_prepare_update
.import __flashcheck_close_update_file
.import __flashcheck_filename
.import __flashcheck_device
.import __flashcheck_name_length

.scope flashcheck
	init     = __flashcheck_init
	message  = __flashcheck_printz
	check    = __flashcheck_prepare_update
	close    = __flashcheck_close_update_file
	filename = __flashcheck_filename
	device   = __flashcheck_device
	namelen  = __flashcheck_name_length
.endscope

.endif
