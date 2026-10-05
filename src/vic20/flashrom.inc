.ifndef VIC20_FLASHROM_INC
VIC20_FLASHROM_INC = 1

.import __flash_ram_start
.import __flash_device
.import __flasher_filename
.import __updater_name_length

.scope flashrom
	start    = __flash_ram_start
	device   = __flash_device
	filename = __flasher_filename
	namelen  = __updater_name_length
.endscope

.endif
