.ifndef DRIVECFG_INC
DRIVECFG_INC = 1

.import __drivecfg_edit
.import __drivecfg_output_device

.scope drivecfg
	edit = __drivecfg_edit
	; zero follows zp::device; nonzero selects a separate object drive
	output_device = __drivecfg_output_device
.endscope

.endif
