.import __mac_add
.import __mac_asm
.import __mac_init
.import __mac_isvalid
.import __mac_get
.import __mac_num
.import __mac_top
.import __mac_view
.import __mac_reset_expansion, __mac_source, __mac_depth
.import __mac_local, __mac_filename
.import __mac_verify_property, __mac_verify_name
.import __mac_namespace_floor
.import __mac_set, __mac_reset_mutables

.scope mac
	add     = __mac_add
	get     = __mac_get
	asm     = __mac_asm
	init    = __mac_init
	isvalid = __mac_isvalid
	num     = __mac_num
	top     = __mac_top
	view    = __mac_view
	reset_expansion = __mac_reset_expansion
	source  = __mac_source
	depth   = __mac_depth
	local   = __mac_local
	filename = __mac_filename
	verify_property = __mac_verify_property
	verify_name = __mac_verify_name
	namespace_floor = __mac_namespace_floor
	set = __mac_set
	reset_mutables = __mac_reset_mutables
.endscope
