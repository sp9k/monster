.import label_addresses

.import __label_add
.import __label_addanon
.import __label_addrmode
.import __label_address_by_id
.import __label_id_by_addr_index
.import __label_by_id
.import __label_by_addr
.import __label_clr
.import __label_dump
.import __label_find
.import __label_get_addr
.import __label_get_banon
.import __label_get_fanon
.import __label_get_name
.import __label_get_segment
.import __label_index
.import __label_is_local
.import __label_isvalid
.import __label_load
.import __label_address
.import __label_num
.import __label_numanon
.import __label_name_by_id
.import __label_popscope
.import __label_set
.import __label_set_addr
.import __label_setscope

; SEGMENT ID FLAGS
SEG_UNDEF = $00	; segment is undefined (external)
SEG_ABS   = $ff	; symbol represents a constant (not tied to a segment)

SYM_UNRESOLVED = $ffff	; magic value for unresolved symbol ID

.define MAX_LABEL_NAME_LEN 32
.define MAX_LABELS         736
.define MAX_ANON           650

.scope lbl
	add           = __label_add
	addanon       = __label_addanon
	addresses     = label_addresses
	addrmode      = __label_addrmode
	idbyaddrindex = __label_id_by_addr_index
	addr          = __label_address
	addr_and_mode = __label_address_by_id
	by_id         = __label_by_id
	by_addr       = __label_by_addr
	clr           = __label_clr
	dump          = __label_dump
	load          = __label_load
	find          = __label_find
	get_banon     = __label_get_banon
	get_fanon     = __label_get_fanon
	getname       = __label_get_name
	getsegment    = __label_get_segment
	getaddr       = __label_get_addr
	index         = __label_index
	islocal       = __label_is_local
	isvalid       = __label_isvalid
	name_by_id    = __label_name_by_id
	num           = __label_num
	numanon       = __label_numanon
	popscope      = __label_popscope
	set           = __label_set
	setaddr       = __label_set_addr
	setscope      = __label_setscope
.endscope
