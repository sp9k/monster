.import __label_add
.import __label_addanon
.import __label_addrmode
.import __label_address_by_id
.import __label_id_by_addr_index
.import __label_by_id
.import __label_by_addr
.import __label_clr
.import __label_count_floats
.import __label_dump
.import __label_find
.import __label_get_addr
.import __label_get_banon
.import __label_get_fanon
.import __label_get_line
.import __label_get_name
.import __label_get_segment
.import __label_id_by_alpha_index
.import __label_index
.import __label_is_local
.import __label_isvalid
.import __label_load
.import __label_remap_files
.import __label_address
.import __label_num
.import __label_numanon
.import __label_popscope
.import __label_set
.import __label_set_addr
.import __label_setscope

; SEGMENT ID FLAGS
SEG_UNDEF = $00	; segment is undefined (external)
SEG_ABS   = $ff	; symbol represents a constant (not tied to a segment)

; A named float constant (.EQ with a float value).  The symbol's ADDR field is
; an index into the evaluator's float pool rather than a value.  The FLAGS byte
; stores the segment in 7 bits and pads $7f back to SEG_ABS, so $7e is the
; highest id available; MAX_SEGMENTS is 64, so it cannot collide with a real one.
SEG_FLOAT = $7e
; On disk this distinct tag is followed by a packed five-byte payload.
; It must never be stored in an in-memory symbol or used as a segment index.
SEG_FLOAT_PACKED = $7d

SYM_UNRESOLVED = $ffff	; magic value for unresolved symbol ID

.define MAX_LABEL_NAME_LEN 32

.scope lbl
	add               = __label_add
	addanon           = __label_addanon
	addrmode          = __label_addrmode
	idbyaddrindex     = __label_id_by_addr_index
	addr              = __label_address
	addr_and_mode     = __label_address_by_id
	by_id             = __label_by_id
	by_addr           = __label_by_addr
	clr               = __label_clr
	; Direct entry in FINAL_BANK_SYMBOLS (no MAIN trampoline).
	count_floats      = __label_count_floats
	dump              = __label_dump
	load              = __label_load
	remapfiles        = __label_remap_files
	find              = __label_find
	get_banon         = __label_get_banon
	get_fanon         = __label_get_fanon
	get_line          = __label_get_line
	getname           = __label_get_name
	getsegment        = __label_get_segment
	getaddr           = __label_get_addr
	id_by_alpha_index = __label_id_by_alpha_index
	index             = __label_index
	islocal           = __label_is_local
	isvalid           = __label_isvalid
	num               = __label_num
	numanon           = __label_numanon
	popscope          = __label_popscope
	set               = __label_set
	setaddr           = __label_set_addr
	setscope          = __label_setscope
.endscope
