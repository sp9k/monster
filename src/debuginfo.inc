.importzp __debug_src_line
.importzp __debug_file

.import __debug_addr2line
.import __debug_end_block
.import __debug_get_filename
.import __debug_line2addr
.import __debuginfo_init
.import __debuginfo_initonce
.import __debug_new_block
.import __debug_numfiles
.import __debug_set_file
.import __debug_store_line
.import __debug_push_block
.import __debug_pop_block

.import __debuginfo_dump
.import __debuginfo_load
.import __debuginfo_get_fileid
.import __debuginfo_set_seg_id
.import __debuginfo_top

BLOCK_START_ADDR     = 0
BLOCK_STOP_ADDR      = 2
BLOCK_LINE_BASE      = 4
BLOCK_LINE_COUNT     = 6
BLOCK_FILE_ID        = 8
BLOCK_SEGMENT_ID     = 9
BLOCK_LINE_PROG      = 10
BLOCK_PROG_STOP_ADDR = 12
SIZEOF_BLOCK_HEADER  = 14

.scope dbgi
	addr2line    = __debug_addr2line
	dump         = __debuginfo_dump
	endblock     = __debug_end_block
	file         = __debug_file
	get_filename = __debug_get_filename
	getfileid    = __debuginfo_get_fileid
	line2addr    = __debug_line2addr
	load         = __debuginfo_load
	init         = __debuginfo_init
	initonce     = __debuginfo_initonce
	newblock     = __debug_new_block
	numfiles     = __debug_numfiles
	setfile      = __debug_set_file
	set_seg_id   = __debuginfo_set_seg_id
	srcline      = __debug_src_line
	storeline    = __debug_store_line
	pop_block    = __debug_pop_block
	push_block   = __debug_push_block
	top          = __debuginfo_top
.endscope
