.import __asm_disassemble
.import __asm_endpass
.import __asm_include
.import __asm_mode
.import __asm_segtype
.import __asm_origin
.import __asm_pcset
.import __asm_reset
.import __asm_segmentid
.import __asm_set_pc
.import __asm_startpass
.import __asm_tokenize
.import __asm_top
.import __asm_linenum
.import __asm_is_opcode
.import __asm_type_to_mode
.import __asm_raw_opcode
.import __asm_raw_operand

.scope asm
	disassemble    = __asm_disassemble
	endpass        = __asm_endpass
	include        = __asm_include
	isopcode       = __asm_is_opcode
	linenum        = __asm_linenum
	mode           = __asm_mode
	origin         = __asm_origin
	opcode         = __asm_raw_opcode
	operand        = __asm_raw_operand
	pcset          = __asm_pcset
	reset          = __asm_reset
	setpc          = __asm_set_pc
	startpass      = __asm_startpass
	segment        = __asm_segmentid
	segtype        = __asm_segtype
	tokenize       = __asm_tokenize
	top            = __asm_top
	type_to_mode   = __asm_type_to_mode
.endscope

