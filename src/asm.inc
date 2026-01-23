.import __asm_disassemble
.import __asm_include
.import __asm_mode
.import __asm_segmode
.import __asm_origin
.import __asm_pcset
.import __asm_reset
.import __asm_segmentid
.import __asm_set_pc
.import __asm_startpass
.import __asm_tokenize
.import __asm_tokenize_pass
.import __asm_tokenize_pass1
.import __asm_tokenize_pass2
.import __asm_top
.import __asm_linenum

.scope asm
	disassemble    = __asm_disassemble
	include        = __asm_include
	linenum        = __asm_linenum
	mode           = __asm_mode
	origin         = __asm_origin
	pcset          = __asm_pcset
	reset          = __asm_reset
	setpc          = __asm_set_pc
	startpass      = __asm_startpass
	segment        = __asm_segmentid
	segmode        = __asm_segmode
	tokenize       = __asm_tokenize
	tokenize_pass  = __asm_tokenize_pass
	tokenize_pass1 = __asm_tokenize_pass1
	tokenize_pass2 = __asm_tokenize_pass2
	top            = __asm_top
.endscope

