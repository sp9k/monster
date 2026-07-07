.importzp __ctx_numlines

.import __ctx_active
.import __ctx_addparam
.import __ctx_end
.import __ctx_getparams
.import __ctx_open
.import __ctx_push
.import __ctx_pop
.import __ctx_getdata
.import __ctx_getline
.import __ctx_rewind
.import __ctx_write
.import __ctx_write_parent
.import __ctx_init

; the max number of lines stored in contexts for macros
MAX_CTX_LINES = 16

; the max number of parameters for a context (must match ctx.asm)
MAX_PARAMS = 4

CTX_REPEAT = 1
CTX_MACRO  = 2

.scope repctx
	base      = 0
	iter      = 2
	iter_end  = 4
	cur       = 6 	; ptr to current location being read in (lines)
	params    = 8	; 16 bytes
	numparams = 10  ; 1 byte
	lines     = 11	; rest of buffer ($200-10)
.endscope

.scope ctx
	active       = __ctx_active
	addparam     = __ctx_addparam
	end          = __ctx_end
	getparams    = __ctx_getparams
	init         = __ctx_init
	numlines     = __ctx_numlines
	push         = __ctx_push
	open         = __ctx_open
	pop          = __ctx_pop
	getline      = __ctx_getline
	getdata      = __ctx_getdata
	write        = __ctx_write
	write_parent = __ctx_write_parent
	rewind       = __ctx_rewind
.endscope
