.import __mem_asmbuffer
.import __mem_asmctx
.import __mem_ctxbuffer
.import __mem_debugsave
.import __mem_drive_err
.import __mem_findbuff
.import __mem_spare
.import __mem_spareend
.import __mem_filename
.import __mem_sparevec

.import __statusinfo
.import __statusline

.import __linebuffer
.import __linebuffer2
.import __linesave

.import __mem_coloron
.import __mem_rowcolors
.import __mem_rowcolors_idx
.import __mem_rowcolors_save
.import __mem_init_sig

.scope mem
	asmbuffer       = __mem_asmbuffer
	ctxbuffer       = __mem_ctxbuffer
        spare           = __mem_spare
	spareend	= __mem_spareend
	filename        = __mem_filename
	findbuff        = __mem_findbuff
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	linesave        = __linesave
	init_sig        = __mem_init_sig
	statusinfo      = __statusinfo
	statusline	= __statusline
	drive_err	= __mem_drive_err
	coloron         = __mem_coloron
	rowcolors       = __mem_rowcolors
	rowcolors_idx   = __mem_rowcolors_idx
	rowcolors_save  = __mem_rowcolors_save
	sparevec        = __mem_sparevec
.endscope
