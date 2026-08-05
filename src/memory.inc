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
.import __mem_breakpoint_rows
.ifdef hard8x8			; reserved in vic20/hard8x8/mem.asm
.import __mem_highlight_rows
.import __mem_blink_cell
.import __mem_blink_row
.import __mem_blink_dchar
.import __mem_blink_dclr
.import __mem_blink_active
.import __mem_blink_phase
.import __mem_blink_cnt
.endif
.import __mem_rowcolors
.import __mem_rowcolors_idx
.import __mem_rowcolors_save
.import __mem_init_sig

.scope mem
	asmbuffer       = __mem_asmbuffer
	breakpoint_rows = __mem_breakpoint_rows
.ifdef hard8x8			; reserved in vic20/hard8x8/mem.asm
	highlight_rows  = __mem_highlight_rows
	blink_cell      = __mem_blink_cell
	blink_row       = __mem_blink_row
	blink_dchar     = __mem_blink_dchar
	blink_dclr      = __mem_blink_dclr
	blink_active    = __mem_blink_active
	blink_phase     = __mem_blink_phase
	blink_cnt       = __mem_blink_cnt
.endif
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
