; undo is currently backed by the UltiMem image pool
.ifndef UNDO_INC
UNDO_INC = 1
.ifdef ultimem
.import __undo_init, __undo_commit
.import __undo_insert, __undo_delete, __undo_backspace
.import __undo_undo, __undo_redo, __undo_close, __undo_saved
.import __undo_claim, __undo_release
.scope undo
	init      = __undo_init
	commit    = __undo_commit
	insert    = __undo_insert
	delete    = __undo_delete
	backspace = __undo_backspace
	undo      = __undo_undo
	redo      = __undo_redo
	close     = __undo_close
	saved     = __undo_saved
	claim     = __undo_claim
	release   = __undo_release
.endscope
.endif
.endif
