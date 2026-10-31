.import __jump_begin
.import __jump_commit
.import __jump_back
.import __jump_forward
.import __jump_reset
.import __jump_clear_log

.scope jumps
	begin     = __jump_begin
	commit    = __jump_commit
	back      = __jump_back
	forward   = __jump_forward
	reset     = __jump_reset
	clear_log = __jump_clear_log
.endscope
