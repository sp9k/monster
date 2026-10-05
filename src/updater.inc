.ifndef UPDATER_INC
UPDATER_INC = 1

; Routines and state shared by the standalone and built-in flashers.
.import __updater_prepare
.import __updater_close
.import __updater_staged_byte
.import __updater_commit
.import __updater_confirm
.import __updater_name_length
.import __updater_saved_bank2

.scope updater
	prepare        = __updater_prepare
	close          = __updater_close
	staged_byte    = __updater_staged_byte
	commit         = __updater_commit
	confirm        = __updater_confirm
	namelen        = __updater_name_length
	saved_bank2    = __updater_saved_bank2
.endscope

.endif
