.ifndef SIDPLAY_INC
SIDPLAY_INC = 1
SID_START = $1000
SID_LIMIT = $2000

.ifndef SIDPLAY_IMPL
.import __sid_load
.import __sid_stop
.import __sid_toggle
.import __sid_restart
.import __sid_tick
.import __sid_config_irq
.import __sid_cia_tick

.endif
.endif
