.import __breakpoint_edit
.import __brkpt_getbyline
.import __brkpt_anyinbuff

; breakpoint flags (metadata stored for each breakpoint)
BREAKPOINT_ENABLED = 1

; flags for RENDERING breakpoints
; This is a tristate flag: 0: off, 1: on (inactive), 2: on (active)
BREAKPOINT_UNSET    = 0
BREAKPOINT_INACTIVE = 1
BREAKPOINT_ACTIVE   = 2

.scope brkpt
	anyinbuff = __brkpt_anyinbuff
	edit      = __breakpoint_edit
	getbyline = __brkpt_getbyline
.endscope
