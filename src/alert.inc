.ifndef ALERT_INC
ALERT_INC = 1

.import __alert_show
.import __alert_open
.import __alert_close
.import __alert_prompt

.scope alert
	show   = __alert_show
	open   = __alert_open
	close  = __alert_close
	prompt = __alert_prompt
.endscope

.endif
