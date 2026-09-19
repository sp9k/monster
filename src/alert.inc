.ifndef ALERT_INC
ALERT_INC = 1

.import __alert_show
.import __alert_open
.import __alert_close
.import __alert_prompt

.import __alert_border, __alert_textrow, __alert_textcol

.scope alert
	show   = __alert_show
	open   = __alert_open
	close  = __alert_close
	prompt = __alert_prompt
	; Drawing only: custom modals must restore their own background.
	border = __alert_border       ; A=row, X/Y=left/right corner glyphs
	textrow = __alert_textrow     ; A=row, XY=zero-terminated string
	textcol = __alert_textcol     ; first text column for textrow
.endscope

.endif
