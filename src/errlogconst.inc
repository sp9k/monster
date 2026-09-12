;*******************************************************************************
; ERRLOGCONST.INC
; Constants shared between the error log (errlog.asm) and its callers
; (errlog.inc).  They live here so that the two cannot drift apart: errlog.asm
; cannot include errlog.inc, which imports the symbols errlog.asm exports.
;*******************************************************************************

.ifndef ERRLOG_CONST_INC
ERRLOG_CONST_INC = 1

;*******************************************************************************
; INSERTION MODES
; How a newline affects mapped errors on the line it is inserted into
ERRLOG_SPLIT_LINE  = 0	; text is split: invalidate the original line
ERRLOG_BLANK_ABOVE = 1	; blank line opens above: preserve the original
ERRLOG_BLANK_BELOW = 2	; blank line opens below: preserve the original

; Whole-line deletion preserves the neighboring line at the join.
ERRLOG_JOIN_LINES   = 0	; ordinary join: invalidate both lines
ERRLOG_DELETE_ABOVE = 1	; delete the first line, preserving the next
ERRLOG_DELETE_BELOW = 2	; delete a line, preserving the preceding line

;*******************************************************************************
; PENDING VALIDATION STATES
; Whether the key being handled may check the line that it leaves
ERRLOG_NAV_NONE            = 0	; nothing pending (also cancels a snapshot)
ERRLOG_NAV_CHECK_ON_LEAVE  = 1	; check only if the key leaves line/buffer
ERRLOG_NAV_CHECK_ON_RETURN = 2	; RETURN checks without navigating

.endif
