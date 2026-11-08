;*******************************************************************************
; ERRLOGCONST.INC
; Constants shared between the error log (errlog.asm) and its callers
; (errlog.inc).
;*******************************************************************************

.ifndef ERRLOG_CONST_INC
ERRLOG_CONST_INC = 1

ERRLOG_MAX_ERRORS = $10
ERRLOG_MAX_DISMISSED = $40

; packed undo snapshots contain line, error code, and live/assembly owner kind
ERRLOG_SNAPSHOT_ENTRY = $04
ERRLOG_SNAPSHOT_SIZE = ERRLOG_MAX_ERRORS*ERRLOG_SNAPSHOT_ENTRY

; annotation snapshot: error/dismissal generations, three counts, entries
UNDO_META_HEADER = 7
UNDO_MAX_BREAKPOINTS = $10
UNDO_BREAKPOINT_TABLES = 7
UNDO_BREAKPOINT_BYTES = UNDO_MAX_BREAKPOINTS*UNDO_BREAKPOINT_TABLES
UNDO_META_ENTRIES = ERRLOG_SNAPSHOT_SIZE+UNDO_MAX_BREAKPOINTS*5
UNDO_META_MAX = UNDO_META_HEADER+UNDO_META_ENTRIES+ERRLOG_MAX_DISMISSED*3

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
ERRLOG_NAV_NONE            = 0	; nothing pending
ERRLOG_NAV_CHECK_ON_LEAVE  = 1	; check only if the key leaves line/buffer
ERRLOG_NAV_CHECK_ON_RETURN = 2	; RETURN checks without navigating

.endif
