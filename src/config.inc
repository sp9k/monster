.ifndef CONFIG_INC
.define CONFIG_INC

;*******************************************************************************
; PREFERENCES/COLORS

CUR_BLINK_SPEED = 250	; number of updates before blink

MAX_SEARCH_LEN = 16		; max size of search term in "find"
MAX_BUFFER_NAME_LEN = 16	; max length of names for each buffer


;*******************************************************************************
; SOURE config

.if .defined(vic20) .and .defined(hard8x8)
; Vic 20 (hard)
TAB_WIDTH = 4
LINESIZE  = 22

.elseif .defined(vic20) .and .defined(soft4x8)
; Vic 20 (soft40)
TAB_WIDTH = 10
LINESIZE  = 40

.elseif .defined(c64)
; C64
TAB_WIDTH = 10
LINESIZE  = 40
.endif

; Source capacity is independent of the visible editor row width.
MAX_LINE_LEN = 80

; line continuation character
.ifdef hard8x8
LINE_CONT = $5f
.else
LINE_CONT = $5c
.endif

; size of "spare" buffer
SPARESIZE = $400

.endif
