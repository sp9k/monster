.ifndef CONFIG_INC
.define CONFIG_INC

;*******************************************************************************
; PREFERENCES/COLORS

CUR_BLINK_SPEED = 250	; number of updates before blink

MAX_SEARCH_LEN = 16		; max size of search term in "find"
MAX_BUFFER_NAME_LEN = 16	; max length of names for each buffer

MAX_LABEL_LEN = 32

;*******************************************************************************
; SOURE config

.if .defined(vic20) .and .defined(hard8x8)
; Vic 20 (hard)
TAB_WIDTH = 2
LINESIZE  = 22
MAX_LINE_LEN = LINESIZE*4   ; max logical line length

.elseif .defined(vic20) .and .defined(soft4x8)
; Vic 20 (soft40)
TAB_WIDTH = 10
LINESIZE  = 40
MAX_LINE_LEN = LINESIZE     ; max logical line length

.elseif .defined(c64)
; C64
TAB_WIDTH = 10
LINESIZE  = 40
MAX_LINE_LEN = LINESIZE     ; max logical line length
.endif

; line continuation character
LINE_CONT = $5c

; size of "spare" buffer
SPARESIZE = $400

.endif
