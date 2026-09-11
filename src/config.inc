.ifndef CONFIG_INC
CONFIG_INC = 1

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
TAB_WIDTH = 4
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
.ifdef hard8x8
LINE_CONT = $5f
.else
LINE_CONT = $5c
.endif

; size of "spare" buffer
.ifdef fe3
; Shared scratch must fit the complete 16-entry, 16-character object list.
SPARESIZE = $120
FE3_MAX_SOURCES = 4
.else
SPARESIZE = $400
.endif

.endif
