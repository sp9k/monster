.ifndef CONFIG_INC
.define CONFIG_INC

;*******************************************************************************
; PREFERENCES/COLORS

CUR_BLINK_SPEED = 250	; number of updates before blink

MAX_SEARCH_LEN = 16		; max size of search term in "find"
MAX_BUFFER_NAME_LEN = 16	; max length of names for each buffer

MAX_LABEL_LEN = 32

;*******************************************************************************
; SOURCE FORMATTING
.ifdef vic20
.ifdef hard8x8
TAB_WIDTH = 2
LINESIZE  = 22
.else
TAB_WIDTH = 10
LINESIZE  = 40
.endif
.else
TAB_WIDTH = 10
LINESIZE  = 40
.endif
.endif

SPARESIZE = $400
