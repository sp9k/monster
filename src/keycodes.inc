; NAVIGATION KEYS
K_LEFT      = $9d	; cursor left
K_RIGHT     = $1d	; cursor right
K_UP        = $91       ; cursor up
K_DOWN      = $11       ; cursor down
K_HOME      = $13	; HOME

K_UP_ARROW  = $5e	; up arrow

K_PREV_BUFF  = $b4	; C= + h
K_NEXT_BUFF  = $b6	; C= + l
K_NEW_BUFF   = $aa	; C= + n
K_CLOSE_BUFF = $ab	; C= + q

K_UDG_EDIT         = $b8	; C= + u
K_UDG_TOGGLE_MODE  = $6d	; m
K_UDG_CLEAR        = $93	; SHIFT + CLR/HOME

; GLOBAL INTERFACE KEYS
K_RETURN    = $0d	; RETURN
K_QUIT      = $03	; STOP
K_DEL       = $14	; DELETE

; EDITOR KEYS
K_GO_BASIC       = $85	; F1
;                = $89	; F2
K_ASM_DEBUG      = $86	; F3
K_ASM            = $8a  ; F4 (shift + F3)
K_SHOW_BUFFERS   = $87	; F5
K_SHOW_PROJECT	 = $8b	; F6
K_LINK           = $88	; F7

K_NEXT_PAL       = $db	; SHIFT + +
K_PREV_PAL       = $dd  ; SHIFT + -
K_TOGGLE_FMT     = $bb

K_HELP           = $3f	; ?

K_SET_BREAKPOINT = $bf	; C= + B
K_ASM_FILE       = $b0	; C= + a
K_MONITOR        = $a7	; C= + m
K_REFRESH        = $bc  ; C= + c
K_RENAME         = $b2  ; C= + r
K_DIR            = $2d  ; -
K_LIST_SYMBOLS   = $b7  ; C= + y
K_FIND           = $2f	; /
K_FIND_PREV      = $2f	; /
K_GETCMD         = ':'  ; :
K_GOTO_BUFF1     = $81  ; C= + 1
K_GOTO_BUFF2     = $95  ; C= + 2
K_GOTO_BUFF3     = $96  ; C= + 3
K_GOTO_BUFF4     = $97  ; C= + 4
K_GOTO_BUFF5     = $98  ; C= + 5
K_GOTO_BUFF6     = $99  ; C= + 6
K_GOTO_BUFF7     = $9a  ; C= + 7
K_GOTO_BUFF8     = $9b  ; C= + 8
K_JUMPBACK       = $b9	; C= + o
K_JUMPFORWARD    = $a2	; C= + i
K_NEXT_DRIVE     = $a6  ; C= + +
K_PREV_DRIVE     = $dc  ; C= + -
K_EDIT_STATE     = $89	; F2
K_SWAP_WINS      = $b3	; C= + s
K_NEXT_ERR       = $b1  ; C= + e

;*******************************************************************************
; DEBUGGER KEYS
K_SRCVIEW		= $85	; F1
K_MEMVIEW		= $86	; F3
K_BRKVIEW		= $87	; F5
K_WATCHVIEW		= $88	; F7
K_SWAP_USERMEM		= $20	; SPACE
K_SWAP_USERMEM_TUI	= $85	; F1
K_RESET_STOPWATCH	= $b2	; C= + r
K_QUIT_DEBUGGER		= $bd	; C= + x
K_GOTO_BREAK		= $5e	; up-arrow
K_ENTER_DEBUG_CMD	= $21	; !

K_GO       = $a5	; C= + g
K_JUMP     = $b5	; C= + j
K_STEPOVER = $73	; s
K_TRACE    = $74	; t
K_STEP     = $7a	; z
; K_STEPOUT  = $5e	; up-arrow
K_STEPOUT  = $79	; y

K_SET_WATCH = $b3	; C= + w (for use in MEMORY editor)
K_ADD_WATCH = $b3	; C= + w (for use in WATCH editor)
K_DEL_WATCH = $ac	; C= + d

;*******************************************************************************
; MONITOR KEYS
K_GO_BASIC_TUI		= $89	; F2
K_MON_CLEAR		= $b6	; C= + l

;*******************************************************************************
; KEY REMAPPINGS
K_BACKSLASH   = $5c	; Euro symbol
K_CARET       = $5e	; up-arrow
K_UNDERSCORE  = $5f	; SHIFT + minus
K_GRAVE       = $60	; left-arrow
K_CURLY_OPEN  = $7b	; SHIFT + @
K_PIPE        = $7c	; SHIFT + #
K_CURLY_CLOSE = $7d	; SHIFT + *
K_TILDE       = $7e	; SHIFT + left-arrow
