;*******************************************************************************
; window type IDs
GUI_ERRLOG      = 1
GUI_WATCHES     = 2
GUI_BREAKPOINTS = 3
GUI_BUFFERS     = 4
GUI_MEMVIEW     = 5
GUI_MONITOR     = 6

;*******************************************************************************
; window classes
GUI_CLASS_LIST   = 0	; rows of selectable text (gui.asm handles keys/drawing)
GUI_CLASS_CUSTOM = 1	; window provides its own draw and enter handlers

;*******************************************************************************
; codes returned to the window manager by "enter" handlers
GUI_RET_QUIT   = 0	; return focus to the editor
GUI_RET_CYCLE  = 1	; activate the next window in the stack
GUI_RET_SWITCH = 2	; the active window changed; enter the new active window
GUI_RET_CLOSE  = 3	; close the active window (focus moves to the window
			; below it, or to the editor if it was the last one)
