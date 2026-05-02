;******************************************************************************
; VIDEO SETTINGS

;******************************************************************************
LOGO_HEIGHT=90
LOGO_WIDTH=90/8

;******************************************************************************
; available color schemes
COLORSCHEME_DEFAULT = 1
COLORSCHEME_BW      = 2

;******************************************************************************
COLORSCHEME = COLORSCHEME_DEFAULT	; colorscheme
;COLORSCHEME = COLORSCHEME_BW

;******************************************************************************
; DEFAULT COLORSCHEME
.if COLORSCHEME=COLORSCHEME_DEFAULT
TEXT_COLOR	        = $06
BORDER_COLOR	        = $03|$08
BG_COLOR	        = $01
DEFAULT_900F            = (BG_COLOR<<4 | BORDER_COLOR)
DEFAULT_RVS             = DEFAULT_900F^$08
GUI_SELECT_COLOR        = ($0e << 4) | BORDER_COLOR
BREAKPOINT_ON_COLOR     = ($09<<4 | BORDER_COLOR)
BREAKPOINT_OFF_COLOR    = ($0e<<4 | BORDER_COLOR)
ERROR_COLOR	        = ($00|$08)			; $900f color of error highlight
ASM_SUCCESS_COLOR       = ($0d<<4 | BORDER_COLOR)	; color for asm done message
DEBUG_REG_CHANGED_COLOR = 2				; color for register that was changed during STEP

;******************************************************************************
; B/W COLORSCHEME
.elseif COLORSCHEME = COLORSCHEME_BW
TEXT_COLOR	 = $01	      		; text color for the editor
BORDER_COLOR	 = $00|$08     		; border color for the editor
BG_COLOR	 = $00	      		; bg color for the editor
DEFAULT_900F            = (BG_COLOR<<4 | BORDER_COLOR)
DEFAULT_RVS             = DEFAULT_900F^$08
GUI_SELECT_COLOR        = ($07 << 4) | BORDER_COLOR
BREAKPOINT_ON_COLOR     = ($09<<4 | BORDER_COLOR)
BREAKPOINT_OFF_COLOR    = ($0e<<4 | BORDER_COLOR)
ERROR_COLOR	        = ($00|$08)			; $900f color of error highlight
DEBUG_LINE_COLOR        = ($70|BORDER_COLOR)		; row cursor color during debug
ASM_SUCCESS_COLOR       = ($0d<<4 | BORDER_COLOR)	; color for asm done message
DEBUG_REG_CHANGED_COLOR = 2				; color for register that was changed during STEP
.endif

;******************************************************************************
; BEEP SETTINGS
BEEP_TONE           = $dd
BEEP_ERR_TONE       = $a4
BEEP_VOL            = $04
SHORT_BEEP_DURATION = 10
