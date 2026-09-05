.ifndef BORDER_INC
BORDER_INC = 1

;*******************************************************************************
; BORDER CHARACTERS
; The box-drawing glyphs that modal windows (alerts, the help screen, etc.)
; frame themselves with.
.ifdef soft4x8
; 4x8 font box-drawing glyphs
BORDER_VBAR = 128
BORDER_HBAR = 140
BORDER_TL   = 136
BORDER_TR   = 137
BORDER_BL   = 138
BORDER_BR   = 139
.else

; PETSCII box-drawing glyphs
BORDER_VBAR = $dd	; screen code $5d
BORDER_HBAR = $c0	; screen code $40
BORDER_TL   = $b0	; screen code $70
BORDER_TR   = $ae	; screen code $6e
BORDER_BL   = $ad	; screen code $6d
BORDER_BR   = $bd	; screen code $7d
.endif

.endif
