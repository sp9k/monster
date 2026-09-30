.ifndef LINKFRAGMENTS_INC
LINKFRAGMENTS_INC = 1
; Global fragment references point to these records.
; Object files identify ; fragments by their one-byte index within the object
FR_SEGMENT  = $00
FR_SIZELO   = $01
FR_SIZEHI   = $02
FR_SIZEBANK = $03
FR_ALIGNLO  = $04
FR_ALIGNHI  = $05
FR_FILL     = $06
FR_PADLO    = $07
FR_PADHI    = $08
FR_LOADLO   = $09
FR_LOADHI   = $0a
FR_RUNLO    = $0b
FR_RUNHI    = $0c
FR_BYTES    = $10

;*******************************************************************************
; NEXT FRAGMENT
; Advances a record pointer by one fragment.
; ARGS:
;   - ptr: address of the two-byte record pointer
; CLOBBERS:
;   - .A, .N, .Z, .C
.macro NEXT_FRAGMENT ptr
	clc
	lda ptr
	adc #FR_BYTES
	sta ptr
	bcc :+
	inc ptr+1
:
.endmacro
.endif
