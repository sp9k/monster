;*******************************************************************************
; FASTSCROLL.ASM
;*******************************************************************************

.include "../ram.inc"
.include "banks.inc"

.define BITMAP_ADDR $1100
.define HEIGHT      192
.define SCROLL_ROWS 22*8

.CODE

;*******************************************************************************
; SCROLLUP
; Scrolls the whole screen up 1 character
.export __text_scrollup
.proc __text_scrollup
	JUMP FINAL_BANK_FASTSCROLL_UP, scrollup
.endproc

;*******************************************************************************
; SCROLLDOWN
; Scrolls the whole screen down 1 character
.export __text_scrolldown
.proc __text_scrolldown
	JUMP FINAL_BANK_FASTSCROLL_DOWN, scrolldown
.endproc

.segment "FASTSCROLL_UP"
;*******************************************************************************
; SCROLLUP
; Scrolls the whole screen up 1 character
.export scrollup
scrollup:
.repeat SCROLL_ROWS, i
.repeat 20, j
	lda BITMAP_ADDR + j*HEIGHT + i+8
	sta BITMAP_ADDR + j*HEIGHT + i
.endrep
.endrep
	rts

.segment "FASTSCROLL_DOWN"
;*******************************************************************************
; SCROLLDOWN
; Scrolls the whole screen down 1 character
scrolldown:
.repeat SCROLL_ROWS, i
.repeat 20, j
	lda BITMAP_ADDR + j*HEIGHT + SCROLL_ROWS-i
	sta BITMAP_ADDR + j*HEIGHT + SCROLL_ROWS-i+8
.endrep
.endrep
	rts
