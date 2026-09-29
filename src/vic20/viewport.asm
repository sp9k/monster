;*******************************************************************************
; VIEWPORT CACHE
; Stores expanded characters and selection masks in the viewport RAM bank.
;*******************************************************************************

.include "../config.inc"
.include "../layout.inc"
.include "../macros.inc"
.include "../zeropage.inc"

.segment "VSCREEN_BSS"
.export __view_chars
__view_chars:
chars:   .res MAX_LINE_LEN*SCREEN_HEIGHT
inverse: .res MAX_LINE_LEN*SCREEN_HEIGHT

.segment "VSCREEN"

;*******************************************************************************
; INIT
; Clears every row's selection mask.
.export __viewcache_init
.proc __viewcache_init
	lda #0
	ldx #0
:	.repeat (::MAX_LINE_LEN*::SCREEN_HEIGHT)/256, page
	sta inverse+page*256,x
	.endrepeat
	inx
	bne :-
	ldx #(MAX_LINE_LEN*SCREEN_HEIGHT) .mod 256
:	dex
	sta inverse+((MAX_LINE_LEN*SCREEN_HEIGHT)/256)*256,x
	bne :-
	rts
.endproc

;*******************************************************************************
; ROW
; Locates one slot's character and selection arrays.
; IN:
;   - .X: cache slot
; OUT:
;   - r0: character pointer
;   - r2: selection pointer
.export __viewcache_row
.proc __viewcache_row
@chars=r0
@mask=r2
	lda rowlo,x
	sta @chars
	lda rowhi,x
	sta @chars+1
	lda masklo,x
	sta @mask
	lda maskhi,x
	sta @mask+1
	rts
.endproc

;*******************************************************************************
; ROW ADDRESSES
rowlo: .repeat SCREEN_HEIGHT, i
.byte <(chars+i*MAX_LINE_LEN)
.endrepeat
rowhi: .repeat SCREEN_HEIGHT, i
.byte >(chars+i*MAX_LINE_LEN)
.endrepeat
masklo: .repeat SCREEN_HEIGHT, i
.byte <(inverse+i*MAX_LINE_LEN)
.endrepeat
maskhi: .repeat SCREEN_HEIGHT, i
.byte >(inverse+i*MAX_LINE_LEN)
.endrepeat
