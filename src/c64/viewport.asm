;*******************************************************************************
; VIEWPORT CACHE
; Stores rows in the REU and stages the active slot in resident RAM.
;*******************************************************************************

.include "../config.inc"
.include "layout.inc"
.include "../macros.inc"
.include "ram.inc"

CACHE_ROW_SIZE = MAX_LINE_LEN*2

.segment "VSCREEN_BSS"
.export __view_chars
__view_chars:
chars:   .res MAX_LINE_LEN
inverse: .res MAX_LINE_LEN
loaded:  .byte 0		; slot held in RAM, or $ff if none

.segment "VSCREEN"
SET_CUR_BANK FINAL_BANK_VSCREEN

;*******************************************************************************
; INIT
; Clears the REU rows and discards the staged slot.
.export __viewcache_init
.proc __viewcache_init
	lda #$ff
	sta loaded
	ldxy #0
	stxy reu::reuaddr
	lda #FINAL_BANK_VSCREEN
	sta reu::reuaddr+2
	ldxy #CACHE_ROW_SIZE*SCREEN_HEIGHT
	stxy reu::txlen
	jmp reu::zero
.endproc

;*******************************************************************************
; ROW
; Writes back the previous slot and loads the requested slot on a cache miss.
; IN:
;   - .X: cache slot
; OUT:
;   - r0: character pointer
;   - r2: selection pointer
; PRESERVES:
;   - r4-rf, zp::text
.export __viewcache_row
.proc __viewcache_row
@chars=r0
@mask=r2
	cpx loaded
	beq @pointers
	txa
	pha
	ldx loaded
	bmi @load
	jsr transfer
	jsr reu::store
@load:
	pla
	tax
	stx loaded
	jsr transfer
	jsr reu::load
@pointers:
	ldxy #chars
	stxy @chars
	ldxy #inverse
	stxy @mask
	rts
.endproc

;*******************************************************************************
; TRANSFER
; Sets the REU parameters for one slot and the resident row buffer.
; IN:
;   - .X: cache slot
.proc transfer
	lda rowlo,x
	sta reu::reuaddr
	lda rowhi,x
	sta reu::reuaddr+1
	lda #FINAL_BANK_VSCREEN
	sta reu::reuaddr+2
	ldxy #chars
	stxy reu::c64addr
	ldxy #CACHE_ROW_SIZE
	stxy reu::txlen
	rts
.endproc

;*******************************************************************************
; REU ROW OFFSETS
rowlo: .repeat SCREEN_HEIGHT, i
.byte <(i*CACHE_ROW_SIZE)
.endrepeat
rowhi: .repeat SCREEN_HEIGHT, i
.byte >(i*CACHE_ROW_SIZE)
.endrepeat
