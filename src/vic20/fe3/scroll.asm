.include "banks.inc"
.include "../../ram.inc"
.include "../../macros.inc"
.include "../../zeropage.inc"
.ifdef soft4x8
.macpack longbranch
.CODE
.export __text_scrollup, __text_scrollupn, __text_scrolldown, __text_scrolldownn
__text_scrollup:
	ldy #1
__text_scrollupn:
	JUMP FINAL_BANK_FASTTEXT, up
__text_scrolldown:
	ldy #1
__text_scrolldownn:
	JUMP FINAL_BANK_FASTTEXT, down
.segment "FASTTEXT"
src = zp::text
dst = zp::text+2
count = zp::text+4
columns = zp::text+5
amount = zp::text+6
first = zp::text+7
last = zp::text+8
direction = zp::text+9
.proc up
	sta last
	stx first
	lda #0
	sta direction
	jmp setup
.endproc
.proc down
	sta first
	stx last
	lda #1
	sta direction
.endproc
.proc setup
	cpy #0
	jeq setup::done
	tya
	asl
	asl
	asl
	sta amount
	lda last
	sec
	sbc first
	jcc done
	asl
	asl
	asl
	sta count
	tya
	sec
	sbc #1
	asl
	asl
	asl
	sta amount
	lda count
	sec
	sbc amount
	bcc done
	sta count
	lda amount
	clc
	adc #8
	sta amount
	lda first
	asl
	asl
	asl
	sta src
	sta dst
	lda #$11
	sta src+1
	sta dst+1
	lda direction
	bne @down
	lda src
	clc
	adc amount
	sta src
	jmp @ready
@down:
	lda dst
	clc
	adc amount
	sta dst
@ready:
	lda count
	beq done
	lda #20
	sta columns
@col:
	lda direction
	bne @reverse
	ldy #0
@forward:
	lda (src),y
	sta (dst),y
	iny
	cpy count
	bne @forward
	beq @next
@reverse:
	ldy count
	dey
@byte:
	lda (src),y
	sta (dst),y
	dey
	cpy #$ff
	bne @byte
@next:
	lda src
	clc
	adc #192
	sta src
	bcc :+
	inc src+1
:
	lda dst
	clc
	adc #192
	sta dst
	bcc :+
	inc dst+1
:
	dec columns
	bne @col
done:
	rts
.endproc
.endif
