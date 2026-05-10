;*******************************************************************************
; FASTSCROLL.ASM
;*******************************************************************************

.include "../ram.inc"
.include "banks.inc"

.define BITMAP_ADDR $1100
.define HEIGHT      192
.define ROWS        23
.define SCROLL_ROWS 23*8

.CODE

;*******************************************************************************
; SCROLLUP
; Scrolls all lines from .X to .A up
; IN:
;  - .X: the top line that characters are scrolled to
;  - .A: the bottom line that is scrolled
.export __text_scrollup
.proc __text_scrollup
	JUMP FINAL_BANK_FASTSCROLL_UP, scrollup
.endproc

;*******************************************************************************
; SCROLLDOWN
; Scrolls all rows from .A to .X
; IN:
;  - .A: the first column to scroll down
;  - .X: the last column to scroll down to
.export __text_scrolldown
.proc __text_scrolldown
	ldy #$01

	; fall through to __text_scrolldown
.endproc

;*******************************************************************************
; SCROLLDOWNN
; Scrolls the whole screen down 1 character
.export __text_scrolldownn
.proc __text_scrolldownn
	JUMP FINAL_BANK_FASTSCROLL_DOWN, scrolldownn
.endproc

;*******************************************************************************
; CALC ENTRYPOINT
; Calculate where to jump into the procedure. Each row of the unrolled loop
; is 966 bytes: sizeof(LDA $xxxx STA $xxxx) * 20 * 8.
; The scroll happens from the bottom up, so for each row from the bottom we do
; NOT want to scroll, we jump forward 966 bytes. For example, if we want to
; scroll until row 18, we need to jump 966*(ROWS-18) bytes into the routine.
; IN:
;   -.A: number of rows to NOT scroll (if scrolling up) or number to scroll
;        (if scrolling down)
; OUT:
;   - zp::text: address to jump to
.macro calc_entrypoint speedcode
@entrypoint = zp::text
@rows       = zp::text+2
@tmp0       = zp::text+3
@tmp1       = zp::text+4
	sta @rows

	; calculate where to enter the speedcode
	; 960 * stop or (30 << 5) * rows
	lda #$00
	sta @entrypoint+1

	lda @rows
	asl			; *2
	sta @tmp0
	asl			; *4
	sta @tmp1
	asl			; *8
	adc @tmp1		; *12
	bcc :+
	inc @entrypoint+1
:	adc @tmp0		; *14
	bcc :+
	inc @entrypoint+1
:	adc @rows		; *15
	bcc :+
	inc @entrypoint+1

:	asl			; *30
	rol @entrypoint+1	;

	asl			; *60
	rol @entrypoint+1	;

	asl			; *120
	rol @entrypoint+1	;

	asl			; *240
	rol @entrypoint+1	;
	adc @rows		; *241
	bcc :+
	inc @entrypoint+1

:	asl			; *482
	rol @entrypoint+1
	adc @rows		; *483
	bcc :+
	inc @entrypoint+1

:	asl			; *966
	rol @entrypoint+1	;

	;clc
	adc #<speedcode
	sta @entrypoint
	lda #>speedcode
	adc @entrypoint+1
	sta @entrypoint+1
.endmacro

.segment "FASTSCROLL_UP"
;*******************************************************************************
; SCROLLUP
; Scrolls all lines from .X to .A up
; IN:
;  - .X: the row to start scorlling at
;  - .A: the bottom line to scroll
.export scrollup
scrollup:
@start = zp::text
@stop  = zp::text+1
	stx @start
	sta @stop
	cmp @start
	bcs :+
	rts

:	; get number of characters to scroll (.A - .X)
	;sec
	sbc @start
	tay

	lda @start
	calc_entrypoint @speedcode

;-------------------------------------------------------------------------------
@speedcode:
; scroll down all rows of characters
.repeat ROWS, row

; scroll down 1 row of characters
.repeat 20, col

; scroll down 1 character row
.repeat 8, chrow
	lda BITMAP_ADDR + col*HEIGHT + (row*8)+chrow + 8
	sta BITMAP_ADDR + col*HEIGHT + (row*8)+chrow
.endrep
.endrep
	; check if we're done (6 bytes)
	dey
	bne :+
	jmp @done
:
.endrep
@done:	rts

.segment "FASTSCROLL_DOWN"
;*******************************************************************************
; SCROLLDOWNN
; Scrolls the whole screen down 1 character
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row by
.proc scrolldownn
@stop  = zp::text
@start = zp::text+1
	cpy #$01
	beq :+

	; if scrolling by more than 1 character, we need to fallback to
	; loop-based scrolling
	jmp scrolldownn_slow

:	; get number of rows to scroll (stop - start)
	sta @start
	stx @stop
	lda @stop
	sec
	sbc @start
	tay

	lda #ROWS
	sec
	sbc @stop
	calc_entrypoint @speedcode
	jmp (zp::text)			; execute the scroll

;-------------------------------------------------------------------------------
@speedcode:
; scroll up each row of characters
.repeat ROWS, row

; scroll up one of characters
.repeat 20, col

; scroll up 1 character row
.repeat 8, chrow
	lda BITMAP_ADDR + col*HEIGHT + ((ROWS-1)*8)-(row*8) + chrow
	sta BITMAP_ADDR + col*HEIGHT + ((ROWS-1)*8)-(row*8) + chrow + 8
.endrep
.endrep
	; check if we're done (6 bytes)
	dey
	bne :+
	jmp @done
:
.endrep
@done:
	rts
.endproc

;*******************************************************************************
; SCROLLDOWNN SLOW
; Scrolls all rows in the given range down by the given number of rows
; We can't do the unrolled loop based approach for arbitrary number of
; characters, so this is a fallback for the case where .Y != 1
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row by
.proc scrolldownn_slow
@rowstart = zp::text
@rows     = zp::text+1
@src      = zp::text+2
@dst      = zp::text+4
@offset   = r0
	sta @rowstart
	sty @offset
	dec @offset

	cpx @rowstart
	beq @done	; if first and last rows are equal, no scroll

	; calculate number of pixel rows: (last_row - first_row - offset) * 8
	txa
	sec
	sbc @rowstart
	sbc @offset
	beq @done	; if all rows are off screen, no scroll
	bmi @done
	asl
	asl
	asl
	sta @rows
	dec @rows	; -1 because we will do the last row separately

	; get pixel offset (char_offset * 8)
	tya
	asl
	asl
	asl
	sta @offset

	; get pixel source (first_row * 8)
	; and dest (first_row * 8 + pixel_offset)
	lda @rowstart
	asl
	asl
	asl
	sta @src
	adc @offset
	sta @dst

	lda #>BITMAP_ADDR
	sta @src+1
	sta @dst+1

@l0:	ldy @rows
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bne @l1
	lda (@src),y	; do last row
	sta (@dst),y

	lda @src
	clc
	adc #$c0
	sta @src
	bcc :+
	inc @src+1

:	lda @dst
	clc
	adc #$c0
	sta @dst
	lda @dst+1
	adc #$00
	sta @dst+1
	cmp #$20
	bne @l0
@done:	rts
.endproc
