; STRESS.S
; test all opcodes with almost
; every possible operand value
.org $7700

;--------------------------------------
; test indexed addressing
	ldx #0
	ldy #0
test0	lda #42
	sta $1000,x
	cmp #42
	beq +
	lda #'b'
	jmp fail
:	sta $1000,y
	cmp #42
	beq +
	lda #'c'
	jmp fail
:	dex
	dey
	bne test0

;--------------------------------------
; test (zp),y for all ZP locs
test1
	lda #$00
	sta @zp0
	sta @zp2
	sta @zp3
	lda #$01
	sta @zp1

@l0	; write $1000 as address for
	; current zp loc
	lda #$00
.eq @zp0 *+1
	sta $00
	lda #$10
.eq @zp1 *+1
	sta $00

;--------------------------------------
	; test all values of .Y
	ldy #$00
@l1
:	lda #42
.eq @zp2 *+1
	sta ($00),y
.eq @zp3 *+1
	lda ($00),y
	cmp #42
	beq +
	lda #'a'
	jmp fail
:	dey
	bne @l1

	; next zeropage location
	inc @zp0
	inc @zp1
	inc @zp2
	inc @zp3
	bne @l0

ok	lda #$00
fail	jmp *
