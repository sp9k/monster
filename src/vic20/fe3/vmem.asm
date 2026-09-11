.include "finalex.inc"
.include "banks.inc"
.include "../../macros.inc"
.include "../../zeropage.inc"
.import prog00, prog1000, prog9000, prog9400
.export prog0400
.segment "FASTCOPY_BSS"
prog0400: .res $c00
dbg0400: .res $c00
; Never expose real FE3 control registers through the debugger's memory API.
prog9800: .res $800
.export dbg0400

.segment "BANKCODE"
addr = zp::bankaddr0
savedbank = zp::banktmp+2
savedy = zp::bankaddr1
savedx = zp::bankaddr1+1

.export __vmem_load, __vmem_load_off, __vmem_store, __vmem_store_off
.proc __vmem_load
	lda #0
	; fall through
.endproc
.proc __vmem_load_off
	sta zp::bankoffset
	jsr translate
	lda (addr),y
	jmp done
.endproc
.proc __vmem_store
	sta zp::bankval
	lda #0
.endproc
.proc __vmem_store_off
	sta zp::bankoffset
	jsr translate
	lda zp::bankval
	sta (addr),y
.endproc
.proc done
	pha
	lda savedbank
	sta $9c02
	ldy savedy
	ldx savedx
	pla
	rts
.endproc
.proc translate
	sty savedy
	stx savedx
	lda $9c02
	sta savedbank
	; Apply the offset before translating, including across region boundaries.
	txa
	clc
	adc zp::bankoffset
	tax
	bcc :+
	iny
:
	jsr __vmem_translate
	sta $9c02
	stxy addr
	ldy #0
	rts
.endproc
.export __vmem_translate
.proc __vmem_translate
	cpy #$04
	bcs :+
	add16 #prog00
	lda #FINAL_BANK_SIM
	rts
:
	cpy #$10
	bcs :+
	add16 #prog0400-$0400
	lda #FINAL_BANK_SIM
	rts
:
	cpy #$20
	bcs :+
	add16 #prog1000-$1000
	lda #FINAL_BANK_SIM
	rts
:
	cpy #$90
	bcc @user
	cpy #$94
	bcs :+
	add16 #prog9000-$9000
	lda #FINAL_BANK_SIM
	rts
:
	cpy #$98
	bcs @io
	add16 #prog9400-$9400
	lda #FINAL_BANK_SIM
	rts
@io:
	cpy #$a0
	bcs @user
	add16 #prog9800-$9800
	lda #FINAL_BANK_SIM
	rts
@user:
	lda #FINAL_BANK_USER
	rts
.endproc

.CODE
.export __vmem_writable
.proc __vmem_writable
	cpy #$7f
	bne :+
	cpx #$e0	; top of BLK3 is reserved for native BRK/NMI handlers
	bcs @done
:
	cpy #$80
	bcc @done
	cpy #$c0
	bcs @done
	cpy #$a0
	bcs @yes
	sec
	rts
@yes: clc
@done: rts
.endproc
