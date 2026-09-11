.include "banks.inc"
.include "../../macros.inc"
.include "../../zeropage.inc"
.import enter, __irq_keydecode
.import __FE3BOOT_LOAD__, __FE3BOOT_RUN__, __FE3BOOT_SIZE__
.import __BSS_LOAD__, __BSS_SIZE__
.import __fe3_native_saved, __fe3_native_magic, dbg0400, prog0400, dbg00
.ifdef CART
.segment "CART"
.export __fe3_init
.proc __fe3_init
	sei
	cld
	; First stage runs in internal RAM while every expansion bank changes.
	ldxy #__FE3BOOT_LOAD__
	stxy r0
	ldxy #__FE3BOOT_RUN__
	stxy r2
	ldxy #__FE3BOOT_SIZE__
	stxy r4
	jsr copy
	jmp start
.endproc
.proc copy
	ldy #0
@loop:
	lda r4
	ora r4+1
	beq @done
	lda (r0),y
	sta (r2),y
	incw r0
	incw r2
	decw r4
	jmp @loop
@done: rts
.endproc

.endif

.segment "FE3BOOT"
.ifndef CART
; Normal expanded BASIC load address; the packer supplies the PRG prefix.
.assert __FE3BOOT_RUN__ = $1201, lderror, "Disk boot must start at $1201"
.word basic_end
.word 10
.byte $9e
.asciiz "4621"
basic_end: .word 0
.export __fe3_init
__fe3_init:
	sei
	cld
	ldx #$ff
	txs
	lda zp::device
	cmp #8
	bcs :+
	lda #DEFAULT_DEVICE
:	sta disk_device
	jsr prepare_load	; keep the FE3 fast loader reachable while banking
	jsr $ffe7	; CLALL
	lda #$80
	jsr $ff90	; enable KERNAL loading messages
.endif
.proc start
	; In START mode a BLK5 read locks the registers and a write unlocks
	; them. Do this from internal RAM: fetching ROM instructions re-locks.
	lda $a000
	sta $a000
	lda #$a0
	sta $9c02
	lda #0
	sta $9c03
.ifndef CART
	; Stage shared initializers in unused USER BLK5. Keep the low-RAM wedge
	; intact until the last LOAD, and avoid the console's bank-zero payload.
	cli
	ldx #<(shared_file-copies)
	jsr load_file
	sei
.endif
	ldx #0
@bank:
	stx bank
.ifdef CART
	; Explicitly switch ROM/RAM on every byte. Super ROM writes do not
	; select the matching RAM bank on all FE3 revisions.
	lda copies,x
	sta rombank
	lda copies+1,x
	sta rambank
	lda copies+2,x
	sta r0+1
	lda copies+3,x
	sta pages
	lda #0
	sta r0
	tay
@page:
	lda rombank
	sta $9c02
	lda (r0),y
	ldx rambank
	stx $9c02
	sta (r0),y
	iny
	bne @page
	inc r0+1
	dec pages
	bne @page
.else
	jsr load_file
.endif
	lda bank
	clc
	adc #4
	tax
	cpx #copies_end-copies
	bne @bank

.ifndef CART
	; All disk I/O is complete: retire the wedge before replacing its RAM.
	jsr $fd52
	lda #FINAL_BANK_USER
	sta $9c02
	jsr relocate_shared
.endif

	lda #FINAL_BANK_SIM
	sta $9c02
.ifdef CART
	jsr recover_native
.endif
	; Disk launches always start fresh. Also invalidate cold-start garbage.
	lda #0
	sta __fe3_native_saved

.ifdef CART
	; Restore ROM zero to copy initialized shared code and data.
	lda #$40
	sta $9c02
	jsr relocate_shared
.endif

	lda #FINAL_BANK_MAIN
	sta $9c02
	ldxy #__BSS_LOAD__
	stxy r0
	ldxy #__BSS_SIZE__
	stxy r4
	ldy #0
@zero:
	lda r4
	ora r4+1
	beq @ready
	lda #0
	sta (r0),y
	incw r0
	decw r4
	jmp @zero
@ready:
	jsr $fd52
	lda #<__irq_keydecode
	sta $028f
	lda #>__irq_keydecode
	sta $0290
	lda #10
	sta $0289
	sta $028c
	lda #4
	sta $028b
	lda #$80
	sta $028a
	sta $0291
.ifdef CART
	lda #DEFAULT_DEVICE
.else
	lda disk_device
.endif
	sta zp::device
	lda #0
	sta $98
	lda #$4c
	sta zp::jmpaddr
	sta zp::bankjmpaddr
	jmp enter
.endproc

.ifdef CART
; The loader runs in internal RAM, with SIM mapped and interrupts disabled.
; Restore the native snapshot before relocations and the recovery-signature
; check. The saved zero page includes the active source's gap/cursor state.
.proc recover_native
	ldx #3
@check:
	lda __fe3_native_saved,x
	cmp __fe3_native_magic,x
	beq :+
	rts
:
	dex
	bpl @check
	ldx #0
@byte:
.repeat 12, page
	lda $0400+page*$100,x
	sta prog0400+page*$100,x
	lda dbg0400+page*$100,x
	sta $0400+page*$100,x
.endrepeat
	lda dbg00,x
	sta $00,x
	inx
	beq @done
	jmp @byte
@done:
	rts
.endproc
.endif

.proc relocate_shared
	ldx #0
@reloc:
	lda relocs,x
	sta r0
	lda relocs+1,x
	sta r0+1
	lda relocs+2,x
	sta r2
	lda relocs+3,x
	sta r2+1
	lda relocs+4,x
	sta r4
	lda relocs+5,x
	sta r4+1
	jsr copy_shared
	txa
	clc
	adc #6
	tax
	cpx #relocs_end-relocs
	bne @reloc

	rts
.endproc

.proc copy_shared
	ldy #0
@loop:
	lda r4
	ora r4+1
	beq @done
	lda (r0),y
	sta (r2),y
	incw r0
	incw r2
	decw r4
	jmp @loop
@done: rts
.endproc
bank: .byte 0
rombank: .byte 0
rambank: .byte 0
pages: .byte 0
; ROM mode, RAM mode, first page, page count. Preserve source contents on reset.
; The disk packer reads this same table to extract the required files.
.export __fe3_copies, __fe3_copies_end
__fe3_copies = copies
__fe3_copies_end = copies_end
.ifndef CART
shared_file: .byte $40,FINAL_BANK_USER,$a0,$20
.endif
copies:
.byte $40,$a0,$40,$20
.byte $41,$a1,$20,$5e
.byte $41,$a1,$a0,$20
.byte $42,$a2,$a0,$20
.byte $43,$a3,$70,$10
.byte $43,$a3,$a0,$20
.byte $44,$a4,$a0,$18
.byte $45,$a5,$a0,$20
.byte $46,$a6,$a0,$20
.byte $46,$a7,$a0,$20
.byte $48,$a8,$a0,$20
.byte $49,$a9,$a0,$20
.byte $4b,$a0,$a0,$18
.byte $41,$ab,$a0,$20
.byte $41,$ac,$a0,$20
.byte $41,$ad,$a0,$20
.byte $41,$ae,$a0,$20
.byte $41,$af,$a0,$20
copies_end:
relocs:
.macro relocation name
	.import .ident(.sprintf("__%s_LOAD__", name))
	.import .ident(.sprintf("__%s_RUN__", name))
	.import .ident(.sprintf("__%s_SIZE__", name))
	.word .ident(.sprintf("__%s_LOAD__", name))
	.word .ident(.sprintf("__%s_RUN__", name))
	.word .ident(.sprintf("__%s_SIZE__", name))
.endmacro
relocation "BANKCODE"
relocation "BANKCODE2"
relocation "IRQ"
relocation "DATA"
relocation "FE3CFG"
relocs_end:

.ifndef CART
; A RAM-resident wedge survives Super RAM bank changes. The FE3 firmware's
; public SYS 41006 entry relocates its BLK5 wedge below $1000 when necessary.
; KERNAL-ROM loaders (including JiffyDOS) need no relocation. If the installed
; BLK5 loader has no recognized FE3 entry table, use the standard vectors.
.proc prepare_load
	; The ROM wedge also hooks CHROUT, CLRCHN, CLALL and GETIN. Those
	; handlers are not part of its relocatable loader and disappear when
	; we select payload RAM. Preserve only LOAD; reset the other vectors.
	lda $0330
	pha
	lda $0331
	pha
	jsr $fd52
	pla
	sta $0331
	pla
	sta $0330

	lda $0331		; high byte of the installed LOAD vector
	cmp #$a0
	bcc @done
	cmp #$c0
	bcs @done

	lda $a000
	sta $a000		; unlock while executing from internal RAM
	lda #$40		; firmware ROM zero, writable RAM123
	sta $9c02
	lda #0
	sta $9c03
	lda $a02b		; SYS 41003: initialize wedge
	cmp #$4c
	bne @standard
	lda $a02e		; SYS 41006: relocate wedge to low RAM
	cmp #$4c
	bne @standard
	jsr $a02e
@done:	rts
@standard:
	jmp $fd52
.endproc

; KERNAL owns its zero-page workspace during LOAD. Keep the record and device
; in internal RAM, and use SA=0 so even a wrong PRG header cannot move the load.
.proc load_file
	txa
	clc
	adc #4
	sta disk_record
@retry:
	ldx disk_record
	lda shared_file+1,x
	sta $9c02
	lda shared_file,x
	and #$0f
	tay
	lda hex,y
	sta filename+1
	lda shared_file+2,x
	pha
	lsr
	lsr
	lsr
	lsr
	tay
	lda hex,y
	sta filename+2
	pla
	and #$0f
	tay
	lda hex,y
	sta filename+3
	lda #4
	ldxy #filename
	jsr $ffbd
	lda #1
	ldx disk_device
	ldy #0
	jsr $ffba
	ldx disk_record
	ldy shared_file+2,x
	ldx #0
	lda #0
	cli
	jsr $ffd5
	bcs @error
	cpx #0
	bne @error
	ldx disk_record
	lda shared_file+2,x
	clc
	adc shared_file+3,x
	sta disk_end
	cpy disk_end
	bne @error
	sei
	rts
@error:
	jsr $ffcc
	ldx #0
@print:
	lda load_error,x
	beq @wait
	jsr $ffd2
	inx
	bne @print
@wait:
	cli
	jsr $ffe4
	beq @wait
	jmp @retry
.endproc
hex: .byte "0123456789abcdef"
filename: .byte "m0a0"
load_error: .byte 13,"load error - press key",13,0
disk_record: .byte 0
disk_device: .byte DEFAULT_DEVICE
disk_end: .byte 0
.endif
