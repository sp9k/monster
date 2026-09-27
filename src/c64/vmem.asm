.include "ram.inc"
.include "reu.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../memory.inc"

.import prog00

.DATA

;*******************************************************************************
savexy: .word 0
save01: .byte 0

.CODE

;*******************************************************************************
; LOAD
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load
.proc __vmem_load
@tmp=zp::banktmp
	stxy savexy

	; translate and the prog00 buffer need all RAM visible (prog00 lives
	; under the I/O space); callers may be in a banked context ($01=$37)
	lda $01
	sta save01
	lda #$34
	sta $01

	jsr __vmem_translate
	cmp #FINAL_BANK_MAIN
	bne :+

@00:	stxy @tmp
	ldy #$00
	lda (@tmp),y

	ldx savexy+1
	bne @done
	ldx savexy
	cpx #$01
	bne @done

	; force input bits in register $00 to read as 1
	lda prog00
	eor #$ff		; force INPUT bits '1'
	and #$07		; AND so that OUTPUT bits are 0
	ora prog00+1		; OR actual value of register $01
	jmp @done

:	stxy reu::reuaddr
	cmp #^REU_VMEM_ROM
	bne :+

@rom:	php
	sei			; do not enter a KERNAL IRQ with ROM visible
	lda $01
	pha
	lda #$33		; expose ROM
	sta $01
	stxy @addr
@addr=*+1
	ldx $f00d

	pla
	sta $01			; restore bank register
	plp

	txa
	jmp @done

:	sta reu::reuaddr+2
	jsr reu::load1

@done:	pha
	lda save01
	sta $01			; restore caller's memory context
	pla
	ldx savexy
	ldy savexy+1
	clc
	pha
	pla			; restore .N/.Z from the loaded value (in .A)
	rts
.endproc

;*******************************************************************************
; LOAD OFF
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A: the offset of the virtual address to load
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load_off
.proc __vmem_load_off
@tmp=zp::banktmp
	stxy savexy
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	jsr __vmem_load
	ldxy savexy
	rts
.endproc

;*******************************************************************************
; STORE
; Stores a byte at the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A:  the byte to store
.export __vmem_store
.proc __vmem_store
@addr=zp::banktmp
	stxy savexy

	pha

	; translate and the prog00 buffer need all RAM visible (prog00 lives
	; under the I/O space); callers may be in a banked context ($01=$37)
	lda $01
	sta save01
	lda #$34
	sta $01

	jsr __vmem_translate
	cmp #FINAL_BANK_MAIN
	bne :+

@00:	stxy @addr
	ldy #$00
	pla
	sta (@addr),y
	jmp @done

:	cmp #^REU_VMEM_ROM
	bne :+
	lda #^REU_VMEM_ADDR
:	stxy reu::reuaddr
	sta reu::reuaddr+2
	pla
	jsr reu::store1

@done:	pha
	lda save01
	sta $01			; restore caller's memory context
	pla
	ldxy savexy		; restore .XY
	rts
.endproc

;*******************************************************************************
; STORE OFF
; Stores a byte at the physical address associated with the given virtual
; address offset by the given offset.
; IN:
;  - .XY:         the virtual address
;  - .A:          the offset from the base address
;  - zp::bankval: the value to store
.export __vmem_store_off
.proc __vmem_store_off
@tmp=zp::banktmp
	stxy savexy
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	lda zp::bankval
	jsr __vmem_store
	ldxy savexy
	rts
.endproc

;*******************************************************************************
; TRANSLATE
; Returns the physical address associated with the given virtual address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .XY: the physical address
;  - .A:  the bank number of the physical address
.export __vmem_translate
.proc __vmem_translate
	cpy #>$0400
	bcs :+

@00:	; $00-$400 is stored in the prog00 buffer
	add16 #(prog00-$00)
	lda #FINAL_BANK_MAIN
	rts

:	; check the bank register to see if the virtual address is:
	; - virtual RAM
	; - virtual I/O
	; - BASIC, KERNAL, or character ROM
	; bank-control pins set as inputs are pulled to 1,
	; regardless of the value stored in the bank register
	; the cartridge is disabled while the user's program runs
	lda prog00
	eor #$ff
	ora prog00+1

	cpy #$a0
	bcc @ram
	cpy #$c0
	bcc @basic
	cpy #$d0
	bcc @ram
	cpy #$e0
	bcs @kernal

	; $d000-$dfff is all RAM if both LORAM and HIRAM are low, regardless of
	; CHAREN. Otherwise CHAREN selects I/O or character ROM
	pha
	and #$03
	beq @d000ram
	pla
	and #$04
	bne @io
	beq @rom
@d000ram:
	pla
	jmp @ram

@basic:
	and #$03
	cmp #$03
	beq @rom
	bne @ram

@kernal:
	and #$02
	bne @rom
	beq @ram

@io:	lda #^REU_VMEM_IO
	RETURN_OK

@rom:	lda #^REU_VMEM_ROM
	RETURN_OK

@ram:	lda #^REU_VMEM_ADDR
	RETURN_OK
.endproc

;*******************************************************************************
; WRITABLE
; Checks whether assembled output will be visible at the given address.
; Rejects VISIBLE ROM
; IN:
;   - .XY: the address to check for writability
; OUT:
;   - .C: set if the address is NOT writable
; CLOBBERS: NONE
.export __vmem_writable
.proc __vmem_writable
	pha
	txa
	pha
	tya
	pha

	lda $01
	pha
	lda #$34
	sta $01
	jsr __vmem_translate
	tax
	pla
	sta $01
	cpx #^REU_VMEM_ROM
	beq @rom

@writable:
	pla
	tay
	pla
	tax
	pla
	RETURN_OK		; writable

@rom:	pla
	tay
	pla
	tax
	pla
	sec			; not writable
	rts
.endproc

;*******************************************************************************
; IS INTERNAL ADDRESS
; Always returns .Z set to indicate that address is "internal"
; On the C64 all user RAM is "internal" (must be swapped for debugging)
; IN:
;  - .XY: the address to test
; OUT:
;  - .Z: set
.export is_internal_address
.proc is_internal_address
	lda #$00
	rts
.endproc
