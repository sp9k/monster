.include "ram.inc"
.include "reu.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../memory.inc"

.import prog00

.BSS

;*******************************************************************************
savexy: .word 0

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

	jsr __vmem_translate
	cmp #FINAL_BANK_MAIN
	bne :+

@00:	stxy @tmp
	ldy #$00
	lda (@tmp),y
	jmp @done

:	stxy reu::reuaddr
	cmp #^REU_VMEM_ROM
	bne :+

@rom:	lda $01
	pha
	lda #$33		; expose ROM
	sta $01
	stxy @addr
@addr=*+1
	ldx $f00d

	pla
	sta $01			; restore bank register

	txa
	jmp @done

:	sta reu::reuaddr+2
	jsr reu::load1

@done:	ldx savexy
	ldy savexy+1
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
	jsr __vmem_translate
	cmp #FINAL_BANK_MAIN
	bne :+

@00:	stxy @addr
	ldy #$00
	pla
	sta (@addr),y
	jmp @done

:	stxy reu::reuaddr
	sta reu::reuaddr+2
	pla
	jsr reu::store1

@done:	ldxy savexy		; restore .XY
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

:	; check the bank register to see if virtual address is:
	; - virtual RAM
	; - virtual I/O
	;%0xx: Character ROM visible at $D000-$DFFF. (Except for the value %000, see above.)
	lda prog00+1	; check bank register
	and #$04	; check bit 2, if set, I/O is active
	beq @noio

@ioactive:
	cmpw #$e000
	bcs @noio
	cmpw #$d000
	bcs @io

@noio:	lda prog00+1
	and #$03	; mask bits 0 and 1
	beq @bank00
	cmp #$01
	beq @bank01
	cmp #$02
	beq @bank10
	bne @bank11

;--------------------------------------
;%x01: RAM visible at $a000-$bfff and $e000-$ffff
@bank01:
	cmpw #$d000
	bcc @ram
	cmpw #$e000
	bcc @io
	bcs @ram

;--------------------------------------
;%x10: RAM visible at $a000-$bfff; KERNAL ROM visible at $e000-$ffff
@bank10:
	cmpw #$d000
	bcc @ram
	cmpw #$e000
	bcc @io
	bcs @rom

;--------------------------------------
;%x11: BASIC ROM visible at $a000-$bfff; KERNAL ROM visible at $e000-$ffff
@bank11:
	cmpw #$a000
	bcc @ram
	cmpw #$c000
	bcc @rom	; $a000-$bfff: BASIC ROM
	cmpw #$d000
	bcc @ram	; $c000-$cfff: always RAM
	cmpw #$e000
	bcc @io		; $d000-$dfff: I/O
	bcs @rom	; $e000-$ffff: KERNAL ROM

@io:	lda #^REU_VMEM_IO
	RETURN_OK

@rom:	lda #^REU_VMEM_ROM
	RETURN_OK

;--------------------------------------
;%x00: RAM visible in all areas
@bank00:
@ram:	lda #^REU_VMEM_ADDR
	RETURN_OK
.endproc

;*******************************************************************************
; WRITABLE
; Checks if the given address is within the valid writable range.
; This includes the addresses [$00, $8000) and [$a000, $c000)
; IN:
;   - .XY: the address to check for writability
; OUT:
;   - .C: set if the address is NOT writable
.export __vmem_writable
.proc __vmem_writable
	pha

	jsr __vmem_translate
	cmp #^REU_VMEM_IO
	beq @writable
	cmp #^REU_VMEM_ROM
	bne @writable

@rom:	pla
	sec			; not writable
	rts

@writable:
	pla
	RETURN_OK		; writable
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
