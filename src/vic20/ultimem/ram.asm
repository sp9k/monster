;******************************************************************************
; RAM.ASM
; This file contains routines for reading, writing, and executing code in
; different banks.
;******************************************************************************

.include "banks.inc"
.include "../../config.inc"
.include "../../inline.inc"
.include "../../macros.inc"
.include "../../zeropage.inc"

.import __ultimem_bank
.import __ultimem_select_bank
.import __ram_src
.import __ram_dst

.segment "BANKCODE"

;*******************************************************************************
; STORE_BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; IN:
;  - .XY:         the address to store to
;  - .A:          the bank to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export __ram_store_byte
.proc __ram_store_byte
	pha
	lda #$00
	sta zp::bankoffset
	pla
	; fall through
.endproc

;*******************************************************************************
; STORE_BYTE_REL
; stores the byte given in zp::bankval to the address in .XA in bank .A
; IN:
;  - .XY:            the base address
;  - .A:             the bank to store to
;  - zp::bankoffset: the offset from the base address
;  - zp::bankval:    the byte to write
.export __ram_bank_store_rel
.proc __ram_bank_store_rel
@dst=zp::banktmp
	stxy @dst

	jsr __ultimem_select_bank
	lda zp::bankval
	ldy zp::bankoffset
	sta (@dst),y

	pha
	lda #FINAL_BANK_MAIN
	jsr __ultimem_select_bank
	pla
	ldxy @dst
	rts
.endproc

;*******************************************************************************
; READ_BYTE
; Returns the byte in bank .A at address .YX
; IN:
;  - .XY: the address to read from
;  - .A:  the bank to read from
; OUT:
;  - .A: the byte that was read
.export __ram_load_byte
.proc __ram_load_byte
	pha
	lda #$00
	sta zp::bankval
	pla

	; fall through
.endproc

;*******************************************************************************
; LOAD_BYTE_OFF
; Returns the byte in bank .A at address .YX plus a given offset
; IN:
;  - .XY: the address to read from
;  - .A: the bank to read from
;  - zp::bankval: the offset to read from
; OUT:
;  - .A: the byte that was read
;  - .Y: contains the offset (same that was given as zp::bankval)
.export __ram_load_byte_off
.proc __ram_load_byte_off
@src=zp::banktmp
	stxy @src
	jsr __ultimem_select_bank	; set bank
	ldy zp::bankval
	lda (@src),y
	pha
	lda #FINAL_BANK_MAIN
	jsr __ultimem_select_bank
	pla
	ldx @src
	rts
.endproc

;*******************************************************************************
; RETURN TO X
; Sets the bank to the given bank and returns (RTS)
; IN:
;  - .X: the bank to return to
.proc return_to_x
	txa
	jmp __ultimem_select_bank
.endproc

;*******************************************************************************
; COPY LINE
; Copies up to LINESIZE bytes from zp::bankaddr0 to zp::bankaddr1 stopping at
; the first $0d or $00
; IN:
;  - .A:            the bank to perform the copy within
;  - zp::bankaddr0: the source address to copy from
;  - zp::bankaddr1: the destination address to copy to
;  OUT:
;   - .Y: the number of bytes copied
;   - .A: the last byte copied
.export __ram_copy_line
.proc __ram_copy_line
	ldx __ultimem_bank		; get current bank
	jsr __ultimem_select_bank	; set bank to copy within

	ldy #$00
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	beq return_to_x
	cmp #$0d
	beq return_to_x
	iny
	cpy #LINESIZE
	bne :-
@done:	beq return_to_x			; branch always (restore bank)
.endproc

.CODE

;*******************************************************************************
; GET BYTE
; Reads the value at the given address
; IN:
;   - .XY: the address to get the value of
; OUT:
;   - .A: the value for the given address in the MAIN bank
; CLOBBERS:
;   - .A, .Y, r0-r1
.export __ram_get_byte
.proc __ram_get_byte
	stxy r0
	ldy #$00
	lda (r0),y
	rts
.endproc

;*******************************************************************************
; MEMCPY
; Copies data from ram::src to ram::dst
; The number of bytes is given in .YX
; This routine assumes that IF the memory overlaps, that it will do so from
; the TOP. (dst > src)
; NOTE: ram::src and ram::dst are 24 bit addresses.  The MSB is the bank.
; IN:
;  - ram::src: the address of the data to copy
;  - ram::dst: the address to copy to
;  - .XY: the number of bytes to copy
.export __ram_memcpy
.proc __ram_memcpy
@size    = r0
@src     = __ram_src
@dst     = __ram_dst
@bank    = __ram_src+2
@bankdst = __ram_dst+2
	stxy @size
	iszero @size
	beq @done

	decw @size

	; we need to copy from top to bottom- add @size-1 to the dst and src
	ldxy @src
	add16 @size
	stxy @src

	ldxy @dst
	add16 @size
	stxy @dst

	incw @size

@l0:	; read a byte from the source bank/addr
	ldxy @src
	lda @bank
	jsr __ram_load_byte

	; write the byte to the dest bank/addr
	sta zp::bankval
	ldxy @dst
	lda @bankdst
	jsr __ram_store_byte

	; move to the next location
	decw @src
	decw @dst

	decw @size
	lda @size
	bne @l0
	lda @size+1
	bne @l0

@done:	rts
.endproc

.segment "ULTICFG"
;*******************************************************************************
; PUSH BANK
; Saves the current RAM bank
.export __ultimem_push_bank
.proc __ultimem_push_bank
	lda __ultimem_bank	; get current bank
	ldx zp::banksp
	inc zp::banksp
	sta zp::bankstack,x	; save current bank
	rts
.endproc

;*******************************************************************************
; POP BANK
; Restores the last pushed bank
.export __ultimem_pop_bank
.proc __ultimem_pop_bank
	dec zp::banksp
	ldx zp::banksp
	lda zp::bankstack,x		; get the caller's bank
	jmp __ultimem_select_bank	; restore bank
.endproc
