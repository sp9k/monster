;*******************************************************************************
; REU.ASM
; This file contains C64-specific REU routines
;*******************************************************************************

.export __reu_c64_addr
.export __reu_reu_addr
.export __reu_txlen

.include "macros.inc"
.include "../errors.inc"
.include "../inline.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"

.export __reu_move_src
.export __reu_move_dst
.export __reu_move_size

REU_TMP_ADDR            = $ff0000
REU_VMEM_ADDR           = $fe0000

savex = zp::inline
savey = zp::inline+1

.import prog00

; This state is read by mapreu while I/O is banked in ($01=$36), so it must
; live in low RAM (always visible), NOT under the I/O space or KERNAL ROM
.DATA

;*******************************************************************************
__reu_c64_addr: .word 0	; -> $df02
__reu_reu_addr: .res  3	; -> $df04
__reu_txlen:    .word 0 ; -> $df07

;*******************************************************************************
; load/store scratch. NOT in the zeropage: reu::loadb/storeb are called by the
; source primitives while callers keep live pointers in zp::str0-str3 (e.g.
; edit::find), so these must not alias that space.
savea: .byte 0
savep: .byte 0
tmp:   .word 0

; move parameters (24-bit REU src/dst + 16-bit size). NOT in the zeropage:
; they previously aliased zp::bankstack, so a gap-expanding source insert
; reached inside a ram::call would corrupt the saved bank stack.
__reu_move_src:  .res 3
__reu_move_dst:  .res 3
__reu_move_size: .word 0

;*******************************************************************************
; TABLE STATE
; These parameters contain the properties of the table usedb by the
; tab* procedures
tab_addr:         .res 3
tab_element_size: .byte 0
tab_num_elements: .word 0

.CODE

;*******************************************************************************
; SAVE PROG00
.export __reu_saveprog00
.proc __reu_saveprog00
	stxy @ret

	lda #$36
	sta $01

	ldxy #$0000
	stxy $df02
	stxy $df04
	lda #^REU_VMEM_ADDR
	sta $df04+2

	ldxy #$400	; prog00 window is $0000-$03ff (1024 bytes)
	stxy $df07

	lda #$90	; transfer from c64 -> REU with immediate execution
	sta $df01	; execute

	lda #$34
	sta $01
@ret=*+1
	jmp $f00d
.endproc

;*******************************************************************************
; RESTORE PROG00
.export __reu_restoreprog00
.proc __reu_restoreprog00
	lda #$36
	sta $01

	ldxy #prog00
	stxy $df02

	ldxy #$0000
	stxy $df04
	lda #^REU_VMEM_ADDR
	sta $df04+2

	ldxy #$400	; prog00 window is $0000-$03ff (1024 bytes)
	stxy $df07

	lda #$91	; transfer from REU -> c64 with immediate execution
	sta $df01	; execute

	lda #$34
	sta $01
	rts
.endproc

;*******************************************************************************
; MAPREU
; Copies the virtual registers to their physical ones.
; I/O must be enabled before calling this (IO_BEGIN)
.proc mapreu
	lda __reu_c64_addr
	sta $df02
	lda __reu_c64_addr+1
	sta $df02+1

	lda __reu_reu_addr
	sta $df04
	lda __reu_reu_addr+1
	sta $df04+1
	lda __reu_reu_addr+2
	sta $df04+2

	lda __reu_txlen
	sta $df07
	lda __reu_txlen+1
	sta $df07+1
	rts
.endproc

;*******************************************************************************
; TXLEN EMPTY
; The REC interprets a transfer length of 0 as 64KB, so all block transfers
; must treat a length of 0 as a no-op.
; OUT:
;   - .Z: set if reu::txlen is 0
.proc txlen_empty
	lda __reu_txlen
	ora __reu_txlen+1
	rts
.endproc

;*******************************************************************************
; INIT
.export __reu_init
.proc __reu_init
	IO_BEGIN
	jsr mapreu
	lda #$00
	sta $df0a	; count UP
	IO_DONE

	rts
.endproc

;*******************************************************************************
; STORE1
; Stores one byte to the given source 24-bit address
; IN:
;   - .A:            the value to store
;   - reu::reu_addr: the address to store to (24 bit)
.export __reu_store1
.proc __reu_store1
@tmp=tmp
	sta @tmp

	IO_BEGIN
	jsr mapreu
	lda #<@tmp
	sta $df02	; c64addr
	lda #>@tmp
	sta $df02+1	; c64addr+1

	lda #$01
	sta $df07	; txlen

	lda #$00
	sta $df07+1	; txlen+1
	sta $df0a

	lda #$90	; transfer from c64 -> REU with immediate execution
	sta $df01	; execute
	IO_DONE

	lda @tmp	; restore .A
	rts
.endproc
;*******************************************************************************
; STORE
; Moves the data from the given source 24-bit address to the given
; destination one.
; IN:
;   - reu::c64_addr: the source address (24 bit)
;   - reu::reu_addr: the destination address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_store
.proc __reu_store
	jsr txlen_empty
	beq @done
	IO_BEGIN
	jsr mapreu
	lda #$00
	sta $df0a
	lda #$90	; transfer from c64 -> REU with immediate execution
	sta $df01	; execute
	IO_DONE

@done:	rts
.endproc

;*******************************************************************************
; LOAD1
; Loads one byte from the given source 24-bit address
; IN:
;   - reu::reu_addr: the address to load from (24 bit)
; OUT:
;   - .A: the byte that was read
.export __reu_load1
.proc __reu_load1
@tmp=tmp
	IO_BEGIN
	jsr mapreu
	lda #<@tmp
	sta $df02	; c64addr
	lda #>@tmp
	sta $df02+1	; c64addr+1

	lda #$01
	sta $df07	; txlen

	lda #$00
	sta $df07+1	; txlen+1
	sta $df0a

	lda #$91	; transfer from REU -> c64 with immediate execution
	sta $df01	; execute
	IO_DONE
	lda @tmp	; read the byte we loaded
	rts
.endproc

;*******************************************************************************
; LOAD
; Loads the C64 with data from the given source 24-bit address to the given
; C64 address
; IN:
;   - reu::c64_addr: the source address (24 bit)
;   - reu::reu_addr: the destination address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_load
.proc __reu_load
	jsr txlen_empty
	beq @done
	IO_BEGIN
	jsr mapreu
	lda #$00
	sta $df0a
	lda #$91	; transfer from REU -> c64 with immediate execution
	sta $df01	; execute
	IO_DONE
@done:	rts
.endproc

;*******************************************************************************
; LOAD DELAYED
; Loads the C64 with data from the given source 24-bit address to the given
; C64 address
; This version of the routine can load in the entire 16-bit address space
; IN:
;   - reu::c64_addr: destination address (24 bit)
;   - reu::reu_addr: source address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_load_delayed
.proc __reu_load_delayed
	jsr txlen_empty
	beq @done
	lda #$36
	sta $01

	lda #$81	; transfer REU -> C64 (delayed)
	sta $df01

	jsr mapreu

	lda #$34
	sta $01

	lda $ff00
	sta $ff00

@done:	rts
.endproc

;*******************************************************************************
; STORE DELAYED
; Stores the C64 memory from the given source 24-bit address to the given
; REU address
; This version of the routine can load in the entire 16-bit address space
; IN:
;   - reu::c64_addr: the source address (24 bit)
;   - reu::reu_addr: the destination address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_store_delayed
.proc __reu_store_delayed
	jsr txlen_empty
	beq @done
	lda #$36
	sta $01

	jsr mapreu

	lda #$80	; transfer C64 RAM -> REU delayed
	sta $df01

	lda #$34
	sta $01

	lda $ff00
	sta $ff00

@done:	rts
.endproc

;*******************************************************************************
; COMPARE
; Compares the data at reuaddr and c64addr for up to reu::txlen bytes.
; OUT:
;   .Z: set if there are no differences
.export __reu_compare
.proc __reu_compare
	jsr txlen_empty
	beq @done	; 0 bytes -> no differences (.Z set)
	IO_BEGIN
	jsr mapreu
	lda $df00	; read status to clear fault bit
	lda #$93|$20	; compare C64 <-> REU
	sta $df01	; execute
	ldx $df00
	IO_DONE

	txa
	and #$20	; check fault bit (set if differences found)
@done:	rts
.endproc

;*******************************************************************************
; SWAP
; Swaps the data from the REU at the address in reu::reuaddr with the data
; in the C64 at reu::c64addr.
.export __reu_swap
.proc __reu_swap
	jsr txlen_empty
	beq @done
	IO_BEGIN
	jsr mapreu
	lda #$92	; swap c64 <-> REU with immediate execution
	sta $df01	; execute
	IO_DONE
@done:	rts
.endproc

;*******************************************************************************
; ZERO
; Zeroes out the number of bytes in txlen at reu::move_dst
.export __reu_zero
.proc __reu_zero
	jsr txlen_empty
	beq @skip
	IO_BEGIN
	ldxy #@zero
	stxy __reu_c64_addr

	jsr mapreu

	lda #$80
	sta $df0a		; fix c64 address

	lda #$90
	sta $df01		; transfer c64 -> REU

@zero=*+1			; zero byte
	lda #$00
	sta $df0a		; unfix c64 address
	IO_DONE
@skip:	rts
.endproc

;*******************************************************************************
; MOVE
; Moves the given addresses from one part of the REU to another
; This routine first copies the data to the C64 and then stores
; it back to the REU at the destination address
; IN:
;   - reu::move_src: the address of the data to move
;   - reu::move_dst: the destination address in the REU
;   - reu::move_size: # of byte to relocate
.export __reu_move
.proc __reu_move
@src=__reu_move_src
@dst=__reu_move_dst
@size=__reu_move_size
	lda @size
	ora @size+1
	bne :+
	rts				; nothing to move

:	lda __reu_reu_addr+2
	pha				; save current "bank"

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1

	; backup the C64 memory we will clobber
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr
	lda #^REU_TMP_ADDR
	sta __reu_reu_addr+2
	jsr __reu_swap

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr

	; bring in the source data to relocate
	lda @src
	sta __reu_reu_addr
	lda @src+1
	sta __reu_reu_addr+1
	lda @src+2
	sta __reu_reu_addr+2
	jsr __reu_load

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1
	ldxy #@end
	stxy __reu_c64_addr

	; and store it to its relocation address
	lda @dst
	sta __reu_reu_addr
	lda @dst+1
	sta __reu_reu_addr+1
	lda @dst+2
	sta __reu_reu_addr+2
	jsr __reu_store

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1


	; finally, restore the C64's memory that we used as an intermediate
	; buffer
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr
	lda #^REU_TMP_ADDR
	sta __reu_reu_addr+2
	jsr __reu_swap

	pla
	sta __reu_reu_addr+2	; restore "bank"
	rts
@end=*
.endproc

;*******************************************************************************
; FIND
; Seeks, page by page, for the given string beginning at the given
; address. If no match is found at the 64k page of the given address,
; returns with the .C flag set.
; IN:
;  - .XY:           the string to look for
;  - .A:            the length of the string
;  - reu::reu_addr: the address to start seeking at
; OUT:
;  - .C: set if the string is not found
;  - .A:  the 64k block of the return address (same as one given)
;  - .XY: the address of the string (if found)
.export __reu_find
.proc __reu_find
@str=zp::bankoffset
@len=zp::bankoffset+2
@tmp=zp::bankoffset+3
@pagebuff=@end
	stxy @str
	sta @len

	ldxy #$100
	stxy __reu_txlen
	ldxy #@pagebuff
	stxy __reu_c64_addr
	stxy __reu_c64_addr

	; read one page for compare
	jsr __reu_load

	; search the page for the string
	ldy #$00
	ldx #$00
@l0:	lda (@str),y
	cmp @pagebuff,y
	beq @next

	; .Y -= .X (backtrack the # of chars we matched)
	stx @tmp
	tya
	sec
	sbc @tmp
	tay
	ldx #$ff		; reset char match count

@next:	inx
	cpx @len
	beq @found
	iny
	bne @l0
	inc __reu_reu_addr+1	; next page
	bne @l0			; repeat until end of 64k block
	sec			; flag not found
	rts

@found:	tya
	clc
	adc __reu_reu_addr
	tax
	lda __reu_reu_addr+1
	adc #$00
	tay
	lda __reu_reu_addr+2
	RETURN_OK
@end:
.endproc

;*******************************************************************************
; STORE BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; Because the return address is adjusted, should only be called (JSR)
; e.g.
;	jsr reu::storeb
;	.word addr
; IN:
;  - .A:          the bank to store to
;  - *+3:         the address to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export	__reu_storeb
.proc __reu_storeb
@dst=zp::banktmp
	sta savea

	; save flags register
	php
	pla
	sta savep

	jsr inline::setup

	; read the address to load from
	jsr inline::getarg_zp_ind
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	lda savea
	jsr __reu_store1

	ldx savex
	ldy savey

	; restore flags register
	lda savep
	pha
	lda savea
	plp

	rts
.endproc

;*******************************************************************************
; STOREB OFF
; IN:
;   - *+3: 1 byte - the zeropage address to write to
;   - .A:  the value to write
; OUT:
;   - .P: unaffected
; CLOBBERS:
;   - NONE
.export	__reu_storeb_off
.proc __reu_storeb_off
	sta savea

	; save flags register
	php
	pla
	sta savep

	jsr inline::setup

	; read the address to load from
	jsr inline::getarg_zp_ind_off
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	lda savea
	jsr __reu_store1

	ldx savex
	ldy savey

	; restore flags register
	lda savep
	pha
	lda savea
	plp

	rts
.endproc

;*******************************************************************************
; STOREW
; IN:
;  - *+3: address to write to
;  - .XY: the value to write
; CLOBBERS:
;  - .A, .X, .Y, .P
.export	__reu_storew
.proc __reu_storew
@dst=tmp
	stxy @dst

	jsr inline::setup

	; read the address to store to
	jsr inline::getarg_w
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	; 2 bytes
	ldxy #$02
	stxy __reu_txlen

	ldxy #@dst
	stxy __reu_c64_addr

	jsr __reu_store

	ldx savex
	ldy savey
	rts
.endproc

;*******************************************************************************
; LOADB
; IN:
;  - *+3: address to read
; OUT:
;  - .A: the byte that was read
;  - .N: set if loaded byte is negative
;  - .Z: set if loaded byte is 0
.export	__reu_loadb
.proc __reu_loadb
	; save .C flag
	php
	pla
	and #$01		; mask .C bit
	sta savep

	; read the address to load from
	jsr inline::setup
	jsr inline::getarg_zp_ind
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	jsr __reu_load1
	sta savea

	ldx savex
	ldy savey

	; set flags
	cmp #$00
	php
	pla
	and #$fe
	ora savep	; restore .C bit
	pha

	lda savea

	; restore flags register
	plp
	rts
.endproc

;*******************************************************************************
; LOADB OFF
; IN:
;  - *+3: 1 byte - base address to read
;  - .Y:  offset from base address
; OUT:
;  - .A: the byte that was read
;  - .N: set if loaded byte is negative
;  - .Z: set if loaded byte is 0
.export	__reu_loadb_off
.proc	__reu_loadb_off
	; save .C flag
	php
	pla
	and #$01		; mask .C bit
	sta savep

	; read the address to load from
	jsr inline::setup
	jsr inline::getarg_zp_ind_off
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	jsr __reu_load1
	sta savea

	ldx savex
	ldy savey

	; set flags
	cmp #$00	; set .N and .Z
	php
	pla
	and #$fe	; mask .C bit
	ora savep	; restore .C bit
	pha		; save .N, .Z, and .C

	lda savea

	; restore flags register
	plp
	rts
.endproc

;*******************************************************************************
; LOADW
; IN:
;  - *+3: address to read
; OUT:
;  - .XY: the value that was read
; CLOBBERS:
;  - .A, .X, .Y, .P
.export	__reu_loadw
.proc	__reu_loadw
@dst=tmp
	jsr inline::setup

	jsr inline::getarg_zp_ind_off
	stx __reu_reu_addr
	sta __reu_reu_addr+1
	jsr inline::setup_done

	; 2 bytes
	ldxy #$02
	stxy __reu_txlen

	ldxy #@dst
	stxy __reu_c64_addr

	; load the word
	jsr __reu_load
	ldxy @dst
	rts
.endproc

;*******************************************************************************
; COPY Y
; Copies .Y bytes from the source to destination
; IN:
;  - *+3: source address
;  - *+5: destination address
;  - .Y:  the offset in bytes
; CLOBBERS:
;  - .A, .P
.export __reu_copy_y
.proc __reu_copy_y
	sty __reu_move_size
	lda #$00
	sta __reu_move_size+1

	lda __reu_reu_addr+2
	sta __reu_move_src+2
	sta __reu_move_dst+2

	jsr inline::setup

	; get source and destination addresses
	jsr inline::getarg_zp_ind
	stx __reu_move_src
	sta __reu_move_src+1
	jsr inline::getarg_zp_ind
	stx __reu_move_dst
	sta __reu_move_dst+1
	jsr inline::setup_done

	jsr __reu_move		; move from source -> dest

	ldx savex
	ldy savey

	rts
.endproc
