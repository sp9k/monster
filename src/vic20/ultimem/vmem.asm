.include "ultimem.inc"
.include "../vaddrs.inc"
.include "../../macros.inc"
.include "../../memory.inc"

.segment "BANKCODE"

addr     = zp::bankaddr0
savey    = zp::banktmp
savecfg  = zp::banktmp+1
saveblk1 = zp::banktmp+2

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
	jsr translate
	lda (addr),y
	jmp vmem_done
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
	sta zp::bankoffset
	jsr translate
	ldy zp::bankoffset
	lda (addr),y
	jmp vmem_done
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
	pha
	jsr translate
	pla
	sta (addr),y
	jmp vmem_done
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
	sta zp::bankoffset
	jsr translate
	ldy zp::bankoffset
	lda zp::bankval
	sta (addr),y

	; fall through to vmem_done
.endproc

;*******************************************************************************
; VMEM DONE
; Restores BLK1 to the MAIN bank, restores .Y, and returns
.proc vmem_done
	pha

	; restore BLK1 and its config
	lda savecfg
	sta $9ff2
	lda saveblk1
	sta $9ff8

	pla

	ldy savey
	rts
.endproc

;*******************************************************************************
; TRANSLATE
; Returns the physical address associated with the given virtual address
; Also sets up the Ultimem so that the address is available for reading/writing
; IN:
;  - .XY: the virtual address
; OUT:
;  - addr:  physical address
;  - .Y:    0
.proc translate
BASE=$2000
	sty savey

	; save curent banks
	lda $9ff2
	sta savecfg
	lda $9ff8
	sta saveblk1

	; get translated address relative to BLK1 where it will be mapped
	jsr @translate
	stxy addr

	; activate RAM (r/w) at BLK1
	sta $9ff8
	lda #$57
	sta $9ff2		; make BLK1 RAM (r/w)

	ldy #$00
	rts

;--------------------------------------
; TRANSLATE
; returns the translated address based at BLK1 in .XY and its bank in .A
; bank0: $00-$0400, $1000-$2000, $9000-$9800 (internal)
; bank1: $0400-$1000                         (RAM123)
; bank2: $2000-$4000                         (BLK1)
; bank3: $4000-$6000                         (BLK2)
; bank4: $6000-$8000                         (BLK3)
; bank5: $a000-$c000                         (BLK5)
@translate:
	cpy #>$0400
	bcs :+

@00:	; $00-$400 is stored in the prog00 buffer
	add16 #(prog00-$00+BASE)
	lda #SIMRAM_00_BANK
	rts

:	cpy #>$1000
	bcs :+
@0400:	; $0400-$1000 (RAM123)
	add16 #(BASE+$0400)-$0400	; Ultimem maps RAM1,2,3 offset by $400
	lda #VMEM_RAM123_BANK
	rts

:	cpy #>$2000
	bcs :+
@1000:	; $1000-$2000
	add16 #prog1000-$1000
	lda #SIMRAM_00_BANK
	rts

:	cpy #>$8000
	bcc @expansion		; $2000-$8000 -> BLK1/2/3
	cpy #>$9000
	bcc @rom		; $8000-$9000 -> ROM

	cpy #>$9800
	bcs :+
@9000:	add16 #prog9000-$9000
	rts

:	cpy #$a0
	bcs :+
@9800:	; $9800-$a000 (IO123)
	add16 #(BASE+$1800)-$9800	; Ultimem maps RAM1,2,3 offset by $1800

	lda #VMEM_RAM123_BANK
	rts

:	cpy #$c0		; in ROM?
	bcc @expansion

@rom:	; ROM doesn't need to be translated and will be read directly
	lda #SIMRAM_00_BANK	; any bank (doesn't really matter)
	rts

@expansion:
	; get most significant 3 bits to get the bank to use (BLK 1,2,3, or 5)
	tya
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #VMEM_BLK1_BANK
	pha			; save bank
	tya
	and #$1f		; get lower 5 bits of MSB (offset from bank)
	;clc
	adc #$20		; add BLK1 base ($2000)
	tay
	pla			; restore bank
	rts
.endproc

.CODE

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
	cpy #$80
	bcc @done	; [$00, $8000) -> writable
	cpy #$c0
	bcs @done	; [$c000, $ffff) -> not writable
	cpy #$a0
	bcs @writable
	sec		; [$8000, $a000) -> not writable
	rts
@writable:
	clc
@done:	rts
.endproc

;*******************************************************************************
; IS INTERNAL ADDRESS
; Returns with .Z set if the given address is outside of the address ranges
; [$2000,$8000] or [$a000,$ffff]
;
; IN:
;  - .XY: the address to test
; OUT:
;  - .Z: set if the address in [$00,$2000] or [$8000,$a000]
.export is_internal_address
.proc is_internal_address
	cpy #$20
	bcc @internal
	cpy #$80
	bcc @external
	cmpw #$94f0
	bcc @internal
@external:
	lda #$ff
	rts
@internal:
	lda #$00
	rts
.endproc
