.include "banks.inc"
.include "finalex.inc"
.include "../vaddrs.inc"
.include "../../ram.inc"
.include "../../macros.inc"
.include "../../memory.inc"

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
	jsr __vmem_translate
	jmp ram::load
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
	sta zp::bankval
	jsr __vmem_translate
	jmp ram::load_off
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
	sta zp::bankval
	jsr __vmem_translate
	jmp ram::store
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
	jsr __vmem_translate
	jmp ram::store_off
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
	cmpw #$8000
	bcc @done	; [$00, $8000) -> writable
	cmpw #$c000
	bcs @done	; [$c000, $ffff) -> not writable
	cmpw #$a000
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
	cmpw #$2000
	bcc @internal
	cmpw #$8000
	bcc @external
	cmpw #$9500
	bcc @internal
@external:
	lda #$ff
	rts
@internal:
	lda #$00
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

:	; TODO: this needs to be virtualized ($400-$1000)
	cpy #>$1000
	bcc @done

	cpy #>$2000
	bcs :+

@1000:	; $1000-$2000 is stored in the prog1000 buffer
	add16 #(prog1000-$1000)
	lda #FINAL_BANK_FASTCOPY
	rts

:	cpy #>$9000
	bne :+
	cpx #<$9010
	bcs @done		; $9010-$9100 is not buffered anywhere

@9000:	; $9000-$9010 is stored in the prog9000 buffer
	add16 #(prog9000-$9000)
	lda #FINAL_BANK_FASTCOPY
	rts

:	cpy #>$9500
	bne @done

@9400:	; $9400-$9500 is stored in the prog9400 buffer
	add16 #(prog9400-$9400)
	lda #FINAL_BANK_FASTCOPY
	rts

@done:	; everything else is stored at its unaltered address in the
	; USER bank
	lda #FINAL_BANK_USER
	rts
.endproc
