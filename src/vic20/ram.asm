.include "expansion.inc"
.include "../inline.inc"
.include "../macros.inc"
.include "../zeropage.inc"

.segment "SHAREBSS"

;*******************************************************************************
; COPY SRC/DST
; These 24-bit addresses are used by __ram_copy
.export __ram_src
__ram_src: .res 3
.export __ram_dst
__ram_dst: .res 3

.segment "BANKCODE"

bank = zp::banktmp

;*******************************************************************************
; CALL
; Inline procedure to call a routine in another bank
; Performs a JSR to the target address at the given bank. When the routine is
; done, returns to the caller's bank.
; IN:
;  - *+3: the bank of the procedure to call
;  - *+4: the procedure address
.export __ram_call
.proc __ram_call
@a = zp::banktmp+1
@x = zp::banktmp+2
	stx @x
	sta @a

	jsr inline::setup
	jsr exp::push_bank	; save current bank
	jsr inline::getarg_b	; get bank byte
	sta bank

	jsr inline::getarg_w	; get procedure address
	stx zp::bankjmpvec
	sta zp::bankjmpvec+1

	jsr inline::setup_done

	; fall through to exec
.endproc

;*******************************************************************************
; EXEC
; Calls the vectored address in the bank stored in zp::banktmp
.proc exec
@a = zp::banktmp+1
@x = zp::banktmp+2
	lda bank		; get the bank to activate
	SELECT_BANK_A		; and activate it

	lda @a			; restore .A
	ldx @x			; restore .X
	jsr zp::bankjmpaddr	; call the target routine
	php
	sta @a			; save .A
	stx @x			; save .X

	jsr exp::pop_bank

	lda @a			; restore .A
	ldx @x			; restore .X
	plp
	rts
.endproc
