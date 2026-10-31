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

.CODE

;*******************************************************************************
; TABLE CALL
; Dispatches to the target bank's jump table. The destination address is based
; on the RETURN address of the table call because the calls are arranged
; linearly
;   - proc1
;   - proc2
;   - proc3
; IN:
;  - $100[SP+1]: bank byte and entry-to-target adjustment
;  - $100[SP+3]: identifies the destination jump table slot
; OUT:
;  - .A: target result
;  - .X: target result
;  - .Y: target result
;  - .P: target flags
.export __ram_table_call
.proc __ram_table_call
@a = zp::banktmp+1
@x = zp::banktmp+2
@y = zp::inline+1
@desc = zp::inline+2
	sta @a
	stx @x
	sty @y

	; consume the gate return address to locate its descriptor
	pla
	sta @desc
	pla
	sta @desc+1
	jsr exp::push_bank

	; read the destination's bank byte
	ldy #$01		; offset to bank byte
	lda (@desc),y
	sta bank		; set bank byte

	; translate the stub's return address into its destination JMP address
	; (return_address - adjustment)
	; the adjustment is (entries+$02 - targets) where entries is the MAIN
	; bank's table and targets is the target bank's
	iny			; offset to adjustment
	pla
	sec
	sbc (@desc),y
	sta zp::bankjmpvec
	iny
	pla
	sbc (@desc),y
	sta zp::bankjmpvec+1

	ldy @y
	jmp exec		; enter the banked procedure
.endproc
