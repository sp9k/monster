;*******************************************************************************
; INLINES.ASM
; This file contains routines for handling "inline" procedures.
; These are procedures where the parameters are stored as raw data following
; the JSR that calls them.
; e.g.
;   jsr my_proc
;   .word addr
;   .byte val
;*******************************************************************************

.include "macros.inc"
.include "zeropage.inc"

;*******************************************************************************
savex  = zp::inline
savey  = zp::inline+1
params = zp::inline+2

.segment "BANKCODE2"

;*******************************************************************************
; SETUP
; Sets up pointers for parameterized procedures like reu::store
.export __inline_setup
.proc __inline_setup
	; save the .X and .Y registers
	stx savex
	sty savey

	tsx
	pha			; save .A

	; read the address of the parameters (return address of procedure call)
	; NOTE: this assumes two JSR's were called (one for the orinal
	; procedure and one for the call to this one)
	; jsr myproc -> jsr inline::setup
	lda $103,x
	sta params
	lda $104,x
	sta params+1

	pla			; restore .A
	rts
.endproc

;*******************************************************************************
; SETUP DONE
; Pushes the return address for the inline function and returns execution
; to the instruction after the one that called this
.export __inline_setup_done
.proc __inline_setup_done
	; pull return address and calculate JMP return address
	pla
	clc
	adc #$01
	sta @ret
	pla
	adc #$00
	sta @ret+1

	; pull old return address (which is where our params began)
	pla
	pla

	; push new return address (after the params)
	lda params+1
	pha
	lda params
	pha

@ret=*+1
	jmp $f00d		; return to caller
.endproc

;*******************************************************************************
; GET ARG B
; Gets a byte argument from a parametrized proc and updates the param pointer
; to point to the next argument (if there is one)
; e.g.
;    jsr proc
;    .byte val <- returns this
; OUT:
;   - .A: the byte value that was read
.export __inline_getarg_b
.proc __inline_getarg_b
	incw params		; move to next param
	ldy #$00
	lda (params),y
	ldy savey
	rts
.endproc

;*******************************************************************************
; GET ARG W
; Gets an argument from a parametrized function and updates the param pointer
; to point to the next argument (if there is one)
; e.g.
;    jsr proc
;    .word val <- returns this
; OUT:
;   - .AX: the word value that was read
.export __inline_getarg_w
.proc __inline_getarg_w
	incw params		; move to next param
	ldy #$00
	lda (params),y
	tax
	incw params
	lda (params),y
	ldy savey
	rts
.endproc

;*******************************************************************************
; GET ARG ZP IND
; Gets an argument from a parametrized function and updates the param pointer
; to point to the next argument (if there is one)
; The argument fetched is used to lookup the address in the zeropage location
; that it references.  The value at that location is returned
; e.g.
;    jsr proc
;    .byte $10
; $10: $1234 <- returns this
; OUT:
;   - .AX: the word value that was read
.export __inline_getarg_zp_ind
.proc __inline_getarg_zp_ind
	jsr __inline_getarg_b
	sta @addr0
	clc
	adc #$01
	sta @addr1

	; read the indirect address
@addr0=*+1
	ldx $f0
@addr1=*+1
	lda $0d

	rts
.endproc

;*******************************************************************************
; GET ARG ZP IND OFF
; Gets an argument from a parametrized function and updates the param pointer
; to point to the next argument (if there is one)
; The argument fetched is used to lookup the address in the zeropage location
; that it references.  The value at that base address + the given offset is
; returned
; e.g. given .Y=$03
;    jsr proc
;    .byte $10
; $13: $1234 <- returns this
; IN:
;   - .Y: the offfset of the operand to get the address at
; OUT:
;   - .AX: the word value that was read
.export __inline_getarg_zp_ind_off
.proc __inline_getarg_zp_ind_off
	jsr __inline_getarg_b
	sta @addr0
	clc
	adc #$01
	sta @addr1

	; get the indirect operand + .Y
	tya
	clc
@addr0=*+1
	adc $f0
	tax
@addr1=*+1
	lda $0d
	adc #$00
	rts
.endproc
