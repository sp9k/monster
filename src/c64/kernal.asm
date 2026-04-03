;*******************************************************************************
; KERNAL.ASM
; This file contains procedures for calling the KERNAL functions in Commodore
; BASIC.
;*******************************************************************************

.include "../inline.inc"
.include "../zeropage.inc"

;*******************************************************************************
; OSCALL
; Makes the KERNAL ($e000-$ffff) available and calls the procedure
.proc oscall
@a=zp::banktmp+1
@x=zp::banktmp+2
	stx @x
	sta @a

	lda #$36		; make KERNAL ($e000-$ffff) available
	sta $01
	jsr inline::setup

	jsr inline::getarg_w	; get procedure address
	stx zp::bankjmpvec
	sta zp::bankjmpvec+1
	jsr inline::setup_done

	lda @a			; restore .A
	ldx @x			; restore .X
	jsr zp::bankjmpaddr	; call the target routine
	php
	sta @a			; save .A
	stx @x			; save .X

	lda #$34		; expose RAM in $e000-$ffff again
	sta $01

	lda @a			; restore .A
	ldx @x			; restore .X
	plp			; restore status
	rts
.endproc

;*******************************************************************************
.macro KERNAL_JUMP proc
	jsr oscall
	.word proc
	rts
.endmacro

;*******************************************************************************
; CHRIN
.export __kernal_chrin
.proc __kernal_chrin
	KERNAL_JUMP $ffa5
.endproc

;*******************************************************************************
; CHROUT
.export __kernal_chrout
.proc __kernal_chrout
	KERNAL_JUMP $ffd2
.endproc

;*******************************************************************************
; CIOUT
.proc __kernal_ciout
.proc __kernal_ciout
	KERNAL_JUMP $eddd
.endproc


;*******************************************************************************
; READST
.export __kernal_readst
.proc __kernal_readst
	KERNAL_JUMP $ffb7
.endproc

;*******************************************************************************
; SETNAM
.export __kernal_setnam
.proc __kernal_setnam
	KERNAL_JUMP $ffbd
.endproc

;*******************************************************************************
; SETLFS
.export __kernal_setlfs
.proc __kernal_setlfs
	KERNAL_JUMP $ffba
.endproc

;*******************************************************************************
; LOAD
.export __kernal_load
.proc __kernal_load
	KERNAL_JUMP $ffd5
.endproc

;*******************************************************************************
; CHKIN
.export __kernal_chkin
.proc __kernal_chkin
	KERNAL_JUMP $ffc6
.endproc

;*******************************************************************************
; CHKOUT
.export __kernal_chkout
.proc __kernal_chkout
	KERNAL_JUMP $ffc9
.endproc

;*******************************************************************************
; OPEN
.export __kernal_open
.proc __kernal_open
	KERNAL_JUMP $ffc0
.endproc

;*******************************************************************************
; CLOSE
.export __kernal_close
.proc __kernal_close
	KERNAL_JUMP $ffc3
.endproc

;*******************************************************************************
; CLALL
.export __kernal_clall
.proc __kernal_clall
	KERNAL_JUMP $ffe7
.endproc

;*******************************************************************************
; CLRCHN
.export __kernal_clrchn
.proc __kernal_clrchn
	KERNAL_JUMP $ffcc
.endproc
