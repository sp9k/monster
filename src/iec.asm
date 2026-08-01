.include "config.inc"
.include "errors.inc"
.include "kernal.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "strings.inc"
.include "util.inc"
.include "zeropage.inc"

.CODE
;*******************************************************************************
; MAIN-bank entry points
.export __io_readerr
.export __iec_seterr

.if .defined(CART) .and .defined(c64)
__io_readerr: JUMP FINAL_BANK_FILEDIR, readerr
__iec_seterr: JUMP FINAL_BANK_FILEDIR, seterr
.else
__io_readerr = readerr
__iec_seterr = seterr
.endif

BANKED_CODE "FILEDIR", FINAL_BANK_FILEDIR

;*******************************************************************************
; READERR
; Reads the drive's error into mem::drive_err (0-terminated)
; OUT:
;  - .X:             the error code
;  - mem::drive_err: the drive error message
.proc readerr
@ch=rf
	lda #$00		; no filename
	tax
	tay
	sta @ch
	jsr krn::setnam		; SETNAM

	lda #$0f		; file number 15 (error channel)
	ldx zp::device
	tay			; secondary address 15 (error channel)
	jsr krn::setlfs		; SETLFS
	jsr krn::open		; OPEN
	bcc @ok

	pha			; save the KERNAL error code
	lda #$0f
	jsr krn::close		; close command channel
	pla			; restore the KERNAL error code
	cmp #$05		; DEVICE NOT PRESENT?
	bne :+
	ldxy #strings::device_not_present
	jmp seterr
:	sec
	rts

@ok:	ldx #$0f		; filenumber 15
	jsr krn::chkin		; CHKIN (file 15 now used as input)

	; read the error message to mem::drive_err
@loop:	jsr krn::readst		; READST (read status byte)
	bne @eof		; either EOF or read error

	jsr krn::chrin		; CHRIN (get a byte from file)
	cmp #$0d
	beq @eof
	ldx @ch
	cpx #LINESIZE		; cap size of string
	bcs @eof		; if full, truncate and 0-terminate at the cap
	sta mem::drive_err,x
	inc @ch
	bne @loop		; next byte

@eof:	ldx @ch
	lda #$00
	sta mem::drive_err,x

@done:	; close the command channel (file 15)
	lda #15			; filenumber 15 (command channel)
	jsr krn::close		; CLOSE 15
	ldxy #mem::drive_err
	jmp atoi
.endproc

;*******************************************************************************
; SETERR
; Sets the drive error to the given string.
; This is used to write messages to the drive error buffer directly.
; IN:
;   - .XY: the address of the 0-terminated string to write to the error buffer
.proc seterr
@err=r0
	stxy @err
	ldy #$00
:	lda (@err),y
	sta mem::drive_err,y
	beq @done
	iny
	cpy #LINESIZE
	bcc :-

	lda #$00
	sta mem::drive_err,y		; truncate 0-terminate message

@done:	sec
	rts
.endproc
