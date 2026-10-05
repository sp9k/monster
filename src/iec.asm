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
;  - .C:             set if the drive's code could not be read
;  - mem::drive_err: the drive error message
.proc readerr
@ch=rf
	jsr krn::clrchn
	lda #$00
	sta @ch
	sta zp::io_status
	lda zp::device
	jsr krn::talk
	lda #$6f		; command channel, without opening or closing files
	jsr krn::tksa
	jsr krn::readst
	beq @ok
	jsr krn::untlk
	ldxy #strings::device_not_present
	jmp seterr

@ok:

	; read the error message to mem::drive_err
@loop:	jsr krn::readst		; READST (read status byte)
	bne @eof		; either EOF or read error

	jsr krn::acptr		; receive command-channel data
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

@done:	jsr krn::untlk
	ldxy #mem::drive_err
	jmp atoi
.endproc

;*******************************************************************************
; SETERR
; Sets the drive error to the given string.
; This is used to write messages to the drive error buffer directly when the
; drive itself couldn't be queried
; IN:
;   - .XY: the address of the 0-terminated string to write to the error buffer
; OUT:
;   - .A: ERR_DRIVE_DID_NOT_RESPOND
;   - .C: set
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

@done:	lda #ERR_DRIVE_DID_NOT_RESPOND
	sec
	rts
.endproc
