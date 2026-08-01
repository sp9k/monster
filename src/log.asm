;*******************************************************************************
; LOG.ASM
; This file contains the code for a general purpose logging.
; Log files are stored to disk
;*******************************************************************************

.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "source.inc"
.include "text.inc"
.include "zeropage.inc"

;*******************************************************************************
.BSS
file_id: .byte 0

.export __log_written
__log_written: .byte 0

.CODE
;*******************************************************************************
; MAIN-bank entry points
.export __log_new
.export __log_out
.export __log_banner
.export __log_close

.if .defined(CART) .and .defined(c64)
; the log routines manipulate source-buffer state and dereference caller
; string pointers that may only be visible in the MAIN context, so they must
; run at $01=$34: the thunks far-call to MAIN and the code lives in MIDRAM
__log_new:    JUMP FINAL_BANK_MAIN, lognew
__log_out:    JUMP FINAL_BANK_MAIN, logout
__log_banner: JUMP FINAL_BANK_MAIN, logbanner
__log_close:  JUMP FINAL_BANK_MAIN, logclose
.else
__log_new    = lognew
__log_out    = logout
__log_banner = logbanner
__log_close  = logclose
.endif

.segment "GUICODE"

;*******************************************************************************
; NEW
; Begins a new log by opening a log file (log) and, if necessary, deleting the
; existing one
; OUT:
;   - .C: set on error
;   - .A: error code (on error)
.proc lognew
	lda #$01
	sta __log_written
	jmp src::new_log
.endproc

;*******************************************************************************
; OUT
; Writes the 0-terminated string to the open log file
; IN:
;   - .XY: the string to write
.proc logout
@msg=r0
	lda __log_written	; was a log ever created?
	beq @done		; if not, do nothing
	stxy @msg
	lda src::activebuff
	pha

	; switch to the log buffer temporarily and write the line
	lda #LOG_BUFFER
	jsr src::forceset
	ldxy @msg
	jsr src::insertline
	lda #$0d
	jsr src::insert

	pla
	jmp src::setbuff	; return to buffer we started on
@done:	rts
.endproc

;*******************************************************************************
; WRITE BANNER
; Writes a banner to the log
.proc logbanner
@cnt=zp::util
	lda __log_written	; was a log ever created?
	bne :+			; if so, continue
	rts			; if not, do nothing

:	lda src::activebuff
	pha

	lda #LOG_BUFFER
	jsr src::forceset

	lda #LINESIZE
	sta @cnt

:	lda #'*'
	jsr src::insert
	dec @cnt
	bne :-
	lda #$0d
	jsr src::insert

	pla
	jmp src::setbuff	; return to buffer we started on
.endproc

;*******************************************************************************
; CLOSE
; Closes the log file that was created with log::new.
.proc logclose
	lda __log_written	; was a log ever created?
	beq @done		; if not, do nothing
	lda src::activebuff
	pha

	lda #LOG_BUFFER
	jsr src::forceset
	jsr src::rewind

	pla
	jmp src::setbuff
@done:	rts
.endproc
