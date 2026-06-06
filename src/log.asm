;******************************************************************************
; LOG.ASM
; This file contains the code for a general purpose logging.
; Log files are stored to disk
;******************************************************************************

.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
.BSS
file_id: .byte 0

.export __log_written
__log_written: .byte 0

.CODE

;******************************************************************************
; NEW
; Begins a new log by opening a log file (log) and, if necessary, deleting the
; existing one
; OUT:
;   - .C: set on error
;   - .A: error code (on error)
.export __log_new
.proc __log_new
	lda #$01
	sta __log_written
	jmp src::new_log
.endproc

;******************************************************************************
; OUT
; Writes the 0-terminated string to the open log file
; IN:
;   - .XY: the string to write
.export __log_out
.proc __log_out
@msg=r0
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
.endproc

;******************************************************************************
; WRITE BANNER
; Writes a banner to the log
.export __log_banner
.proc __log_banner
@cnt=r0
	lda src::activebuff
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

;******************************************************************************
; CLOSE
; Closes the log file that was created with log::new.
.export __log_close
.proc __log_close
	lda src::activebuff
	pha

	lda #LOG_BUFFER
	jsr src::forceset
	jsr src::rewind

	pla
	jmp src::setbuff
.endproc
