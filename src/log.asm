;******************************************************************************
; LOG.ASM
; This file contains the code for a general purpose logging.
; Log files are stored to disk
;******************************************************************************

.include "file.inc"
.include "kernal.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
.BSS
file_id: .byte 0

.CODE

;******************************************************************************
; NEW
; Begins a new log by opening a log file (log) and, if necessary, deleting the
; existing one
.export __log_new
.proc __log_new
	ldxy #@filename
	jsr file::open_w
	sta file_id
	rts

.PUSHSEG
.RODATA
@filename: .byte "log",0
.POPSEG
.endproc

;******************************************************************************
; OUT
; Writes the 0-terminated string to the open log file
; IN:
;   - .XY: the string to write
.export __log_out
.proc __log_out
@str=zp::util
	stxy @str
	ldx file_id
	jsr krn::chkout

	ldy #$00
@l0:	lda (@str),y
	beq @done
	jsr krn::ciout
	iny
	cpy #40
	bcc @l0

@done:	; emit a newline
	lda #$0d
	jmp krn::ciout
.endproc

;******************************************************************************
; CLOSE
; Closes the log file that was created with log::new.
.export __log_close
.proc __log_close
	lda file_id
	jmp file::close
.endproc
