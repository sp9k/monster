;******************************************************************************
; ERRLOG.ASM
; This file contains the code for the error window, which displays a log of
; errors to the user. It is activated by the editor when an error occurs and
; is closed by the editor when the errors have all been addressed or when the
; user closes it.
;******************************************************************************

.include "asm.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "edit.inc"
.include "errors.inc"
.include "gui.inc"
.include "guis.inc"
.include "key.inc"
.include "keycodes.inc"
.include "log.inc"
.include "macros.inc"
.include "screen.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
MAX_ERRORS = 8
MAX_HEIGHT = 4

.BSS
errcodes:   .res MAX_ERRORS
errlineslo: .res MAX_ERRORS
errlineshi: .res MAX_ERRORS
errfileids: .res MAX_ERRORS

.export __errlog_numerrs
__errlog_numerrs:
numerrs: .byte 0

.CODE

;******************************************************************************
; ACTIVATE
; Displays the error window and resizes the editor to fit it.
; IN:
;  - .A: the row to base the window at (grows upward)
.export __errlog_activate
.proc __errlog_activate
	ldxy #@menu
	jmp gui::listmenu

.PUSHSEG
.RODATA
@menu:
.byte GUI_ERRLOG	; id for errlog
.byte MAX_HEIGHT	; max height
.word @keyhandler	; key handler
.word @getline		; get line handler
.word numerrs		; pointer to number of errors
.word strings::errors	; title
.POPSEG

;--------------------------------------
; callback to get the item in .A
@getline:
	cmp numerrs
	bcc render_error
	rts			; out of range

;--------------------------------------
; callback to handle keypress
@keyhandler:
	cmp #K_RETURN
	beq :+
@ret:	clc			; flag to stay in menu
	rts

:	txa
	pha			; save index

	lda errfileids,x
	jsr dbg::loadfile	; load the file containing the error
	bcs :+			; if failed to load file -> continue

	pla			; restore index
	tax

	ldy errlineshi,x
	lda errlineslo,x
	tax
	cmpw #0
	beq @ret
	jsr edit::gotoline	; go to the line # corresponding to the error
	sec			; flag to exit menu
:	rts
.endproc

;******************************************************************************
; CLEAR
; Removes all errors and closes the error window (if it's active)
.export __errlog_clear
.proc __errlog_clear
	lda #$00
	sta numerrs
	rts
.endproc

;******************************************************************************
; LOG
; Adds the given error to the error log.  The current source line number and
; file is mapped to it if applicable
; IN:
;  - .A:  the error code
; OUT:
;  - .C: set if MAX_ERRORS have been logged since log was cleared
;        or if the error we provided is considered fatal
.export __errlog_log
.proc __errlog_log
	ldx numerrs
	cpx #MAX_ERRORS-1
	bcs @done		; if already at max errors, return with .C set

	pha
	sta errcodes,x

	; map the line # and file ID
	lda asm::linenum
	sta errlineslo,x
	lda asm::linenum+1
	sta errlineshi,x
	lda dbgi::file
	sta errfileids,x
	inc numerrs

	pla
	ldx #@num_fatal_errors-1
@isfatal:
	cmp @fatal_errors,x
	beq @done		; fatal -> exit (with .C set)
	dex
	bpl @isfatal

	clc			; not fatal
@done:	php

	; log the error to the log file too
	;jsr render_error
	;jsr log::out

	plp
	rts

.PUSHSEG
.RODATA
@fatal_errors:
	.byte ERR_NO_ORIGIN
@num_fatal_errors=*-@fatal_errors
.POPSEG
.endproc

;******************************************************************************
; RENDER ERROR
; Renders the given error for display
; IN:
;   - .A: index of the error to get
; OUT:
;   - .XY: address to the rendered error message (file + line #)
.proc render_error
@err=ra
	sta @err
	tax

	lda errcodes,x
	jsr err::get

	; push error string
	tya
	pha
	txa
	pha

	; push line #
	ldx @err
	lda errlineslo,x
	pha
	lda errlineshi,x
	pha

	; get the filename and push it
	lda errfileids,x
	jsr dbgi::get_filename
	bcc :+
	ldxy #strings::question_marks
:	tya
	pha
	txa
	pha

	ldxy #strings::edit_line_err
	jsr text::render
	rts
.endproc

;******************************************************************************
; NEXT
; Returns the line number of the next error after the current source line
; OUT:
;   - .XY: the line number of the next errror
;   - .Z:  set if no next error
.export __errlog_next
.proc __errlog_next
@min=r0
@found=r2
@matchfile=r3
@fileid=r4
	ldxy #$ffff
	stxy @min
	stx @matchfile

	jsr edit::currentfile
	sta @fileid

	ldx #$00
	stx @found

@l0:	; prioritize lines in the same file
	lda @matchfile
	beq :+
	lda errfileids,x
	cmp @fileid
	bne @next		; file doesn't match

:	lda src::line+1
	cmp errlineshi,x
	beq @chklsb
	bcc @chkmin
	bcs @next

@chklsb:
	lda src::line
	cmp errlineslo,x
	bcs @next

@chkmin:
	; check if this line is < our current min
	ldy errlineshi,x
	cpy @min+1
	beq :+
	bcs @next
:	lda errlineslo,x
	cmp @min
	bcs @next
	; set min = errlines,x
	sta @min
	sty @min+1
	inc @found

@next:	inx
	cpx numerrs
	bcc @l0

	lda @found		; was a next line found?
	bne @done
	lda @matchfile		; did we already search without matching file?
	beq @done		; if so, we're done

	inc @matchfile		; try again but don't match file
	ldx #$00
	beq @l0			; branch always

@done:	ldxy @min
	lda @found		; clear .Z if found
	rts
.endproc
