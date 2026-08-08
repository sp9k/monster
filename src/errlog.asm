;*******************************************************************************
; ERRLOG.ASM
; This file contains the code for the error window, which displays a log of
; errors to the user. It is activated by the editor when an error occurs and
; is closed by the editor when the errors have all been addressed or when the
; user closes it.
;*******************************************************************************

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
.include "ram.inc"
.include "screen.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
MAX_ERRORS = 8
MAX_HEIGHT = 4

;*******************************************************************************
; ERRORS
; Parallel arrays that make up the error log.  Each error is comprised of a
; line number (LSB/MSB), the source file it belongs to, and an error code.
.ifdef ultimem
.segment "SHAREBSS2"
.else
.BSS
.endif
errcodes:   .res MAX_ERRORS
errlineslo: .res MAX_ERRORS
errlineshi: .res MAX_ERRORS
errfileids: .res MAX_ERRORS

.export __errlog_numerrs
__errlog_numerrs:
numerrs: .byte 0

;*******************************************************************************
; SHIFT ERRORS D
; Shifts the line numbers for all errors on lines at or below the current one
; DOWN by the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift within
.ifdef ultimem
.segment "DEBUGGER"
.else
.CODE
.endif
.export __errlog_shift_errorsd
.proc __errlog_shift_errorsd
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx numerrs
	beq @done
	dex

@l0:	lda @fileid
	cmp errfileids,x
	bne @next
	lda errlineshi,x
	cmp @line+1
	bcc @next
	lda errlineslo,x
	adc #$00
	cmp @line
	bcc @next
	sbc #$01
	clc
	adc @offset
	sta errlineslo,x
	bcc @next
	inc errlineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; SHIFT ERRORS U
; Shifts UP the line numbers for all errors on lines below the current one by
; the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift within
.export __errlog_shift_errorsu
.proc __errlog_shift_errorsu
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx numerrs
	beq @done
	dex

@l0:	lda @fileid
	cmp errfileids,x
	bne @next
	lda errlineshi,x
	cmp @line+1
	bcc @next
	lda errlineslo,x
	cmp @line
	beq @next
	bcc @next
	sec
	sbc @offset
	sta errlineslo,x
	bcs @next
	dec errlineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; GETBYLINE
; Returns whether an error is logged at the given line, in the given file.
; IN:
;  - .XY: line # to look up
;  - .A:  file ID of the line
; OUT:
;  - .A: error code logged at the given line (if one exists)
;  - .X: index of the error at the given line (if one exists)
;  - .C: set if there is no error at the given line
.export __errlog_getbyline
.proc __errlog_getbyline
@line=r2
@file=r4
	stxy @line
	sta @file

	ldx numerrs
	beq @notfound
	dex
@l0:	lda @file
	cmp errfileids,x
	bne @next
	lda @line
	cmp errlineslo,x
	bne @next
	lda @line+1
	cmp errlineshi,x
	bne @next
	lda errcodes,x
	RETURN_OK		; found -> .C clear, .A = error code

@next:	dex
	bpl @l0
@notfound:
	sec			; no error on this line
	rts
.endproc

.CODE
;*******************************************************************************
; MAIN-bank entry points
.export __errlog_activate
.export __errlog_clear
.export __errlog_log
.export __errlog_next

.if .defined(CART) .and .defined(c64)
__errlog_activate: JUMP FINAL_BANK_DBGUI, activate
__errlog_clear:    JUMP FINAL_BANK_DBGUI, clear
__errlog_log:      JUMP FINAL_BANK_DBGUI, logerr
__errlog_next:     JUMP FINAL_BANK_DBGUI, next
keyhandler_vec:    JUMP FINAL_BANK_DBGUI, keyhandler
getline_vec:       JUMP FINAL_BANK_DBGUI, getline
.else
__errlog_activate = activate
__errlog_clear    = clear
__errlog_log      = logerr
__errlog_next     = next
keyhandler_vec    = keyhandler
getline_vec       = getline
.endif

.PUSHSEG
.RODATA
;*******************************************************************************
; MENU
; The window descriptor for the error log (read by the window manager from
; the MAIN bank)
menu:
.byte GUI_ERRLOG	; id for errlog
.byte GUI_CLASS_LIST
.byte MAX_HEIGHT	; initial height
.byte 1			; min height
.byte 12		; max height
.word strings::errors	; title
.word keyhandler_vec	; key handler
.word getline_vec	; get line handler
.word numerrs		; pointer to number of errors
.POPSEG

BANKED_CODE "DBGUI", FINAL_BANK_DBGUI

;*******************************************************************************
; ACTIVATE
; Displays the error window and resizes the editor to fit it.
.proc activate
	ldxy #menu
	JUMPMAIN gui::open
.endproc

;*******************************************************************************
; GETLINE
; callback to get the item in .A
getline:
	cmp numerrs
	bcc render_error
	rts			; out of range

;*******************************************************************************
; KEYHANDLER
; callback to handle keypress
.proc keyhandler
	cmp #K_RETURN
	beq :+
@ret:	clc			; flag to stay in menu
	rts

:	txa
	pha			; save index

	lda errfileids,x
	jsr dbg::loadfile	; load the file containing the error
	pla			; restore index
	bcs @ret		; if failed to load file -> continue

	tax

	ldy errlineshi,x
	lda errlineslo,x
	tax
	cmpw #0
	beq @ret
	; edit::gotoline lives in MIDRAM (unreadable from this banked context)
	CALLMAIN edit::gotoline	; go to the line # corresponding to the error

	; update saved cursor to new position after gotoline
	lda zp::curx
	sta gui::cursave_x
	lda zp::cury
	sta gui::cursave_y

	lda #GUI_RET_QUIT
	sec			; flag to exit menu
	rts
.endproc

;*******************************************************************************
; CLEAR
; Removes all errors and closes the error window (if it's active)
.proc clear
	lda #$00
	sta numerrs
	rts
.endproc

;*******************************************************************************
; LOG
; Adds the given error to the error log.  The current source line number and
; file is mapped to it if applicable
; IN:
;  - .A:  the error code
; OUT:
;  - .C: set if MAX_ERRORS have been logged since log was cleared
;        or if the error we provided is considered fatal
.proc logerr
	ldx numerrs
	cpx #MAX_ERRORS
	bcs @ret		; if already at max errors, return with .C set

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
	lda numerrs
	sec
	sbc #$01		; render_error takes the log INDEX, not the code
	jsr render_error
	jsr log::out

	plp
@ret:	rts

@fatal_errors:
	.byte ERR_NO_ORIGIN
@num_fatal_errors=*-@fatal_errors
.endproc

;*******************************************************************************
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
	CALLMAIN dbgi::get_filename
	bcc :+
	ldxy #strings::question_marks
:	tya
	pha
	txa
	pha

	ldxy #strings::edit_line_err
	RENDER_STR
	rts
.endproc

;*******************************************************************************
; NEXT
; Returns the line number of the next error after the current source line
; OUT:
;   - .XY: the line number of the next errror
;   - .Z:  set if no next error
.proc next
@min=r0
@found=r2
@matchfile=r3
@fileid=r4
	; edit::currentfile lives in MIDRAM (unreadable from this banked context)
	CALLMAIN edit::currentfile
	sta @fileid

	ldxy #$ffff
	stxy @min
	stx @matchfile

	ldx #$00
	stx @found
	lda numerrs		; any errors logged at all?
	beq @done		; if not, we're done

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
