;*******************************************************************************
; ERRLOG.ASM
; This file contains the code for the error window, which displays a log of
; errors to the user. It is activated by the editor when an error occurs and
; is closed by the editor when the errors have all been addressed or when the
; user closes it.
;*******************************************************************************

.include "asm.inc"
.include "beep.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "errlogconst.inc"
.include "errors.inc"
.include "format.inc"
.include "gui.inc"
.include "guis.inc"
.include "key.inc"
.include "keycodes.inc"
.include "log.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "screen.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.ifdef vic20
CUR_BANK .set FINAL_BANK_MAIN
.endif

;*******************************************************************************
; CONSTANTS
ASM_MAX_ERRORS = 8
MAX_ERRORS     = 16
MAX_DISMISSED  = 64
LIVE_BUFFER    = $80
MAX_HEIGHT     = 4

; Short names for the constants shared with errlog.inc
SPLIT_LINE  = ERRLOG_SPLIT_LINE
BLANK_ABOVE = ERRLOG_BLANK_ABOVE
BLANK_BELOW = ERRLOG_BLANK_BELOW
JOIN_LINES   = ERRLOG_JOIN_LINES
DELETE_ABOVE = ERRLOG_DELETE_ABOVE

NAV_NONE            = ERRLOG_NAV_NONE
NAV_CHECK_ON_LEAVE  = ERRLOG_NAV_CHECK_ON_LEAVE
NAV_CHECK_ON_RETURN = ERRLOG_NAV_CHECK_ON_RETURN

;*******************************************************************************
; ERRORS
; Parallel arrays that make up the error log.  Each error is comprised of a
; line number (LSB/MSB), an owner, and an error code. Owners below $80
; are assembly file IDs; $80+buffer identifies a live syntax error.
; Source close compacts these buffer IDs along with the source table.
.if .defined(ultimem) .or .defined(fe3)
.segment "SHAREBSS2"
.else
.BSS
.endif
; These values are read directly by the editor, source and GUI code.
.export __errlog_numerrs
__errlog_numerrs:
numerrs: .byte 0
.export __errlog_asmerrors
__errlog_asmerrors: .byte 0	; assembly error count, independent of visible entries

; Source position before an editor key. Source mutations cancel this snapshot;
; only navigation (or command-mode RETURN) may validate the line being left.
.export __errlog_navpending
__errlog_navpending: .byte NAV_NONE

; flags if current line needs validation when navigated from
.export __errlog_editpending
__errlog_editpending: .byte 0

.if .defined(ultimem) .or .defined(fe3)
.segment "ERRLOG_BSS"
.endif
errcodes:   .res MAX_ERRORS
errlineslo: .res MAX_ERRORS
errlineshi: .res MAX_ERRORS
errfileids: .res MAX_ERRORS

navbuffer:           .byte 0
navline:             .word 0
navpos:              .word 0
navrow:              .byte 0
navkey:              .byte 0 ; the key before_key is deciding about
navdest:             .byte 0 ; buffer after_key must return the editor to
livechanged:         .byte 0
insertmode:          .byte 0 ; scoped to the editor's blank-line insertion
deletemode:          .byte 0 ; scoped to the editor's whole-line deletion

; Dismissal belongs to a source line, independently of its current error
; code. Keep it through edits until RETURN explicitly checks the line again.
numdismissed:  .byte 0
dismisslo:     .res MAX_DISMISSED
dismisshi:     .res MAX_DISMISSED
dismissowners: .res MAX_DISMISSED

;*******************************************************************************
; SHIFT ERRORS D
; Shifts the line numbers for all errors on lines at or below the current one
; DOWN by the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift within
.if .defined(ultimem) .or .defined(fe3)
BANKED_SEG "ERRLOG_CODE", FINAL_BANK_ERRLOG
CUR_BANK .set FINAL_BANK_ERRLOG
.else
.CODE
.endif
.export __errlog_shift_errorsd
.if (.defined(ultimem) .or .defined(fe3)) = 0
__errlog_shift_errorsd = shift_errorsd
.endif
.proc shift_errorsd
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
	bne @shift
	lda errlineslo,x
	cmp @line
	bcc @next
@shift:	lda errlineslo,x
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
.if (.defined(ultimem) .or .defined(fe3)) = 0
__errlog_shift_errorsu = shift_errorsu
.endif
.proc shift_errorsu
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
	bne @shift
	lda errlineslo,x
	cmp @line
	beq @next
	bcc @next
@shift:	lda errlineslo,x
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
.if (.defined(ultimem) .or .defined(fe3)) = 0
__errlog_getbyline = getbyline
.endif
.proc getbyline
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
.if .defined(ultimem) .or .defined(fe3)
CUR_BANK .set FINAL_BANK_MAIN
.endif
;*******************************************************************************
; MAIN-bank entry points
.export __errlog_activate
.export __errlog_clear
.export __errlog_reset
.export __errlog_log
.export __errlog_next
.export __errlog_after_key
.export __errlog_before_key
.export __errlog_show
.export __errlog_check_line
.export __errlog_refresh
.export __errlog_close_buffer
.export __errlog_deleted
.export __errlog_delete_linebreak
.export __errlog_dismiss
.export __errlog_undismiss
.export __errlog_inserted
.export __errlog_insertion_mode
.export __errlog_get_curent
.export __errlog_set_live

.if .defined(ultimem) .or .defined(fe3)
ERRLOG_BANK = FINAL_BANK_ERRLOG
__errlog_shift_errorsd: JUMP ERRLOG_BANK, shift_errorsd
__errlog_shift_errorsu: JUMP ERRLOG_BANK, shift_errorsu
__errlog_getbyline:     JUMP ERRLOG_BANK, getbyline
.else
ERRLOG_BANK = FINAL_BANK_DBGUI
.endif

.if .defined(ultimem) .or .defined(fe3) .or (.defined(CART) .and .defined(c64))
__errlog_activate:      JUMP ERRLOG_BANK, activate
__errlog_clear:         JUMP ERRLOG_BANK, clear
__errlog_reset:         JUMP ERRLOG_BANK, reset
__errlog_log:           JUMP ERRLOG_BANK, logerr
__errlog_next:          JUMP ERRLOG_BANK, next
__errlog_set_live:       JUMP ERRLOG_BANK, set_live
__errlog_get_curent:    JUMP ERRLOG_BANK, get_curent
__errlog_inserted:      JUMP ERRLOG_BANK, inserted
__errlog_insertion_mode: JUMP ERRLOG_BANK, insertion_mode
__errlog_deleted:       JUMP ERRLOG_BANK, deleted
__errlog_delete_linebreak: JUMP ERRLOG_BANK, delete_linebreak
__errlog_dismiss:       JUMP ERRLOG_BANK, dismiss_current
__errlog_undismiss:     JUMP ERRLOG_BANK, undismiss_current
__errlog_close_buffer:  JUMP ERRLOG_BANK, close_buffer
__errlog_refresh:       JUMP ERRLOG_BANK, refresh
__errlog_check_line:    JUMP ERRLOG_BANK, check_line
__errlog_show:          JUMP ERRLOG_BANK, show
__errlog_before_key:    JUMP ERRLOG_BANK, before_key
__errlog_after_key:     JUMP ERRLOG_BANK, after_key
keyhandler_vec:         JUMP ERRLOG_BANK, keyhandler
getline_vec:            JUMP ERRLOG_BANK, getline
.else
__errlog_activate       = activate
__errlog_clear          = clear
__errlog_reset          = reset
__errlog_log            = logerr
__errlog_next           = next
__errlog_set_live        = set_live
__errlog_get_curent     = get_curent
__errlog_inserted       = inserted
__errlog_insertion_mode = insertion_mode
__errlog_deleted        = deleted
__errlog_delete_linebreak = delete_linebreak
__errlog_dismiss        = dismiss_current
__errlog_undismiss      = undismiss_current
__errlog_close_buffer   = close_buffer
__errlog_refresh        = refresh
__errlog_check_line     = check_line
__errlog_show           = show
__errlog_before_key     = before_key
__errlog_after_key      = after_key
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

.if .defined(ultimem) .or .defined(fe3)
; The error log has its own ROM and three RAM blocks. Editor, source and
; rendering calls cross back through their MAIN entries.
BANKED_SEG "ERRLOG_CODE", FINAL_BANK_ERRLOG
CUR_BANK .set FINAL_BANK_ERRLOG
.else
BANKED_CODE "DBGUI", FINAL_BANK_DBGUI
.endif

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
	bcs :+
	jmp render_error
:	rts			; out of range

;*******************************************************************************
; KEYHANDLER
; callback to handle keypress
.proc keyhandler
	cmp #K_DEL
	beq @dismiss
	cmp #K_DISMISS_ERR
	beq @dismiss
	cmp #K_RETURN
	beq :+
@ret:	clc			; flag to stay in menu
	rts

:	txa
	pha			; save index

	jsr load_owner		; open the buffer/file holding the error
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
@dismiss:
	cpx numerrs
	bcs @ret

	jsr dismiss_selected	; dismiss the selected error
	bcs @ret

	CALLMAIN edit::refresh
	lda numerrs		; any errors left?
	bne @ret		; if so, return

	; no errors left, flag to close window
	lda #GUI_RET_CLOSE
	sec
	rts
.endproc

;*******************************************************************************
; RESET
; Removes all errors and dismissals, and cancels pending validation.
.proc reset
	lda #0
	sta numdismissed
	; fall through to clear
.endproc

;*******************************************************************************
; CLEAR
; Removes all errors and cancels pending validation. Dismissals are preserved.
.proc clear
	lda #NAV_NONE
	sta numerrs
	sta __errlog_asmerrors
	sta __errlog_navpending
	sta __errlog_editpending
	sta insertmode
	sta deletemode
	rts
.endproc

;*******************************************************************************
; LOG
; Adds the given error to the error log.  The current source line number and
; file is mapped to it if applicable
; IN:
;  - .A:  the error code
; OUT:
;  - .C: set if ASM_MAX_ERRORS have been logged since log was cleared
;        or if the error we provided is considered fatal
.proc logerr
	ldx __errlog_asmerrors
	cpx #ASM_MAX_ERRORS
	bcc :+
	rts
:
	ldx numerrs
	cpx #MAX_ERRORS
	bcc :+
	rts
:
	inc __errlog_asmerrors

	pha
	sta errcodes,x

	; map the line # and file ID
	lda asm::linenum
	sta errlineslo,x
	lda asm::linenum+1
	tay
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
	CALLMAIN log::out

	CALLMAIN edit::currentfile
	bcs @result
	cmp dbgi::file
	bne @result
	lda asm::linenum
	sec
	sbc edit::base
	tax
	lda asm::linenum+1
	sbc edit::base+1
	bne @result
	cpx edit::height
	bcs @result
	lda #COLOR_ERROR
	CALLMAIN draw::hline
@result:
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
	CALLMAIN err::get

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
	bpl @file
	and #$7f
	CALLMAIN src::filename	; also returns [NO NAME] for unnamed buffers
	jmp @name
@file:	CALLMAIN dbgi::get_filename
	bcc @name
	ldxy #strings::question_marks
@name:	tya
	pha
	txa
	pha

	ldxy #strings::edit_line_err
	RENDER_STR
	rts
.endproc

;*******************************************************************************
; NEXT
; Returns the next error line in the active buffer, wrapping at EOF
; OUT:
;  - .XY: the next error line (if one exists)
;  - .Z: set if the active buffer has no mapped errors
.proc next
@min=r6
@first=r8
@found=ra
	jsr current_owners
	ldxy #$ffff
	stxy @min
	stxy @first
	lda #0
	sta @found
	ldx numerrs
	beq @done
	dex

@loop:	jsr matches_owner
	bne @next
	lda #1
	sta @found
	lda errlineshi,x
	cmp @first+1
	bcc @savefirst
	bne @after
	lda errlineslo,x
	cmp @first
	bcs @after

@savefirst:
	lda errlineslo,x
	sta @first
	lda errlineshi,x
	sta @first+1

@after:	lda errlineshi,x
	cmp src::line+1
	bcc @next
	bne @candidate
	lda errlineslo,x
	cmp src::line
	bcc @next
	beq @next

@candidate:
	lda errlineshi,x
	cmp @min+1
	bcc @take
	bne @next
	lda errlineslo,x
	cmp @min
	bcs @next

@take:	lda errlineslo,x
	sta @min
	lda errlineshi,x
	sta @min+1

@next:	dex
	bpl @loop
	ldxy @min
	cmpw #$ffff
	bne @result
	ldxy @first

@result:
	lda @found
@done:	rts
.endproc

;*******************************************************************************
; LOAD OWNER
; Loads the buffer or file containing the error. Live buffers need no debug
; file table.
; IN:
;  - .X: index of the error
; OUT:
;  - .C: set if the buffer or file could not be loaded
.proc load_owner
	lda errfileids,x
	bpl @file
	and #$7f
	CALLMAIN src::setbuff
	lda zp::cury
	cmp edit::height
	bcc :+
	lda edit::height
	sta zp::cury
:	CALLMAIN edit::refresh
	clc
	rts
@file:	JUMPMAIN dbg::loadfile
.endproc

;*******************************************************************************
; CURRENT OWNERS
; Cache both identities of the active buffer. $ff is never a valid file ID.
; IN:
;  - src::activebuff: the buffer whose identities to look up
; OUT:
;  - r4: live owner ID ($80 + buffer number)
;  - r5: debug file ID ($ff if the buffer has no file ID)
; CLOBBERS:
;  - r0-r3, .AXY
.proc current_owners
@ownerid=r4
@fileid=r5
	CALLMAIN edit::currentfile
	bcc :+
	lda #$ff
:	sta @fileid
	lda src::activebuff
	ora #LIVE_BUFFER
	sta @ownerid
	rts
.endproc

;*******************************************************************************
; MATCHES OWNER
; Checks whether the error belongs to either cached owner.
; IN:
;  - .X: index of the error
;  - r4: primary owner ID
;  - r5: alternate debug file ID ($ff if none)
; OUT:
;  - .Z: set if the error belongs to either owner
;  - .XY: unchanged
.proc matches_owner
@ownerid=r4
@fileid=r5
	lda errfileids,x	; get error's file ID
	cmp @ownerid		; does it match the "owner"'s?
	beq @done		; yes -> done
	cmp #LIVE_BUFFER	; is this a source (non-asm) error?
	bcs @different		; if so, and it didn't match, we're done
	cmp @fileid		; if it's a file ID, compare against that
	rts

@different:
	lda #1			; no match, including the invalid $ff owner
@done:	rts
.endproc

;*******************************************************************************
; GET CURRENT
; Query mapped errors at the current source line, including unnamed buffers.
; IN:
;  - src::activebuff: the buffer to check
;  - src::line: the source line to check
; OUT:
;  - .A: error code (if one exists)
;  - .X: index of the error (if one exists)
;  - .C: set if the line has no mapped error
.proc get_curent
	jsr current_owners
	ldx numerrs
	beq @missing
	dex
@loop:	jsr matches_owner
	bne @next
	lda errlineslo,x
	cmp src::line
	bne @next
	lda errlineshi,x
	cmp src::line+1
	bne @next
	lda errcodes,x
	clc
	rts
@next:	dex
	bpl @loop
@missing:
	sec
	rts
.endproc

;*******************************************************************************
; REMOVE
; Removes an error entry, retaining the order of the remaining entries.
; IN:
;  - .X: index of the error to remove
; OUT:
;  - .X: unchanged
;  - r0-rf: unchanged
.proc remove
	txa
	pha
@loop:	inx
	cpx numerrs
	bcs @done
	lda errcodes,x
	sta errcodes-1,x
	lda errlineslo,x
	sta errlineslo-1,x
	lda errlineshi,x
	sta errlineshi-1,x
	lda errfileids,x
	sta errfileids-1,x
	jmp @loop
@done:	dec numerrs
	pla
	tax
	rts
.endproc

;*******************************************************************************
; MATCHES DISMISSED
; Match a dismissal to either cached identity of a buffer.
; IN:
;  - .X: index of the dismissal
;  - r4: primary owner ID
;  - r5: alternate debug file ID ($ff if none)
; OUT:
;  - .Z: set if the dismissal belongs to either owner
;  - .XY: unchanged
.proc matches_dismissed
@ownerid=r4
@fileid=r5
	lda dismissowners,x
	cmp @ownerid
	beq @done
	cmp #LIVE_BUFFER
	bcs @different
	cmp @fileid
	rts
@different:
	lda #1
@done:	rts
.endproc

;*******************************************************************************
; FIND DISMISSED
; Finds a dismissal for the given line and either owner.
; IN:
;  - r0-r1: source line to look up
;  - r4: primary owner ID
;  - r5: alternate debug file ID ($ff if none)
; OUT:
;  - .X: index of the dismissal (if one exists)
;  - .C: set if no matching dismissal exists
.proc find_dismissed
@line=r0
	ldx numdismissed
	beq @missing

	dex
@loop:	jsr matches_dismissed
	bne @next
	lda dismisslo,x
	cmp @line
	bne @next
	lda dismisshi,x
	cmp @line+1
	bne @next
	clc
	rts
@next:	dex
	bpl @loop

@missing:
	sec
	rts
.endproc

;*******************************************************************************
; REMOVE DISMISSED
; Removes a "dismissal"
; IN:
;  - .X: index of the dismissal to remove
; OUT:
;  - .X: unchanged
;  - r0-rf: unchanged
.proc remove_dismissed
	txa
	pha

@loop:	inx
	cpx numdismissed
	bcs @done
	lda dismisslo,x
	sta dismisslo-1,x
	lda dismisshi,x
	sta dismisshi-1,x
	lda dismissowners,x
	sta dismissowners-1,x
	jmp @loop
@done:	dec numdismissed
	pla
	tax
	rts
.endproc

;*******************************************************************************
; DISMISS SELECTED
; Remove one visible entry and saves it as a "dismissal" so that navigating back
; to the line does not redraw the dismissed error.
; IN:
;  - .X: index of the error to dismiss
; OUT:
;  - .C: set if the dismissal table is full (the error stays visible)
.proc dismiss_selected
@line=r0
@ownerid=r4
@fileid=r5
	txa
	pha			; save the error's index

	lda errfileids,x
	bmi @owner		; live error: it already names its buffer

	CALLMAIN dbgi::get_filename
	bcs @byfile
	CALLMAIN src::buffer_by_name
	bcs @byfile
	ora #LIVE_BUFFER
	bmi @owner		; branch always (LIVE_BUFFER is bit 7)

@byfile:
	pla
	pha			; peek at the error's index
	tax
	lda errfileids,x	; no open buffer owns it: keep the file ID

@owner: sta @ownerid
	lda #$ff
	sta @fileid		; no alternate owner ID to match
	pla
	tax			; restore the error's index
	pha
	lda errlineslo,x
	sta @line
	lda errlineshi,x
	sta @line+1
	jsr find_dismissed
	bcc @remove
	ldx numdismissed
	cpx #MAX_DISMISSED
	bcs @full
	lda @line
	sta dismisslo,x
	lda @line+1
	sta dismisshi,x
	lda @ownerid
	sta dismissowners,x
	inc numdismissed

@remove:
	pla
	tax
	jsr remove
	clc
	rts
@full:	pla
	CALLMAIN beep::short
	sec
	rts
.endproc

;*******************************************************************************
; DISMISS CURRENT
; Editor shortcut: dismiss all mapped errors at the cursor without moving it.
; IN:
;  - src::activebuff: the buffer containing the mapped errors
;  - src::line: the source line whose mapped errors to dismiss
.proc dismiss_current
@loop:	jsr get_curent
	bcs @done
	jsr dismiss_selected
	bcc @loop
@done:	CALLMAIN edit::redrawline
	jmp refresh
.endproc

;*******************************************************************************
; UNDISMISS CURRENT
; RETURN releases all dismissals for the original line before it is split.
; IN:
;  - src::activebuff: the buffer containing the dismissals
;  - src::line: the original source line
.proc undismiss_current
@line=r0
	lda numdismissed
	beq @done
	jsr current_owners
	ldxy src::line
	stxy @line
@loop:	jsr find_dismissed
	bcs @done
	jsr remove_dismissed
	jmp @loop
@done:	rts
.endproc

;*******************************************************************************
; SET LIVE
; Replaces or removes a line's live error. Assembly errors are not cleared by
; a syntax-only check.
; IN:
;  - .XY: the source line to update
;  - .A: the error code (0 to clear the live error)
;  - r0: the source buffer number
; OUT:
;  - .C: set if a new mapped error did not fit
;  - livechanged: nonzero if a mapped error was added, changed, or removed
.proc set_live
@ownerid=r0
@linehi=r1
@linelo=r2
	pha
	lda #0
	sta livechanged
	txa
	pha
	tya
	pha
	lda @ownerid
	ora #LIVE_BUFFER
	sta @ownerid

.if .defined(ultimem) .or .defined(fe3)
	jsr getbyline
.else
	CALLMAIN __errlog_getbyline
.endif

	bcs @append
	pla
	pla
	pla
	beq @remove
	cmp errcodes,x
	beq @ok
	sta errcodes,x
	inc livechanged
	clc
	rts
@remove:
	jsr remove
	inc livechanged
	clc
	rts

@append:
	ldx numerrs
	pla
	sta @linehi
	pla
	sta @linelo
	pla
	beq @ok
	cpx #MAX_ERRORS
	bcs @done
	sta errcodes,x
	lda @ownerid
	sta errfileids,x
	lda @linelo
	sta errlineslo,x
	lda @linehi
	sta errlineshi,x
	inc numerrs
	inc livechanged
@ok:	clc
@done:	rts
.endproc

;*******************************************************************************
; INSERTION MODE
; The editor scopes this mode around insertion of a blank line. Ordinary
; source edits split text and invalidate mapped errors on the split line.
; IN:
;  - .A: SPLIT_LINE, BLANK_ABOVE, or BLANK_BELOW
.proc insertion_mode
	sta insertmode
	rts
.endproc

;*******************************************************************************
; INSERTED
; Called AFTER src::line advances. Splits invalidate the original line;
; opening above/below preserves it. Shift all subsequent source lines.
; IN:
;  - src::activebuff: the buffer being edited
;  - src::line: the source line after the inserted newline
;  - insertmode: SPLIT_LINE, BLANK_ABOVE, or BLANK_BELOW
; OUT:
;  - insertmode: reset to SPLIT_LINE
.proc inserted
@line=r0
@preserve=r3
	jsr current_owners
	lda insertmode
	sta @preserve		; preserve text when opening a blank line
	lda #SPLIT_LINE
	sta insertmode		; consume the mode before another source edit
	ldxy src::line
	stxy @line
	lda @line
	bne :+
	dec @line+1
:	dec @line		; original line that was split
	lda @preserve
	cmp #BLANK_ABOVE
	bne @shift

	; inserting above shifts the original line too; use the above line as
	; limit
	lda @line
	bne :+
	dec @line+1
:	dec @line
@shift:
	lda #1
	jmp edit_lines
.endproc

;*******************************************************************************
; DELETE LINEBREAK
; Remove the separator after the editor has cleared a line. Preserve
; the unchanged neighbor's errors, and reset the mode even for an empty buffer.
; IN:
;  - .A: DELETE_ABOVE (first line) or DELETE_BELOW (any later line)
.proc delete_linebreak
	sta deletemode
	jsr clear_line_state
	lda deletemode
	cmp #DELETE_ABOVE
	bne @below
	CALLMAIN src::delete
	jmp @done
@below:
	CALLMAIN src::backspace
@done:
	lda #JOIN_LINES
	sta deletemode
	rts
.endproc

;*******************************************************************************
; DELETED
; Invalidates joined text (or just the deleted line for whole-line deletion)
; and shifts later entries up.
; IN:
;  - src::activebuff: the buffer being edited
;  - src::line: the surviving joined line
.proc deleted
@line=r0
@preserve=r3
	jsr current_owners
	lda deletemode
	sta @preserve
	lda #JOIN_LINES
	sta deletemode

	ldxy src::line
	stxy @line
	lda @preserve
	cmp #DELETE_ABOVE
	bne :+
	decw @line
:
	lda #$ff
	; fall through
.endproc

;*******************************************************************************
; EDIT LINES
; Shifts mapped errors, dismissals and breakpoints after an insertion/deletion.
; Invalidate both sides of a join; syntax/assembly results there are stale.
; IN:
;  - .A:    line offset ($01 for insertion, $ff for deletion)
;  - r0-r1: affected line (entries after it are shifted)
;  - r3:    nonzero to preserve mapped errors on the affected line
;  - r4:    primary owner ID
;  - r5:    alternate debug file ID ($ff if none)
.proc edit_lines
@offset=r2
	sta @offset
	jsr edit_annotations
	jmp shift_dismissed
.endproc

;*******************************************************************************
; EDIT ANNOTATIONS
; Shifts and/or removes mapped-errors and breakpoints as needed.
; Other files and address-only breakpoints are unaffected. Scratch inputs are
; as for edit_lines; an offset of zero removes just the affected line's entries.
; IN:
;   - r5: id of file that was edited
; CLOBBERS:
;   - zp::util+3: table index (available throughout source edit callbacks)
.proc edit_annotations
@fileid=r5
@index=zp::util+3	; table index saved before map_edited_line uses .XY
	ldx numerrs
	beq @breakpoints

;-------------------------------------------------------------------------------
	dex
@errors:
	jsr matches_owner
	bne @next_error
	stx @index
	lda errlineslo,x
	ldy errlineshi,x
	tax
	jsr map_edited_line

	txa
	ldx @index		; restore the table index without changing carry
	bcs @remove_error
	sta errlineslo,x
	tya
	sta errlineshi,x
	jmp @next_error
@remove_error:
	jsr remove
@next_error:
	dex
	bpl @errors

;-------------------------------------------------------------------------------
@breakpoints:
	lda @fileid
	cmp #$ff
	beq @done
	ldx dbg::numbreakpoints
	beq @done
	dex

@l0:	lda dbg::breakpoint_fileids,x
	cmp @fileid
	bne @next
	stx @index
	lda dbg::breakpoint_lineslo,x
	ldy dbg::breakpoint_lineshi,x
	tax
	jsr map_edited_line

	txa
	ldx @index
	bcs @remove_breakpoint
	sta dbg::breakpoint_lineslo,x
	tya
	sta dbg::breakpoint_lineshi,x
	jmp @next

@remove_breakpoint:
	CALLMAIN dbg::removebreakpointbyid
	ldx @index		; breakpoint removal leaves this scratch byte intact
@next:	dex
	bpl @l0

@done:	rts
.endproc

;*******************************************************************************
; MAP EDITED LINE
; Invalidates split/joined lines and shifts later lines.
; IN:
;  - .XY:   entry's line number
;  - r0-r1: affected line
;  - r2:    offset ($01=INSERT, $ff=DELETE, $00 clear without shifting)
;  - r3:    nonzero to preserve entries on the affected line
; OUT:
;  - .XY: mapped line number
;  - .C:  set if the entry must be removed
; CLOBBERS:
;  - .A
.proc map_edited_line
@line=r0
@offset=r2
@preserve=r3
	cpy @line+1
	bcc @keep
	bne @later
	cpx @line
	bcc @keep
	bne @later
	lda @preserve
	beq @remove

@keep:	RETURN_OK

@remove:
	sec
	rts

@later: lda @offset
	beq @keep
	bmi @up
	inx
	bne @mapped
	iny

@mapped:
	clc
	rts

@up:	cpx #0
	bne :+
	dey
:	dex
	cpy @line+1
	bne @mapped
	cpx @line
	bne @mapped
	; The second side of a join (or the deleted whole line) shifted onto
	; the affected line. CPX left carry set: discard its entry.
	rts
.endproc

;*******************************************************************************
; SHIFT DISMISSED
; Shifts any "dismissed" errors.
; Unlike mapped errors, "dismissals" need to survive changes to the affected
; line.
; IN:
;  - r0-r1: affected line (entries after it are shifted)
;  - r2:    line offset ($01 for insertion, $ff for deletion)
;  - r4:    primary owner ID
;  - r5:    alternate debug file ID ($ff if none)
.proc shift_dismissed
@line=r0
@offset=r2
	ldx numdismissed
	beq @done

	dex
@loop:	jsr matches_dismissed
	bne @next
	lda dismisshi,x
	cmp @line+1
	bcc @next
	bne @later
	lda dismisslo,x
	cmp @line
	bcc @next
	beq @next

@later: ; increment the dismissal's line #
	lda @offset
	bmi @up
	inc dismisslo,x
	bne @next
	inc dismisshi,x
	jmp @next

@up:	; decrement the dismissal's line #
	lda dismisslo,x
	bne :+
	dec dismisshi,x
:	dec dismisslo,x
@next:	dex
	bpl @loop

@done:	rts
.endproc

;*******************************************************************************
; CLEAR LINE STATE
; Called upon line deletion. Removes all breakpoints/errors belonging to the
; affected line.
.proc clear_line_state
@line=r0
@offset=r2
@preserve=r3
	jsr current_owners
	ldxy src::line
	stxy @line

	lda #$00
	sta @offset
	sta @preserve
	jsr edit_annotations

@dismissals:
	jsr find_dismissed
	bcs @done
	jsr remove_dismissed
	jmp @dismissals

@done:	rts
.endproc

;*******************************************************************************
; CLOSE BUFFER
; Called before a source buffers closes. Drops this buffer's mapped errors and
; dismissals and decrements live "owner IDs" above it
; IN:
;  - src::activebuff: buffer being closed (before buffer IDs compact)
.proc close_buffer
@ownerid=r0
	lda #NAV_NONE
	sta __errlog_navpending
	sta __errlog_editpending
	lda src::activebuff
	ora #LIVE_BUFFER	; OR to mark as "live" (not-debug id) buffer
	sta @ownerid

;------------------------------------------------------------------------------
; shift errors
	ldx numerrs
	beq @dismissals
	dex

@loop:	lda errfileids,x
	cmp @ownerid
	bcc @next
	bne @shift
	jsr remove
	jmp @next
@shift:	dec errfileids,x
@next:	dex
	bpl @loop

;------------------------------------------------------------------------------
; shift dismissals
@dismissals:
	ldx numdismissed
	beq @done
	dex

@loopdismiss:
	lda dismissowners,x
	cmp @ownerid
	bcc @nextdismiss
	bne @shiftdismiss
	jsr remove_dismissed
	jmp @nextdismiss
@shiftdismiss:
	dec dismissowners,x

@nextdismiss:
	dex
	bpl @loopdismiss

@done:	rts
.endproc

;*******************************************************************************
; REFRESH
; Refresh an already-open error window without stealing focus from others.
.proc refresh
	lda gui::active_type
	cmp #GUI_ERRLOG
	bne @done
	lda numerrs
	bne :+
	JUMPMAIN gui::close
:	JUMPMAIN gui::refresh
@done:	rts
.endproc

;*******************************************************************************
; SHOW
; Display live errors without entering the window's keyboard loop.
.proc show
	lda numerrs
	beq refresh
	ldxy #menu
	JUMPMAIN gui::select
.endproc

;*******************************************************************************
; CHECK LINE
; Verify the current line and replace its live error.  Returns the result
; of the verification assembly pass as well.
; IN:
;  - mem::linebuffer: source text to check
;  - src::activebuff: buffer containing the line
;  - src::line:       source line being checked
; OUT:
;  - .A:          token type/error (0 if checking was skipped)
;  - .C:          set on syntax error
;  - livechanged: nonzero if the live error changed
.proc check_line
@line=r0
@buffer=r0
	lda #$00
	sta __errlog_editpending
	lda fmt::enable
	beq @skip
	lda numdismissed
	beq @check
	jsr current_owners
	ldxy src::line
	stxy @line
	jsr find_dismissed
	bcs @check

@skip:	lda #0
	sta livechanged
	RETURN_OK

@check: ; tokenize and check for errors
	ldxy #mem::linebuffer
	lda #FINAL_BANK_MAIN
	CALLMAIN asm::tokenize
	php
	pha
	bcs :+
	lda #0
:	pha
	lda src::activebuff
	sta @buffer
	ldxy src::line
	pla
	jsr set_live
	pla
	plp
	rts
.endproc

;*******************************************************************************
; BEFOREKEY
; Snapshot an editor key's starting position without disturbing its arguments.
; Only edited lines are checked upon leaving. Source edits cancel navpending
; and set editpending in src::mark_dirty. RETURN (breaking/ending line)
; forces validation.
; IN:
;  - .A: the editor key to handle
; OUT:
;  - .AXY: unchanged
;  - .P: unchanged
;  - navpending: NAV_NONE, NAV_CHECK_ON_LEAVE, or NAV_CHECK_ON_RETURN
; CLOBBERS:
;  - navkey, navbuffer, navline, navpos
.proc before_key
	; save registers
	php
	pha
	sta navkey		; keep the key for the checks below
	txa
	pha
	tya
	pha

	lda #NAV_NONE
	sta __errlog_navpending
	lda zp::verify
	beq @done
	lda fmt::enable
	beq @done
	lda src::activebuff
	cmp #MAX_SOURCES
	bcs @done
	sta navbuffer		; set buffer we're on BEFORE key is handled

	lda navkey
	cmp #K_RETURN
	beq @return		; set navpending to check always (RETURN)
	cmp #K_FORCE_NEWLINE
	beq @return		; same as RETURN - always check
	lda __errlog_editpending
	beq @done		; don't validate motion from unchanged lines
	lda #NAV_CHECK_ON_LEAVE
	bne @cont		; branch always (only check if we leave line)

@return:
	jsr undismiss_current
	lda #NAV_CHECK_ON_RETURN
@cont:	sta __errlog_navpending	; set navpending to check if we leave the line
	ldxy src::line
	stxy navline		; set line we're on BEFORE key is handled
	CALLMAIN src::pos
	stxy navpos		; save source position before handling key too

@done:	; restore registers
	pla
	tay
	pla
	tax
	pla
	plp
	rts
.endproc

;*******************************************************************************
; AFTER KEY
; After navigation, check an edited departed line using its source text. Preserve
; the destination buffer, source/cursor position and line buffer. No formatting
; or beep: an invalid line must not prevent the user from navigating away.
; IN:
;  - navpending: the pending validation state from before_key
;  - navbuffer, navline, navpos: the position saved before the key
; OUT:
;  - navpending: NAV_NONE
;  - mem::linebuffer: restored to the destination line's text
; CLOBBERS:
;  - r0-r5, navrow, navdest, livechanged
.proc after_key
@delta=r0
	ldx __errlog_navpending
	bne :+
	rts

:	lda #NAV_NONE
	sta __errlog_navpending
	cpx #NAV_CHECK_ON_RETURN
	beq @check
	lda src::activebuff
	cmp navbuffer
	bne @check
	lda src::line
	cmp navline
	bne @check
	lda src::line+1
	cmp navline+1
	bne @check
	rts			; stayed on the same line: no validation

@check:	lda navbuffer
	cmp src::numbuffers
	bcc :+
	rts

:	; find the line we left and see if it's still visible
	; (it may have scrolled offscreen, or be in a different buffer)
	lda #$ff		; -1
	sta navrow
	lda src::activebuff
	cmp navbuffer
	bne @save
	sec

	; @delta = navline - srcline
	lda navline
	sbc src::line
	sta @delta
	lda navline+1
	sbc src::line+1
	sta @delta+1
	clc

	; .X = @delta + cury
	lda @delta
	adc zp::cury
	tax
	lda @delta+1
	adc #0
	bne @save

	; is .X on screen?
	cpx edit::height
	beq @visible
	bcs @save

@visible:
	stx navrow

@save:  CALLMAIN text::savebuff
	lda src::activebuff
	sta navdest		; buffer to return to
	CALLMAIN src::pushp	; destination source position

	lda navbuffer
	CALLMAIN src::setbuff

	ldxy navpos
	CALLMAIN src::goto
	CALLMAIN src::home

	CALLMAIN src::get
	jsr check_line
	php			; save the syntax result over the restore
	lda livechanged
	beq @restore
	lda navrow		; is line still on screen?
	bmi @restore		; if not, continue

	; redraw the line that was navigated from
	lda zp::cury
	pha
	lda navrow
	sta zp::cury
	CALLMAIN edit::redrawline
	pla
	sta zp::cury

@restore:
	; restore both source cursors (the current source position and the
	; previous one before the key was handled)
	ldxy navpos
	CALLMAIN src::goto
	lda navdest
	CALLMAIN src::setbuff
	CALLMAIN src::popgoto
	CALLMAIN text::restorebuff
	plp			; restore the syntax result

	lda livechanged		; .C is untouched by LDA
	beq @done
	bcc @valid
	jmp show

@valid:	jmp refresh
@done:	rts
.endproc
