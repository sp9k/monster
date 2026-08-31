;*******************************************************************************
; HELP.ASM
; This file contains the code for the "help" screen: a modal window, framed like
; an alert, that reports how much of each of the assembler's resources is in
; use.
;*******************************************************************************

.include "asm.inc"
.include "border.inc"
.include "config.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "key.inc"
.include "labels.inc"
.include "layout.inc"
.include "limits.inc"
.include "macro.inc"
.include "macros.inc"
.include "memory.inc"
.include "object.inc"
.include "ram.inc"
.include "screen.inc"
.include "target.inc"
.include "text.inc"
.include "watches.inc"

.import puts

;*******************************************************************************
; GEOMETRY
; The window spans the full width of the screen and is drawn from its top row
; down.  Its text rows are clipped to the interior so that nothing can overwrite
; the left/right borders.
HELP_NUM_LINES = 13		; number of info lines the window displays

START_ROW       = 0				; the top border's row
HELP_TEXT_ROW   = START_ROW+1			; first info line
HELP_PROMPT_ROW = HELP_TEXT_ROW+HELP_NUM_LINES+1	; prompt (blank row above it)
HELP_BOT_ROW    = HELP_PROMPT_ROW+1		; the bottom border's row

.assert HELP_BOT_ROW < SCREEN_HEIGHT, error, "help window doesn't fit on screen"

; the columns the left and right borders are drawn in
HELP_LCOL = 0
HELP_RCOL = LINESIZE-1

.if LINESIZE >= 40
HELP_TEXT_COL = 2		; column the info lines' labels start at
HELP_VAL_COL  = 14		; column their values start at
.else
HELP_TEXT_COL = 1
HELP_VAL_COL  = 10
.endif

HELP_TEXT_LEN = HELP_RCOL-HELP_TEXT_COL		; usable width of the interior

;*******************************************************************************
; PROMPT
HELP_PROMPT_LEN = 13		; sizeof("press any key")

; the field is rounded out to even columns, and never wider than the text area
.if HELP_TEXT_LEN < ((HELP_PROMPT_LEN+1) & $fe)
HELP_RVS_LEN = HELP_TEXT_LEN & $fe
.else
HELP_RVS_LEN = (HELP_PROMPT_LEN+1) & $fe
.endif

HELP_RVS_START  = (HELP_TEXT_COL+((HELP_TEXT_LEN-HELP_RVS_LEN)/2)) & $fe
HELP_RVS_STOP   = HELP_RVS_START+HELP_RVS_LEN
HELP_PROMPT_COL = HELP_RVS_START+((HELP_RVS_LEN-HELP_PROMPT_LEN)/2)

.ifdef soft4x8
.assert (HELP_LCOL .mod 2) = 0, error, "help must start on an even column"
.assert ((HELP_RCOL+1) .mod 2) = 0, error, "help must end on an even column"
.assert (HELP_RVS_START .mod 2) = 0, error, "reversed field must start even"
.assert (HELP_RVS_STOP .mod 2) = 0, error, "reversed field must end even"
.endif

.if .defined(c64)
.define DEBUGGER_START "$cf00"
.define DEBUGGER_STOP  "$cfff"
.define DEBUGINFO_LOAD $0000	; from __DEBUGINFO_LOAD__
.define DEBUGINFO_SIZE "$ffff"	; from __DEBUGINFO_SIZE__
.define MACROS_START   $0000	; from __MACROBSS_LOAD__
.define MACROS_STOP    "$6000"	; from __MACROBSS_SIZE__
.elseif .defined(vic20)
.define DEBUGGER_START "$9800"
.define DEBUGGER_STOP  "$9fff"
.define DEBUGINFO_LOAD $2000	; from __DEBUGINFO_LOAD__
.define DEBUGINFO_SIZE "$2000"	; from __DEBUGINFO_SIZE__
.define MACROS_START   $2100	; from __MACROBSS_LOAD__
.define MACROS_STOP    "$5f00"	; from __MACROBSS_SIZE__
.endif

;*******************************************************************************
; LABELS
; The narrow (22 column) display has no room for the full names
.if LINESIZE >= 40
.define LBL_ANON    "anon labels"
.define LBL_MACUSE  "macro usage"
.define LBL_DBGINFO "debug info"
.define LBL_BRKPTS  "breakpoints"
.else
.define LBL_ANON    "anon"
.define LBL_MACUSE  "mac use"
.define LBL_DBGINFO "dbg info"
.define LBL_BRKPTS  "brkpts"
.endif

;*******************************************************************************
; HELPLINE
; Verifies that the line that begins at the given label is short enough for
; text::print, which stops reading a format string after LINESIZE bytes
.macro CHECKLINE lbl
	.assert (*-lbl)-1 <= ::LINESIZE, error, "help line is too long to print"
.endmacro

;*******************************************************************************
; the window's row buffer is kept out of RAM123, which is too tight to grow
.ifdef ultimem
.segment "SHAREBSS2"
.else
.BSS
.endif

rowbuf: .res LINESIZE	; row being composed

.CODE

;*******************************************************************************
.export __help_show
.proc __help_show
	JUMP FINAL_BANK_HELP, show
.endproc

.segment "HELP"

;*******************************************************************************
; SHOW
; Displays information about the system
;   DEBUGGER   start-addr, stop-addr
;   LABELS      used/max
;   ANON LABELS used/max
.proc show
	CALLMAIN scr::save
	jsr drawframe

	lda asm::pcset
	bne @prog

@noprog:
	lda #HELP_TEXT_ROW
	ldxy #@noasm
	jsr print
	jmp @debugger

;------------------------------------------------------------------------------
@prog:
; print the memory area used by the user's assembled program
	; push the top address
	lda asm::top
	pha
	lda asm::top+1
	pha

	; push the start address
	lda asm::origin
	pha
	lda asm::origin+1
	pha

	ldxy #@program_msg
	lda #HELP_TEXT_ROW
	jsr print

;------------------------------------------------------------------------------
; print the memory area used by the debugger
@debugger:
	ldxy #@debugger_msg
	lda #HELP_TEXT_ROW+1
	jsr print

;------------------------------------------------------------------------------
; print the number of labels used and how many are available
@labels:
	; push the number of labels used
	lda lbl::num
	pha
	lda lbl::num+1
	pha

	ldxy #@labels_msg
	lda #HELP_TEXT_ROW+2
	jsr print

;------------------------------------------------------------------------------
; print the number of anonymous labels used and how many are available
@anon_labels:
	; push the number of anonymous labels used
	lda lbl::numanon
	pha
	lda lbl::numanon+1
	pha
	ldxy #@alabels_msg
	lda #HELP_TEXT_ROW+3
	jsr print

;------------------------------------------------------------------------------
; print number of macros used
@macros:
	lda mac::num
	pha
	lda #$00
	pha
	ldxy #@macros_msg
	lda #HELP_TEXT_ROW+4
	jsr print

;------------------------------------------------------------------------------
; print macro usage (amout of memory)
@macusage:
	lda mac::top
	sec
	sbc #<MACROS_START
	pha
	lda mac::top+1
	sbc #>MACROS_START
	pha
	ldxy #@macro_usage
	lda #HELP_TEXT_ROW+5
	jsr print

;------------------------------------------------------------------------------
; print the number of files used in the assembly unit
@files:
	lda dbgi::numfiles
	pha
	lda #$00
	pha
	ldxy #@files_msg
	lda #HELP_TEXT_ROW+6
	jsr print

;------------------------------------------------------------------------------
; print the size of the debug information and total available space for it
@dbgi:	lda dbgi::top
	sec
	sbc #<DEBUGINFO_LOAD
	pha
	lda dbgi::top+1
	sbc #>DEBUGINFO_LOAD
	pha
	ldxy #@dbginfo_msg
	lda #HELP_TEXT_ROW+7
	jsr print

;------------------------------------------------------------------------------
; print the number of breakpoints and maximum available
@brkpts:
	lda dbg::numbreakpoints
	pha
	lda #$00
	pha
	ldxy #@breakpoints_msg
	lda #HELP_TEXT_ROW+8
	jsr print

;------------------------------------------------------------------------------
; print the number of watchpoints and maximum available
@watches:
	lda watch::num
	pha
	lda #$00
	pha
	ldxy #@watchpoints_msg
	lda #HELP_TEXT_ROW+9
	jsr print

;------------------------------------------------------------------------------
; print the number of segments defined / available
@segments:
	lda obj::numsegments
	pha
	lda #$00
	pha
	ldxy #@segments_msg
	lda #HELP_TEXT_ROW+10
	jsr print

;------------------------------------------------------------------------------
; print the number of imports defined / available
@imports:
	lda obj::numimports
	pha
	lda #$00
	pha
	ldxy #@imports_msg
	lda #HELP_TEXT_ROW+11
	jsr print

;------------------------------------------------------------------------------
; print the number of exports defined / available
@exports:
	lda obj::numexports
	pha
	lda #$00
	pha
	ldxy #@exports_msg
	lda #HELP_TEXT_ROW+12
	jsr print

;------------------------------------------------------------------------------
; leave a blank row between the info and the prompt, then say what we're
; waiting for.  This screen is modal and covers the editor
; already, so it gets a prompt of its own rather than an alert window on top
; of it.
	ldxy #@blank_msg
	lda #HELP_PROMPT_ROW-1
	jsr print

	ldxy #@anykey_msg
	lda #HELP_PROMPT_ROW
	jsr print

	; reverse the field the prompt sits in
	ldy #HELP_RVS_START
	ldx #HELP_RVS_STOP
	lda #HELP_PROMPT_ROW
	CALLMAIN scr::rvsline_part

	CALLMAIN key::flush	; a held key would scroll straight past this
	CALLMAIN key::waitch
@done:
	CALLMAIN scr::restore
	rts

.PUSHSEG
.RODATA
; NOTE: these strings live in the main bank (not the HELP bank) because
; text::print reads them with the main bank swapped in
@noasm:           .res ::HELP_TEXT_COL, ' '
                  .byte "program", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte "no assembly", 0
                  CHECKLINE @noasm
@program_msg:     .res ::HELP_TEXT_COL, ' '
                  .byte "program", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte "$", ESCAPE_VALUE, "-$", ESCAPE_VALUE, 0
                  CHECKLINE @program_msg
@debugger_msg:    .res ::HELP_TEXT_COL, ' '
                  .byte "debugger", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte DEBUGGER_START, "-", DEBUGGER_STOP, 0
                  CHECKLINE @debugger_msg
@labels_msg:      .res ::HELP_TEXT_COL, ' '
                  .byte "labels", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_LABELS), 0
                  CHECKLINE @labels_msg
@alabels_msg:     .res ::HELP_TEXT_COL, ' '
                  .byte LBL_ANON, ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_ANON), 0
                  CHECKLINE @alabels_msg
@macros_msg:      .res ::HELP_TEXT_COL, ' '
                  .byte "macros", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_MACROS), 0
                  CHECKLINE @macros_msg
@macro_usage:     .res ::HELP_TEXT_COL, ' '
                  .byte LBL_MACUSE, ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte "$", ESCAPE_VALUE, "/", MACROS_STOP, 0
                  CHECKLINE @macro_usage
@files_msg:       .res ::HELP_TEXT_COL, ' '
                  .byte "files", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_FILES), 0
                  CHECKLINE @files_msg
@dbginfo_msg:     .res ::HELP_TEXT_COL, ' '
                  .byte LBL_DBGINFO, ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte "$", ESCAPE_VALUE, "/", DEBUGINFO_SIZE, 0
                  CHECKLINE @dbginfo_msg
@breakpoints_msg: .res ::HELP_TEXT_COL, ' '
                  .byte LBL_BRKPTS, ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_BREAKPOINTS), 0
                  CHECKLINE @breakpoints_msg
@watchpoints_msg: .res ::HELP_TEXT_COL, ' '
                  .byte "watches", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_WATCHPOINTS), 0
                  CHECKLINE @watchpoints_msg
@segments_msg:    .res ::HELP_TEXT_COL, ' '
                  .byte "segments", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_SEGMENTS), 0
                  CHECKLINE @segments_msg
@imports_msg:     .res ::HELP_TEXT_COL, ' '
                  .byte "imports", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_IMPORTS), 0
                  CHECKLINE @imports_msg
@exports_msg:     .res ::HELP_TEXT_COL, ' '
                  .byte "exports", ESCAPE_GOTO, ::HELP_VAL_COL
                  .byte ESCAPE_VALUE_DEC, "/", .string(MAX_EXPORTS), 0
                  CHECKLINE @exports_msg
@blank_msg:       .byte 0
@anykey_msg:      .byte ESCAPE_GOTO, ::HELP_PROMPT_COL
                  .byte "press any key", 0
                  CHECKLINE @anykey_msg
.POPSEG
.endproc

;*******************************************************************************
; DRAWFRAME
; Draws the window's top and bottom borders.  Every row between them draws its
; own left/right border along with its text (see PRINT)
.proc drawframe
	; the window is drawn at full width
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop

	; top border
	ldy #BORDER_HBAR
	lda #BORDER_TL
	ldx #BORDER_TR
	jsr mkrow
	lda #START_ROW
	jsr showrow

	; bottom border
	ldy #BORDER_HBAR
	lda #BORDER_BL
	ldx #BORDER_BR
	jsr mkrow
	lda #HELP_BOT_ROW

	; fall through to showrow
.endproc

;*******************************************************************************
; SHOWROW
; Draws rowbuf on the given row
; IN:
;  - .A: the row to draw it at
.proc showrow
	ldxy #rowbuf
	CALLMAIN puts
	rts
.endproc

;*******************************************************************************
; MKROW
; Builds a row of the window in rowbuf
; IN:
;  - .A: the character to draw in the leftmost column
;  - .X: the character to draw in the rightmost column
;  - .Y: the character to fill the columns between them with
.proc mkrow
	pha			; save the left character
	txa
	pha			; and the right one

	tya
	ldx #HELP_RCOL-1
:	sta rowbuf,x
	dex
	bne :-			; stop at HELP_LCOL; the corner goes there

	pla
	sta rowbuf+HELP_RCOL
	pla
	sta rowbuf+HELP_LCOL
	rts
.endproc

;*******************************************************************************
; PRINT
; Renders the given format string and draws it as one of the window's rows,
; borders included.  The borders are part of the row that is drawn so that no
; second pass can clip (and, on the 4x8 display, clobber) them.
; IN:
;  - .XY: the format string to draw
;  - .A:  the row to draw it at
.proc print
@ret=mem::sparevec
@a=r0
	sta @a
	pla
	sta @ret
	pla
	sta @ret+1

.ifdef vic20
	CALLMAIN text::render_ind
.else
	CALLMAIN text::render
.endif

	; copy the rendered line into the row.  Rendering doesn't pad the line,
	; so the row is blanked first; its column is its index in the buffer
	jsr blankrow
	ldx #$00
:	lda mem::linebuffer2,x
	beq :+
	sta rowbuf,x
	inx
	cpx #HELP_RCOL
	bcc :-

:	lda #BORDER_VBAR
	sta rowbuf+HELP_LCOL
	sta rowbuf+HELP_RCOL

	lda @a
	jsr showrow

	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc

;*******************************************************************************
; BLANKROW
; Fills the row to construct with spaces
.proc blankrow
	lda #' '
	ldx #HELP_RCOL
:	sta rowbuf,x
	dex
	bpl :-
	rts
.endproc
