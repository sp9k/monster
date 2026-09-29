;*******************************************************************************
; FORMAT.ASM
;
; This file contains the code to format a line of the user's program based
; on its contents.
; The main procedure, fmt::line, looks at the given "type" value and indents
; or unindents depending on what the line contains.
; Labels and directives are unindented (except .ORG), instructions are indented.
; Also removes trailing whitespace.
;*******************************************************************************

.include "asm.inc"
.include "codes.inc"
.include "config.inc"
.include "ram.inc"
.macpack longbranch
.include "linebuffer.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

.BSS

;*******************************************************************************
.export __fmt_enable
__fmt_enable: .byte 0	; flag to enable (!0) or disable (0) formatting

offset = r7
position = r9

.CODE
.export __fmt_word_space, __fmt_line
__fmt_word_space: JUMP FINAL_BANK_VSCREEN, format_word_space
__fmt_line: JUMP FINAL_BANK_VSCREEN, format_line
.segment "VSCREEN"
SET_CUR_BANK FINAL_BANK_VSCREEN

;*******************************************************************************
; WORD SPACE
; Before inserting SPACE, unindent a label or directive (except .ORG).
; Only inspects the first word; operands may be invalid
; IN:
;  - zp::verify: must be !0
; OUT:
;  - .C: set if the line was unindented (caller must sync/redraw the cursor)
.proc format_word_space
@end=r5
	lda __fmt_enable
	jeq @done

	CALLMAIN text::char_index
	sty @end

	; find first word on the line
	ldy #$00
:	cpy @end
	bcs @done
	lda mem::linebuffer,y
	CALLMAIN util::is_whitespace
	bne @name
	iny
	bne :-

@name:	cpy #$00		; is first word at index 0?
	jeq @done		; yes -> already left aligned
	ldx #$00

@copy:	; copy first word to asmbuffer
	lda mem::linebuffer,y
	CALLMAIN util::is_whitespace
	jeq @done		; end of word -> done
	sta mem::asmbuffer,x
	inx
	iny
	cpy @end
	bcc @copy		; repeat til end of line

	lda #$00
	sta mem::asmbuffer,x	; terminate buff

	; assemble the word to see if it's a label or directive
	ldxy #mem::asmbuffer
	CALLMAIN str::toupper
	stxy zp::line
	CALLMAIN asm::word_type
	bcs @done

	; if first word is a label or directive, format it immediately
	and #ASM_LABEL|ASM_DIRECTIVE
	jeq @done
	lda #ASM_DIRECTIVE	; treat as DIRECTIVE (JUST strip indentation)
	jsr format_line		; remove indentation
	sec			; flag for caller to redraw
	rts

@done:	clc
	rts
.endproc

;*******************************************************************************
; LINE
; Formats the linebuffer according to the given content type.
; IN:
;  - .A: the "type" to format see (codes.inc) e.g. ASM_OPCODE, etc.
.proc format_line
@linecontent = r6
	sta @linecontent	; save format "type"
	lda __fmt_enable
	jeq @done		; if formatting is disabled, just quit

	; get current character index of cursor
	CALLMAIN text::char_index
	sty offset		; save character index

	jsr @fmt		; format the line

	; remove trailing whitespace
	CALLMAIN src::lineend
@trim:	CALLMAIN src::left
	bcs @restore
	CALLMAIN util::is_whitespace
	bne @restore
	CALLMAIN src::delete
	bcc @trim		; branch always

@restore:
	; fix cursor position for newly formatted line
	CALLMAIN src::home
	CALLMAIN src::getwide

	; touch up source and cursor position, accounting for all the
	; characters inserted and deleted during the formatting
	lda offset
	jeq @done

@l0:	CALLMAIN src::right
	bcs @done
	dec offset
	bne @l0
	rts

;-------------------------------------------------------------------------------
@fmt:	; remove spaces from start of line
	CALLMAIN src::home
	lda #0
	sta position		; source character index being formatted

@removespaces:
	CALLMAIN src::after_cursor
	bcs @done		; empty final line -> leave it empty
	cmp #$0d
	jeq @done		; empty line -> don't insert indentation
	CALLMAIN util::is_whitespace
	bne @left_aligned

	lda offset
	beq :+			; a cursor in leading whitespace stops at column zero
	dec offset
:
	CALLMAIN src::delete		; delete whitespace character
	ldx #$00
	ldy #MAX_LINE_LEN-1
	CALLMAIN linebuff::shl
	beq @removespaces	; branch always

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	and #ASM_LABEL		; if formatting includes label
	bne label		; -> format it as one

@notlabel:
	; if COMMENT, DIRECTIVE, or NONE -> don't indent
	lda @linecontent
	and #ASM_COMMENT|ASM_DIRECTIVE
	beq indent			; anything else -> indent

@done:  rts			; line is COMMENT, DIRECTIVE, NONE, we're done
.endproc

;*******************************************************************************
; INDENT
; Insert one indent at current source position, then refresh the
; line buffer. Adjust the saved cursor only if the TAB precedes it.
.proc indent
	lda #$09
	CALLMAIN src::insert		; insert a TAB at start of line

	lda position
	cmp offset
	bcc @shift
	bne @refresh
@shift:	inc offset
@refresh:
	jsr refresh

	; check the size of the line now that it has a TAB
	CALLMAIN text::rendered_line_len
	bcs @undo
	rts

	; line would be oversized with a TAB, undo the addition of it
@undo:	lda position
	cmp offset
	bcs @remove		; only undo a cursor adjustment that was made above
	dec offset
@remove:
	JUMPMAIN src::backspace	; delete the TAB
.endproc

;*******************************************************************************
; LABEL
; Formats linebuffer as a label.
.proc label
	; read past the label
@l0:	CALLMAIN src::right_rep
	bcs @done			; nothing on the line after the label
	inc position
	; TODO: check invalid label characters

	CALLMAIN util::is_whitespace
	bne @l0

	; delete all whitespace until the opcode/macro/etc.
@l1:	CALLMAIN src::after_cursor
	bcs @done		; no chars left -> done
	cmp #$0d
	jeq @done		; newline -> done
	CALLMAIN util::is_whitespace
	bne indent		; non-whitespace -> separate with tab
	lda position
	cmp offset
	bcs :+			; deleting after the saved cursor does not move it
	dec offset
:
	CALLMAIN src::delete		; delete whitespaced
	bcc @l1

@done:	; fall through to refresh
.endproc

;*******************************************************************************
; REFRESH
; Refreshses the line
.proc refresh
	CALLMAIN src::pushp
	CALLMAIN src::home
	CALLMAIN src::getwide
	JUMPMAIN src::popgoto
.endproc
