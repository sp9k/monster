;*******************************************************************************
; FORMAT.ASM
;
; This file contains the code to format a line of the user's program based
; on its contents.
; The main procedure, fmt::line, looks at the given "type" value and indents
; or unindents depending on what the line contains.
; Labels are unindented, instructions are indented.
;*******************************************************************************

.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

.BSS

;******************************************************************************
.export __fmt_enable
__fmt_enable: .byte 0	; flag to enable (!0) or disable (0) formatting

offset = r7

.CODE

;******************************************************************************
; LINE
; Formats the linebuffer according to the given content type.
; IN:
;  - .A: the "type" to format see (codes.inc) e.g. ASM_OPCODE, etc.
.export __fmt_line
.proc __fmt_line
@linecontent = r6
@cnt         = r8
	sta @linecontent	; save format "type"
	lda __fmt_enable
	beq @done		; if formatting is disabled, just quit

	; get current character index of cursor
	jsr text::char_index
	sty offset		; save character index

	jsr @fmt		; format the line

	; fix cursor position for newly formatted line
	jsr src::home
	jsr src::get

	; touch up source and cursor position, accounting for all the
	; characters inserted and deleted during the formatting
	lda offset
	beq @done
	sta @cnt

@l0:	lda text::insertmode
	beq :+
	jsr src::right
	jmp @cont
:	jsr src::right_rep
@cont:	bcs @done
	dec @cnt
	bne @l0
	rts

;-------------------------------------------------------------------------------
@fmt:	; remove spaces from start of line
	jsr src::home

@removespaces:
	jsr src::after_cursor
	cmp #$0d
	beq @left_aligned
	jsr util::is_whitespace
	bne @left_aligned

	dec offset		; decrement offset to restore cursor to
	jsr src::delete		; delete whitespace character
	ldx #$00
	ldy #LINESIZE-1
	jsr linebuff::shl
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

:				; <- indent
@done:  rts			; line is COMMENT, DIRECTIVE, NONE, we're done
.endproc

;******************************************************************************
; INDENT
; Insert one indent at current source position, then refresh the
; line buffer and move to the end of it
.proc indent
	lda #$09
	jsr src::insert		; insert a TAB at start of line

	inc offset
	jsr refresh

	; check the size of the line now that it has a TAB
	jsr text::rendered_line_len
	bcc :-				; ok

	; line would be oversized with a TAB, undo the addition of it
	dec offset
	jmp src::backspace	; delete the TAB
.endproc

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.proc label
	; read past the label
@l0:	jsr src::right_rep
	bcs @done			; nothing on the line after the label
	; TODO: check invalid label characters

	jsr util::is_whitespace
	bne @l0

	; delete all whitespace until the opcode/macro/etc.
@l1:	jsr src::after_cursor
	bcs @done		; no chars left -> done
	cmp #$0d
	beq @done		; newline -> done
	jsr util::is_whitespace
	bne indent		; non-whitespace -> separate with tab
	dec offset
	jsr src::delete		; delete whitespaced
	bcc @l1

@done:	; fall through to refresh
.endproc

;******************************************************************************
; REFRESH
; Refreshses the line
.proc refresh
	jsr src::pushp
	jsr src::home
	jsr src::get
	jmp src::popgoto
.endproc
