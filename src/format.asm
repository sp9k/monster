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

.export __fmt_enable
__fmt_enable: .byte 0	; flag to enable (!0) or disable (0) formatting

.CODE

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
	jsr src::delete		; delete whitespaced
	bcc @l1
@done:	jmp refresh
.endproc

;******************************************************************************
; INDENT
; Insert one indent at current source position, then refresh the
; line buffer and move to the end of it
.proc indent
	lda #$09
	jsr src::insert

@refresh:
	jsr refresh

	; check the size of the line now that it has a TAB
	jsr text::rendered_line_len
	bcc :+				; ok

	; line would be oversized with a TAB, use a space instead
	jsr src::backspace	; delete the TAB
	lda #' '
	jsr src::insert		; insert a space
	jmp @refresh		; and refresh the line again
:	rts
.endproc

;******************************************************************************
; LINE
; Formats the linebuffer according to the given content type.
; IN:
;  - .A: the "type" to format see (codes.inc) e.g. ASM_OPCODE, etc.
.export __fmt_line
.proc __fmt_line
@linecontent=r6
@cnt=r0
	sta @linecontent	; save format "type"

	lda __fmt_enable
	beq @done

	; get current character index of cursor
	jsr text::char_index
	tya
	pha			; save character index

	jsr @fmt

	; fix cursor position for newly formatted line
	jsr src::home
	jsr src::get

	pla			; restore character index
	pha
	jsr text::index2cursor
	stx zp::curx

	pla
	sta @cnt
	beq @done
:	jsr src::right_rep
	bcs @done
	dec @cnt
	bne :-
	rts

;--------------------------------------
@fmt:	; remove spaces from start of line
	jsr src::home

@removespaces:
	jsr src::after_cursor
	cmp #$0d
	beq @left_aligned
	jsr util::is_whitespace
	bne @left_aligned

	jsr src::delete
	ldx #$00
	ldy #39
	jsr linebuff::shl
	beq @removespaces	; branch always

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	and #ASM_LABEL		; if formatting includes label, do __fmt_label

@label: beq @notlabel
	jmp label

@notlabel:
	; if comment/directive/NONE don't indent
	lda @linecontent
	beq @done			; ASM_NONE
	and #ASM_COMMENT|ASM_DIRECTIVE
	bne @done

@ident: jmp indent		; anything else- indent

@done:  rts
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
