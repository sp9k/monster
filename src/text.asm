;*******************************************************************************
; TEXT.ASM
; This file contains procedures for drawing text or interacting with it in
; a physical (rendering related) sense.
; This includes utilities to determine how wide a TAB will be rendered at a
; given offset, or where the cursor should be relative to a given character
; offset.
;*******************************************************************************

.include "asm.inc"
.include "beep.inc"
.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "file.inc"
.include "irq.inc"
.include "key.inc"
.include "layout.inc"
.include "linebuffer.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "screen.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "ui.inc"
.include "util.inc"
.include "zeropage.inc"

.import puts
.import putch

;*******************************************************************************
; CHARACTER ESCAPE CODES
; These codes, when encountered by text::print, will be expanded to values
; from the stack.  For example, when text::print encounters an ESCAPE_STRING
; byte in the string, it will pull two bytes from the stack and add the
; string at that address to the rendered string.
ESCAPE_STRING    = $ff
ESCAPE_VALUE     = $fe
ESCAPE_VALUE_DEC = $fd
ESCAPE_SPACING   = $fc
ESCAPE_BYTE      = $fb
ESCAPE_CHAR      = $fa
ESCAPE_8x8UDG    = $f9
ESCAPE_GOTO_COL  = $f8
NUM_ESCAPE_CODES = 8

STATUS_FORMAT_DEFAULT = 0	; display x, line/total lines
STATUS_FORMAT_XY      = 1	; display x,y position of cursor

.BSS
;*******************************************************************************
.export __text_len
__text_len: .byte 0

.export  __text_buffer
__text_buffer: .byte 0	; if 0, putch immediately draws to the screen

.export __text_insertmode
__text_insertmode: .byte 0	; the insert mode (1 = insert, 0 = replace)

.export __text_status_mode
__text_status_mode: .byte 0	; the mode to display on the status line

.export __text_status_fmt
__text_status_fmt: .byte 0	; the format to use for the status display

render_off: .byte 0
indirect:   .byte 0

tempbuff: .res LINESIZE

.CODE
;*******************************************************************************
; BUFFERON
; enables buffering for text (text will only be drawn when lines are
; drawn, inserts will not redraw screen)
.export __text_bufferon
.proc __text_bufferon
	lda #$01
	skw

	; fallthrough
.endproc

;*******************************************************************************
; BUFFEROFF
; Disables buffering. All characters will be drawn to screen
.export __text_bufferoff
.proc __text_bufferoff
	lda #$00
	sta __text_buffer
	rts
.endproc

;*******************************************************************************
; STATUS
; Draws the text status (row, column, etc.) at the given row
; IN:
;  - .A: the row to display the status details on
.export __text_status
.proc __text_status
	ldxy #mem::statusline
	jmp puts
.endproc

;*******************************************************************************
; UPDATE
; updates the statusline according to the current cursor position
; and blinks the cursor if it's time
; OUT:
;   - .Z: always clear
.export __text_update
.proc __text_update
	jsr ui::update_statusline
@blink: lda cur::mode
	bne @done			; if not NORMAL (SELECT), don't blink
	dec zp::curtmr
	bne @done

	jsr cur::toggle
	lda #CUR_BLINK_SPEED
	sta zp::curtmr
@done:	rts
.endproc

;*******************************************************************************
; CLRLINE
; Clears the linebuffer by setting its first byte to 0.
.export __text_clrline
.proc __text_clrline
	lda #0
	sta mem::linebuffer
	rts
.endproc

;*******************************************************************************
; PUTCH
; Adds the character in .A to the current cursor position in the
; text linebuffer. The cursor is then updated to the next position.
; IN:
;  - .A: the character to put
; OUT:
;  - .C: set if character was unsuccessfully put
.export __text_putch
.proc __text_putch
@curi=zp::text+3
@len=zp::text+4
@ch=zp::text+5
@len2=zp::text+6
@savecurx=zp::text+7
@savecury=zp::text+8
	sta @ch
	jsr __text_linelen
	stx @len

	lda zp::curx
	sta @savecurx
	lda zp::cury
	sta @savecury

	jsr __text_char_index
	sty @curi

	lda @ch
	bne :+
@zero:  ;lda #$00
	sta mem::linebuffer,y
	beq @redraw		; branch always

:	cmp #$14
	bne @printing

@backspace:
	ldx zp::curx
	beq @err_silent	; cannot delete (cursor is at left side of screen)
	dex
	cpx cur::minx
	bcc @err	; minx >= curx, cursor is limited

	; get the new x position
	lda mem::linebuffer-1,y
	pha
	lda #$00
	sta mem::linebuffer-1,y		; temporarily 0-terminate line
	jsr __text_rendered_line_len
	stx zp::curx
	pla
	ldy @curi
	sta mem::linebuffer-1,y		; restore

	lda __text_insertmode
	beq @bs_done		; if REPLACE, don't alter text

@shift_left:
	; shift everything to the right of the char we replaced left
	lda #$00
	ldy @len		; index to stop shifting
	sta mem::linebuffer,y
	ldx @curi		; index to start shifting
	dex
	jsr linebuff::shl
@bs_done:
	RETURN_OK	; "put" was successful

@err:	jmp beep::short
@err_silent:
	sec		; couldn't perform action
	rts

@printing:
	jsr savetmp
	ldx zp::curx
	cpx cur::maxx
	bcs @err		; cursor is limited

	lda __text_insertmode
	beq @fastput	; if REPLACE, no need to shift, do fast put

@slowput:
@shift_right:
	; insert a new char and redraw the line
	lda #$00
	ldx @len
	cpx @curi
	beq @fastputi
	sta mem::linebuffer+2,x
@shr:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	cpx @curi
	bmi @cont		; if we've shifted all columns, continue
	bcc @cont
	bcs @shr		; if more to shift, repeat

@fastputi:
	;lda #$00
	sta mem::linebuffer+1,x	; keep the line 0-terminated

@fastput:
	; replace the underlying character
@cont:	ldx @curi
	lda @ch
	sta mem::linebuffer,x
	beq @done		; terminating 0, we're done

	cmp #$09		; TAB
	bne @redraw
@tab:	jsr __text_tabr_dist
	clc
	adc zp::curx
	sta zp::curx
	bpl @redrawline		; re-render the line (branch always)

@redraw:
	jsr __text_rendered_line_len
	dex
	bmi :+				; if len is 0 -> continue
	cpx cur::maxx
	bcc :+

	; line is oversized now - abort the operation
	lda @savecurx
	sta zp::curx
	lda @savecury
	sta zp::cury
	jmp restoretmp

:	lda @ch
	beq :+		; don't increment line if character was 0
	inc zp::curx

:	lda __text_buffer
	bne @done

@redrawline:
	lda zp::cury
	jsr __text_drawline	; re-render whole line
	RETURN_OK

@done:	RETURN_OK		; "put" was successful
.endproc

;*******************************************************************************
; DRAWLINE
; Renders the text in mem::linebuffer at the given row
; IN:
;  - .A: the row to draw the line at
;  - mem::linebuffer: the text to draw
.export __text_drawline
.proc __text_drawline
	ldx #LINESIZE
	stx __text_len
	ldxy #mem::linebuffer
	jmp __text_print
.endproc

;*******************************************************************************
; RENDER INDIRECT
; This procedure functions the same as RENDER, but it saves two procedures from
; the stack for the return address instead of just one.
; This is for use in other RAM banks than the one which the text code resides
; in, where a trampoline is needed to call/return from the procedure.
.export __text_render_indirect
.proc __text_render_indirect
@retbank = zp::text+7+2
	lda #$01
	sta indirect
	pla
	sta @retbank
	pla
	sta @retbank+1

	; fall through to RENDER
.endproc

;*******************************************************************************
; RENDER
; Renders the given string. The given format string follows the same format as
; "text::print"
; IN:
;   - .XY: the format string to render
; OUT:
;  - mem::linebuffer2: the rendered format string
;  - .XY:              address of rendered string (mem::linebuffer2)
.export __text_render
.proc __text_render
	inc render_off

	; fall through to __text_print
.endproc

;******************************************************************************
; PRINT
; displays the format string in (<X,>Y) at the row in .A.
; NOTE: you MUST call it with JSR (not JMP) because it manipulates the stack to
; get operands
; IN:
;  - .XY: the address of the message to print
;  - .A:  the row to print the string at
.export __text_print
.proc __text_print
@sub   = zp::text	; address of string to replace escape char with
@str   = zp::text+2
@row   = zp::text+4
@savex = zp::text+5
@savey = zp::text+6
@ret   = zp::text+7
@tmp   = zp::text+9
@buff = mem::linebuffer2
        stxy @str
	sta @row

	; save the return address (variadic args are stored on stack)
	pla
	sta @ret
	pla
	sta @ret+1

        ldx #$00
        ldy #$00
;copy the string (substituting escaped characters)
@l0:    lda (@str),y
	beq @gotodisp
	cmp #$09		; TAB
	bne :+

	; insert as many spaces as needed to get to next tab column
	sty @savey
	txa
	jsr __text_tabr_dist_a
	tay
	lda #' '
@tab:	sta @buff,x
	inx
	dey
	bne @tab
	ldy @savey
	jmp @cont

:	; check for escape codes
	cmp #$100-NUM_ESCAPE_CODES
	bcc @putch

	; get the escape code vector and execute it
	sbc #($100-NUM_ESCAPE_CODES)
	stx @savex
	tax
	lda @escvecs_lo,x
	sta zp::jmpvec
	lda @escvecs_hi,x
	sta zp::jmpvec+1
	ldx @savex
	jmp (zp::jmpvec)

;--------------------------------------
@esc_ch:
	pla			; get character from stack
@putch:	sta @buff,x
	inx

@cont:	; escape-code handlers JMP back here when they are done
	iny
	cpx #LINESIZE
	bcc @l0
@gotodisp:
        jmp @disp

;--------------------------------------
@esc_spacing:
;substitute escape character with number of spaces from next byte
	iny
	sty @savey
	lda (@str),y	; get next byte (number of spaces to insert)

@insert_spaces:
	tay
	beq :++		; 0 -> we're done
:	lda #' '
	sta @buff,x
	inx
	cpx #LINESIZE
	beq @gotodisp
	dey
	bne :-
:	ldy @savey
	jmp @cont

;--------------------------------------
@esc_byte:
	stx @savex
	sty @savey

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+1,x
	tya
	sta @buff,x
	jmp @value_1byte_done

;--------------------------------------
@esc_value_dec:
	stx @savex
	sty @savey

	pla
	tay
	pla
	tax
	jsr util::todec
	ldy #0

	ldx @savex
:	lda mem::spare,y
	beq @decdone
	sta @buff,x
	inx
	iny
	bne :-
@decdone:
	ldy @savey
	jmp @cont

;--------------------------------------
@esc_value:
	stx @savex
	sty @savey

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+1,x
	tya
	sta @buff,x

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+3,x
	tya
	sta @buff+2,x

	inx
	inx
@value_1byte_done:
	inx
	inx
	ldy @savey
	jmp @cont

;--------------------------------------
@esc_string:
;substitute escape character with string from stack
	pla
	sta @sub
	pla
	sta @sub+1
	sty @savey
	ldy #$00

@l1:	lda (@sub),y
	beq @esc_string_done
	sta @buff,x
	iny
	inx
	cpx #LINESIZE
	bcc @l1

@esc_string_done:
	ldy @savey
	jmp @cont

;--------------------------------------
@esc_goto_col:
	iny
	sty @savey
	lda (@str),y	; get next byte (column to go to)
	stx @tmp

	; get number of spaces we need to insert (target_col - current_col)
	sec
	sbc @tmp
	bmi @esc_string_done
	jmp @insert_spaces

;--------------------------------------
.define escape_vectors @esc_goto_col, $0000, @esc_ch, @esc_byte, @esc_spacing, @esc_value_dec, @esc_value, @esc_string
.PUSHSEG
.RODATA
@escvecs_lo: .lobytes escape_vectors
@escvecs_hi: .hibytes escape_vectors
.POPSEG

;--------------------------------------
@disp:	; fill the rest of the line buffer with spaces
	lda render_off
	beq :+
	; if rendering is disabled don't fill buffer with spaces
	lda #$00
	sta @buff,x
	beq @buffdone

:	lda #' '
:	sta @buff,x
	inx
	cpx #LINESIZE
	bcc :-

@buffdone:
	; restore the return address
	lda @ret+1
	pha
	lda @ret
	pha

	; do we need to restore two addresses?
	lda indirect
	beq :+
	lda @ret+3
	pha
	lda @ret+2
	pha

:	lda #$00
	sta indirect	; reset indirect flag

	; check if we want to render to screen or just return
	lda render_off
	beq @draw
	dec render_off	; clear disable render flag
	; return the buffer
	ldxy #mem::linebuffer2
	rts

@draw:	; print the rendered string
	ldxy #@buff
	lda @row

	jmp puts
.endproc

;*******************************************************************************
; LINELEN
; Returns the length of mem::linebuffer
; OUT:
;  - .X: the length of mem::linebuffer
.proc __text_linelen
.export __text_linelen
	ldx #$ff
@l0:	inx
	lda mem::linebuffer,x
	beq @done
	cpx #LINESIZE
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; RENDERED LINE LEN
; Returns the length of mem::linebuffer in number of rendered characters
; OUT:
;  - .X: the length of mem::linebuffer as it appears on screen
;  - .Y: the length of the linebuffer in characters
;  - .C: set if the line size is oversized
.proc __text_rendered_line_len
.export __text_rendered_line_len
@tabsz=r0
@savey=r1
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	cpy #LINESIZE
	beq @done
	lda mem::linebuffer,y
	beq @done
	cmp #$09
	bne :+
	txa
	sty @savey
	jsr __text_tabr_dist_a
	ldy @savey
	sta @tabsz
	txa
	clc
	adc @tabsz
	tax
	dex			; undo the INX
:	jmp @l0
@done:	cpx #LINESIZE+1
	rts
.endproc

;*******************************************************************************
; CHAR INDEX
; Returns the character index of the current cursor position
; If the cursor is within a TAB character's rendered range, returns the
; index of the start of the TAB
; OUT:
;  .A: the character under the cursor
;  .X: the x column position of the cursor
;  .Y: the character index of the cursor
.export __text_char_index
.proc __text_char_index
@tabsz=r0
@savey=r1
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	lda mem::linebuffer,y
	beq @done
	pha
	cmp #$09
	bne :+
	txa
	sty @savey
	jsr __text_tabr_dist_a
	ldy @savey
	sta @tabsz
	txa
	clc
	adc @tabsz
	tax
	dex			; undo the INX
:	cpx zp::curx
	pla
	bcc @l0
@done:	lda mem::linebuffer,y
	rts
.endproc

;*******************************************************************************
; INDEX2CURSOR
; Returns the x cursor position for the given offset in linebuffer
; Assumes that the editor is NOT in INSERT mode.
; If the given index is greater than the number of characters in the line
; buffer, returns the position of the last character in the buffer.
; IN:
;  - .A: the index in linebuffer to get the cursor position for
; OUT:
;  - .X: the corresponding cursor position
.export __text_index2cursor
.proc __text_index2cursor
@i=zp::text
@x=zp::text+1
@seek=zp::text+2
	sta @seek
	lda #$00
	sta @i
	sta @x
	lda mem::linebuffer
	beq @done		; if line is empty, cursor is on column 0

@l0:	ldx @i
	lda mem::linebuffer,x
	beq @end	; end of buffer
	cmp #$09	; TAB
	bne :+

	; tabs need to be handled if we end on them
	lda @x
	jsr __text_tabr_dist_a
	clc
	adc @x
	sta @x
	dec @x

:	ldx @i
	inc @i
	cpx @seek
	bcs @done
	inc @x
	bne @l0

@end:	dec @x
@done:	ldx @x
	rts
.endproc

;*******************************************************************************
; TABR_DIST
; Returns the # of columns to the next tab from the current cursor position
; OUT:
;  - .A: the number of characters to the next tab
.export __text_tabr_dist
__text_tabr_dist_a=*+2
.proc __text_tabr_dist
@xstart=zp::util
	lda zp::curx
@tabr_dist_a:
	sta @xstart
	ldy #$00
:	iny
	cmp tabs,y
	bcs :-

	lda tabs,y
	sec
	sbc @xstart
@done:	rts
.endproc

;*******************************************************************************
; TABL_DIST
; Returns the number of columns left to the previous tab column
; OUT:
;  - .A: the number of characters to the previous tab
.export __text_tabl_dist
.proc __text_tabl_dist
	lda zp::curx
	beq @done
	ldy #tabs_end
:	dey
	cmp tabs,y
	bcc :-
	beq :-
	lda tabs,y
	sta zp::util
	lda zp::curx
	; sec
	sbc zp::util
@done:	rts
.endproc

;*******************************************************************************
; TABS
; This table stores the offsets to each TAB column
.PUSHSEG
.RODATA
tabs:
.repeat SCREEN_WIDTH/TAB_WIDTH, i
	.byte i*TAB_WIDTH
.endrepeat
tabs_end=*-tabs
LAST_TAB_COL=TAB_WIDTH*(tabs_end-tabs)
.POPSEG

;*******************************************************************************
; SAVEBUFF
; Stores the contents of kinebuffer to spare memory. The contents of the
; most recent call to this routine will be restored when text::restorebuff is
; called
.export __text_savebuff
.proc __text_savebuff
	ldy #LINESIZE-1
:	lda mem::linebuffer,y
	sta mem::linesave,y
	dey
	bpl :-
	rts
.endproc

;*******************************************************************************
; RESTOREBUFF
; Restores the linebuffer from the contents of spare memory (saved by the most
; recent call to text::savebuff)
.export __text_restorebuff
.proc __text_restorebuff
	ldy #LINESIZE-1
:	lda mem::linesave,y
	sta mem::linebuffer,y
	dey
	bpl :-
	rts
.endproc

;*******************************************************************************
; SAVETMP
.proc savetmp
	ldy #LINESIZE-1
:	lda mem::linebuffer,y
	sta tempbuff,y
	dey
	bpl :-
	rts
.endproc

;*******************************************************************************
; RESTORETMP
.proc restoretmp
	ldy #LINESIZE-1
:	lda tempbuff,y
	sta mem::linebuffer,y
	dey
	bpl :-
	rts
.endproc

;*******************************************************************************
; INFO
; Copies the given message directly to the status line.
; ; IN:
;  - .XY: the string to print
.export __text_info
.proc __text_info
@info = zp::text
@ret  = zp::asmresult
	; save return address
	pla
	sta @ret+1
	pla
	sta @ret

	jsr __text_render
	stxy @info

	ldy #$00
:	lda (@info),y
	sta mem::statusinfo,y
	beq @done
	iny
	cpy #20
	bne :-

@done:	; restore return address
	lda @ret
	pha
	lda @ret+1
	pha
	rts
.endproc

;*******************************************************************************
; CLRINFO
; Clears the info part of the status line
.export __text_clrinfo
.proc __text_clrinfo
@info=zp::text
	lda #$00
	sta mem::statusinfo
	rts
.endproc
