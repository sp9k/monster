;*******************************************************************************
; SOURCE.ASM
; This file contains the procedures to interacting with the buffer that backs
; the editor. When data is entered in the editor, it is stored in one of 8
; "source buffers", each in their own memory bank. These are gap buffers to
; allow for efficient insertion of text.
; Because there are 8 different buffers, the procedures to read and write to
; the current active buffer are stored in shared RAM.
;*******************************************************************************

.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "target.inc"
.include "zeropage.inc"

.include "ram.inc"

.import __src_atcursor
.import __src_insert
.import __src_init_buff
.import __src_next
.import __src_prev
.import __src_start

;*******************************************************************************
; CONSTANTS
MAX_SOURCES    = 8	; # of source buffers that can be loaded at once
POS_STACK_SIZE = 16 	; size of source position stack

;*******************************************************************************
; FLAGS
FLAG_DIRTY = 1

.segment "BSS_NOINIT"

;*******************************************************************************
data_start:
sp:	.byte 0			; stack pointer for source position stack
stack:	.res POS_STACK_SIZE	; stack for source positions

;*******************************************************************************
; BUFFSTATE
; This block of zeropage variables are stored in the order they are
; enumerated below.  When a source buffer is activated, the state for that
; buffer is copied to these zeropage locations.
; The values for the buffer that is being deactivated are copied to the
; "savestate" array
buffstate      = zp::srccur
cursorzp       = zp::srccur
poststartzp    = zp::srccur2
line           = zp::srcline
lines          = zp::srclines
end            = zp::srcend
SAVESTATE_SIZE = 10		; space used by above zeropage addresses

.exportzp __src_line
__src_line = line
.exportzp __src_lines
__src_lines = lines

;*******************************************************************************
; SAVESTATE
; This buffer holds the "buffer state" for each source buffer. See BUFFSTATE
savestate:  .res MAX_SOURCES*10	; 10 bytes: curl, curr, line, lines, end

.export __src_names
__src_names:
names:	   .res MAX_SOURCES*MAX_BUFFER_NAME_LEN

;*******************************************************************************
.export __src_numbuffers
__src_numbuffers:
numsrcs:    .byte 0		; number of buffers
.export __src_activebuff
__src_activebuff:
activesrc:  .byte 0		; index of active buffer (also bank offset)

.export __src_bank
__src_bank:
bank:	    .byte 0
buffs_curx: .res MAX_SOURCES	; cursor X positions for each inactive buffer
buffs_cury: .res MAX_SOURCES	; cursor Y positions for each inactive buffer
banks:      .res MAX_SOURCES	; the corresponding bank for each buffer
flags:      .res MAX_SOURCES	; flags for each source buffer

.CODE
;*******************************************************************************
; INIT
; Initializes the source state such that no buffers exist
.export __src_init
.proc __src_init
	lda #$00
	sta numsrcs
	sta sp
	sta line
	sta line+1
	sta lines
	sta lines+1

	ldx #MAX_SOURCES
:	sta bank-1,x
	dex
	bne :-

	rts
.endproc

;*******************************************************************************
; SAVE
; Backs up the pointers for the active source so that they may be set for
; another source
.export __src_save
.proc __src_save
@save=r0
	; save the cursor position in the current buffer
	ldx activesrc

	lda zp::curx
	sta buffs_curx,x
	lda zp::cury
	sta buffs_cury,x

	; save the bank
	lda bank
	sta banks,x

	; save the data for the source we're switching from
	txa
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tay

	; save the buffer state
	ldx #$00
@l0:	lda buffstate,x
	sta savestate,y
	iny
	inx
	cpx #SAVESTATE_SIZE
	bne @l0

	rts
.endproc

;*******************************************************************************
; SET
; Sets the active source to the source in the given ID.
; IN:
;  - .A: ID of the source buffer we're switching to
; OUT:
;  - .C: set if the buffer could not be switched to
.export __src_set
.proc __src_set
@save=r0
	; set the pointers to those of the source we're switching to
	cmp numsrcs
	bcc @ok
	rts		; buffer doesn't exist; return with .C set

@ok:	tax

	sta activesrc
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tay

	; set the active bank ID
	lda banks,x
	sta bank

	; set the cursor position in the new source
	lda buffs_cury,x
	sta zp::cury
	lda buffs_curx,x
	sta zp::curx

	; restore the state for this buffer
	ldx #$00
@l0:	lda savestate,y
	sta buffstate,x
	iny
	inx
	cpx #SAVESTATE_SIZE
	bne @l0

	RETURN_OK
.endproc

;*******************************************************************************
; FIND_BANK
; Returns the ID of a free bank to store source in
; OUT:
;  - .A: the next available ID
;  - .C: set if no open bank was found
.proc find_bank
@free=r0
	lda #FINAL_BANK_SOURCE0
@l0:	ldx #$00
	stx @free
@l1:	cmp banks,x
	bne :+
	inc @free		; flag NOT free
:	inx
	cpx #MAX_SOURCES
	bne @l1
	ldx @free
	beq @found
	clc
	adc #$01
	cmp #FINAL_BANK_SOURCE0+MAX_SOURCES
	bcc @l0
	sec		; no open banks
	rts

@found:	RETURN_OK
.endproc

;*******************************************************************************
; NEW
; Initializes a new source buffer and sets it as the current buffer
; OUT:
;  - .C: set if the source could not be initialized (e.g. too many open
;        sources)
.export __src_new
.proc __src_new
	ldx numsrcs
	beq @init
	inx
	cpx #MAX_SOURCES
	bcc @saveold
	rts		; err, too many sources

@saveold:
	lda numsrcs
	jsr __src_save	; save current source data
@init:
	; clear the state for the new buffer
	ldx #SAVESTATE_SIZE-1
	lda #$00
:	sta buffstate-1,x
	dex
	bne :-

	; init line and lines to 1
	inc line
	inc lines

	lda numsrcs
	sta activesrc
	inc numsrcs
	pha

	; set name to 0 (unnamed)
	asl
	asl
	asl
	asl
	tax
	lda #$00
	sta names,x

	; mark the buffer as clean
	pla
	tax
	lda #$00
	sta flags,x

	jsr find_bank
	sta bank
	jsr __src_init_buff

	rts
.endproc

;*******************************************************************************
; BUFFER_BY_NAME
; Returns the ID of the buffer associated with the given filename. The carry
; is set if no buffer by the given name exists.
; IN:
;  - .XY: the name of the buffer to search for
; OUT:
;  - .A: the ID of the buffer; use src::set to make it the active buffer
;  - .C: set if no buffer was found by the given name
.export __src_buffer_by_name
.proc __src_buffer_by_name
@name=zp::str0
@other=zp::str2
@names=r0
@cnt=r2
@len=r3
	jsr str::len	; sets @name (str0) to .XY
	sta @len

	ldxy #names
	stxy @names
	lda #$ff
	sta @cnt

@l0:	inc @cnt
	lda @cnt
	cmp numsrcs
	bcs @notfound
	ldxy @names
	stxy @other
	lda @len
	jsr str::compare
	php
	lda @names
	clc
	adc #16
	sta @names
	bcc :+
	inc @names+1
:	plp
	bne @l0
	lda @cnt
	clc		; flag as FOUND
@notfound:
	rts
.endproc

;*******************************************************************************
; CURRENT_FILENAME
; Returns the filename for the active buffer
; OUT:
;  - .XY: the filename of the buffer or [NO NAME] if it has no name
;  - .C:  set if the file has no name ([NO NAME])
; CLOBBERS:
;  - r0-r1
.export __src_current_filename
.proc __src_current_filename
	lda __src_activebuff

	; fall through to __src_get_filename
.endproc

;*******************************************************************************
; GET_FILENAME
; Returns the filename for the given buffer
; IN:
;  - .A: the id of the buffer to get the name of
; OUT:
;  - .XY: the filename of the buffer or [NO NAME] if it has no name
;  - r0:  the filename (same as .XY)
;  - .C:  set if the file has no name ([NO NAME])
; CLOBBERS:
;  - r0-r1
.export __src_get_filename
.proc __src_get_filename
@out=r0
	asl			; * 16
	asl
	asl
	asl
	tax
	lda __src_names,x
	bne @named

@noname:
	ldxy #strings::noname
	RETURN_ERR ERR_UNNAMED_BUFFER

@named:	txa
	adc #<__src_names
	tax
	lda #>__src_names
	adc #$00
	tay
	;clc
	rts
.endproc

;*******************************************************************************
; ISDIRTY
; Returns .Z clear if the active buffer is dirty (has changed since it was last
; marked !DIRTY)
; OUT:
;  - .Z: clear if the buffer has changed since last flagged clean
.export __src_isdirty
.proc __src_isdirty
	ldy activesrc
	lda flags,y
	and #FLAG_DIRTY
	rts
.endproc

;*******************************************************************************
; ANYDIRTY
; Returns .Z clear if ANY buffer is dirty (has changed since it was last
; marked !DIRTY)
; OUT:
;  - .Z: clear if any buffer has changed since last flagged clean
.export __src_anydirty
.proc __src_anydirty
	ldy numsrcs
:	lda flags-1,y
	and #FLAG_DIRTY
	bne @done
	dey
	bne :-
@done:	rts
.endproc

;*******************************************************************************
; GET_FLAGS
; Returns the flags for the requested buffer
; IN:
;  - .A: the buffer to get flags for
; OUT:
;  - .A: the flags for the active buffer
.export __src_getflags
.proc __src_getflags
	tay
	lda flags,y
	rts
.endproc

;*******************************************************************************
; SET_FLAGS
; Sets the flags for the active source buffer
; IN:
;  - .A: the flags to set on the active source buffer
.export __src_setflags
.proc __src_setflags
	ldy activesrc
	sta flags,y
	rts
.endproc

;*******************************************************************************
; CLOSE
; Closes the active buffer. If the buffer being closed is the only one open,
; initializes a new buffer and makes it the active buffer.
; OUT:
;  - .C: set if a new buffer was created (the last buffer was closed)
.export __src_close
.proc __src_close
@cnt=r0
@end=r0
; get number of last byte to move (num_buffers)*2
	lda numsrcs
	beq @ok		; no buffer to close

	cmp #$01	; is the current buffer the last one?
	bne @close

	; only one buffer open; re-initialize it to "close" it
	dec numsrcs	; set num sources back to 0
	jsr __src_new	; and initialize the buffer
	sec		; flag that a new buffer was created/initialized
	rts

@close:
	; get the number of buffers to shift (numsrcs - activesrc - 1)
	lda numsrcs
	sbc #$01	; .C is set
	sbc activesrc
	beq @cont	; if this was the last buffer, skip shifting
	sta @cnt
	pha		; save this count for moving names

	; get offset to start shifting at
	lda activesrc
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tax

@l0:	; copy all the buffers' data down
	ldy #SAVESTATE_SIZE
:	lda savestate+SAVESTATE_SIZE,x
	sta savestate,x
	inx
	dey
	bne :-
	dec @cnt
	bne @l0

	; copy cursor data and banks down
	ldx activesrc
	inx
@l1:	lda banks,x
	sta banks-1,x
	lda buffs_curx,x
	sta buffs_curx-1,x
	lda buffs_cury,x
	sta buffs_cury-1,x
	inx
	cpx numsrcs
	bne @l1

	; copy the names down
	lda activesrc
	asl		; *2
	asl		; *4
	asl		; *8
	asl		; *16
	tax

	pla
	sta @cnt

@l2:	ldy #MAX_BUFFER_NAME_LEN
:	lda names+MAX_BUFFER_NAME_LEN,x
	sta names,x
	inx
	dey
	bne :-
	dec @cnt
	bne @l2

@cont:	; if there is no next buffer, open the previous
	dec numsrcs
	lda activesrc
	cmp numsrcs
	bcc :+
	dec activesrc
:	lda activesrc
	jsr __src_set
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; NAME
; Sets the name for the active buffer
; IN:
;  - .XY: the 0-terminated name to set for the active buffer
.export __src_name
.proc __src_name
@name=r0
	stxy @name
	lda activesrc
	asl
	asl
	asl
	asl			; *16
	tax
	ldy #$00
@l0:	lda (@name),y
	sta names,x
	beq @done
	inx
	iny
	cpy #MAX_BUFFER_NAME_LEN
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; PUSHP
; Pushes the current source position to an internal stack.
; OUT:
;   - .C: set on error (the stack is full)
; CLOBBERS:
;   - .A, .X
.export __src_pushp
.proc __src_pushp
	lda sp
	cmp #POS_STACK_SIZE-1
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	asl
	tax
	inc sp
	lda cursorzp
	sta stack,x
	lda cursorzp+1
	sta stack+1,x
	RETURN_OK
.endproc

;******************************************************************************
; POPP
; Returns the the most recent source position pushed in .YX
; OUT:
;   - .XY: the most recently pushed source position
;   - .C: set on error (the stack is empty)
.export __src_popp
.proc __src_popp
	lda sp
	bne :+
	RETURN_ERR ERR_STACK_UNDERFLOW

:	dec sp
	lda sp
	asl
	tax
	ldy stack+1,x
	lda stack,x
	tax
	RETURN_OK
.endproc

;*******************************************************************************
; CURR LINE
; Returns the line number that the source cursor is on
; OUT:
;   - .XY: the current line
.export __src_currline
.proc __src_currline
	ldxy line
	rts
.endproc

;*******************************************************************************
; END
; Returns .Z set if the cursor is at the end of the buffer.
; OUT:
;  - .Z: set if the cursor is at the end of the buffer
.export __src_end
.proc __src_end
	ldx poststartzp
	cpx end
	bne @done
	ldx poststartzp+1
	cpx end+1
@done:	rts
.endproc

;*******************************************************************************
; END_REP
; Returns .Z set if the current or next cursor position is at the end of the
; buffer.
; OUT:
;  - .Z: set if the cursor is at the end of the buffer
.export __src_end_rep
.proc __src_end_rep
	ldxy end
	sub16 poststartzp
	cpy #$00
	bne @done
	cpx #$00
	beq @done
	cpx #1		; set .Z if LSB is 1
@done:	rts
.endproc

;*******************************************************************************
; BEFORE END
; Checks if the source cursor is located just before the end of the buffer.
; OUT:
;  - .Z: set if the cursor is before the end of the buffer
.export __src_before_end
.proc __src_before_end
	ldxy end
	sub16 poststartzp
	cmpw #1
	rts
.endproc

;*******************************************************************************
; POS
; Returns the current source position.  You may go to this position with the
; src::goto routine.  Note that if the source changes since this procedure is
; called, this may not be the same (or expected) position
; OUT:
;  - .XY: the current source position
.export __src_pos
.proc __src_pos
	ldxy cursorzp
	rts
.endproc

;*******************************************************************************
; BACKSPACE
; Deletes the character immediately before the current cursor position.
; OUT:
;  - .A: the character that was deleted
;  - .C: set if the backspace failed (we're at the START of the source)
.export __src_backspace
.proc __src_backspace
	jsr __src_mark_dirty
	jsr __src_start
	beq @skip
	jsr __src_atcursor
	pha
	cmp #$0d
	bne :+
	decw line
	jsr on_line_deleted
:	decw cursorzp
	pla
	clc
	rts
@skip:	sec
	rts
.endproc

;*******************************************************************************
; ON LINE DELETED
; Callback to handle a line deletion. Various state needs to be shifted when
; this occurs (breakpoints for now, TODO: debug info)
.proc on_line_deleted
	decw lines

	; update debug info: find all line programs in the current file with
	; start lines greater than the current line and decrement them
	; TODO:
	;jsr dbgi::delete_line

	; shift breakpoints
	jsr edit::currentfile
	sta r0
	lda #$01
	jmp dbg::shift_breakpointsu
.endproc

;*******************************************************************************
; DELETE
; Deletes the character at the current cursor position.
; OUT:
;  - .A: the character that was deleted
;  - .C: set if there is nothing to delete (cursor is at END)
.export __src_delete
.proc __src_delete
	jsr __src_end
	beq @skip

	jsr __src_mark_dirty
	jsr __src_after_cursor
	cmp #$0d
	bne :+
	jsr on_line_deleted
:	incw poststartzp
	clc
	rts
@skip:	sec
	rts
.endproc


;*******************************************************************************
; HOME
; Moves left until at the start of the source buffer or the start of the line
.export __src_home
.proc __src_home
:	jsr __src_left
	bcc :-
	rts
.endproc

;*******************************************************************************
; LINE_END
; Moves right until at the end of the source buffer or the end of the line
.export __src_line_end
.proc __src_line_end
:	jsr __src_right
	bcc :-
	rts
.endproc

;*******************************************************************************
; LEFT
; Moves left to the previous character unless it is a newline
; OUT:
;  - .C: set if the source cursor was unmoved
;  - .A: the character at the new source cursor position
.export __src_left
.proc __src_left
	jsr __src_prev
	bcs @nomove
	jsr __src_after_cursor
	cmp #$0d
	bne @done
	jsr __src_next
@nomove:
	sec
	rts
@done:	RETURN_OK
.endproc

;*******************************************************************************
; RIGHT
; Moves to the next character unless it is a newline
; OUT:
;  - .C: set if the cursor wasn't moved, clear if it was
;  - .A: the character at the position that was moved to
.export __src_right
.proc __src_right
	jsr __src_end
	beq @endofline

	jsr __src_next
	cmp #$0d
	bne @done
	jsr __src_prev
@endofline:
	sec
	rts
@done:	RETURN_OK
.endproc

;*******************************************************************************
; RIGHT_REP
; Moves to the next character unless a newline exists AFTER the destination
; OUT:
;  - .C: set if the cursor wasn't moved, clear if it was
;  - .A: the character at the position that was moved to
.export __src_right_rep
.proc __src_right_rep
	jsr __src_before_end
	beq @endofline
	jsr __src_end		; should be impossible in REPLACE
	beq @endofline

	jsr __src_after_cursor	; if we're at end of line, don't move
	cmp #$0d
	beq @endofline
	jsr __src_next
	jsr __src_after_cursor	;if moving would put us at end of line, stay
	bcs @back
	cmp #$0d
	bne @done
@back:	jsr __src_prev
@endofline:
	sec
	rts
@done:	RETURN_OK
.endproc

;*******************************************************************************
; UP
; Moves the cursor back one line or to the start of the buffer if it is
; already on the first line
; this will leave the cursor on the first newline character encountered while
; going backwards through the source.
; OUT:
;  - .A: the character at the cursor position
;  - .C: set if cursor is at the start of the buffer
.export __src_up
.proc __src_up
	jsr __src_start
	bne @l0
	sec
@beginning:
	rts

@l0:	jsr __src_prev
	bcs @beginning
	cmp #$0d
	bne @l0
	RETURN_OK
.endproc

;*******************************************************************************
; DOWN
; Moves the cursor beyond the next RETURN character (or to the end of
; the buffer if there is no such character
; OUT:
;  - .C: set if the end of the buffer was reached (cannot move "down")
.export __src_down
.proc __src_down
	jsr __src_end
	beq @eof
@l0:	jsr __src_next
	cmp #$0d
	beq @ok
	jsr __src_end
	bne @l0
@eof:	sec	; end of the buffer
	rts
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; INSERT_LINE
; Inserts the given string into the source at the current cursor position.
; IN:
;  - .XY: the string to insert
.export __src_insertline
.proc __src_insertline
@str=zp::tmp12
@offset=zp::tmp14
	stxy @str
	lda #$00
	sta @offset

@l0:	ldy @offset
	lda (@str),y
	beq @done
	jsr __src_insert
	inc @offset
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; REPLACE
; Adds the character in .A to the buffer at the cursor position,
; replacing the character that currently resides there
; IN:
;  - .A: the character to replace the existing one with
; OUT:
;  - .C: set if there is nothing to replace
.export __src_replace
.proc __src_replace
	pha
	jsr __src_delete
	pla
	bcs @ret
	jsr __src_insert
	jsr __src_left
	clc
@ret:	rts
.endproc

;*******************************************************************************
; GAPLEN
; Returns the length of the gap
; OUT:
;  - .XY: the length of the gap
.proc gaplen
	ldxy poststartzp
	sub16 cursorzp
	rts
.endproc

;*******************************************************************************
; BEFORE_NEWL
; Checks if src::aftercursor is a newline ($0d)
; OUT:
;  - .Z: set if src::aftercursor == $0d
.export __src_before_newl
.proc __src_before_newl
	jsr __src_after_cursor
	cmp #$0d
	rts
.endproc

;*******************************************************************************
; AFTER CURSOR
; Returns the character AFTER the cursor position.
; OUT:
;  - .A: the character after the current cursor position
;  - .C: set if there is no character after the cursor
.export __src_after_cursor
.proc __src_after_cursor
	jsr __src_end
	beq @end
	jsr __src_next
	pha
	jsr __src_prev
	pla
	RETURN_OK

@end:	sec		; end of buffer
	rts
.endproc

;*******************************************************************************
; REWIND
; Moves the cursor back to the start of the buffer
.export __src_rewind
.proc __src_rewind
@l0:	jsr __src_prev
	bcc @l0
	rts
.endproc

;*******************************************************************************
; READLINE
; Reads one line at the cursor positon and advances the cursor
; OUT:
;  - .A: the length of the line
;  - mem::linebuffer: the line that was read will be 0-terminated
;  - .C: set if the end of the source was reached
.export __src_readline
.proc __src_readline
@cnt=r4
	lda #$00
	sta mem::linebuffer	; initialize the buffer
	sta @cnt

	jsr __src_end
	beq @eofdone

@l0:	jsr __src_next
	ldx @cnt
	cmp #$0d
	bne @store
	lda #$00

@store:	cpx #LINESIZE
	bcs :+			; if we have >= LINESIZE characters don't store
	sta mem::linebuffer,x

:	cmp #$00
	beq @done
	inc @cnt
	jsr __src_end
	bne @l0
@eof:	; go back a character; read last byte and null terminate if end of source
	ldx @cnt
	lda #$00
	sta mem::linebuffer,x
@eofdone:
	lda @cnt
	sec
	rts
@done:	txa
	clc
	rts
.endproc

;*******************************************************************************
; POPGOTO
; Navigates to the the most recent source position pushed in .YX
.export __src_popgoto
.proc __src_popgoto
	jsr __src_popp
	bcc __src_goto
	rts
.endproc

;*******************************************************************************
; GOTO
; Goes to the source position given
; IN:
;  - .XY: the source position to go to (see src::pos, src::pushp, src::popp)
.export __src_goto
.proc __src_goto
@dest=r4
	cmpw cursorzp
	beq @done
	stxy @dest
	bcc @backwards

@forwards:
	jsr __src_end
	beq @done
	jsr __src_next
	lda cursorzp
	cmp @dest
	bne @forwards
	lda cursorzp+1
	cmp @dest+1
	bne @forwards
	rts

@backwards:
	jsr __src_start
	beq @done
	jsr __src_prev
	lda cursorzp
	cmp @dest
	bne @backwards
	lda cursorzp+1
	cmp @dest+1
	bne @backwards
@done:  rts
.endproc

;*******************************************************************************
; GET
; Returns the text at the current cursor position in mem::linebuffer
; OUT:
;  - mem::linebuffer: a line of text from the cursor position
;  - .C: set if the end of the buffer was reached as we were reading
.export __src_get
.proc __src_get
	ldxy #mem::linebuffer

	; fall through to copy_line
.endproc

;*******************************************************************************
; copy line
; returns the text at the current cursor position and stores it to the given
; target location
; in:
;  - .xy: the destination to copy to
;  - r1:  the destination to copy to
; out:
;  - (.xy): a line of text from the cursor position
.proc copy_line
@src=zp::bankaddr0
@target=zp::bankaddr1
	stxy @target		; dest
	ldxy poststartzp
	stxy @src		; source

	jsr __src_on_last_line	; on last line already?
	bne :+
	ldxy end
	sub16 poststartzp	; bytes to copy
	txa
	pha
	tay			; .Y = bytes to copy
	dey
	lda __src_bank
.ifdef ultimem
	.import src_copyline
	jsr src_copyline
.else
	jsr ram::copyline	; may copy garbage
.endif

	pla			; restore end of line index
	tay
	jmp @done

:	lda __src_bank
	jsr ram::copyline

@done:	lda #$00
	sta (@target),y
	RETURN_OK
.endproc

;*******************************************************************************
; DOWNN
; Advances the source by the number of lines in .XY
; IN:
;   - .XY: the number of lines to advance
; OUT:
;   - .C: set if the end was reached before the total lines requested could be
;   - .YX: the number of lines that were not read
;        reached
.export __src_downn
.proc __src_downn
@cnt=r4
	stxy @cnt
@loop:	ldxy @cnt
	decw @cnt
	txa
	bne :+
	tya
	beq @done
:	jsr __src_down
	bcc @loop
@done:	ldxy @cnt
	rts
.endproc

;*******************************************************************************
; UPN
; Advances the source by the number of lines in .XY
; IN:
;  - .XY: the number of lines to move "up"
; OUT:
;  - .XY: contains the number of lines that were not read
;  - .C: set if the beginning was reached before the total lines requested could
;        be reached
.export __src_upn
.proc __src_upn
@cnt=r4
	stxy @cnt
@loop:	ldxy @cnt
	decw @cnt
	cmpw #$0000
	beq @done
	jsr __src_up
	bcc @loop
@done:	ldxy @cnt
	rts
.endproc

;*******************************************************************************
; ON_LAST_LINE
; Checks if the cursor is on the last line of the active source buffer.
; OUT:
;  - .Z: set if the cursor is on the last line of the active source
.export __src_on_last_line
.proc __src_on_last_line
	ldxy line
	cmpw lines
	rts
.endproc

;*******************************************************************************
; MARK DIRTY
; Marks the given buffer as "dirty" by setting its appropriate flag.
; IN:
;  - .A: the buffer ID to flag as DIRTY
.export __src_mark_dirty
.proc __src_mark_dirty
	lda #FLAG_DIRTY
	ldx activesrc
	sta flags,x
	rts
.endproc
