.include "expansion.inc"
.include "../config.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../edit.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../ram.inc"
.include "../zeropage.inc"

.import __src_bank
.import __src_get_filename
.import __src_mark_dirty
.import __src_on_last_line

.macpack longbranch

buffstate   = zp::srccur
cursorzp    = zp::srccur
poststartzp = zp::srccur2
line        = zp::srcline
lines       = zp::srclines
end         = zp::srcend

BUFFER_SIZE = $6000	; max size of buffer
GAPSIZE     = $100	; size of gap in gap buffer

;*******************************************************************************
; DATA
; This buffer holds the text data.  It is a large contiguous chunk of memory
.segment "SOURCE"
.assert * & $ff = 0, error, "source buffers must be page aligned"
data: .res BUFFER_SIZE

.CODE

;*******************************************************************************
; INIT BUFF
; Initializes a new source buffer by setting its pointers to the
; start/end of the gap
.export __src_init_buff
.proc __src_init_buff
	lda #<data
	sta cursorzp
	lda #>data
	sta cursorzp+1

	lda #<(data+GAPSIZE)
	sta end
	sta poststartzp
	lda #>(data+GAPSIZE)
	sta end+1
	sta poststartzp+1
	rts
.endproc

;*******************************************************************************
; INSERT
; Adds the character in .A to the buffer at the gap position (gap).
; If the character is not valid, it is not inserted, but the operation is
; still considereed a success (.C is returned clear)
; IN:
;  - .A: the character to insert
; OUT:
;  - .C: set if the character could not be inserted (buffer full)
; CLOBBERS:
;  - $120-$130: may be clobbered if newline is inserted
.export __src_insert
.proc __src_insert
	cmp #$0d
	beq :+
	cmp #$0a
	beq :+
	cmp #$09
	beq :+
	cmp #$20
	jcc @done
	cmp #$80
	jcs @done	; not displayable

:	pha		; save char to insert

	jsr __src_mark_dirty
	jsr gaplen
	cmpw #0		; is gap closed?
	bne @ins	; no, insert as usual

	; check if there is room to expand the gap
	lda poststartzp+1
	cmp #>(BUFFER_SIZE+data)-1	; -1 to save space for a $100 byte gap
	bcc @ok

@err:	; buffer overflow, cannot insert character
	pla				; clean stack
	lda #ERR_BUFFER_FULL
	rts

@ok:	; gap is closed, create a new one
	; copy data[poststart] to data[poststart + GAPSIZE]
	ldxy cursorzp
	stxy ram::src

	inc poststartzp+1
	inc end+1		; increase size by $100
	ldxy poststartzp
	stxy ram::dst

	; get number of bytes to copy
	ldxy end
	sub16 poststartzp

	lda __src_bank
	sta ram::src+2
	sta ram::dst+2
	jsr ram::copy

@ins:	pla
	ldy cursorzp+1
	bmi @done	; out of range

.ifndef ultimem
	sta24 __src_bank, cursorzp
.else
	jsr insert
.endif
	cmp #$0d
	bne @insdone

	incw line
	jsr on_line_inserted
	incw lines
	lda #$ff
	sta zp::srcx

@insdone:
	inc zp::srcx
	incw cursorzp
@done:	RETURN_OK
.endproc

.PUSHSEG
.RODATA

;*******************************************************************************
; SYNC X
; Syncs the zp::srcx based on the distance from the start of the line or buffer
.export sync_x
.proc sync_x
@cur=r0
@x=r2
	jsr activate_source
	ldxy cursorzp
	stxy @cur

	ldy #$00
	sty @x

@l0:	lda @cur
	cmp #<data
	bne :+
	lda @cur+1
	cmp #>data
	beq @done

:	decw @cur
	lda (@cur),y
	cmp #$0d
	beq @done
	inc @x
	bne @l0			; branch always

@done:	lda @x
	sta zp::srcx
	jsr deactivate_source
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
	; all paths will reset cursor "column" to 0
	lda #$00
	sta zp::srcx

	jsr activate_source

	; if MSB of (end-poststart) > 0, use $ff for counter
	; if they're equal, use the LSB of difference as the counter
	lda end
	sec
	sbc poststartzp
	tax
	lda end+1
	sbc poststartzp+1
	beq :+
	ldx #$ff

:	ldy #$00
@l0:	lda (poststartzp),y
	sta (cursorzp),y
	iny
	cmp #$0d
	beq @ok

	; check if we're at the end of the buffer
	dex
	bne @l0

@eof:	jsr deactivate_source
	sec			; end of the buffer
	rts

@ok:	incw line
	tya
	clc
	adc poststartzp
	sta poststartzp
	bcc :+
	inc poststartzp+1
	clc

:	tya
	adc cursorzp
	sta cursorzp
	bcc :+
	inc cursorzp+1
	clc
:	jmp deactivate_source
.endproc

;*******************************************************************************
; UPN
; Advances the source by the number of lines in .XY
; IN:
;  - .XY: the number of lines to move "up"
; OUT:
;  - .C: set if the beginning was reached before the total lines requested could
;        be reached
.export __src_upn
.proc __src_upn
@cnt=r4
	stxy @cnt
	jsr activate_source
@loop:	lda @cnt
	bne :+
	dec @cnt+1
	bmi @done
:	dec @cnt
	jsr up
	bcc @loop
@done:	jmp deactivate_source
.endproc

;*******************************************************************************
; UP
; Moves the cursor back one line or to the start of the buffer if it is
; already on the first line
; this will leave the cursor on the first newline character encountered while
; going backwards through the source.
; OUT:
;  - .C: set if cursor is at the start of the buffer
.export __src_up
.proc __src_up
	jsr activate_source
	jsr up
	jmp deactivate_source
.endproc

;*******************************************************************************
; UP
; Helper for UP/UPN
; Moves the cursor back one line or to the start of the buffer if it is
; already on the first line
; this will leave the cursor on the first newline character encountered while
; going backwards through the source.
; OUT:
;  - .C: set if cursor is at the start of the buffer
.proc up
	; all paths will reset cursor "column" to 0
	lda #$00
	sta zp::srcx

	; if MSB of of cursorzp > (>data), use max value for counter ($ff)
	; if MSB is >data, use the LSB of the cursorzp pointer as counter
	lda cursorzp
	sec
	sbc #<data
	tax
	lda cursorzp+1
	sbc #>data
	beq :+
	ldx #$ff

:	txa
	beq @eof		; cursorzp == data
	decw cursorzp
	decw poststartzp

	ldy #$00
	lda (cursorzp),y
	sta (poststartzp),y
	cmp #$0d
	bne :+
	decw line

:	dex
	beq @eof

@l0:	decw cursorzp
	decw poststartzp
	lda (cursorzp),y
	sta (poststartzp),y
	cmp #$0d
	beq @done
	dex
	bne @l0

@eof:	jsr deactivate_source
	sec				; end of the buffer
	rts

@done:	; increment pointers once (we want to end just before the newline
	incw cursorzp
	incw poststartzp
	RETURN_OK
.endproc

;******************************************************************************
; NEXT
; Moves the cursor up one character in the gap buffer
; OUT:
;  - .A: the character at the new cursor position in .A
;  - .C: clear on success (always clear)
.export __src_next
.proc __src_next
	; do __src_end inline to save the cycles from JSR and RTS
	ldx poststartzp
	cpx end
	bne @cont
	ldx poststartzp+1
	cpx end+1
	beq @done

@cont:	; switch to the bank that contains the source buffer's data
	jsr activate_source

	; move one byte from the end of the gap to the start
	ldy #$00
	lda (poststartzp),y
	sta (cursorzp),y

	incw cursorzp
	incw poststartzp

	; switch back to main bank
	jsr deactivate_source

	cmp #$0d
	bne @done
	incw line

	ldx #$ff
	stx zp::srcx		; reset cursor "column"

@done:	inc zp::srcx		; move to next "column"
	RETURN_OK
.endproc

;*******************************************************************************
; PREV
; Moves the cursor back one character in the gap buffer.
; NOTE: srcx will not be accurate if a newline is crossed by this routine.
; OUT:
;  - .A: the character at the new cursor position (if not at the start of buff)
;  - .C: set if we're at the start of the buffer and couldn't move back
.export __src_prev
.proc __src_prev
	jsr __src_start
	bne :+
	jsr __src_atcursor
	sec
	rts

:	; move char from start of gap to the end of the gap
	decw cursorzp
	decw poststartzp

	; switch to the bank that contains the source buffer's data
	jsr activate_source

	; move one byte from the start of the gap to the end
	ldy #$00
	lda (cursorzp),y
	sta (poststartzp),y

	cmp #$0d
	bne :+
	decw line

:	dec zp::srcx		; decrement cursor "column"
	bpl @done
	inc zp::srcx

@done:	; get the character at the new cursor position
	decw cursorzp
	lda (cursorzp),y
	incw cursorzp

	jsr deactivate_source

	RETURN_OK
.endproc

;*******************************************************************************
; INSERT ON LOAD
; Inserts a character into a buffer that is known to be "clean"
; That means the user has not added breakpoints, debug-info, etc.
; This should be used when loading a source file but not otherwise.
; The reason this procedure must be used when inserting before the file is
; loaded is that the association between filename and debug-info doesn't yet
; exist, but this association is required to do the extra logic in the
; aforementioned cases.
; IN:
;  - .A: the character to insert
; OUT:
;  - .C: set if the character could not be inserted (buffer full)
.export __src_insert_on_load
.proc __src_insert_on_load
	cmp #$0a
	bne :+
	lda #$0d
:	cmp #$0d
	bne :+
	incw lines
	bne @store		; branch always

:	cmp #$09
	beq @store
	cmp #$20
	bcc @done
	cmp #$80
	bcs @done		; not displayable, don't insert

@store:	pha
	jsr activate_source
	pla
	ldy #$00
	sta (end),y
	incw end
	jsr deactivate_source
@done:	RETURN_OK
.endproc
.POPSEG

;*******************************************************************************
; START
; Returns .Z set if the cursor is at the start of the buffer.
; OUT:
;  - .Z: set if the cursor is at the start of the buffer
.export __src_start
.proc __src_start
	ldx cursorzp
	bne @done	; if LSB is !0, not the start
	ldx cursorzp+1
	cpx #>data
@done:	rts
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
; ON LINE INSERTED
; Callback to handle a line insertion. Various state needs to be shifted when
; this occurs (breakpoints, etc.)
.proc on_line_inserted
	; TODO:
	; update debug info: find all line programs in the current file with
	; start lines greater than the current line and increment those
	jsr __src_get_filename
	jsr dbgi::getfileid

	; shift breakpoints
	jsr edit::currentfile
	sta r0
	lda #$01
	jmp dbg::shift_breakpointsd
.endproc

.ifdef ultimem
.segment "BANKCODE"
.endif
;*******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
.proc __src_atcursor
	decw cursorzp
.ifndef ultimem
	lda24 __src_bank, cursorzp
.else
	jsr activate_source
	ldy #$00
	lda (cursorzp),y
	jsr deactivate_source
.endif
	incw cursorzp
	rts
.endproc

.ifdef ultimem
;*******************************************************************************
; ACTIVATE SOURCE
.proc activate_source
	; bank in the source buffer
	ldx __src_bank
	stx $9ff8	; BLK1 = base of source bank
	inx
	stx $9ffa	; BLK2 = source base bank + 1
	inx
	stx $9ffc	; BLK3 = source base bank + 2
	ldx #$7f
	stx $9ff2	; RAM in BLK 1/2/3
	rts
.endproc

;*******************************************************************************
; INSERT
.proc insert
	jsr activate_source
	ldy #$00
	sta (cursorzp),y

	; fall through to deactivate_source
.endproc

;*******************************************************************************
; DEACTIVATE SOURCE
.proc deactivate_source
	; restore MAIN bank
	ldx #$01
	stx $9ff8
	inx
	stx $9ffa
	inx
	stx $9ffc
	ldx #$55		; ROM in BLK 1/2/3
	stx $9ff2
	rts
.endproc

.segment "BANKCODE"
;*******************************************************************************
; COPY LINE
.export src_copyline
.proc src_copyline
@src=zp::bankaddr0
@target=zp::bankaddr1
	jsr activate_source
	cpy #LINESIZE
	bcc :+
	ldy #LINESIZE-1
:	COPY_Y @src, @target
	jmp deactivate_source
.endproc
.endif
