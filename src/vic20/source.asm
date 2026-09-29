.include "expansion.inc"
.include "../config.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../edit.inc"
.include "../errlog.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../ram.inc"
.include "../zeropage.inc"

.import __src_bank
.import __src_get_filename
.import __src_mark_dirty
.import __src_on_last_line

.import append
.import atcursor_prev
.import bank_next
.import cross_next
.import cross_prev
.import find_prev
.import split

.macpack longbranch

buffstate   = zp::srccur
cursorzp    = zp::srccur
poststartzp = zp::srccur2
line        = zp::srcline
lines       = zp::srclines
end         = zp::srcend
srctmp      = zp::srctmp

;*******************************************************************************
; NOTE: must match source.asm
POOL_TAIL = $fe
next_of   = bank_next-FINAL_BANK_SOURCE0

;*******************************************************************************
; each segment is 1 bank mapped to BLK1; last 2 bytes are length while inactive
BUFFER_SIZE = $2000-2	; max size of a segment
GAPSIZE     = $100	; size of gap in gap buffer

;*******************************************************************************
; where destination of a copy between segments is mapped (BLK2)
COPY_DST = $4000

;*******************************************************************************
; DATA
; This buffer holds the text data.  It is a large contiguous chunk of memory
.segment "SOURCE"
.assert * & $ff = 0, error, "source buffers must be page aligned"
data:   .res BUFFER_SIZE
seglen: .word 0

.export __src_data := data

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
	RETURN_OK
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
	; (the expansion moves [poststart, end) up by $100, so it is END that
	; must stay below the top of the buffer)
	lda end+1
	cmp #>(BUFFER_SIZE+data)-1	; -1 to save space for a $100 byte gap
	bcc @ok

	; segment is full; split it
	jsr split
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

	jsr insert
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

	CALLMAIN errlog::inserted
	rts
.endproc

;*******************************************************************************
; The following routines run outside BLK1 and map source segments into BLK1.
.RODATA

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

@chunk:	; if MSB of (end-poststart) > 0, use $ff for counter
	; if they're equal, use the LSB of difference as the counter
	lda end
	sec
	sbc poststartzp
	tax
	lda end+1
	sbc poststartzp+1
	beq @small
	ldx #$ff
	bne @go			; branch always

@small:	cpx #$00
	bne @go

	; this segment is exhausted; continue in the next one
	jsr deactivate_source
	jsr cross_next
	bcs @eof		; end of the buffer
	jsr activate_source
	jmp @chunk

@go:	ldy #$00
@l0:	lda (poststartzp),y
	sta (cursorzp),y
	iny
	cmp #$0d
	beq @ok

	; check if we've exhausted this chunk
	dex
	bne @l0

	; no newline in this chunk; advance past the copied bytes and
	; continue with the next chunk
	jsr @advance
	jmp @chunk

@eof:	sec			; end of the buffer
	rts

@ok:	incw line
	jsr @advance
	jsr deactivate_source
	jsr at_seg_end
	bne :+
	jsr cross_next
:	clc			; success
	rts

@advance:
	; move both gap pointers past the .Y bytes that were copied
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
:	rts
.endproc

;*******************************************************************************
; AT SEG END
; OUT:
;  - .Z: set if there is no after-gap text in the active segment
.proc at_seg_end
	ldx poststartzp
	cpx end
	bne :+
	ldx poststartzp+1
	cpx end+1
:	rts
.endproc

;*******************************************************************************
; NEXT
; Moves the cursor up one character in the gap buffer
; OUT:
;  - .A: the character at the new cursor position in .A
;  - .C: clear on success (always clear)
.export __src_next
.proc __src_next
	ldx poststartzp
	cpx end
	bne @move
	ldx poststartzp+1
	cpx end+1
	beq @done
@move:

	; switch to the bank that contains the source buffer's data
	jsr activate_source

	; move one byte from the end of the gap to the start
	ldy #$00
	lda (poststartzp),y
	sta (cursorzp),y

	incw cursorzp
	incw poststartzp

	; switch back to main bank
	jsr deactivate_source

	ldx poststartzp
	cpx end
	bne :+
	ldx poststartzp+1
	cpx end+1
	bne :+
	pha
	jsr cross_next
	pla

:	cmp #$0d
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
	lda cursorzp
	bne @cont
	lda cursorzp+1
	cmp #>data
	bne @cont
	jsr cross_prev
	bcc @cont
	jsr __src_atcursor
	sec
	rts

@cont:	; move char from start of gap to the end of the gap
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

@done:	; read the new cursor character and restore MAIN
	jsr atcursor_mapped
	RETURN_OK
.endproc

;*******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
.proc __src_atcursor
	jsr activate_source
	ldy #$00

	; fall through to atcursor_mapped
.endproc

;*******************************************************************************
; ATCURSOR MAPPED
; Reads the byte before cursorzp, or the previous segment's last byte when
; cursorzp is at the segment start. Restores MAIN before returning.
; Assumes the active source segment is mapped to BLK1.
; IN:
;  - .Y:   zero
; OUT:
;  - .A: the character read, or zero at the start of the buffer
.proc atcursor_mapped
	lda cursorzp
	bne @local
	lda cursorzp+1
	cmp #>data
	bne @local
	jsr deactivate_source
	jmp atcursor_prev

@local:	decw cursorzp
	lda (cursorzp),y
	incw cursorzp
	jmp deactivate_source
.endproc

;*******************************************************************************
; ACTIVATE SOURCE
; Maps the active segment into BLK1
; CLOBBERS:
;  - .X
.proc activate_source
	ldx __src_bank
	; fall through to map_bank
.endproc

;*******************************************************************************
; MAP BANK
; Maps the given bank into BLK1
; IN:
;  - .X: the bank to map
; CLOBBERS:
;  - .X
.proc map_bank
	stx $9ff8
	ldx #$57
	stx $9ff2	; RAM in BLK1
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
; Restores the MAIN bank to BLK1
; CLOBBERS:
;  - none (.C is preserved)
.proc deactivate_source
	pha
	lda #$01
	sta $9ff8
	lda #$55		; ROM in BLK 1/2/3
	sta $9ff2
	pla
	rts
.endproc

;*******************************************************************************
; SYNC X
; Syncs the zp::srcx based on the distance from the start of the line or buffer
.export sync_x
.proc sync_x
@cur=r0
@x=r2
@bank=r3
	lda __src_bank
	sta @bank
	ldxy cursorzp
	stxy @cur
	lda #$00
	sta @x

@seg:	ldx @bank
	jsr map_bank
	ldy #$00
@l0:	lda @cur
	bne @scan
	lda @cur+1
	cmp #>data
	bne @scan

	; start of this segment; continue in the previous one
	jsr deactivate_source
	ldx @bank
	jsr find_prev
	bcs @done
	sta @bank
	jsr __src_seg_len
	stx @cur
	tya
	clc
	adc #>data
	sta @cur+1
	jmp @seg

@scan:	decw @cur
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
; SEG SET LEN
; Stores the length of an inactive segment
; IN:
;  - .A:  the segment's bank
;  - .XY: its length
.export __src_seg_set_len
.proc __src_seg_set_len
	pha
	txa
	pha
	tsx
	lda $0102,x
	tax
	jsr map_bank
	pla
	sta seglen
	sty seglen+1
	pla

	jmp deactivate_source
.endproc

;*******************************************************************************
; SEG PEEK
; Reads a byte of a segment
; IN:
;  - .A:  the segment's bank
;  - .XY: the offset to read
; OUT:
;  - .A: the byte read
.export __src_seg_peek
.proc __src_seg_peek
@ptr=zp::bankaddr0
	.assert <data = 0, error, "data must be page aligned"
	pha
	stx @ptr
	tya
	clc
	adc #>data
	sta @ptr+1
	pla
	tax
	jsr map_bank
	ldy #$00
	lda (@ptr),y
	jmp deactivate_source
.endproc

.segment "SRCCODE"
.import __SRCCODE_RUN__
.assert __SRCCODE_RUN__ >= $4000, error, "SRCCODE must not be in BLK1"

;*******************************************************************************
; UPN
; Advances the source by the given number of lines.
; IN:
;  - .XY: the number of lines to move "up"
; OUT:
;  - .C: set if the beginning was reached before total lines requested could
;        be reached
; Runs from BLK5 while source segments are mapped into BLK1.
.pushseg
.RODATA
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
.popseg

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
; Must be called with the source activated.
; OUT:
;  - .C: set if cursor is at the start of the buffer
.proc up
	; all paths will reset cursor "column" to 0
	lda #$00
	sta zp::srcx

	jsr @avail
	bcs @eof
	decw cursorzp
	decw poststartzp
	lda (cursorzp),y
	sta (poststartzp),y
	cmp #$0d
	bne @chunk
	decw line

;-------------------------------------------------------------------------------
@chunk:	jsr @avail
	bcs @eof
@l0:	decw cursorzp
	decw poststartzp
	lda (cursorzp),y
	sta (poststartzp),y
	cmp #$0d
	beq @done
	dex
	bne @l0
	beq @chunk		; branch always

@eof:	rts

;-------------------------------------------------------------------------------
@done:	; increment pointers once (we want to end just before the newline
	incw cursorzp
	incw poststartzp

	; the newline may have ended its segment
	jsr at_seg_end
	bne :+
	jsr deactivate_source
	jsr cross_next
	jsr activate_source
:	RETURN_OK

;-------------------------------------------------------------------------------
; .X = bytes before the cursor in this segment (max $ff), moving to the
; previous segment if there are none. .C is set at the start of the buffer.
@avail:	lda cursorzp
	sec
	sbc #<data
	tax
	lda cursorzp+1
	sbc #>data
	beq :+
	ldx #$ff
:	ldy #$00
	txa
	bne @ok

	jsr deactivate_source
	jsr cross_prev

	php
	jsr activate_source
	plp
	bcc @avail
	rts

@ok:	clc
	rts
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

@store:	; make sure there is room for the character
	ldy end+1
	cpy #>(BUFFER_SIZE+data)
	bcc @room

	; segment is full; continue in a new one
	pha
	jsr append
	pla
	bcs @full

@room:	pha
	jsr activate_source
	pla
	ldy #$00
	sta (end),y
	incw end
	jsr deactivate_source
@done:	RETURN_OK

@full:	lda #ERR_BUFFER_FULL
	sec
	rts
.endproc

;*******************************************************************************
; GET IN
; Reads the line at the cursor into the given address
; IN:
;  - .XY: destination to copy to
; OUT:
;  - (.XY): a line of text from the cursor position (0-terminated)
.export __src_getin
.proc __src_getin
	lda #LINESIZE
	; fall through to the bank-spanning reader
.endproc

;*******************************************************************************
; READSPAN
; Reads the given number of bytes to the given destination.
; IN:
;   - .A: maximum characters
;   - .XY: destination to read to
.export __src_readspan
.proc __src_readspan
@max=zp::banktmp
@src=zp::bankaddr0
@dst=zp::bankaddr1
@end=srctmp
@bank=srctmp+2
	sta @max
	stxy @dst
	ldxy poststartzp
	stxy @src
	ldxy end
	stxy @end
	lda __src_bank
	sta @bank
	jsr activate_source

	ldy #$00
@chunk:
	; @src includes a -Y offset; .Y indexes both source and destination
	; across segment boundaries.
	lda @end
	sec
	sbc @src
	tax
	lda @end+1
	sbc @src+1
	bne @full
	cpx @max
	bcc @limit

@full:	ldx @max
@limit:	stx srctmp+3
	cpy srctmp+3
	beq @cross

@get:	lda (@src),y
	cmp #$0d
	beq @done
	sta (@dst),y
	iny
	cpy srctmp+3
	bcc @get
	cpy @max
	beq @done

;-------------------------------------------------------------------------------
@cross:	ldx @bank
	lda next_of,x
	cmp #POOL_TAIL
	bcs @done
	sta @bank
	tax
	jsr map_bank
	lda seglen
	sta @end
	lda seglen+1
	clc
	adc #>data
	sta @end+1
	; Bias the new source pointer by -Y without moving the destination.
	tya
	eor #$ff
	sec
	adc #<data
	sta @src
	lda #>data-1
	adc #$00
	sta @src+1
	jmp @chunk

@done:	lda #$00
	sta (@dst),y
	jsr deactivate_source
	RETURN_OK
.endproc

;*******************************************************************************
; SEG LEN
; Returns the stored length of an inactive segment
; IN:
;  - .A: the segment's bank
; OUT:
;  - .XY: its length
.export __src_seg_len
.proc __src_seg_len
	tax
	jsr map_bank
	ldx seglen
	ldy seglen+1
	jmp deactivate_source
.endproc

;*******************************************************************************
; SEG CLOSE
; Closes the active segment's gap and stores its length
; OUT:
;  - .XY: segment's length
;  - end: end of the text (other pointers are invalidated)
.export __src_seg_close
.proc __src_seg_close
@src=zp::bankaddr0
@dst=zp::bankaddr1
	jsr activate_source

	ldxy cursorzp
	cmpw poststartzp
	beq @nogap

	; move [poststart, end) down to the cursor
	stxy @dst
	ldxy poststartzp
	stxy @src
	jsr copy_to_end
	stxy end

@nogap:	lda end
	sta seglen
	tax
	lda end+1
	sec
	sbc #>data
	sta seglen+1
	tay

	jmp deactivate_source
.endproc

;*******************************************************************************
; SEG COPY
; Copies the active segment's text from the given page to its end into the
; start of another segment and stores that segment's length.
; IN:
;  - .A: the destination segment's bank
;  - .X: the page (offset from the start of the segment) to copy from
.export __src_seg_copy
.proc __src_seg_copy
@src=zp::bankaddr0
@dst=zp::bankaddr1
	sta $9ffa		; BLK2 = destination
	txa
	clc
	adc #>data
	sta @src+1
	lda #$00
	sta @src
	sta @dst
	lda #>COPY_DST
	sta @dst+1
	jsr activate_source
	lda #$5f
	sta $9ff2		; RAM in BLK1 and BLK2

	jsr copy_to_end
	stx COPY_DST+BUFFER_SIZE
	tya
	sec
	sbc #>COPY_DST
	sta COPY_DST+BUFFER_SIZE+1

	lda #$02
	sta $9ffa		; restore BLK2
	jmp deactivate_source
.endproc

;*******************************************************************************
; COPY TO END
; Copies [bankaddr0, end) to bankaddr1, lowest address first
; OUT:
;  - .XY: the end of the copy's destination
.proc copy_to_end
@src=zp::bankaddr0
@dst=zp::bankaddr1
	lda end
	sec
	sbc @src
	pha
	lda end+1
	sbc @src+1
	tax
	ldy #$00
	txa
	beq @rest

@page:	lda (@src),y
	sta (@dst),y
	iny
	bne @page
	inc @src+1
	inc @dst+1
	dex
	bne @page
@rest:	pla
	tax
	beq @done

@l0:	lda (@src),y
	sta (@dst),y
	iny
	dex
	bne @l0

@done:	tya
	clc
	adc @dst
	tax
	lda @dst+1
	adc #$00
	tay
	rts
.endproc
