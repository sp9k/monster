.include "ram.inc"
.include "reu.inc"
.include "../config.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../edit.inc"
.include "../errlog.inc"
.include "../errors.inc"
.include "../macros.inc"
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

; NOTE: must match source.asm
POOL_TAIL = $fe
next_of   = bank_next-FINAL_BANK_SOURCE0

BUFFER_SIZE = $8000	; max size of a segment
GAPSIZE     = $100	; size of gap in gap buffer
PAGESIZE    = $100	; size of data "page" (amount stored in c64 RAM)

SEGLEN_ADDR = BUFFER_SIZE

.export __src_data: abs = 0

.BSS
seglen: .word 0

;*******************************************************************************
; private buffer for source DMA transfers
.ifdef CART
.segment "SRCVARS"	; always-visible high RAM
.else
.DATA			; resident RAM visible during DMA
.endif
READ_CHUNK = 64
readbuf: .res READ_CHUNK
.assert (readbuf+READ_CHUNK <= $a000) .or ((readbuf >= $c000) .and (readbuf+READ_CHUNK <= $d000)), lderror, "source DMA buffer must be visible with I/O enabled"

.CODE
;*******************************************************************************
; COPY LINE
; Copies a counted span from the source bank to a resident buffer.
; IN:
;   - .A: source REU bank
;   - .Y: last byte index (inclusive)
;   - zp::bankaddr0: source offset
;   - zp::bankaddr1: destination address
.export src_copyline
.proc src_copyline
	sta reu::reuaddr+2
	iny
	sty reu::txlen
	lda #0
	sta reu::txlen+1
	ldxy zp::bankaddr0
	stxy reu::reuaddr
	ldxy zp::bankaddr1
	stxy reu::c64addr
	jmp reu::load
.endproc

;*******************************************************************************
; INIT BUFF
; Initializes a new source buffer by setting its pointers to the
; start/end of the gap and clearing the buffer's REU bank.
; The bank to initialize is the active buffer's bank (__src_bank), which the
; caller (init_buff) sets before this is called.
.export __src_init_buff
.proc __src_init_buff
	lda __src_bank
	sta reu::reuaddr+2

	lda #$00
	sta cursorzp
	sta cursorzp+1
	sta reu::reuaddr
	sta reu::reuaddr+1

	lda #<GAPSIZE
	sta end
	sta poststartzp
	lda #>GAPSIZE
	sta end+1
	sta poststartzp+1

	ldxy #$ffff
	stxy reu::txlen
	jsr reu::zero

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
	cpy #>BUFFER_SIZE
	bcc @room

	; segment is full; continue in a new one
	pha
	jsr append
	pla
	bcs @full

@room:	ldy __src_bank
	sty reu::reuaddr+2
	STOREB end
	incw end
@done:	RETURN_OK

@full:	RETURN_ERR ERR_BUFFER_FULL
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
	bcc @nodisp
	cmp #$80
	bcc :+
@nodisp:
	RETURN_OK	; not displayable; not inserted but still a success

:	pha
	jsr __src_mark_dirty
	ldxy cursorzp
	cmpw poststartzp	; is gap closed?
	bne @ins		; no, insert as usual

	jsr __src_open_gap
	bcc @ins

@err:	; buffer overflow, cannot insert character
	pla				; clean stack
	lda #ERR_BUFFER_FULL
	rts

@ins:	pla
	ldy cursorzp+1
	bmi @done	; out of range

	; write the character to insert
	ldy __src_bank
	sty reu::reuaddr+2
	STOREB cursorzp

	cmp #$0d
	bne @insdone
	incw line
	jsr on_line_inserted
	incw lines
	lda #$ff
	sta zp::srcx		; reset cursor "column"

@insdone:
	inc zp::srcx		; move to next "column"
	incw cursorzp
@done:	RETURN_OK
.endproc

;*******************************************************************************
; OPEN GAP
; opens the default-sized gap at the cursor, splitting a full segment as needed
; IN:
;  - cursorzp: cursor at the closed gap
;  - poststartzp: equal to cursorzp
;  - end: end of the active segment's text
; OUT:
;  - .A: error code if the gap could not be opened
;  - .C: set if the bank pool is exhausted
.export __src_open_gap
.proc __src_open_gap
	; check if there is room to expand the gap
	; the expansion moves [poststart, end) up by $100, so it is END that
	; must stay below the top of the buffer
	lda end+1
	cmp #>BUFFER_SIZE-1	; -1 to save space for a $100 byte gap
	bcc @ok

	; segment is full; split it
	jsr split
	bcc @ok

	lda #ERR_BUFFER_FULL
	rts

@ok:	; gap is closed, create a new one
	; copy data[poststart] to data[poststart + GAPSIZE]
	lda __src_bank
	sta reu::move_src+2
	sta reu::move_dst+2

	; source address
	ldxy cursorzp
	stxy reu::move_src

	; get number of bytes to copy
	ldxy end
	sub16 poststartzp
	stxy reu::move_size

	; calculate the new destination in the REU to store the data
	inc poststartzp+1
	inc end+1		; increase size by $100
	ldxy poststartzp
	stxy reu::move_dst	; set REU destination

	; move the memory (open a new gap)
	jsr reu::move
	RETURN_OK
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

	; move one byte from the end of the gap to the start
	lda __src_bank
	sta reu::reuaddr+2

	LOADB poststartzp
	STOREB cursorzp

	incw cursorzp
	incw poststartzp

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
; DOWN
; Scan bounded REU chunks and move only the bytes through the first newline.
; OUT: .C set at EOF, clear after consuming a newline.
.export __src_down
.proc __src_down
@chunk:
	ldxy end
	sub16 poststartzp
	cpy #$00
	bne @full
	cpx #READ_CHUNK
	bcs @full
	cpx #$00
	bne @load
	jsr cross_next
	bcc @chunk
	rts
@full:	ldx #READ_CHUNK
@load:	stx reu::txlen
	ldxy poststartzp
	lda __src_bank
	jsr load_chunk

	ldy #$00
@scan:	lda readbuf,y
	iny
	cmp #$0d
	beq @newline
	cpy reu::txlen
	bcc @scan
	clc
	bcc @move
@newline:
	incw line
	sec
@move:	php			; remember whether this chunk ended a line
	sty reu::txlen		; store only the bytes actually consumed
	ldxy cursorzp
	stxy reu::reuaddr
	jsr reu::store

	lda reu::txlen
	clc
	adc cursorzp
	sta cursorzp
	bcc :+
	inc cursorzp+1
:	lda reu::txlen
	clc
	adc poststartzp
	sta poststartzp
	bcc :+
	inc poststartzp+1
:	lda reu::txlen
	clc
	adc zp::srcx
	sta zp::srcx

	jsr at_seg_end
	bne :+
	jsr cross_next
:	plp
	bcs :+
	jmp @chunk
:	lda #$00
	sta zp::srcx
	RETURN_OK
.endproc

;*******************************************************************************
; LOAD CHUNK
; The transfer parameters remain ready to store this chunk back to the REU.
; IN:
;   - .A = REU bank
;   - .XY = offset
;   - reu::txlen low = count (1..READ_CHUNK).
.proc load_chunk
	sta reu::reuaddr+2
	stxy reu::reuaddr
	ldxy #readbuf
	stxy reu::c64addr
	lda #$00
	sta reu::txlen+1
	jmp reu::load
.endproc

;*******************************************************************************
; PREV
; Moves the cursor back one character in the gap buffer.
; OUT:
;  - .A: the character at the new cursor position (if not at the start of buff)
;  - .C: set if we're at the start of the buffer and couldn't move back
.export __src_prev
.proc __src_prev
	lda cursorzp
	ora cursorzp+1
	bne @cont
	jsr cross_prev
	bcc @cont
	jsr __src_atcursor
	sec
	rts

@cont:	; move char from start of gap to the end of the gap
	decw cursorzp
	decw poststartzp

	; move one byte from the start of the gap to the end
	lda __src_bank
	sta reu::reuaddr+2

	LOADB cursorzp
	STOREB poststartzp

	cmp #$0d
	bne :+
	decw line

:	dec zp::srcx		; decrement cursor "column"
	bpl @done
	inc zp::srcx		; NOTE: srcx is inaccurate if a newline is crossed

@done:	; get the character at the new cursor position
	jsr __src_atcursor
	RETURN_OK
.endproc

;*******************************************************************************
; ON LINE INSERTED
; Callback to handle a line insertion. Various state needs to be shifted when
; this occurs (breakpoints, etc.)
.proc on_line_inserted
	; TODO: shift debug info line programs after the current line

	CALLMAIN errlog::inserted
	rts
.endproc

;*******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
.proc __src_atcursor
	lda cursorzp
	ora cursorzp+1
	bne :+
	jmp atcursor_prev

:	decw cursorzp
	lda24 __src_bank, cursorzp
	incw cursorzp
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

@l0:	lda @cur
	ora @cur+1
	bne @scan

	; start of this segment; continue in the previous one
	ldx @bank
	jsr find_prev
	bcs @done
	sta @bank
	jsr __src_seg_len
	stxy @cur
	jmp @l0

@scan:	decw @cur
	lda24 @bank, @cur
	cmp #$0d
	beq @done
	inc @x
	bne @l0			; branch always

@done:	lda @x
	sta zp::srcx
	RETURN_OK
.endproc

;*******************************************************************************
; GET IN
; Reads the line at the cursor into the given address
; IN:
;  - .XY: destination to copy to
; OUT:
;  - (.XY): a line of text from the cursor position (0-terminated)
.pushseg
.ifdef CART
.segment "GUICODE"
.endif
.export __src_getin
.proc __src_getin
	lda #LINESIZE

	; fall through __src_readspan
.endproc

;*******************************************************************************
; READSPAN
; Reads the given number of bytes into the given buffer
; IN:
;   - .A: maximum characters
;   - .XY: destination to read to
.export __src_readspan
.proc __src_readspan
@limit=zp::banktmp
@src=zp::bankaddr0
@dst=zp::bankaddr1
@end=srctmp
@bank=srctmp+2
@count=srctmp+3
	sta @limit
	stxy @dst
	ldxy poststartzp
	stxy @src
	ldxy end
	stxy @end
	lda __src_bank
	sta @bank
	lda #$00
	sta @count

;-------------------------------------------------------------------------------
@chunk:	lda @limit
	sec
	sbc @count
	cmp #READ_CHUNK
	bcc :+
	lda #READ_CHUNK

:	sta reu::txlen
	ldxy @end
	sub16 @src
	cpy #$00
	bne @load
	cpx #$00
	beq @cross
	cpx reu::txlen
	bcs @load
	stx reu::txlen

@load:	ldxy @src
	lda @bank
	jsr load_chunk
	ldy @count

;-------------------------------------------------------------------------------
	ldx #$00
@copy:	lda readbuf,x
	cmp #$0d
	beq @done
	sta (@dst),y
	iny
	inx
	cpx reu::txlen
	bcc @copy

	sty @count
	cpy @limit
	beq @done
	lda @src
	clc
	adc reu::txlen
	sta @src
	bcc @chunk
	inc @src+1
	bcs @chunk

;-------------------------------------------------------------------------------
@cross:	ldx @bank
	lda next_of,x
	cmp #POOL_TAIL
	bcs @eof
	sta @bank
	jsr __src_seg_len
	stxy @end
	lda #$00
	sta @src
	sta @src+1
	beq @chunk

@eof:	ldy @count
@done:	lda #$00
	sta (@dst),y
	RETURN_OK
.endproc
.popseg

;*******************************************************************************
; SEG LEN
; Returns the stored length of an inactive segment
; IN:
;  - .A: the segment's bank
; OUT:
;  - .XY: its length
.export __src_seg_len
.proc __src_seg_len
	sta reu::reuaddr+2

	ldxy #SEGLEN_ADDR
	stxy reu::reuaddr
	ldxy #$02
	stxy reu::txlen

	ldxy #seglen
	stxy reu::c64addr

	jsr reu::load
	ldxy seglen
	rts
.endproc

;*******************************************************************************
; SEG SET LEN
; Stores the length of an inactive segment
; IN:
;  - .A:  the segment's bank
;  - .XY: its length
.export __src_seg_set_len
.proc __src_seg_set_len
	sta reu::reuaddr+2
	STOREW SEGLEN_ADDR
	rts
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
	sta reu::reuaddr+2
	stxy zp::bankaddr0
	LOADB zp::bankaddr0
	rts
.endproc

;*******************************************************************************
; SEG CLOSE
; Closes the active segment's gap and stores its length
; OUT:
;  - .XY: the segment's length
.export __src_seg_close
.proc __src_seg_close
	ldxy end
	sub16 poststartzp
	stxy reu::move_size

	ldxy poststartzp
	cmpw cursorzp
	beq @nogap

	; close the gap in the segment we are done with
	stxy reu::move_src
	ldxy cursorzp
	stxy reu::move_dst

	lda __src_bank
	sta reu::move_src+2
	sta reu::move_dst+2
	jsr reu::move

@nogap:	ldxy cursorzp
	add16 reu::move_size
	lda __src_bank
	jmp __src_seg_set_len
.endproc

;*******************************************************************************
; SEG COPY
; Copies the active segment's text from the given page to its end into the
; start of another segment and stores that segment's length
; IN:
;  - .A: the destination segment's bank
;  - .X: the page (offset from the start of the segment) to copy from
.export __src_seg_copy
.proc __src_seg_copy
	sta reu::move_dst+2
	stx reu::move_src+1

	lda #$00
	sta reu::move_src
	sta reu::move_dst
	sta reu::move_dst+1
	lda __src_bank
	sta reu::move_src+2

	ldxy end
	sub16 reu::move_src
	stxy reu::move_size
	jsr reu::move

	ldxy reu::move_size
	lda reu::move_dst+2
	jmp __src_seg_set_len
.endproc
