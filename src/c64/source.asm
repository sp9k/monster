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

.macpack longbranch

buffstate   = zp::srccur
cursorzp    = zp::srccur
poststartzp = zp::srccur2
line        = zp::srcline
lines       = zp::srclines
end         = zp::srcend

; TODO:
BUFFER_SIZE = $8000	; max size of buffer
GAPSIZE     = $100	; size of gap in gap buffer
PAGESIZE    = $100	; size of data "page" (amount stored in c64 RAM)

.CODE
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
	bcs @full		; buffer is full

	ldy __src_bank
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
@src=r2
@dst=r4
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
	jsr gaplen
	cmpw #0			; is gap closed?
	bne @ins		; no, insert as usual

	; check if there is room to expand the gap
	; the expansion moves [poststart, end) up by $100, so it is END that
	; must stay below the top of the buffer
	lda end+1
	cmp #>BUFFER_SIZE-1	; -1 to save space for a $100 byte gap
	bcc @ok

@err:	; buffer overflow, cannot insert character
	pla				; clean stack
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

@cont: ; move one byte from the end of the gap to the start
	lda __src_bank
	sta reu::reuaddr+2

	LOADB poststartzp
	STOREB cursorzp

	incw cursorzp
	incw poststartzp

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
; START
; Returns .Z set if the cursor is at the start of the buffer.
; OUT:
;  - .Z: set if the cursor is at the start of the buffer
.export __src_start
.proc __src_start
	ldx cursorzp
	bne @done	; if LSB is !0, not the start
	ldx cursorzp+1	; set .Z if MSB is 0
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
	; TODO: shift debug info line programs after the current line

	; shift breakpoints; line to shift from is current line+1
	; edit::currentfile lives in MIDRAM: on the cart build this callback can
	; run from a banked context (e.g. the log writing to the LOG buffer)
	CALLMAIN edit::currentfile	; .A=file id, .XY=current line
	inx
	bne :+
	iny
:	sta r0			; file ID (preserved across both shifts)
	txa
	pha			; save line+1 (LSB)
	tya
	pha			; save line+1 (MSB)

	lda #$01
	jsr dbg::shift_breakpointsd

	pla
	tay			; restore line+1 (MSB)
	pla
	tax			; restore line+1 (LSB)
	lda #$01
	jmp errlog::shift_errorsd
.endproc

;*******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
.proc __src_atcursor
	decw cursorzp
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
	ldxy cursorzp
	stxy @cur

	lda #$00
	sta @x

@l0:	lda @cur
	ora @cur+1
	beq @done		; start of buffer
	decw @cur
	lda24 __src_bank, @cur
	cmp #$0d
	beq @done
	inc @x
	bne @l0			; branch always

@done:	lda @x
	sta zp::srcx
	RETURN_OK
.endproc
