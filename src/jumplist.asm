;*******************************************************************************
; JUMPLIST.ASM
; This file contains the code/data to manage "jumplists". These allow the user
; to quickly move back and forth after executing certain commands that navigate
; some/many lines away (gg, G, goto-line, etc.)
;*******************************************************************************

.include "config.inc"
.include "errors.inc"
.include "errlogconst.inc"
.include "macros.inc"
.include "ram.inc"
.include "source.inc"
.include "log.inc"
.include "zeropage.inc"

;*******************************************************************************
MAX_JUMPS  = 8
JUMP_SIZE  = 4
JUMP_BYTES = (MAX_JUMPS + 1) * JUMP_SIZE
J_BUFFER   = 0
J_LINE     = 1
J_COLUMN   = 3

.ifdef ultimem
.segment "ERRLOG_BSS"
.else
.BSS
.endif

;*******************************************************************************
; DATA
jumplist:    .res JUMP_BYTES
jumpcount:   .byte 0                 ; used bytes
jumpindex:   .byte 0                 ; current entry, or count if not recorded
jumppending: .res JUMP_SIZE

;*******************************************************************************
.CODE
.export __jump_begin, __jump_commit, __jump_back, __jump_forward
.export __jump_reset, __jump_clear_log
.ifdef ultimem
JUMP_BANK = FINAL_BANK_ERRLOG
BANK_TABLE jump_table, JUMP_BANK, "ERRLOG_CODE"
.else
JUMP_BANK = FINAL_BANK_DBGUI
BANK_TABLE jump_table, JUMP_BANK, "DBGUI"
.endif
.macro JUMPENTRY target
.ifdef ultimem
	BANK_ENTRY jump_table, "ERRLOG_CODE", target
.elseif .defined(CART) .and .defined(c64)
	JUMP JUMP_BANK, target
.else
	jmp target
.endif
.endmacro
__jump_begin:     JUMPENTRY jump_begin
__jump_commit:    JUMPENTRY jump_commit
__jump_back:      JUMPENTRY jump_back
__jump_forward:   JUMPENTRY jump_forward
__jump_reset:     JUMPENTRY jump_reset
__jump_clear_log: JUMPENTRY jump_clear_log

.ifdef ultimem
BANKED_SEG "ERRLOG_CODE", FINAL_BANK_ERRLOG
.else
BANKED_CODE "DBGUI", FINAL_BANK_DBGUI
.endif

;*******************************************************************************
; RESET
; Clears the navigation history (called when the source is initialized).
.proc jump_reset
	lda #$00
	sta jumpcount
	sta jumpindex
	rts
.endproc

;*******************************************************************************
; BEGIN
; Prepares a new jump at the current source position. Call jumps::commit to
; confirm the jump.
; PRESERVED:
;  - .XY: caller's navigation target
.proc jump_begin
	lda src::activebuff
	sta jumppending+J_BUFFER
	lda src::line
	sta jumppending+J_LINE
	lda src::line+1
	sta jumppending+J_LINE+1
	lda zp::srcx
	sta jumppending+J_COLUMN
	rts
.endproc

;*******************************************************************************
; MATCHES CURRENT POSITION
; Compares an entry with the current source position
; IN:
;  - .X: byte offset of the entry
; OUT:
;  - .Z: set if buffer, line and column match
.proc matches_current_position
	lda jumplist+J_BUFFER,x
	cmp src::activebuff
	bne @done			; if different buffer -> not current pos

	lda jumplist+J_LINE,x
	cmp src::line
	bne @done			; if different line -> not current pos

	; check if the current line/column are a match
	lda jumplist+J_LINE+1,x
	cmp src::line+1
	bne @done
	lda jumplist+J_COLUMN,x
	cmp zp::srcx
@done:	rts
.endproc

;*******************************************************************************
; STORE CURRENT
; Writes the current source position into an entry
; IN:
;  - .X: byte offset of the entry
.proc store_current
	lda src::activebuff
	sta jumplist+J_BUFFER,x
	lda src::line
	sta jumplist+J_LINE,x
	lda src::line+1
	sta jumplist+J_LINE+1,x
	lda zp::srcx
	sta jumplist+J_COLUMN,x
	rts
.endproc

;*******************************************************************************
; RESERVE
; Appends an entry, evicting the oldest when history is full
; OUT:
;  - .X: byte offset of the new entry
.proc reserve
	ldx jumpcount
	cpx #JUMP_BYTES
	bcc @add
	ldx #$00

	; shift out the oldest jump
@shift:
	lda jumplist+JUMP_SIZE,x
	sta jumplist,x
	inx
	cpx #JUMP_BYTES-JUMP_SIZE
	bcc @shift

@add:	stx jumpindex
	txa
	clc
	adc #JUMP_SIZE
	sta jumpcount
	rts
.endproc

;*******************************************************************************
; COMMIT
; Records both ends of a successful jump, discarding forward history
; A jump that leaves the position unchanged does not consume history
; IN:
;  - source cursor: destination buffer, line and byte column
.proc jump_commit
	; pending follows the entries and counters; compare it as an entry
	ldx #jumppending-jumplist
	jsr matches_current_position
	beq @done

	ldx jumpindex
	cpx jumpcount
	bcs @origin

	; branching from an older jump discards its forward destinations
	txa
	clc
	adc #JUMP_SIZE
	sta jumpcount

	ldy #$00
@compare:
	lda jumplist,x
	cmp jumppending,y
	bne @origin
	inx
	iny
	cpy #JUMP_SIZE
	bne @compare
	beq @destination

@origin:
	jsr reserve
	ldy #$00
@copy:	lda jumppending,y
	sta jumplist,x
	inx
	iny
	cpy #JUMP_SIZE
	bne @copy

@destination:
	jsr reserve
	jsr store_current
@done:	RETURN_OK
.endproc

;*******************************************************************************
; BACK
; Saves the current destination and finds the preceding distinct position
; OUT:
;  - .C: set if there is no valid preceding position
;  - .A: destination buffer on success
;  - .XY: destination line on success
;  - r0: destination byte column on success
.proc jump_back
	lda jumpcount
	beq @none
	ldx jumpindex
	cpx jumpcount
	bcs @append

	lda jumplist+J_BUFFER,x
	cmp src::activebuff
	bne @branch
	cpx #$00
	beq @none
	bne @save		; branch always

	; an unrecorded buffer switch must not overwrite its return point
@branch:
	txa
	clc
	adc #JUMP_SIZE
	sta jumpcount

@append:
	jsr reserve
@save:	jsr store_current
@prev:	lda jumpindex
	beq @none
	sec
	sbc #JUMP_SIZE
	sta jumpindex
	tax
	jsr matches_current_position
	beq @prev
	bne load_target		; branch always

@none:	sec
	rts
.endproc

;*******************************************************************************
; FORWARD
; Saves the current position and finds the next distinct destination
; OUT:
;  - .C: set if there is no valid following position
;  - .A: destination buffer on success
;  - .XY: destination line on success
;  - r0: destination byte column on success
.proc jump_forward
	lda jumpindex
	clc
	adc #JUMP_SIZE
	cmp jumpcount
	bcs @none
	ldx jumpindex
	lda jumplist+J_BUFFER,x
	cmp src::activebuff
	bne @none
	jsr store_current

@next:	lda jumpindex
	clc
	adc #JUMP_SIZE
	cmp jumpcount
	bcs @none
	sta jumpindex
	tax
	jsr matches_current_position
	beq @next
	bne load_target		; branch always

@none:	sec
	rts
.endproc

;*******************************************************************************
; LOAD TARGET
; Returns a valid history entry to the editor
; IN:
;  - .X: byte offset of the entry
; OUT:
;  - .C: set if the destination is unavailable
;  - .A: buffer on success
;  - .XY: line on success
;  - r0: byte column on success
.proc load_target
@column=r0
	lda jumplist+J_BUFFER,x
	cmp src::numbuffers
	bcc @load
	cmp #LOG_BUFFER
	bne @bad
	lda log::written
	beq @bad

@load:	lda jumplist+J_COLUMN,x
	sta @column
	lda jumplist+J_BUFFER,x
	ldy jumplist+J_LINE+1,x
	pha
	lda jumplist+J_LINE,x
	tax
	pla
	RETURN_OK

@bad:	sec
	rts
.endproc

;*******************************************************************************
; CLOSE
; Removes a closing source and renumbers surviving source IDs
; IN:
;  - src::activebuff: source being closed, before source IDs compact
.export __jump_close
.proc __jump_close
	lda src::activebuff
	jmp forget
.endproc

;*******************************************************************************
; CLEAR LOG
; Removes positions belonging to the previous log contents
.proc jump_clear_log
	lda #LOG_BUFFER
	; fall through to forget
.endproc

;*******************************************************************************
; FORGET
; Compacts history after removing one buffer
; IN:
;  - .A: buffer to remove (LOG does not renumber other buffers)
.proc forget
@buffer=zp::util
	sta @buffer
	ldx #$00
	ldy #$00
@l0:	cpx jumpcount
	beq @done
	lda jumplist+J_BUFFER,x
	cmp @buffer
	beq @remove
	bcc @keep
	cmp #LOG_BUFFER
	beq @keep
	sec
	sbc #$01

@keep:	sta jumplist+J_BUFFER,y
	lda jumplist+J_LINE,x
	sta jumplist+J_LINE,y
	lda jumplist+J_LINE+1,x
	sta jumplist+J_LINE+1,y
	lda jumplist+J_COLUMN,x
	sta jumplist+J_COLUMN,y
	cpx jumpindex
	bne :+
	sty jumpindex

:	tya
	clc
	adc #JUMP_SIZE
	tay
	jmp @next

@remove:
	cpx jumpindex
	bne @next
	lda #$ff
	sta jumpindex              ; current endpoint no longer exists
@next:	txa
	clc
	adc #JUMP_SIZE
	tax
	bne @l0

@done:	sty jumpcount
	lda jumpindex
	cmp jumpcount
	bcc :+
	sty jumpindex              ; next Back captures the current position
:	rts
.endproc

;*******************************************************************************
; SHIFT
; Tracks source line insertion/deletion in both directions of history
; IN:
;  - r0-r1:           line before the affected range
;  - r2:              $01 to insert, $ff to delete
;  - r3:              insertion/deletion mode (ERRLOG_* constants)
;  - src::activebuff: source being edited
;  - zp::srcx:        byte column at the split/join
.export __jump_shift
.proc __jump_shift
@line=r0
@offset=r2
@preserve=r3
	ldx #$00
@l0:	cpx jumpcount
	beq @done			; if no jumps -> done

	lda jumplist+J_BUFFER,x
	cmp src::activebuff
	bne @next

	lda jumplist+J_LINE+1,x
	cmp @line+1
	bcc @next
	bne @later
	lda jumplist+J_LINE,x
	cmp @line
	bcc @next
	bne @later

	; a split carries positions in the right half onto the new line
	lda @offset
	bmi @next
	lda @preserve
	cmp #ERRLOG_BLANK_BELOW
	beq @next		; opening below leaves the original line intact
	lda jumplist+J_COLUMN,x
	sec
	sbc zp::srcx
	bcc @next
	sta jumplist+J_COLUMN,x

@later: lda @offset
	bmi @up
	inc jumplist+J_LINE,x
	bne @next
	inc jumplist+J_LINE+1,x
	jmp @next

@up:	lda jumplist+J_LINE,x
	bne :+
	dec jumplist+J_LINE+1,x
:	dec jumplist+J_LINE,x
	lda jumplist+J_LINE,x
	ora jumplist+J_LINE+1,x
	bne @joined
	inc jumplist+J_LINE,x       ; deleted first line clamps to line one
	bne @next

@joined:
	lda @preserve
	bne @next
	lda jumplist+J_LINE,x
	cmp @line
	bne @next
	lda jumplist+J_LINE+1,x
	cmp @line+1
	bne @next

	; positions in the second line follow its text across a join
	lda jumplist+J_COLUMN,x
	clc
	adc zp::srcx
	bcc :+
	lda #$ff
:	sta jumplist+J_COLUMN,x
@next:	txa
	clc
	adc #JUMP_SIZE
	tax
	jmp @l0

@done:	rts
.endproc
