;*******************************************************************************
; SOURCE.ASM
; This file contains the procedures to interacting with the buffer that backs
; the editor. When data is entered in the editor, it is stored in one of 8
; "source buffers". These are gap buffers to allow for efficient insertion of
; text, and each may span as many banks from a shared pool as it needs.
;*******************************************************************************

.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "errlog.inc"
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
.import __src_getin
.import __src_readspan
.import __src_insert
.import __src_init_buff
.import __src_open_gap
.import __src_next
.import __src_prev

.import __src_data
.assert <__src_data = 0, error, "source data must be page aligned"
.import __src_seg_copy
.import __src_seg_len
.import __src_seg_close
.import __src_seg_peek
.import __src_seg_set_len

.import sync_x

;*******************************************************************************
; CONSTANTS
; NOTE: MAX_SOURCES and LOG_BUFFER must match their definitions in source.inc
MAX_SOURCES    = 8		; max # of user source buffers
NUM_BUFFERS    = MAX_SOURCES+1	; user buffers + the reserved LOG buffer
POS_STACK_SIZE = 16 		; size of source position stack
NEAR_DIST      = $400		; popgoto walks distances shorter than this

LOG_BUFFER	= MAX_SOURCES	; reserved buffer for LOG

;*******************************************************************************
; FLAGS
FLAG_DIRTY = 1

.segment "SRCVARS"

;*******************************************************************************
data_start:
sp:	.byte 0			; stack pointer for source position stack

;*******************************************************************************
; source position stack: 24-bit position and line it is on
stk_lo:   .res POS_STACK_SIZE
stk_mid:  .res POS_STACK_SIZE
stk_hi:   .res POS_STACK_SIZE
stk_llo:  .res POS_STACK_SIZE
stk_lhi:  .res POS_STACK_SIZE

;*******************************************************************************
; BANK CHAIN
POOL_FREE = $ff		; flag for banks that are free
POOL_TAIL = $fe		; flag for the end of a chain of banks

.export bank_next
bank_next: .res SOURCE_POOL_SIZE
next_of = bank_next-FINAL_BANK_SOURCE0

; offset of the active segment's first byte within its buffer
base: .res 3

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
srctmp      = zp::srctmp
srcx           = zp::srcx
SAVESTATE_SIZE = 11		; space used by above zeropage addresses

.exportzp __src_line
__src_line = line
.exportzp __src_lines
__src_lines = lines

;*******************************************************************************
; SAVESTATE
; This buffer holds the "buffer state" for each source buffer. See BUFFSTATE
; 11 bytes: curl, curr, line, lines, end, srcx
savestate:  .res NUM_BUFFERS*SAVESTATE_SIZE

.export __src_names
__src_names:
names:	   .res NUM_BUFFERS*MAX_BUFFER_NAME_LEN

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
buffs_curx: .res NUM_BUFFERS	; cursor X positions for each inactive buffer
buffs_cury: .res NUM_BUFFERS	; cursor Y positions for each inactive buffer
.export banks
banks:      .res NUM_BUFFERS	; the corresponding bank for each buffer
flags:      .res NUM_BUFFERS	; flags for each source buffer

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
	sta zp::srcx

	; clear the active bank and all per-buffer state
	.assert (flags+NUM_BUFFERS)-bank = (NUM_BUFFERS*4)+1, error, "bank/buffs_curx/buffs_cury/banks/flags must be contiguous"
	ldx #(NUM_BUFFERS*4)+1
:	sta bank-1,x
	dex
	bne :-

	lda #POOL_FREE
	ldx #SOURCE_POOL_SIZE
:	sta bank_next-1,x
	dex
	bne :-

	; buffers are reset so all errors are now invalid, clear them too
	jmp errlog::reset
.endproc

;*******************************************************************************
; SAVE
; Backs up the pointers for the active source so that they may be set for
; another source
.export __src_save
.proc __src_save
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
	jsr mul_state_size
	tay

	; save the buffer state
	ldx #$00
@l0:	lda buffstate,x
	sta savestate,y
	iny
	inx
	cpx #SAVESTATE_SIZE
	bne @l0

:	rts			; <- __src_set
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
	cmp numsrcs
	bcs :-		; buffer doesn't exist; return with .C set

	; fall through to __src_force_set
.endproc

;*******************************************************************************
; FORCE SET
; Entrypoint for src::set that bypasses the "numsrcs" check
.export __src_force_set
.proc __src_force_set
	pha

	jsr __src_save	; save current buffer before switching

	; set the pointers to those of the source we're switching to
	pla
	tax

	; fall through to set_no_save
.endproc

;*******************************************************************************
; SET NO SAVE
; Entrypoint for src::set that bypasses saving the current source buffer state.
; Used when closing a buffer because we don't need to save anything since the
; buffer we're closing is gone
; IN:
;   - .X: buffer to set
.proc set_no_save
	stx activesrc
	lda activesrc
	jsr mul_state_size
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

	jmp calc_base
.endproc

;*******************************************************************************
; ALLOC BANK
; Takes a bank from the pool and marks it as the tail of a new chain
; OUT:
;  - .A: the bank that was allocated
;  - .C: set if the pool is exhausted
.export __src_reserve_bank = alloc_bank
.proc alloc_bank
	ldx #FINAL_BANK_SOURCE0
@l0:	lda next_of,x
	cmp #POOL_FREE
	beq @found
	inx
	cpx #FINAL_BANK_SOURCE0+SOURCE_POOL_SIZE
	bne @l0
	rts			; .C set

@found:	lda #POOL_TAIL
	sta next_of,x
	txa
	clc
	rts
.endproc

;*******************************************************************************
; ALLOC AFTER
; Allocates a bank and links it into the active chain after the active segment
; OUT:
;  - .A: the bank that was allocated
;  - .C: set if the pool is exhausted
.proc alloc_after
	jsr alloc_bank
	bcs @done
	tay
	ldx bank
	lda next_of,x
	sta next_of,y
	tya
	sta next_of,x
	clc
@done:	rts
.endproc

;*******************************************************************************
; FIND PREV
; IN:
;  - .X: a bank in a chain
; OUT:
;  - .A: the bank before it
;  - .C: set if .X is the head of its chain
; CLOBBERS:
;  - .Y
.export find_prev
.proc find_prev
	txa
	ldy #FINAL_BANK_SOURCE0
@l0:	cmp next_of,y
	beq @found
	iny
	cpy #FINAL_BANK_SOURCE0+SOURCE_POOL_SIZE
	bne @l0
	rts			; .C set

@found:	tya
	clc
	rts
.endproc

;*******************************************************************************
; HEAD OF
; IN:
;  - .X: a bank in a chain
; OUT:
;  - .X: the first bank in the chain
.proc head_of
:	jsr find_prev
	bcs @done
	tax
	bcc :-
@done:	rts
.endproc

;*******************************************************************************
; FREE CHAIN
; Returns every bank in the chain containing the given bank to the pool
; IN:
;  - .A: any bank in the chain
.export __src_release_bank = free_chain
.proc free_chain
	tax
	jsr head_of
@l0:	ldy next_of,x
	lda #POOL_FREE
	sta next_of,x
	cpy #POOL_TAIL
	beq @done
	tya
	tax
	bne @l0			; branch always
@done:	rts
.endproc

;*******************************************************************************
; UNLINK
; Removes a bank from its chain and returns it to the pool
; IN:
;  - .X: the bank to remove
.proc unlink
	jsr find_prev
	bcs @free
	tay
	lda next_of,x
	sta next_of,y

@free:	lda #POOL_FREE
	sta next_of,x
	rts
.endproc

;*******************************************************************************
; ADD BASE
; Adds .XY to the active segment's base
.proc add_base
	txa
	clc
	adc base
	sta base
	tya
	adc base+1
	sta base+1
	bcc :+
	inc base+2
:	rts
.endproc

;*******************************************************************************
; SUB BASE
; Subtracts .XY from the active segment's base
.proc sub_base
	stxy srctmp+2
	lda base
	sec
	sbc srctmp+2
	sta base
	lda base+1
	sbc srctmp+3
	sta base+1
	bcs :+
	dec base+2
:	rts
.endproc

;*******************************************************************************
; CALC BASE
; Recomputes base for the active segment from the lengths of the segments
; before it
.proc calc_base
	lda #$00
	sta base
	sta base+1
	sta base+2
	ldx bank

@l0:	jsr find_prev
	bcs @done
	pha
	jsr __src_seg_len
	jsr add_base
	pla
	tax
	jmp @l0

@done:	clc
	rts
.endproc

;*******************************************************************************
; ENTER
; Makes the given bank the active segment with the cursor at the given offset.
; IN:
;  - .A:  the bank to enter
;  - .XY: the offset to put the cursor at
; OUT:
;  - .C: clear
.proc enter
	sta bank
	stx cursorzp
	stx poststartzp
	tya
	clc
	adc #>__src_data
	sta cursorzp+1
	sta poststartzp+1
	lda bank
	jsr __src_seg_len
	stx end
	tya
	clc
	adc #>__src_data
	sta end+1
	rts
.endproc

;*******************************************************************************
; LEAVE
; Closes the active segment's gap and removes it from its chain if it is empty
; and has neighbors.
; OUT:
;  - .XY: the length of the segment that was left
.proc leave
	jsr __src_seg_close

	txa
	bne @done
	tya
	bne @done
	ldx bank
	lda next_of,x
	cmp #POOL_TAIL
	bne @free
	jsr find_prev
	bcs @empty		; lone segment: keep it

@free:	jsr unlink
@empty:	ldxy #$0000
@done:	rts
.endproc

;*******************************************************************************
; CROSS NEXT
; Moves the cursor to the start of the next segment
; OUT:
;  - .C: set if there is no next segment
.export cross_next
.proc cross_next
	ldx bank
	lda next_of,x
	cmp #POOL_TAIL
	bcs @done
	pha
	jsr leave
	jsr add_base
	pla
	ldxy #$0000
	jmp enter
@done:	rts
.endproc

;*******************************************************************************
; CROSS PREV
; Moves the cursor to the end of the previous segment
; OUT:
;  - .C: set if there is no previous segment
.export cross_prev
.proc cross_prev
	ldx bank
	jsr find_prev
	bcs @done

	pha
	jsr leave
	pla
	pha
	jsr __src_seg_len
	jsr sub_base
	pla
	ldxy srctmp+2
	jmp enter
@done:	rts
.endproc

;*******************************************************************************
; NORMALIZE
; Restores the chain after the after-gap text of the active segment shrinks.
; Moves to the next segment if the cursor is at the end of this one,
; and leaves a segment that has become empty.
.export normalize
.proc normalize
	lda poststartzp
	cmp end
	bne @done

	lda poststartzp+1
	cmp end+1
	bne @done

	jsr cross_next
	bcc @done
	lda cursorzp
	bne @done

	lda cursorzp+1
	cmp #>__src_data
	bne @done
	jmp cross_prev

@done:	rts
.endproc

;*******************************************************************************
; SPLIT
; Called when the active segment is full and its gap is closed. Moves the
; upper half of the segment to a new one and moves into it if the cursor was
; there.
; OUT:
;  - .C: set if the pool is exhausted
.export split
.proc split
@off=srctmp
	jsr alloc_after
	bcs @done

	pha
	lda end+1
	sec
	sbc #>__src_data
	lsr
	sta @off		; page offset of the midpoint
	tax
	pla
	jsr __src_seg_copy	; move [midpoint, end) to the new segment

	lda #$00
	sta end
	lda @off
	clc
	adc #>__src_data
	sta end+1

	; if the cursor is in the lower half, we're done
	lda cursorzp
	cmp end
	lda cursorzp+1
	sbc end+1
	bcc @ok

	lda cursorzp
	sec
	sbc end
	sta @off
	lda cursorzp+1
	sbc end+1
	sta @off+1
	ldxy end
	stxy cursorzp
	stxy poststartzp
	jsr cross_next

	lda cursorzp
	clc
	adc @off
	sta cursorzp
	sta poststartzp
	lda cursorzp+1
	adc @off+1
	sta cursorzp+1
	sta poststartzp+1
@ok:	clc
@done:	rts
.endproc

;*******************************************************************************
; APPEND
; Called when loading and the active segment is full. Starts a new, empty
; segment after it and moves to it.
; OUT:
;  - .C: set if the pool is exhausted
.export append
.proc append
	jsr alloc_after
	bcs @done
	ldxy #$0000
	jsr __src_seg_set_len
	jmp cross_next
@done:	rts
.endproc

;*******************************************************************************
; JUMP
; Moves the cursor to a buffer position directly.
; IN:
;  - srctmp: the 24-bit position to move to (clamped to the end of the buffer)
.proc jump
@len=zp::bankaddr0
@pos=srctmp
	jsr leave
	ldx bank
	jsr head_of
	lda #$00
	sta base
	sta base+1
	sta base+2

@l0:	txa
	pha
	jsr __src_seg_len
	stxy @len

	; offset = pos - base
	sec
	lda @pos
	sbc base
	tax
	lda @pos+1
	sbc base+1
	tay
	lda @pos+2
	sbc base+2
	bne @next
	cpy @len+1
	bcc @found
	bne @next
	cpx @len
	bcc @found

@next:	pla
	pha
	tax
	lda next_of,x
	cmp #POOL_TAIL
	bcs @tail
	ldxy @len
	jsr add_base
	pla
	tax
	lda next_of,x
	tax
	bne @l0			; branch always

@tail:	ldxy @len
@found:	pla
	jmp enter
.endproc

;*******************************************************************************
; ATCURSOR PREV
; __src_atcursor for a cursor at the start of its segment: gets the last byte
; of the previous segment
; OUT:
;  - .A:        the byte (0 if the cursor is at the start of the buffer)
;  - .C:        set if the cursor is at the start of the buffer
;  - srctmp+1:  the previous segment's bank
;  - srctmp+2:  the previous segment's length - 1
.export atcursor_prev
.proc atcursor_prev
	ldx bank
	jsr find_prev
	bcc :+
	lda #$00
	rts

:	sta srctmp+1
	jsr __src_seg_len
	txa
	bne :+
	dey
:	dex
	stxy srctmp+2
	lda srctmp+1
	jsr __src_seg_peek
	clc
	rts
.endproc

;*******************************************************************************
; BACKSPACE PREV
; Deletes the last byte of the segment before the active one
.proc backspace_prev
	jsr atcursor_prev
	lda srctmp+1
	ldxy srctmp+2
	jsr __src_seg_set_len
	lda srctmp+2
	ora srctmp+3
	bne :+

	ldx srctmp+1
	jsr unlink

:	ldxy #$0001
	jmp sub_base
.endproc

;*******************************************************************************
; NEW LOG
; Initializes the LOG buffer.  This is a reserved buffer for use as a log
.export __src_new_log
.proc __src_new_log
	; save current source buffer state
	lda activesrc
	pha
	jsr __src_save

	; the LOG buffer draws from the same pool as user buffers; release the
	; previous log's banks first
	ldx banks+LOG_BUFFER
	beq :+
	txa
	jsr free_chain
	lda #$00
	sta banks+LOG_BUFFER
:	jsr alloc_bank
	bcc :+
	pla
	tax
	jsr set_no_save
	sec
	rts
:	sta bank

	; init the LOG buffer
	lda #LOG_BUFFER
	sta activesrc
	jsr init_buff

	; set cursor to (0,0) for the new log buffer
	lda #$00
	sta zp::curx
	sta zp::cury

	; name the buffer
	ldxy #@filename
	jsr __src_name

	; go back to the buffer we started on
	pla
	jmp __src_set
.PUSHSEG
.RODATA
@filename: .byte "log",0
.POPSEG
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
	beq @cont
	cpx #MAX_SOURCES	; all 8 user buffers in use?
	bcc @saveold
	rts			; err, too many sources

@saveold:
	lda activesrc
	jsr __src_save	; save current source data

@cont:	; find a free bank for the new buffer
	jsr alloc_bank
	bcc :+
	rts			; pool exhausted
:	sta bank

	lda numsrcs
	sta activesrc
	inc numsrcs

	; fall through to init_buff (.A = ID of the new buffer)
.endproc

;*******************************************************************************
; INIT BUFF
; Initializes the active state for a new buffer
; IN:
;   - .A: the buffer to initialize
.proc init_buff
	tay				; save buffer id

	; set name to 0 (unnamed)
	asl
	asl
	asl
	asl
	tax
	lda #$00
	sta names,x

	; clear the state for the new buffer (all SAVESTATE_SIZE bytes)
	ldx #SAVESTATE_SIZE
	;lda #$00
:	sta buffstate-1,x
	dex
	bne :-

	; mark the buffer as clean
	;lda #$00
	sta flags,y

	sta base
	sta base+1
	sta base+2

	; init line and lines to 1
	inc line
	inc lines

	jmp __src_init_buff
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
	adc #MAX_BUFFER_NAME_LEN
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
	lda numsrcs
	bne :+
	clc
	rts		; no buffer to close

:	jsr errlog::close_buffer
	lda bank
	jsr free_chain
	lda numsrcs
	cmp #$01	; is the current buffer the last one?
	bne @close

	; only one buffer open; re-initialize it to "close" it
	dec numsrcs	; set num sources back to 0
	jsr __src_new	; and initialize the buffer
	sec		; flag that a new buffer was created/initialized
	rts

@close: ; get the number of buffers to shift (numsrcs - activesrc - 1)
	lda numsrcs
	;sec
	sbc #$01
	sbc activesrc
	beq @cont	; if this was the last buffer, skip shifting
	sta @cnt
	pha		; save this count for moving names

	; get offset to start shifting at
	lda activesrc
	jsr mul_state_size
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
@l1:	lda banks+1,x
	sta banks,x
	lda buffs_curx+1,x
	sta buffs_curx,x
	lda buffs_cury+1,x
	sta buffs_cury,x
	inx
	cpx numsrcs
	bcc @l1
	; copy the names down
	jsr active_times_16	; .X=activesrc * 16

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
	ldx activesrc
	cpx numsrcs
	bcc :+

	dec activesrc
	dex

:	lda banks,x
	jsr set_no_save

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
	jsr active_times_16	; .X=activesrc * 16

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

:	tya
	pha
	jsr __src_pos
	stxy srctmp
	ldx sp
	inc sp
	sta stk_hi,x
	lda srctmp
	sta stk_lo,x
	lda srctmp+1
	sta stk_mid,x
	lda line
	sta stk_llo,x
	lda line+1
	sta stk_lhi,x
	pla
	tay
	RETURN_OK
.endproc

;*******************************************************************************
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
	ldx sp
	ldy stk_mid,x
	lda stk_lo,x
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
; BEFORE END
; Checks if the source cursor is located just before the end of the buffer.
; OUT:
;  - .Z: set if the cursor is before the end of the buffer
.export __src_before_end
.proc __src_before_end
	ldxy end
	sub16 poststartzp
	cmpw #1
	bne @done
	ldx bank
	lda next_of,x
	cmp #POOL_TAIL
@done:	rts
.endproc

;*******************************************************************************
; POS
; Returns the current source position.  You may go to this position with the
; src::goto routine.  Note that if the source changes since this procedure is
; called, this may not be the same (or expected) position
; OUT:
;  - .XY: the current source position (low 16 bits)
;  - .A:  the high 8 bits of the position
.export __src_pos
.proc __src_pos
	lda cursorzp+1
	sec
	sbc #>__src_data
	tay
	lda cursorzp
	clc
	adc base
	tax
	tya
	adc base+1
	tay
	lda base+2
	adc #$00
	rts
.endproc

;*******************************************************************************
; START
; Returns .Z set if the cursor is at the start of the buffer.
; OUT:
;  - .Z: set if the cursor is at the start of the buffer
.export __src_start
.proc __src_start
	ldx cursorzp
	bne @done
	ldx cursorzp+1
	cpx #>__src_data
	bne @done
	ldx base
	bne @done
	ldx base+1
	bne @done
	ldx base+2
@done:	rts
.endproc

;*******************************************************************************
; BACKSPACE
; Deletes the character immediately before the current cursor position.
;
; NOTE: srcx is resynchronized (via sync_x) if a newline is deleted.
;
; OUT:
;  - .A: the character that was deleted
;  - .C: set if the backspace failed (we're at the START of the source)
.export __src_backspace
.proc __src_backspace
	jsr __src_start
	beq @skip
	jsr __src_mark_dirty
	jsr __src_atcursor
	pha
	cmp #$0d
	bne :+
	decw line
	jsr on_line_deleted

:	lda cursorzp
	bne @local
	lda cursorzp+1
	cmp #>__src_data
	bne @local
	jsr backspace_prev
	jmp @x

@local:	decw cursorzp
	jsr normalize

@x:	dec srcx
	bpl :+
	jsr sync_x
:	pla
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

	; shift breakpoints and errors
	jsr errlog::deleted
	rts
.endproc

;*******************************************************************************
; DELETE
; Deletes the character at the current cursor position.
; OUT:
;  - .A: the character that was deleted
;  - .C: set if there is nothing to delete (buffer empty)
.export __src_delete
.proc __src_delete
	jsr __src_end
	bne @cont
	jsr __src_start
	beq @skip		; buffer is completely empty

	jmp __src_backspace	; at end of buffer, BACKSPACE instead of DELETE

@cont:	jsr __src_before_newl	; .A = char that will be deleted
	bne :+
	jsr on_line_deleted
	lda #$0d		; deleted char was a newline
:	pha
	incw poststartzp
	jsr normalize
	jsr __src_mark_dirty
	pla			; restore deleted char
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
.ifdef ultimem
	; stop at line boundary or scan back with the source bank mapped
	jsr __src_atcursor
	cmp #$0d
	beq @done
	jsr __src_start
	beq @done

	jsr __src_up
@done:	lda #$00
	sta srcx
	sec
	rts
.else
:	jsr __src_left
	bcc :-
	rts
.endif
.endproc

;*******************************************************************************
; LINE_END
; Moves right until at the end of the source buffer or the end of the line
.export __src_line_end
.proc __src_line_end
:	jsr __src_right
	bcc :-
	jmp sync_x
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
	bcs @ret
	jsr __src_after_cursor
	cmp #$0d
	bne @done
	jsr __src_next

@nomove:
	sec
@ret:	rts
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
@savex=r0
	jsr __src_end
	beq @endofline

	ldx srcx
	stx @savex

	jsr __src_next
	cmp #$0d
	bne @done

	jsr __src_prev
	ldx @savex
	stx srcx
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
@savex=r0
	jsr __src_before_end
	beq @endofline

	jsr __src_end		; should be impossible in REPLACE
	beq @endofline

	jsr __src_after_cursor	; if we're at end of line, don't move
	cmp #$0d
	beq @endofline

	ldx srcx
	stx @savex
	jsr __src_next
	jsr __src_after_cursor	;if moving would put us at end of buffer, stay
	bcs @back

	cmp #$0d
	bne @done

@back:	jsr __src_prev
	ldx @savex
	stx srcx

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
.ifndef ultimem
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
.else
	.import __src_up
.endif

;*******************************************************************************
; DOWN
; Moves the cursor beyond the next RETURN character (or to the end of
; the buffer if there is no such character
; OUT:
;  - .C: set if the end of the buffer was reached (cannot move "down")
.if .not (.defined(ultimem) .or .defined(c64))
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
.else
	.import __src_down
.endif

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
	jsr __src_after_cursor
	bcs :+			; if at end of buffer -> nothing to replace
	cmp #$0d
	beq :+
	jsr __src_delete
:	pla
	jsr __src_insert
	clc
	rts
.endproc

;*******************************************************************************
; BEFORE_NEWL
; Checks if src::after_cursor is a newline ($0d) or if the cursor is at the end
; of the buffer.
; OUT:
;  - .Z: set if src::aftercursor == $0d
.export __src_before_newl
.proc __src_before_newl
	jsr __src_end
	beq :+
	jsr __src_after_cursor
	cmp #$0d
:	rts
.endproc

;*******************************************************************************
; AFTER CURSOR
; Returns the character AFTER the cursor position.
; OUT:
;  - .A: the character after the current cursor position
;  - .C: set if there is no character after the cursor
.export __src_after_cursor
.proc __src_after_cursor
@xsave=zp::util
	jsr __src_end
	beq @end
.ifdef ultimem
	; peek byte after gap
	ldx poststartzp
	lda poststartzp+1
	sec
	sbc #>__src_data
	tay
	lda bank
	jsr __src_seg_peek
.else
	lda srcx
	sta @xsave
	jsr __src_next
	pha
	jsr __src_prev
	lda @xsave	; restore srcx (clobbered if next/prev crossed a newline)
	sta srcx
	pla
.endif
	RETURN_OK

@end:	lda #$00	; no character, return 0
	sec		; end of buffer
	rts
.endproc

;*******************************************************************************
; REWIND
; Moves the cursor to the start of the buffer and opens the default-sized gap
; OUT:
;  - .A: error code if the gap could not be opened
;  - .C: set if bank pool is used up and gap remains closed
.export __src_rewind
.proc __src_rewind
	lda #$00
	sta srctmp
	sta srctmp+1
	sta srctmp+2
	jsr jump

	; reset cursors (x/line)
	lda #$01
	sta line
	lda #$00
	sta line+1
	sta srcx
	jmp __src_open_gap
.endproc

;*******************************************************************************
; READLINE CONT
; Reads one line from the current source cursor position AND reads any
; subsequent lines so long as each line read ends with a '<-'
.export __src_readline_cont
.proc __src_readline_cont
	lda #$01		; enable <- continuation
	skw

	; fall through to __src_readline
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
@cont=r5
@lim=r6
	lda #$00		; disable <- continuation (READLINE entry)
	sta @cont		; set/clear continue flag

	; set read limit to LINESIZE for reading physical lines or
	; MAX_LINE_LEN+1 if reading a logical line (has continuation chars)
	ldx #LINESIZE
	lda @cont
	beq :+
	ldx #MAX_LINE_LEN+1
:	stx @lim

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

@store:	cpx @lim
	bcs :+			; if we have >= the store limit, don't store
	sta mem::linebuffer,x

:	cmp #$00
	beq @done
	inc @cnt
	bne @chkend
	dec @cnt		; saturate the count for very long lines
@chkend:
	jsr __src_end
	bne @l0
@eof:	; end of source: null terminate at the line length (or buffer cap)
	ldx @cnt
	cpx @lim
	bcc :+
	ldx @lim		; clamp: line was truncated at the buffer size
	stx @cnt
:	lda #$00
	sta mem::linebuffer,x
@eofdone:
	lda @cnt
	sec
	rts
@done:	cpx @lim
	bcs @trunc		; full/truncated line: no continuation

	; check for a line-continuation marker (<-) as the last character
	lda @cont
	beq @term		; continuation disabled: normal end of line
	cpx #$00
	beq @term		; empty line, nothing to check
	lda mem::linebuffer-1,x
	cmp #LINE_CONT
	bne @term		; normal end of line

	; continuation: back up so the next line overwrites the marker
	dex
	stx @cnt
	jsr __src_end
	bne @l0			; more source: keep filling the same buffer

	; end of source right after the marker: terminate where it was
	ldx @cnt
	lda #$00
	sta mem::linebuffer,x
	txa
	clc
	rts

@term:	txa			; return length, terminator already in place
	clc
	rts

@trunc:	ldx @lim		; a full-length line still needs termination
	lda #$00
	sta mem::linebuffer,x
	txa			; return the (clamped) length
	clc
	rts
.endproc

;*******************************************************************************
; POPGOTO
; Navigates to the the most recent source position pushed
.export __src_popgoto
.proc __src_popgoto
@d=r4
	jsr __src_popp
	bcs @ret

	jsr __src_pos
	stxy srctmp
	sta srctmp+2

	; distance = target - current
	ldx sp
	sec
	lda stk_lo,x
	sbc srctmp
	sta @d
	lda stk_mid,x
	sbc srctmp+1
	sta @d+1
	lda stk_hi,x
	sbc srctmp+2
	beq @fwd
	cmp #$ff
	bne @jump
	lda @d+1
	cmp #<-(NEAR_DIST>>8)
	bcs move_by
	bcc @jump		; branch always
@fwd:	lda @d+1
	cmp #>NEAR_DIST
	bcc move_by

@jump:	; far away: go straight there and restore the line it was on
	lda stk_lo,x
	sta srctmp
	lda stk_mid,x
	sta srctmp+1
	lda stk_hi,x
	sta srctmp+2
	lda stk_llo,x
	sta line
	lda stk_lhi,x
	sta line+1
	jsr jump
	jmp sync_x
@ret:	rts
.endproc

;*******************************************************************************
; GOTO
; Goes to the source position given. As the position is only 16 bits, the
; nearest position with those low bits is used.
; IN:
;  - .XY: the source position to go to (see src::pos, src::pushp, src::popp)
.export __src_goto
.proc __src_goto
@d=r4
	stxy @d
	jsr __src_pos
	stxy srctmp
	lda @d
	sec
	sbc srctmp
	sta @d
	lda @d+1
	sbc srctmp+1
	sta @d+1

	; fall through to move_by
.endproc

;*******************************************************************************
; MOVE BY
; Moves the cursor by the signed 16-bit count in r4
.proc move_by
@d=r4
	lda @d+1
	bmi @back

@fwd:	lda @d
	ora @d+1
	beq @done
	jsr __src_end
	beq @done
	jsr __src_next
	decw @d
	jmp @fwd

@back:	lda @d
	ora @d+1
	beq @done
	jsr __src_prev
	bcs @done
	incw @d
	jmp @back

@done:	jmp sync_x
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
	jmp __src_getin
.endproc

;*******************************************************************************
; GET WIDE
; Reads up to MAX_LINE_LEN characters without moving the source cursor.
.export __src_getwide
.proc __src_getwide
	ldxy #mem::linebuffer
	lda #MAX_LINE_LEN
	jmp __src_readspan
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

	; if .XY == 0, we're done
	txa
	bne :+
	tya
	beq @ok

:	jsr __src_down
	bcc @loop
@done:	ldxy @cnt
	rts

@ok:	RETURN_OK
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
.ifndef ultimem
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
.else
	.import __src_upn
.endif

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
	lda #errlog::NAV_NONE
	sta errlog::navpending
	lda #FLAG_DIRTY
	sta errlog::editpending
	ldx activesrc
	sta flags,x
	rts
.endproc

;*******************************************************************************
; MUL STATE SIZE
; Returns the input value times the size of each buffer's state
; IN:
;    - .A: value to multiply by SAVESTATE_SIZE
; OUT:
;    - .A: SAVESTATE_SIZE*.A
.proc mul_state_size
@tmp=r2
	sta @tmp
	asl		; *2
	asl		; *4
	adc @tmp	; *5
	asl		; *10
	adc @tmp	; *11
	rts
.endproc

;*******************************************************************************
; ACTIVE TIMES 16
; Multiplies the active source id by 16
; OUT:
;   - .A: activesrc * 16
;   - .X: activesrc * 16
.proc active_times_16
	lda activesrc
	asl
	asl
	asl
	asl
	tax
	rts
.endproc
