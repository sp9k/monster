;*******************************************************************************
; UNDO.ASM
; Records per-buffer edits in a shared 32 KiB circular journal
; an insert session stays pending until command mode, save, or buffer switch
; only complete records contribute to head, tail, and used
; each record contains a 22-byte header, five-byte text changes, before/after
; annotation snapshots, and a two-byte trailer containing the record size
; each text change stores an operation, a 24-bit source offset, and one byte
; undo reads changes backward and swaps insertion with deletion
; redo reads changes forward and repeats the recorded operations
; image writes to pages 28..31 clear and disable the journal until release
;*******************************************************************************

.include "config.inc"
.ifdef ultimem
.include "macros.inc"
.include "ram.inc"
.include "source.inc"
.include "edit.inc"
.include "text.inc"
.include "memory.inc"
.include "viewport.inc"
.include "zeropage.inc"
.include "errlogconst.inc"

; undo and error-log banks map the same annotation RAM
.import __errlog_insertmode, __errlog_deletemode
.import __errlog_undo_snapshot, __errlog_undo_restore
.import __errlog_undo_backup, __errlog_undo_rollback
.import __errlog_recheck_full, __edit_enter_command
.import __edit_visual_line, __edit_visual_pos, __edit_visual_x
.import __errlog_refresh
.import __errlog_clear_live_changed, __errlog_recheck_line

.import sync_x
.import bank_next, __src_bank, find_prev, __src_seg_len

.macpack longbranch

;*******************************************************************************
CAPACITY      = UNDO_RAM_BANKS*$2000
HEADER        = 22
TRAILER       = 2
CHANGE        = 5
H_SIZE        = 0
H_STATE       = 2
H_OWNER       = 3
H_BEFORE      = 4
H_AFTER       = 7
H_OLDREV      = 10
H_NEWREV      = 12
H_BEFORE_ROW  = 14
H_AFTER_ROW   = 15
H_BEFORE_VIEW = 16
H_AFTER_VIEW  = 17
H_BEFORE_META = 18
H_AFTER_META  = 20
APPLIED       = $01
INSERT        = $00
DELETE        = $01
OP_MASK       = $01

;*******************************************************************************
; newline modes occupy bits 1..2 beside the insertion/deletion bit
.assert ERRLOG_SPLIT_LINE  = 0, error, "split mode"
.assert ERRLOG_JOIN_LINES  = 0, error, "join mode"
.assert ERRLOG_BLANK_ABOVE = ERRLOG_DELETE_ABOVE, error, "above mode"
.assert ERRLOG_BLANK_BELOW = ERRLOG_DELETE_BELOW, error, "below mode"

;*******************************************************************************
; recording hooks save and restore r0..r9 around journal traversal
scan      = r0
remaining = r2
io        = r8

; block I/O saves and restores ra..rf around each transfer
io_count  = ra
io_config = rb
io_banklo = rc
io_bankhi = rd
io_zplo   = re
io_zphi   = rf

; offsets above SP after SAVE_REGS or SAVE_IO finishes saving scratch
; saved hook state: scratch, Y, X, A, P; saved I/O state: scratch, X, A, P
STACK_PAGE        = $0100
HOOK_SCRATCH_SIZE = io+2-scan
HOOK_SAVED_A      = HOOK_SCRATCH_SIZE+3
IO_SCRATCH_SIZE   = io_zphi+1-io_count
IO_SAVED_X        = IO_SCRATCH_SIZE+1
IO_SAVED_A        = IO_SCRATCH_SIZE+2

;*******************************************************************************
; MACROS

;*******************************************************************************
; UNDO ENTRY
; defines an exported bank-switching entry point
; IN:
;  - public: exported entry-point name
;  - target: routine in the undo bank
; OUT:
;  - public: callable entry point for target
.macro UNDO_ENTRY public, target
;*******************************************************************************
; BANK ENTRY
; IN:
;  - .A: operation argument
; OUT:
;  - .AXY: target result
.export public
.proc public
	BANK_ENTRY undo_table, "UNDO_CODE", target
.endproc
.endmacro

;*******************************************************************************
; SAVE REGS
; saves caller registers, flags and hook scratch on the stack
; IN:
;  - .A: caller accumulator
;  - .X: caller X register
;  - .Y: caller Y register
;  - .P: caller processor flags
;  - scan..io+1: caller hook scratch
; OUT:
;  - stack: state consumed by RESTORE_REGS
.macro SAVE_REGS
.local @save
	php
	pha
	txa
	pha
	tya
	pha

	ldx #$00
@save:	lda scan,x
	pha
	inx
	cpx #HOOK_SCRATCH_SIZE
	bne @save
.endmacro

;*******************************************************************************
; RESTORE REGS
; restores saved hook state and returns to the caller
; IN:
;  - stack: state saved by SAVE_REGS
; OUT:
;  - .A: restored caller accumulator
;  - .X: restored caller X register
;  - .Y: restored caller Y register
;  - .P: restored caller processor flags
;  - scan..io+1: restored caller hook scratch
.macro RESTORE_REGS
.local @restore
	ldx #HOOK_SCRATCH_SIZE-1
@restore:
	pla
	sta scan,x
	dex
	bpl @restore
	pla
	tay
	pla
	tax
	pla
	plp
	rts
.endmacro

;*******************************************************************************
; SAVE IO
; saves transfer state and maps the journal fragment for block I/O
; IN:
;  - .A: transfer byte count
;  - .X: transfer buffer index
;  - io: journal offset
; OUT:
;  - .X: preserved transfer buffer index
;  - .Y: zero
;  - io_count: transfer byte count
;  - zp::bankaddr0: mapped journal address
;  - stack: state consumed by io_done
.macro SAVE_IO
.local @save
	php
	pha
	txa
	pha
	ldx #$00
@save:	lda io_count,x
	pha
	inx
	cpx #IO_SCRATCH_SIZE
	bne @save
	tsx
	lda STACK_PAGE+IO_SAVED_A,x
	sta io_count
	jsr map
	tsx
	lda STACK_PAGE+IO_SAVED_X,x
	tax
.endmacro

;*******************************************************************************
; DATA
.segment "UNDO_VARS"
undo_vars:
blocked:      .byte 0 ; image owns the journal banks
recording:    .byte 0 ; capture source changes for this key
replaying:    .byte 0 ; skip recording during replay or rollback
pending:      .byte 0 ; record is open for changes before commit
head:         .word 0 ; oldest record offset
tail:         .word 0 ; offset after committed records
used:         .word 0 ; committed journal byte count
transaction:  .word 0
size:         .word 0
selected:     .word 0
redo_pending: .res  MAX_SOURCES
owner:        .byte 0
operation:    .byte 0 ; replay direction: zero undo, one redo
position:     .res  3
before:       .res  3
before_row:   .byte 0
before_view:  .byte 0
sequence:     .word 0
revisionlo:   .res  MAX_SOURCES
revisionhi:   .res  MAX_SOURCES
savedlo:      .res  MAX_SOURCES
savedhi:      .res  MAX_SOURCES
header:       .res  HEADER

;*******************************************************************************
; JOURNAL BUFFER
; Contains a five-byte text change or up to $40 bytes of annotation data
journal_buffer: .res $40
.assert journal_buffer-header = HEADER, error, "journal transfer buffer layout"

before_bytes:      .word 0
metadata_bytes:    .word 0
write_pos:         .word 0 ; next pending text change or annotation footer
transfer_ptr:      .word 0
transfer_left:     .word 0
transfer_read:     .byte 0
transfer_count:    .byte 0
rollback_position: .res 3
rollback_row:      .byte 0
rollback_view:     .byte 0
rollback_flags:    .byte 0
rollback_reason:   .byte 0
rollback_mode:     .byte 0

walk:           .word 0
walkend:        .word 0
session_lost:   .byte 0 ; recording stopped after an oversized transaction
run_cursor:     .word 0
run_bank:       .byte 0
check_first:    .word 0
check_last:     .word 0
check_current:  .word 0
restore_line:   .word 0
check_position: .res  3
undo_vars_end:
.assert undo_vars_end-undo_vars < $100, error, "undo initialization size"

;*******************************************************************************
metadata_before:  .res UNDO_META_MAX
metadata_after:   .res UNDO_META_MAX
metadata_restore: .res UNDO_META_MAX
.assert * <= $4000, lderror, "annotation transfers must stay in BLK1"
.assert HEADER+$40 <= $100, error, "journal transfer buffer index"
.assert UNDO_META_MAX*2+CHANGE < $400, error, "fast typing footer reserve"

; line_pages flags initialized 256-byte blocks of line_bits
; each block holds 2048 line flags and is cleared on its first use per replay
line_pages: .res  $20
line_bits:  .res  $2000

; fast_key and fast_char are resident across all bank mappings
.DATA
fast_key:  .byte $01        ; $80 armed, $00 captured, $01 inactive
.BSS
fast_char: .byte 0

;*******************************************************************************
; dispatch table for calls from MAIN to the undo bank
.RODATA
BANK_TABLE undo_table, FINAL_BANK_UNDO, "UNDO_CODE"

;*******************************************************************************
; ENTRY POINTS
; IN:
;  - .A: source byte for mutation hooks; image page for claim
; OUT:
;  - .AXY: preserved by recording and ownership hooks
UNDO_ENTRY __undo_init, init
UNDO_ENTRY __undo_discard, discard
UNDO_ENTRY __undo_commit, commit
UNDO_ENTRY insert_slow, inserted
UNDO_ENTRY __undo_delete, deleted
UNDO_ENTRY __undo_backspace, backspaced
UNDO_ENTRY __undo_saved, saved
UNDO_ENTRY __undo_close, closed
UNDO_ENTRY __undo_claim, claim
UNDO_ENTRY __undo_release, release
UNDO_ENTRY __undo_undo, undo
UNDO_ENTRY __undo_redo, redo

;*******************************************************************************
; INSERT
; Stores a typing byte in fast_char when fast_key is armed
; otherwise records the insertion through insert_slow
; IN:
;  - .A: inserted source byte
; OUT:
;  - .AXY: preserved
.CODE
.export __undo_insert
.proc __undo_insert
	asl fast_key
	bcc @slow
	sta fast_char
	rts
@slow:	jmp insert_slow
.endproc

BANKED_SEG "UNDO_CODE", FINAL_BANK_UNDO

;*******************************************************************************
; INIT
; Clears journal ownership, buffer revisions and transaction state
; OUT:
;  - .AXY: preserved
.proc init
	SAVE_REGS
	lda #$01
	sta fast_key
	lda #$00
	ldx #undo_vars_end-undo_vars-1
@clear:	sta undo_vars,x
	dex
	cpx #$ff
	bne @clear
	RESTORE_REGS
.endproc

;*******************************************************************************
; CLEAR
; Empties the journal and clears pending edits, typing state and redo flags
.proc clear
	; discarded pending edits no longer have a revision to undo back to
	lda pending
	beq @empty
	ldx owner
	lda #$ff
	sta savedlo,x
	sta savedhi,x
@empty:	lda #$01
	sta fast_key
	lda #$00
	sta head
	sta head+1
	sta tail
	sta tail+1
	sta used
	sta used+1
	sta pending
	ldx #MAX_SOURCES-1
@redo:	sta redo_pending,x
	dex
	bpl @redo
	rts
.endproc

;*******************************************************************************
; DISCARD
; Empties the journal and preserves caller registers
; OUT:
;  - .AXY: preserved
.proc discard
	SAVE_REGS
	jsr clear
	RESTORE_REGS
.endproc

;*******************************************************************************
; CLAIM
; Clears history and disables recording and replay when the image page
; falls in the journal's RAM banks
; IN:
;  - .A: image page index
; OUT:
;  - .AXY: preserved
.proc claim
	jsr flush_key
	SAVE_REGS
	tsx
	lda STACK_PAGE+HOOK_SAVED_A,x
	cmp #UNDO_IMAGE_PAGE
	bcc @done
	lda blocked
	bne @done
	jsr clear
	lda #$01
	sta blocked
	lda #$00
	sta recording
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; RELEASE
; Clears history and re-enables the journal if its banks belong to the image
; OUT:
;  - .AXY: preserved
.proc release
	SAVE_REGS
	lda blocked
	beq @done
	jsr clear
	lda #$00
	sta blocked
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; BEGIN
; Prepares recording for an editor key and captures its starting position
; captures annotations when starting a new editing group
; OUT:
;  - .AXY: preserved
.proc begin
	jsr flush_key
	SAVE_REGS
	lda #$00
	sta recording
	lda zp::editor_mode
	cmp #MODE_INSERT
	beq @session
	jsr commit
	lda #$00
	sta session_lost
@session:
	lda session_lost
	beq @available
	lda src::activebuff
	cmp owner
	beq @done
	lda #$00
	sta session_lost
@available:
	lda blocked
	bne @done
	lda src::activebuff
	cmp #MAX_SOURCES
	bcs @done
	sta owner
	lda pending
	bne @record
	ldxy #metadata_before
	CALL FINAL_BANK_ERRLOG, __errlog_undo_snapshot
	stxy before_bytes
	CALLMAIN src::pos
	stxy before
	sta before+2
	lda zp::cury
	sta before_row
	lda viewport::offset
	sta before_view
@record:
	lda #$01
	sta recording
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; MAP
; Saves the BLK2 mapping and maps the journal bank containing io
; IN:
;  - io: journal offset
; OUT:
;  - zp::bankaddr0: mapped byte address
;  - .Y: zero
.proc map
	lda zp::bankaddr0
	sta io_zplo
	lda zp::bankaddr0+1
	sta io_zphi
	lda $9ff2
	sta io_config
	ora #$0c
	sta $9ff2
	lda $9ffa
	sta io_banklo
	lda $9ffb
	sta io_bankhi
	lda io+1
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #UNDO_RAM_BANK
	sta $9ffa
	lda #$00
	sta $9ffb
	lda io
	sta zp::bankaddr0
	lda io+1
	and #$1f
	ora #$40
	sta zp::bankaddr0+1
	ldy #$00
	rts
.endproc

;*******************************************************************************
; UNMAP
; Restores the BLK2 mapping and byte pointer saved by map
.proc unmap
	lda io_banklo
	sta $9ffa
	lda io_bankhi
	sta $9ffb
	lda io_config
	sta $9ff2
	lda io_zplo
	sta zp::bankaddr0
	lda io_zphi
	sta zp::bankaddr0+1
	rts
.endproc

;*******************************************************************************
; READ BLOCK
; Copies journal bytes into header and journal_buffer
; IN:
;  - .A: byte count
;  - .X: destination index relative to header
;  - io: journal offset
; OUT:
;  - header/journal_buffer: bytes read at the destination index
;  - io: next offset
.proc read_block
	SAVE_IO
@loop:	lda (zp::bankaddr0),y
	sta header,x
	inx
	dec io_count
	beq io_done
	jsr next_byte
	jmp @loop
.endproc

;*******************************************************************************
; WRITE BLOCK
; Copies bytes from header and journal_buffer into the journal
; IN:
;  - .A: byte count
;  - .X: source index relative to header
;  - io: journal offset
; OUT:
;  - io: next offset
.proc write_block
	SAVE_IO
@loop:	lda header,x
	sta (zp::bankaddr0),y
	inx
	dec io_count
	beq io_done
	jsr next_byte
	jmp @loop
.endproc

;*******************************************************************************
; NEXT BYTE
; Advances the byte pointer, switching journal banks and wrapping at the end
; OUT:
;  - zp::bankaddr0: next mapped byte
.proc next_byte
	inc zp::bankaddr0
	bne @done
	inc zp::bankaddr0+1
	lda zp::bankaddr0+1
	cmp #$60
	bne @done
	lda #$40
	sta zp::bankaddr0+1
	inc $9ffa
	lda $9ffa
	cmp #UNDO_RAM_BANK+UNDO_RAM_BANKS
	bcc @done
	lda #UNDO_RAM_BANK
	sta $9ffa
@done:	rts
.endproc

;*******************************************************************************
; IO DONE
; Advances io by the byte count, then restores the mapping and saved state
; IN:
;  - stack: saved scratch and caller registers
; OUT:
;  - .AX: preserved
;  - io: next offset
.proc io_done
	tsx
	lda STACK_PAGE+IO_SAVED_A,x
	clc
	adc io
	sta io
	lda io+1
	adc #$00
	and #$7f
	sta io+1
	jsr unmap
	ldx #IO_SCRATCH_SIZE-1
@restore:
	pla
	sta io_count,x
	dex
	bpl @restore
	pla
	tax
	pla
	plp
	rts
.endproc

;*******************************************************************************
; READ HEADER
; Reads the transaction header at scan into header
; IN:
;  - scan: journal offset
; OUT:
;  - header: transaction header
.proc read_header
	ldxy scan
	stxy io
	ldx #$00
	lda #HEADER
	jmp read_block
.endproc

;*******************************************************************************
; WRITE HEADER
; Writes header to the transaction at scan
; IN:
;  - scan: journal offset
;  - header: transaction header
.proc write_header
	ldxy scan
	stxy io
	ldx #$00
	lda #HEADER
	jmp write_block
.endproc

;*******************************************************************************
; SCAN START
; Sets scan to the oldest record and remaining to the journal byte count
; OUT:
;  - scan: first transaction
;  - remaining: bytes to traverse
.proc scan_start
	ldxy head
	stxy scan
	ldxy used
	stxy remaining
	rts
.endproc

;*******************************************************************************
; SCAN NEXT
; Advances scan by the loaded record size and subtracts it from remaining
; IN:
;  - header: current transaction
; OUT:
;  - scan: next transaction
;  - remaining: bytes left
.proc scan_next
	sec
	lda remaining
	sbc header+H_SIZE
	sta remaining
	lda remaining+1
	sbc header+H_SIZE+1
	sta remaining+1
	clc
	lda scan
	adc header+H_SIZE
	sta scan
	lda scan+1
	adc header+H_SIZE+1
	and #$7f
	sta scan+1
	rts
.endproc

;*******************************************************************************
; SCAN PREV
; Reads the preceding record size from its trailer and moves scan backward
; IN:
;  - scan: offset immediately after the transaction
;  - remaining: bytes left to traverse
; OUT:
;  - scan: preceding transaction
;  - remaining: bytes preceding that transaction
.proc scan_prev
	sec
	lda scan
	sbc #TRAILER
	sta io
	lda scan+1
	sbc #$00
	and #$7f
	sta io+1
	ldx #H_SIZE
	lda #TRAILER
	jsr read_block
	sec
	lda scan
	sbc header+H_SIZE
	sta scan
	lda scan+1
	sbc header+H_SIZE+1
	and #$7f
	sta scan+1
	sec
	lda remaining
	sbc header+H_SIZE
	sta remaining
	lda remaining+1
	sbc header+H_SIZE+1
	sta remaining+1
	rts
.endproc

;*******************************************************************************
; PURGE REDO
; Marks the buffer's undone records with owner $ff and clears its redo flag
; reclaims consecutive orphaned records at the journal tail
; IN:
;  - owner: buffer index
.proc purge_redo
	ldx owner
	lda redo_pending,x
	beq @done
	lda #$00
	sta redo_pending,x
	jsr scan_start
@loop:	lda remaining
	ora remaining+1
	beq @trim
	jsr read_header
	lda header+H_OWNER
	cmp owner
	bne @next
	lda header+H_STATE
	bne @next
	lda #$ff
	sta header+H_OWNER
	jsr write_header
@next:	jsr scan_next
	jmp @loop

@trim:	ldxy tail
	stxy scan
	ldxy used
	stxy remaining
@last:	lda remaining
	ora remaining+1
	beq @done
	jsr scan_prev
	jsr read_header
	lda header+H_OWNER
	cmp #$ff
	bne @done
	ldxy scan
	stxy tail
	ldxy remaining
	stxy used
	jmp @last
@done:	rts
.endproc

;*******************************************************************************
; MAKE ROOM
; Removes oldest records until the pending record fits in the journal
; IN:
;  - size: total pending bytes, including its trailer
; OUT:
;  - .C: set if size is greater than or equal to CAPACITY
.proc make_room
	lda size+1
	cmp #>CAPACITY
	bcs @too_big
@check:	lda used
	adc size
	lda used+1
	adc size+1
	cmp #>CAPACITY
	bcc @done
	ldx #$00
@save:	lda header,x
	pha
	inx
	cpx #HEADER
	bne @save
	jsr scan_start
@evict:	jsr read_header
	jsr scan_next
	ldxy scan
	stxy head
	ldxy remaining
	stxy used
	clc
	lda used
	adc size
	lda used+1
	adc size+1
	cmp #>CAPACITY
	bcs @evict
	ldx #HEADER-1
@restore:
	pla
	sta header,x
	dex
	bpl @restore
	rts

@too_big:
	jsr clear
	lda #$01
	sta session_lost
	lda #$00
	sta recording
	sec
@done:	rts
.endproc

;*******************************************************************************
; START TRANSACTION
; Starts a pending record at tail, outside the committed journal
; new records capture the buffer revision, cursor and viewport before editing
; IN:
;  - owner: buffer index
; OUT:
;  - transaction: journal offset
;  - size: header, text changes and trailer bytes, excluding annotations
.proc start_transaction
	jsr purge_redo
	ldxy tail
	stxy transaction
	ldxy #HEADER+TRAILER
	stxy size
	jsr make_room
	ldx #HEADER-1
	lda #$00

@clear:	sta header,x
	dex
	bpl @clear
	lda owner
	sta header+H_OWNER
	tax
	lda revisionlo,x
	sta header+H_OLDREV
	lda revisionhi,x
	sta header+H_OLDREV+1

	ldx #$02
@pos:	lda before,x
	sta header+H_BEFORE,x
	dex
	bpl @pos
	lda before_row
	sta header+H_BEFORE_ROW
	lda before_view
	sta header+H_BEFORE_VIEW
	lda #$01
	sta pending
	clc
	lda transaction
	adc #HEADER
	sta write_pos
	lda transaction+1
	adc #$00
	and #$7f
	sta write_pos+1
	rts
.endproc

;*******************************************************************************
; INSERTED
; Records an insertion at the current source position
; IN:
;  - .A: inserted source byte
; OUT:
;  - .AXY: preserved
.proc inserted
	jsr flush_key
	SAVE_REGS
	lda #INSERT
	jmp capture
.endproc

;*******************************************************************************
; DELETED
; Records deletion of the byte after the source gap
; IN:
;  - .A: deleted byte
; OUT:
;  - .AXY: preserved
.proc deleted
	jsr flush_key
	SAVE_REGS
	lda #DELETE
	jmp capture
.endproc

;*******************************************************************************
; BACKSPACED
; Records deletion of the byte before the source gap
; IN:
;  - .A: deleted byte
; OUT:
;  - .AXY: preserved
.proc backspaced
	jsr flush_key
	SAVE_REGS
	lda #$02
	jmp capture
.endproc

;*******************************************************************************
; CAPTURE
; Records one insertion or deletion with its source offset and newline mode
; cancels adjacent inverse changes to the same non-newline byte
; IN:
;  - .A: operation (insert, delete, or backspace)
;  - stack: saved caller registers
; OUT:
;  - .AXY: restored
.proc capture
@captureop = r4
@character = r5
	sta @captureop
	tsx
	lda STACK_PAGE+HOOK_SAVED_A,x
	sta @character
	lda replaying
	jne @done
	lda src::activebuff
	cmp #MAX_SOURCES
	jcs @done
	lda blocked
	jne @untracked
	lda recording
	jeq @untracked
	lda src::activebuff
	cmp owner
	jne @untracked
	CALLMAIN src::pos
	stxy position
	sta position+2
	lda @captureop
	cmp #$02
	bne @pending
	lda #DELETE
	sta @captureop
	lda position
	bne :+
	lda position+1
	bne :+
	dec position+2

:	decw position
@pending:
	lda @character
	cmp #$0d
	bne @transaction
	lda __errlog_insertmode
	ldx @captureop
	beq :+
	lda __errlog_deletemode
:	asl
	ora @captureop
	sta @captureop

;-------------------------------------------------------------------------------
@transaction:
	lda pending
	bne :+
	jsr start_transaction
:	lda size+1
	bne @previous
	lda size
	cmp #HEADER+TRAILER
	beq @append

;-------------------------------------------------------------------------------
@previous:
	sec
	lda write_pos
	sbc #CHANGE
	sta walk
	lda write_pos+1
	sbc #$00
	and #$7f
	sta walk+1
	jsr read_change
	lda journal_buffer
	eor @captureop
	and #OP_MASK
	beq @append
	lda journal_buffer+4
	cmp @character
	bne @append
	cmp #$0d
	beq @append		; retain newline insertion/deletion pairs
	ldx #$02
@same:	lda journal_buffer+1,x
	cmp position,x
	bne @append
	dex
	bpl @same
	lda size
	sbc #CHANGE
	sta size
	lda size+1
	sbc #$00
	sta size+1
	ldxy walk
	stxy write_pos
	jmp @done

;-------------------------------------------------------------------------------
@append:
	clc
	lda size
	adc #CHANGE
	sta size
	bcc :+
	inc size+1
:	jsr make_room
	bcs @done
	ldxy write_pos
	stxy io
	lda @captureop
	sta journal_buffer
	ldx #$02
@pos:	lda position,x
	sta journal_buffer+1,x
	dex
	bpl @pos
	lda @character
	sta journal_buffer+4
	ldx #HEADER
	lda #CHANGE
	jsr write_block
	ldxy io
	stxy write_pos
@done:	RESTORE_REGS

;-------------------------------------------------------------------------------
; invalidate this buffer's saved revision and history after an unrecorded edit
@untracked:
	ldx src::activebuff
	lda #$ff
	sta savedlo,x
	sta savedhi,x
	lda #$00
	sta recording
	sta pending
	sta redo_pending,x
	lda blocked
	bne @done
	jsr scan_start
@purge:	lda remaining
	ora remaining+1
	beq @done
	jsr read_header
	lda header+H_OWNER
	cmp src::activebuff
	bne @next
	lda #$ff
	sta header+H_OWNER
	jsr write_header
@next:	jsr scan_next
	jmp @purge
.endproc

;*******************************************************************************
; TRANSFER METADATA
; Copies annotations between RAM and the journal in chunks of up to $40 bytes
; IN:
;  - transfer_ptr: annotation address
;  - transfer_left: byte count
;  - transfer_read: nonzero to read
;  - io: journal offset
; OUT:
;  - io: next journal offset
.proc transfer_metadata
@ptr=r6
@again:	lda transfer_left
	ora transfer_left+1
	beq @done
	lda #$40
	ldx transfer_left+1
	bne @count
	cmp transfer_left
	bcc @count
	lda transfer_left
@count:	sta transfer_count
	ldxy transfer_ptr
	stxy @ptr
	lda transfer_read
	bne @read

	ldy #$00
@pack:	lda (@ptr),y
	sta journal_buffer,y
	iny
	cpy transfer_count
	bne @pack
	ldx #HEADER
	lda transfer_count
	jsr write_block
	jmp @next

@read:	ldx #HEADER
	lda transfer_count
	jsr read_block

	ldy #$00
@unpack:
	lda journal_buffer,y
	sta (@ptr),y
	iny
	cpy transfer_count
	bne @unpack
@next:	clc
	lda transfer_ptr
	adc transfer_count
	sta transfer_ptr
	bcc :+
	inc transfer_ptr+1
:	sec
	lda transfer_left
	sbc transfer_count
	sta transfer_left
	lda transfer_left+1
	sbc #$00
	sta transfer_left+1
	jmp @again
@done:	rts
.endproc

;*******************************************************************************
; WRITE METADATA
; Stores cached before/after annotations after the transaction's text changes
; IN:
;  - write_pos: annotation footer offset
;  - header: snapshot byte counts
;  - metadata_before: pre-edit annotations
;  - metadata_after: post-edit annotations
.proc write_metadata
	ldxy write_pos
	stxy io
	lda #$00
	sta transfer_read
	ldxy #metadata_before
	stxy transfer_ptr
	ldxy header+H_BEFORE_META
	stxy transfer_left
	jsr transfer_metadata
	ldxy #metadata_after
	stxy transfer_ptr
	ldxy header+H_AFTER_META
	stxy transfer_left
	jmp transfer_metadata
.endproc

;*******************************************************************************
; READ METADATA
; Loads the selected annotation snapshot from the transaction footer
; IN:
;  - selected: transaction offset
;  - header: transaction header
;  - operation: zero for undo, one for redo
; OUT:
;  - metadata_restore: selected annotations
.proc read_metadata
	clc
	lda selected
	adc header+H_SIZE
	sta io
	lda selected+1
	adc header+H_SIZE+1
	and #$7f
	sta io+1
	sec
	lda io
	sbc #TRAILER
	sta io
	lda io+1
	sbc #$00
	sta io+1
	sec
	lda io
	sbc header+H_AFTER_META
	sta io
	lda io+1
	sbc header+H_AFTER_META+1
	and #$7f
	sta io+1
	ldx #H_AFTER_META
	lda operation
	bne @read
	sec
	lda io
	sbc header+H_BEFORE_META
	sta io
	lda io+1
	sbc header+H_BEFORE_META+1
	and #$7f
	sta io+1

	ldx #H_BEFORE_META
@read:	lda header,x
	sta transfer_left
	lda header+1,x
	sta transfer_left+1
	ldxy #metadata_restore
	stxy transfer_ptr
	lda #$01
	sta transfer_read
	jmp transfer_metadata
.endproc

;*******************************************************************************
; COMMIT
; Writes the transaction header, annotations and size trailer
; records the resulting cursor, viewport and buffer revision
; OUT:
;  - .AXY: preserved
.proc commit
	jsr flush_key
	SAVE_REGS
	lda #$00
	sta recording
	lda pending
	jeq @done
	lda size+1
	bne @publish
	lda size
	cmp #HEADER+TRAILER
	bne @publish
	lda #$00
	sta pending
	jsr restore_dirty
	jmp @done

;-------------------------------------------------------------------------------
@publish:
	ldxy #metadata_after
	CALL FINAL_BANK_ERRLOG, __errlog_undo_snapshot
	stxy header+H_AFTER_META
	clc
	txa
	adc before_bytes
	sta metadata_bytes
	tya
	adc before_bytes+1
	sta metadata_bytes+1
	ldxy before_bytes
	stxy header+H_BEFORE_META

	lda size
	clc
	adc metadata_bytes
	sta size
	lda size+1
	adc metadata_bytes+1
	sta size+1
	jsr make_room
	jcs @done

	ldxy transaction
	stxy scan
	jsr capture_cursor
	ldxy size
	stxy header+H_SIZE
	lda #APPLIED
	sta header+H_STATE
	incw sequence
	lda sequence
	and sequence+1
	cmp #$ff
	bne @revision

	; clear history and saved revisions when the sequence reaches $ffff
	jsr clear
	ldx #MAX_SOURCES-1
	lda #$ff

@invalidate:
	sta savedlo,x
	sta savedhi,x
	lda #$00
	sta revisionlo,x
	sta revisionhi,x
	lda #$ff
	dex
	bpl @invalidate
	lda #$00
	sta sequence
	sta sequence+1
	jmp @done

;-------------------------------------------------------------------------------
@revision:
	ldx owner
	lda sequence
	sta header+H_NEWREV
	sta revisionlo,x
	lda sequence+1
	sta header+H_NEWREV+1
	sta revisionhi,x
	jsr write_metadata
	jsr write_header
	clc
	lda transaction
	adc size
	sta tail
	lda transaction+1
	adc size+1
	and #$7f
	sta tail+1
	sec
	lda tail
	sbc #TRAILER
	sta io
	lda tail+1
	sbc #$00
	and #$7f
	sta io+1
	ldx #H_SIZE
	lda #TRAILER
	jsr write_block
	clc
	lda used
	adc size
	sta used
	lda used+1
	adc size+1
	sta used+1
	lda #$00
	sta pending
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; CAPTURE CURSOR
; Records the pending transaction's endpoint and fast typing position
; OUT:
;  - header: resulting cursor and viewport
.proc capture_cursor
	CALLMAIN src::pos
	stxy header+H_AFTER
	sta header+H_AFTER+2
	lda zp::cury
	sta header+H_AFTER_ROW
	lda viewport::offset
	sta header+H_AFTER_VIEW
	lda zp::srccur
	sta run_cursor
	lda zp::srccur+1
	sta run_cursor+1
	lda __src_bank
	sta run_bank
	rts
.endproc

;*******************************************************************************
; END KEY
; Updates a pending insert session or publishes a completed command
; OUT:
;  - .AXY: preserved
.proc end_key
	SAVE_REGS
	lda #$00
	sta recording
	lda zp::editor_mode
	cmp #MODE_INSERT
	bne @finish
	lda pending
	beq @done
	jsr capture_cursor
	jmp @done
@finish:
	jsr commit
	lda #$00
	sta session_lost
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; FLUSH KEY
; Appends a staged typing byte before another mutation or session boundary
; OUT:
;  - .AXY: preserved
.proc flush_key
	php
	pha
	lda fast_key
	bne @done
	CALL FINAL_BANK_ERRLOG, record_key
@done:	lda #$01
	sta fast_key
	pla
	plp
	rts
.endproc

;*******************************************************************************
; SAVED
; Flushes deferred typing and closes the current editing group
; records the current revision as saved when FLAG_DIRTY is clear
; IN:
;  - .A: new source flags
; OUT:
;  - .AXY: preserved
.proc saved
	jsr commit
	SAVE_REGS
	tsx
	lda STACK_PAGE+HOOK_SAVED_A,x
	and #FLAG_DIRTY
	bne @done
	ldx src::activebuff
	cpx #MAX_SOURCES
	bcs @done
	lda revisionlo,x
	sta savedlo,x
	lda revisionhi,x
	sta savedhi,x
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; CLOSED
; Removes the closing buffer's history and decrements higher buffer IDs
; IN:
;  - src::activebuff: closing buffer
; OUT:
;  - .AXY: preserved
.proc closed
	jsr flush_key
	SAVE_REGS
	lda src::activebuff
	cmp #MAX_SOURCES
	bcs @done
	lda #$00
	sta recording
	sta pending
	lda blocked
	bne @arrays
	jsr scan_start

@loop:	lda remaining
	ora remaining+1
	beq @arrays
	jsr read_header
	lda header+H_OWNER
	cmp #$ff
	beq @next
	cmp src::activebuff
	bcc @next
	bne @shift
	lda #$00
@shift:	sbc #$01
	sta header+H_OWNER
	jsr write_header
@next:	jsr scan_next
	jmp @loop
@arrays:
	ldx src::activebuff
@copy:	cpx #MAX_SOURCES-1
	bcs @last
	lda revisionlo+1,x
	sta revisionlo,x
	lda revisionhi+1,x
	sta revisionhi,x
	lda savedlo+1,x
	sta savedlo,x
	lda savedhi+1,x
	sta savedhi,x
	lda redo_pending+1,x
	sta redo_pending,x
	inx
	bne @copy

@last:	lda #$00
	sta revisionlo,x
	sta revisionhi,x
	sta savedlo,x
	sta savedhi,x
	sta redo_pending,x
@done:	RESTORE_REGS
.endproc

;*******************************************************************************
; SEEK
; Walks source bytes to position and updates the source line and column
; IN:
;  - position: destination source offset
.proc seek
@current = r4
	CALLMAIN src::pos
	stxy @current
	sta @current+2
@compare:
	ldx #$02
@byte:	lda @current,x
	cmp position,x
	bne @move
	dex
	bpl @byte
	CALLMAIN sync_x
	rts

@move:	bcs @back
	CALLMAIN src::next
	incw @current
	lda @current
	ora @current+1
	bne @compare
	inc @current+2
	jmp @compare
@back:	CALLMAIN src::prev
	lda @current
	bne :+
	lda @current+1
	bne :+
	dec @current+2
:	decw @current
	jmp @compare
.endproc

;*******************************************************************************
; READ CHANGE
; Reads the five-byte text change at walk into journal_buffer
; IN:
;  - walk: journal offset
; OUT:
;  - journal_buffer: operation, source position and source byte
.proc read_change
	ldxy walk
	stxy io
	ldx #HEADER
	lda #CHANGE
	jmp read_block
.endproc

;*******************************************************************************
; ADVANCE
; Moves walk backward by CHANGE for undo, or forward by CHANGE for redo
; IN:
;  - operation: zero for undo, one for redo
; OUT:
;  - walk: next change offset
.proc advance
	lda operation
	beq @back
	clc
	lda walk
	adc #CHANGE
	sta walk
	lda walk+1
	adc #$00
	jmp @high
@back:	sec
	lda walk
	sbc #CHANGE
	sta walk
	lda walk+1
	sbc #$00
@high:	and #$7f
	sta walk+1
	rts
.endproc

;*******************************************************************************
; BOUNDS
; Locates the text changes between the record header and annotation snapshots
; IN:
;  - selected: transaction offset
;  - header: selected transaction
; OUT:
;  - walk: first change
;  - walkend: first byte after the changes
.proc bounds
	clc
	lda selected
	adc #HEADER
	sta walk
	lda selected+1
	adc #$00
	and #$7f
	sta walk+1
	clc
	lda selected
	adc header+H_SIZE
	sta walkend
	lda selected+1
	adc header+H_SIZE+1
	and #$7f
	sta walkend+1
	sec
	lda walkend
	sbc #TRAILER
	sta walkend
	lda walkend+1
	sbc #$00
	and #$7f
	sta walkend+1
	clc
	lda header+H_BEFORE_META
	adc header+H_AFTER_META
	sta metadata_bytes
	lda header+H_BEFORE_META+1
	adc header+H_AFTER_META+1
	sta metadata_bytes+1
	sec
	lda walkend
	sbc metadata_bytes
	sta walkend
	lda walkend+1
	sbc metadata_bytes+1
	and #$7f
	sta walkend+1
	rts
.endproc

;*******************************************************************************
; PREFLIGHT
; Compares free source banks with a split estimate for replay and rollback
; bounds changed offsets by the total byte count in either replay direction
; only segments overlapping that range can grow or split
; reserves a split per affected segment reaching $1e00 and per $0e00 changes
; IN:
;  - selected: transaction offset
;  - header: selected transaction
; OUT:
;  - .C: set if the estimated bank count exceeds the available banks
.proc preflight
@insertions = r4
@needed = r6
@available = r7
@checkbank = r8
@banklen = r9
@first = check_position
@last = rollback_position
@start = before
@end = position
	jsr bounds
	ldx #$02
	lda #$ff
@range:	sta @first,x
	lda #$00
	sta @last,x
	lda #$ff
	dex
	bpl @range
	ldxy #$0000
	stxy @insertions
@count:	ldxy walk
	cmpw walkend
	jeq @banks
	incw @insertions
	jsr read_change
	ldx #$02
@low:	lda journal_buffer+1,x
	cmp @first,x
	bcc @save_low
	bne @high_start
	dex
	bpl @low
@save_low:
	ldx #$02
@copy_low:
	lda journal_buffer+1,x
	sta @first,x
	dex
	bpl @copy_low
@high_start:
	ldx #$02
@high:	lda journal_buffer+1,x
	cmp @last,x
	bcc @next
	bne @save_high
	dex
	bpl @high
@save_high:
	ldx #$02
@copy_high:
	lda journal_buffer+1,x
	sta @last,x
	dex
	bpl @copy_high
@next:	clc
	lda walk
	adc #CHANGE
	sta walk
	lda walk+1
	adc #$00
	and #$7f
	sta walk+1
	jmp @count
@banks:	lda @insertions
	ora @insertions+1
	jeq @ok

	; boundaries can move by at most one byte per recorded change
	sec
	lda @first
	sbc @insertions
	sta @first
	lda @first+1
	sbc @insertions+1
	sta @first+1
	lda @first+2
	sbc #$00
	sta @first+2
	bcs @upper
	lda #$00
	sta @first
	sta @first+1
	sta @first+2
@upper:	clc
	lda @last
	adc @insertions
	sta @last
	lda @last+1
	adc @insertions+1
	sta @last+1
	bcc :+
	inc @last+2
:	lda #$00
	sta @start
	sta @start+1
	sta @start+2
	sta @available
	sta @needed
	ldx #SOURCE_POOL_SIZE-1
@free:	lda bank_next,x
	cmp #$ff
	bne :+
	inc @available
:	dex
	bpl @free
	ldx __src_bank
@head:	CALLMAIN find_prev
	bcs @length
	tax
	jmp @head
@length:
	stx @checkbank
	cpx __src_bank
	bne @inactive

	; calculate active text length from the pointers, excluding the gap
	lda zp::srcend
	sbc zp::srccur2
	sta @banklen
	lda zp::srcend+1
	sbc zp::srccur2+1
	sta @banklen+1
	clc
	lda @banklen
	adc zp::srccur
	tax
	lda @banklen+1
	adc zp::srccur+1
	sec
	sbc #$20
	tay
	jmp @sum

@inactive:
	txa
	CALLMAIN __src_seg_len
@sum:	stxy @banklen
	clc
	txa
	adc @start
	sta @end
	tya
	adc @start+1
	sta @end+1
	lda @start+2
	adc #$00
	sta @end+2
	ldx #$02
@past:	lda @start,x
	cmp @last,x
	bcc @overlap
	bne @extra
	dex
	bpl @past
@overlap:
	ldx #$02
@before:
	lda @end,x
	cmp @first,x
	bcc @following
	bne @grow
	dex
	bpl @before
@grow:	lda @banklen
	clc
	adc @insertions
	tax
	lda @banklen+1
	adc @insertions+1
	; count one split when segment length plus change count reaches $1e00
	cmp #$1e
	bcc @following
	inc @needed
@following:
	ldx #$02
@advance:
	lda @end,x
	sta @start,x
	dex
	bpl @advance
	ldx @checkbank
	lda bank_next-FINAL_BANK_SOURCE0,x
	cmp #$fe
	beq @extra
	tax
	jmp @length
@extra:	lda @needed
	beq @ok
@more:	lda @insertions+1
	cmp #$0e
	bcc @check
	sbc #$0e
	sta @insertions+1
	inc @needed
	jmp @more
@check:	lda @available
	cmp @needed
	bcc @full
@ok:	clc
	rts
@full:	sec
	rts
.endproc

;*******************************************************************************
; UNDO
; Reverses the active buffer's most recent applied transaction and redraws
; OUT:
;  - .C: set if no record is available or text/annotation capacity is exceeded
.proc undo
	lda #$00
	jmp replay
.endproc
;*******************************************************************************
; REDO
; Reapplies the active buffer's earliest undone transaction and redraws
; OUT:
;  - .C: set if no record is available or text/annotation capacity is exceeded
.proc redo
	lda #$01
	jmp replay
.endproc

;*******************************************************************************
; BITMAP ADDRESS
; Returns a bitmap byte address and clears its 256-byte block on first use
; IN:
;  - r0-r1: bitmap byte offset
; OUT:
;  - r8-r9: byte address in the annotation RAM mapping
;  - .Y: zero
.proc bitmap_address
@offset=r0
@ptr=r8
	ldx @offset+1
	lda line_pages,x
	bne @address
	inc line_pages,x
	lda #<line_bits
	sta @ptr
	txa
	clc
	adc #>line_bits
	sta @ptr+1
	ldy #$00
	lda #$00
@clear:	sta (@ptr),y
	iny
	bne @clear
@address:
	clc
	lda @offset
	adc #<line_bits
	sta @ptr
	lda @offset+1
	adc #>line_bits
	sta @ptr+1
	ldy #$00
	rts
.endproc

;*******************************************************************************
; BITMAP OFFSET
; Divides the line number by eight to obtain its bitmap byte offset
; IN:
;  - .XY: source line number
; OUT:
;  - r0-r1: bitmap byte offset
.proc bitmap_offset
@offset=r0
	stxy @offset
	ldx #$03
@shift:	lsr @offset+1
	ror @offset
	dex
	bne @shift
	rts
.endproc

;*******************************************************************************
; TRACK LINE
; Marks the edited line and shifts later marks for newline insertion/deletion
; IN:
;  - src::line: line at the mutation's source position
;  - position: 24-bit mutation offset
;  - journal_buffer: text mutation
;  - operation: zero for undo, one for redo
; OUT:
;  - check_first: line containing check_position in the resulting source
;  - check_last: last affected line in the resulting source
;  - check_position: earliest mutation offset
.proc track_line
@offset=r0
@limit=r2
@mask=r4
@old=r5
@shifted=r6
@ptr=r8
	ldx #$02
@position:
	lda position,x
	cmp check_position,x
	bcc @savepos
	bne @last
	dex
	bpl @position
	bmi @last
@savepos:
	; keep the line and offset paired across cursor direction changes
	; later mutations at or after this offset cannot change its line number
	ldxy src::line
	stxy check_first
	ldx #$02
@copy:	lda position,x
	sta check_position,x
	dex
	bpl @copy
@last:	lda src::line+1
	cmp check_last+1
	bcc @newline
	bne @extend
	lda src::line
	cmp check_last
	bcc @newline
@extend:
	ldxy src::line
	stxy check_last

@newline:
	lda journal_buffer+4
	cmp #$0d
	jne @mark
	ldxy src::line
	jsr bitmap_offset
	lda src::line
	and #$07
	tax
	lda low_masks,x
	sta @mask
	lda journal_buffer
	eor operation
	and #OP_MASK
	jeq @join

	; above shifts the original line; below and split shift its successors
	lda journal_buffer
	lsr
	cmp #ERRLOG_BLANK_ABOVE
	bne :+
	lsr @mask
:
;-------------------------------------------------------------------------------
; shift existing marks up before marking the inserted line or split text
	ldxy @offset
	stxy @limit
	incw check_last
	ldxy check_last
	jsr bitmap_offset
	ldxy @offset
	lda @limit
	sta @offset
	lda @limit+1
	sta @offset+1
	stxy @limit
	jsr bitmap_address
	lda (@ptr),y
	sta @old
	asl
	php
	sta @shifted
	jsr @merge
@insert_next:
	ldxy @offset
	cmpw @limit
	beq @shift_done
	incw @offset
	jsr bitmap_address
	lda (@ptr),y
	plp
	rol
	php
	sta (@ptr),y
	jmp @insert_next
@shift_done:
	plp
	lda journal_buffer
	lsr
	cmp #ERRLOG_BLANK_ABOVE
	beq @mark
	cmp #ERRLOG_BLANK_BELOW
	beq @below
	jsr @mark
@below:	ldxy src::line
	inx
	bne :+
	iny
:	jmp mark_line
@mark:	ldxy src::line
	jmp mark_line

;-------------------------------------------------------------------------------
; above/join discard the first line's mark; below preserves it
@join:	lda journal_buffer
	lsr
	cmp #ERRLOG_DELETE_BELOW
	beq :+
	lsr @mask
:	ldxy src::line
	cmpw check_last
	bne :+
	incw check_last
:
	ldxy @offset
	stxy @limit
	ldxy check_last
	jsr bitmap_offset
	decw check_last
	clc
	php
@delete_byte:
	jsr bitmap_address
	lda (@ptr),y
	sta @old
	plp
	ror
	php
	sta @shifted
	ldxy @offset
	cmpw @limit
	beq @delete_first
	ldy #$00
	lda @shifted
	sta (@ptr),y
	decw @offset
	jmp @delete_byte
@delete_first:
	jsr @merge
	plp
	lda journal_buffer
	lsr
	beq @mark
	rts

; retain the edited line and lower bits in the first shifted byte
@merge:	lda @mask
	eor #$ff
	and @shifted
	sta @shifted
	lda @old
	and @mask
	ora @shifted
	ldy #$00
	sta (@ptr),y
	rts
.endproc

;*******************************************************************************
; MARK LINE
; Marks one resulting source line for annotation restoration and validation
; IN:
;  - .XY: source line
.proc mark_line
@mask=r2
@ptr=r8
	txa
	and #$07
	pha
	jsr bitmap_offset
	pla
	tax
	lda bit_masks,x
	sta @mask
	jsr bitmap_address
	lda (@ptr),y
	ora @mask
	sta (@ptr),y
	rts
.endproc

;*******************************************************************************
; LINE CHANGED
; Tests whether replay affected an annotation's resulting source line
; IN:
;  - .XY: source line
; OUT:
;  - .A: nonzero if the line changed
;  - r0-r9: preserved
.export __undo_line_changed
.proc __undo_line_changed
@mask=r2
	SAVE_REGS
	tsx
	lda STACK_PAGE+HOOK_SAVED_A-1,x
	and #$07
	tax
	lda bit_masks,x
	sta @mask
	tsx
	lda STACK_PAGE+HOOK_SAVED_A-2,x
	tay
	lda STACK_PAGE+HOOK_SAVED_A-1,x
	tax
	jsr bitmap_offset
	ldx scan+1
	lda line_pages,x
	beq @result
	jsr bitmap_address
	lda (io),y
	and @mask

@result:
	tsx
	sta STACK_PAGE+HOOK_SAVED_A,x
	RESTORE_REGS
.endproc

;*******************************************************************************
; FIND CHANGED
; Scans for the next marked line, skipping empty bytes and uninitialized blocks
; IN:
;  - check_current: first candidate line
;  - check_last: final candidate line
; OUT:
;  - check_current: next changed line
;  - .C: set if no changed lines remain
.proc find_changed
@offset=r0
@limit=r2
@mask=r4
	ldxy check_last
	jsr bitmap_offset
	ldxy @offset
	stxy @limit
	ldxy check_current
	jsr bitmap_offset
	lda check_current
	and #$07
	tax
	lda high_masks,x
	sta @mask
@byte:	ldxy @offset
	cmpw @limit
	bcc @page
	bne @done

@page:	ldx @offset+1
	lda line_pages,x
	bne @read
	lda #$00
	sta @offset
	inc @offset+1
	bne @all
@read:	jsr bitmap_address
	lda (io),y
	and @mask
	bne @found
	incw @offset
@all:	lda #$ff
	sta @mask
	bne @byte
@found:	ldx #$00
@bit:	lsr
	bcs @line
	inx
	bne @bit
@line:	stx @mask
	ldxy @offset
	stxy check_current
	ldx #$03
@shift:	asl check_current
	rol check_current+1
	dex
	bne @shift
	lda check_current
	ora @mask
	sta check_current
	clc
@done:	rts
.endproc

;*******************************************************************************
; NEXT CHANGED
; Stops at check_last or searches for the next marked line
; IN:
;  - check_current: current changed line
;  - check_last: last affected line
; OUT:
;  - check_current: next changed line
;  - .C: set if no changed lines remain
.proc next_changed
	ldxy check_current
	cmpw check_last
	beq @done
	incw check_current
	jmp find_changed
@done:	rts
.endproc

;*******************************************************************************
; SEEK CHECK LINE
; Moves to the start of check_current using whole-line navigation
; IN:
;  - check_current: line to check
.proc seek_check_line
	CALLMAIN src::home
	sec
	lda check_current
	sbc src::line
	tax
	lda check_current+1
	sbc src::line+1
	tay
	bcc @up
	JUMPMAIN src::downn

@up:	sec
	lda src::line
	sbc check_current
	tax
	lda src::line+1
	sbc check_current+1
	tay
	JUMPMAIN src::upn
.endproc

;*******************************************************************************
; RECHECK LINES
; Clears and rechecks live diagnostics on marked lines using restored dismissals
; returns the source cursor to position after checking
; IN:
;  - line_bits: changed lines in the restored source
;  - check_first: line containing check_position
;  - check_last: last affected line
;  - check_position: earliest mutation offset
;  - position: final transaction cursor offset
.proc recheck_lines
@last=r0
	ldxy src::line
	stxy restore_line
	ldxy check_first
	stxy check_current
	jsr find_changed
	jcs @done
	ldxy check_last
	stxy @last
	ldxy check_first
	CALL FINAL_BANK_ERRLOG, __errlog_clear_live_changed

	; restore the earliest edited position, then visit marked lines in order
	ldxy src::line
	cmpw check_first
	beq @begin
	ldxy check_first
	stxy src::line
	ldxy check_position
	stxy zp::srctmp
	lda check_position+2
	sta zp::srctmp+2
	CALLMAIN src::restore_position

@begin: ldxy check_first
	stxy check_current
	jsr find_changed
@line:	jsr seek_check_line
	CALLMAIN src::getwide
	CALL FINAL_BANK_ERRLOG, __errlog_recheck_line
	jsr next_changed
	bcc @line
	ldxy restore_line
	stxy src::line
	ldxy position
	stxy zp::srctmp
	lda position+2
	sta zp::srctmp+2
	CALLMAIN src::restore_position
@done:	rts
.endproc

;*******************************************************************************
bit_masks:  .byte $01,$02,$04,$08,$10,$20,$40,$80
low_masks:  .byte $01,$03,$07,$0f,$1f,$3f,$7f,$ff
high_masks: .byte $ff,$fe,$fc,$f8,$f0,$e0,$c0,$80

;*******************************************************************************
; APPLY CHANGES
; Applies text changes in undo or redo order and tracks the resulting lines
; IN:
;  - operation: zero for undo, one for redo
;  - rollback_mode: nonzero to leave the changed-line bitmap unchanged
;  - selected: transaction to apply
;  - header: selected transaction header
; OUT:
;  - .C: set if source insertion failed
.proc apply_changes
	jsr bounds
	lda operation
	bne @apply
	ldxy walk
	lda walkend
	sta walk
	lda walkend+1
	sta walk+1
	stxy walkend
	jsr advance
	ldxy walk
	stxy io
	ldxy walkend
	stxy walk
	jsr advance
	ldxy walk
	stxy walkend
	ldxy io
	stxy walk

@apply: lda rollback_mode
	bne @start
	lda #$00
	ldx #$1f
@pages:	sta line_pages,x
	dex
	bpl @pages
	ldxy #$ffff
	stxy check_first
	stxy check_position
	stx check_position+2
	ldxy #$0000
	stxy check_last

;-------------------------------------------------------------------------------
@start: lda #$01
	sta replaying
@loop:	ldxy walk
	cmpw walkend
	beq @finish
	jsr read_change
	ldx #$02
@pos:	lda journal_buffer+1,x
	sta position,x
	dex
	bpl @pos
	jsr seek
	lda rollback_mode
	bne :+
	jsr track_line
:	lda journal_buffer
	lsr
	tax
	lda journal_buffer
	eor operation
	and #OP_MASK
	beq @delete
	stx __errlog_insertmode
	lda journal_buffer+4
	CALLMAIN src::insert
	bcs @done
	jmp @step

;-------------------------------------------------------------------------------
@delete:
	stx __errlog_deletemode
	CALLMAIN src::delete
@step:	jsr advance
	jmp @loop
@finish:
	clc
@done:	rts
.endproc

;*******************************************************************************
; REPLAY
; Applies text changes, restores annotations and rechecks changed lines
; commits history on success or reverses the changes on annotation overflow
; IN:
;  - .A: zero for undo, one for redo
; OUT:
;  - .C: set if replay was unavailable or rejected
.proc replay
	jsr commit
	sta operation
	lda #$00
	sta recording
	sta pending
	sta rollback_mode
	sta __errlog_recheck_full
	lda blocked
	jne @unavailable
	lda src::activebuff
	cmp #MAX_SOURCES
	jcs @empty
	sta owner
	jsr scan_start
	lda operation
	bne @find
	ldxy tail
	stxy scan
@find:	lda remaining
	ora remaining+1
	jeq @empty
	lda operation
	bne :+
	jsr scan_prev
:	jsr read_header
	lda header+H_OWNER
	cmp owner
	bne @next
	lda header+H_STATE
	eor operation
	beq @next
	ldxy scan
	stxy selected
	jmp @selected
@next:	lda operation
	beq @find
	jsr scan_next
	jmp @find

;-------------------------------------------------------------------------------
@selected:
	jsr preflight
	jcs @full
	CALLMAIN src::pos
	stxy rollback_position
	sta rollback_position+2
	lda zp::cury
	sta rollback_row
	lda viewport::offset
	sta rollback_view
	lda owner
	CALLMAIN src::getflags
	sta rollback_flags
	CALL FINAL_BANK_ERRLOG, __errlog_undo_backup
	jsr read_metadata
	jsr apply_changes
	jcs @failed
	ldxy #metadata_restore
	CALL FINAL_BANK_ERRLOG, __errlog_undo_restore
	jcs @rollback

	ldx #H_BEFORE
	lda operation
	beq :+
	ldx #H_AFTER
:	lda header,x
	sta position
	lda header+1,x
	sta position+1
	lda header+2,x
	sta position+2
	jsr seek
	jsr recheck_lines
	lda __errlog_recheck_full
	beq @commit
	lda #$00
	jmp @rollback

;-------------------------------------------------------------------------------
@commit:
	ldxy selected
	stxy scan
	lda operation
	sta header+H_STATE
	jsr write_header
	ldx #H_OLDREV
	lda operation
	beq :+
	ldx #H_NEWREV
:	ldy owner
	lda header,x
	sta revisionlo,y
	lda header+1,x
	sta revisionhi,y
	lda #$01
	sta redo_pending,y
	jsr restore_dirty
	lda #$00
	sta replaying
	lda zp::editor_mode
	cmp #MODE_INSERT+1
	bcc :+
	CALLMAIN __edit_enter_command
	lda #$00
	sta __edit_visual_line
	sta __edit_visual_line+1
	sta __edit_visual_pos
	sta __edit_visual_pos+1
	sta __edit_visual_x
:	jsr restore_view
	jsr redraw
	clc
	rts

;-------------------------------------------------------------------------------
@rollback:
	sta rollback_reason
	lda #$01
	sta rollback_mode
	lda operation
	eor #$01
	sta operation
	jsr apply_changes
	bcs @failed
	lda operation
	eor #$01
	sta operation
	ldx #$02
@position:
	lda rollback_position,x
	sta position,x
	dex
	bpl @position
	jsr seek
	CALL FINAL_BANK_ERRLOG, __errlog_undo_rollback
	lda rollback_flags
	CALLMAIN src::setflags
	lda rollback_row
	sta zp::cury
	lda rollback_view
	sta viewport::offset
	lda #$00
	sta replaying
	sta rollback_mode
	jsr redraw
	lda rollback_reason
	bne @annotations_full
	ldxy #msg_errors_full
	jmp message

;-------------------------------------------------------------------------------
; discard history and report source-full after an insertion failure
@failed:
	lda #$00
	sta replaying
	sta __errlog_insertmode
	sta __errlog_deletemode
	jsr clear
@full:	ldxy #msg_full
	jmp message
@empty:	ldxy #msg_empty
	jmp message
@unavailable:
	ldxy #msg_unavailable
	jmp message
@annotations_full:
	ldxy #msg_annotations_full
	jmp message
.endproc

;*******************************************************************************
; REDRAW
; Refreshes the source viewport and diagnostic panel after replay
.proc redraw
	CALLMAIN viewport::invalidate
	CALLMAIN edit::refresh
	CALLMAIN edit::sync_cur
	CALLMAIN viewport::follow
	JUMPMAIN __errlog_refresh
.endproc

;*******************************************************************************
; RESTORE VIEW
; Restores horizontal scroll and clamps the cursor row to the editor and source
; IN:
;  - operation: zero for undo, one for redo
;  - header: selected transaction
; OUT:
;  - zp::cury: restored screen row
;  - viewport::offset: restored horizontal scroll
.proc restore_view
	ldx operation
	lda header+H_BEFORE_VIEW,x
	sta viewport::offset
	lda edit::height
	bpl :+
	lda #$00
:	cmp header+H_BEFORE_ROW,x
	bcc :+
	lda header+H_BEFORE_ROW,x
:	tay
	lda src::line+1
	bne @row
	tya
	cmp src::line
	bcc @row
	ldy src::line
	dey
@row:	sty zp::cury
	rts
.endproc

;*******************************************************************************
; RESTORE DIRTY
; Clears FLAG_DIRTY when current and saved revisions match; sets it otherwise
.proc restore_dirty
	ldy src::activebuff
	lda revisionhi,y
	cmp savedhi,y
	bne @dirty
	lda revisionlo,y
	cmp savedlo,y
	bne @dirty
	lda #$00
	beq @flags
@dirty:	lda #FLAG_DIRTY
@flags:	JUMPMAIN src::setflags
.endproc

;*******************************************************************************
; MESSAGE
; Copies the status string to mem::spare and displays it with text::info
; IN:
;  - .XY: string address in this ROM
; OUT:
;  - .C: set
.proc message
	stxy zp::bankaddr0

	ldy #$00
@copy:	lda (zp::bankaddr0),y
	sta mem::spare,y
	beq @show
	iny
	bne @copy
@show:	ldxy #mem::spare
	CALLMAIN text::info
	sec
	rts
.endproc

;*******************************************************************************
; status strings use lowercase character codes
msg_unavailable:        .byte "undo unavailable",0
msg_empty:              .byte "no history",0
msg_full:               .byte "undo: source full",0
msg_errors_full:        .byte "undo: error log full",0
msg_annotations_full:   .byte "undo: annotations full",0

; key-boundary hooks run in the error-log bank with RAM 61..63 mapped
.segment "ERRLOG_CODE"
CUR_BANK .set FINAL_BANK_ERRLOG

;*******************************************************************************
; KEY BEGIN
; Arms fast_key for a printable insertion continuing at the cached cursor
; otherwise prepares the key through begin
; IN:
;  - .A: editor key
; OUT:
;  - .AXY: preserved
.export __undo_key_begin
.proc __undo_key_begin
	pha
	cmp #$20
	bcc @slow
	cmp #$80
	bcs @slow
	lda pending
	cmp #$01
	bne @slow

	lda zp::editor_mode
	cmp #MODE_INSERT
	bne @slow
	lda text::insertmode
	beq @slow
	lda header+H_OWNER
	cmp src::activebuff
	bne @slow
	lda run_cursor
	cmp zp::srccur
	bne @slow
	lda run_cursor+1
	cmp zp::srccur+1
	bne @slow
	lda run_bank
	cmp __src_bank
	bne @slow
	clc
	lda used
	adc size
	lda used+1
	adc size+1
	cmp #$7c
	bcs @slow

	; use begin when the five-byte append would cross a mapped page boundary
	lda write_pos
	cmp #$fc
	bcs @slow
	lda #$80
	sta fast_key
	lda #$01
	sta recording
	pla
	rts

@slow:	pla
	CALL FINAL_BANK_UNDO, begin
	rts
.endproc

;*******************************************************************************
; KEY COMMIT
; Appends a staged typing byte or updates the pending session through end_key
; OUT:
;  - .AXY: preserved
.export __undo_key_commit
.proc __undo_key_commit
	php
	pha
	lda fast_key
	beq @record
	bmi @empty
	pla
	plp
	CALL FINAL_BANK_UNDO, end_key
	rts

@record:
	jsr record_key
@empty:	lda #$01
	sta fast_key
	lda #$00
	sta recording
	pla
	plp
	rts
.endproc

;*******************************************************************************
; RECORD KEY
; Appends the staged insertion and updates the pending size and endpoint
; IN:
;  - fast_char: inserted character
;  - header: active typing transaction
; OUT:
;  - .AXY: preserved
.proc record_key
	pha
	tya
	pha
	lda zp::bankaddr0
	pha
	lda zp::bankaddr0+1
	pha

	; map the journal bank into BLK2, then restore annotation RAM bank 62
	lda write_pos
	sta zp::bankaddr0
	lda write_pos+1
	and #$1f
	ora #$40
	sta zp::bankaddr0+1
	lda write_pos+1
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #UNDO_RAM_BANK
	sta $9ffa
	ldy #$00
	lda #INSERT
	sta (zp::bankaddr0),y
	iny
	lda header+H_AFTER
	sta (zp::bankaddr0),y
	iny
	lda header+H_AFTER+1
	sta (zp::bankaddr0),y
	iny
	lda header+H_AFTER+2
	sta (zp::bankaddr0),y
	iny
	lda fast_char
	sta (zp::bankaddr0),y
	lda #ERRLOG_RAM_BANK+1
	sta $9ffa

	inc header+H_AFTER
	bne :+
	inc header+H_AFTER+1
	bne :+
	inc header+H_AFTER+2
:	lda size
	adc #CHANGE
	sta size
	bcc :+
	inc size+1
:	clc
	lda write_pos
	adc #CHANGE
	sta write_pos
	lda write_pos+1
	adc #$00
	and #$7f
	sta write_pos+1
	lda zp::cury
	sta header+H_AFTER_ROW
	lda viewport::offset
	sta header+H_AFTER_VIEW
	lda zp::srccur
	sta run_cursor
	lda zp::srccur+1
	sta run_cursor+1
	lda __src_bank
	sta run_bank
	lda #$01
	sta fast_key

	pla
	sta zp::bankaddr0+1
	pla
	sta zp::bankaddr0
	pla
	tay
	pla
	rts
.endproc
.endif
