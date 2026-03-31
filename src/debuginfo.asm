;*******************************************************************************
; DEBUGINFO.ASM
; This file contains definitions for procedures to store debug info for
; assembled programs.  This info is the map of file and line-number to address.
; Refer to docs/debug-info.md for more information about the format of the
; debug information
;*******************************************************************************
.include "errors.inc"
.include "kernal.inc"
.include "linker.inc"
.include "macros.inc"
.include "memory.inc"
.include "object.inc"
.include "string.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.macpack longbranch

; Debug info constants
MAX_FILES  = 64		; max files debug info may be generated for
MAX_BLOCKS = 256

MAX_FILENAME_LEN = 16

BLOCK_START_ADDR     = 0    	; offset in block header for base address
BLOCK_STOP_ADDR      = 2    	; offset in block header for stop address
BLOCK_LINE_BASE      = 4	; offset in block header for base line
BLOCK_LINE_COUNT     = 6	; offset in block header for line count
BLOCK_FILE_ID        = 8	; offset in block header to file id
BLOCK_SEGMENT_ID     = 9	; offset in block header for SEGMENT id
BLOCK_LINE_PROG      = 10	; offset to address of the line program data
BLOCK_PROG_STOP_ADDR = 12	; offset to address of end of line program

DATA_FILE = 0	; offset of FILE ID in debug info
DATA_LINE = 1	; offset of line number in debug info
DATA_ADDR = 3	; offset of line address in debug info

; Opcodes for extended instructions
OP_SET_ADDR     = $01
; FREE          = $02
; FREE
OP_ADVANCE_LINE = $04
OP_ADVANCE_ADDR = $05
OP_SET_PC       = $06

SIZEOF_BLOCK_HEADER     = 14
SIZEOF_BLOCK_HEADER_OBJ = 10	; .O header doesn't contain line prog start/stop

;*******************************************************************************
; Debug info pointers
; active line data, these represent the HEAD state of the line program
; state machine
addr    = zp::debug		; address of next line/addr to store
srcline = zp::debug+2		; address of current source line
line    = zp::debug+4		; address of current line program instruction
block   = zp::debug+6		; address of current block pointer

; the offsets of these state variables correspond to the offsets
; of the BLOCK_ constants. e.g. BLOCK_START_ADDR -> blockstart
blockstate    = block+2		; base of block state
blockstart    = blockstate	; base of active block's address range
blockstop     = blockstate+2	; top of active block's address range
blocklinebase = blockstate+4 	; base of active block's line range
linestop      = blockstate+6	; number of lines in active block
file          = blockstate+8 	; current file id being worked on
seg_id        = blockstate+9 	; current file id being worked on
progstart     = blockstate+10	; address of start of line program for block
progstop      = blockstate+12	; address of end of line program

; scratchpad
debugtmp = block+16

.export __debug_file
.export __debug_src_line
__debug_file     = file		; the active file
__debug_src_line = srcline	; the line # stored by dbg::storeline

.segment "SHAREBSS"
; number of files that we have debug info for
.export __debug_numfiles
__debug_numfiles:
numfiles:   .byte 0

.segment "DEBUGINFO"

;*******************************************************************************
; DEBUGINFO
; This large block of memory stores the line programs for each block.
; The location of each block is found by summing the sizes of each block before
; the block in question
.export debuginfo

debuginfo:
.ifdef vic20
.ifdef ultimem
; only $2000 is banked in at a time on the Ultimem
.res $2000
.else
.res $6000
.endif
.endif

.segment "DEBUGINFO_CODE"

.export __debug_addr2line
.export __debug_end_block
.export __debug_line2addr
.export __debuginfo_init
.export __debuginfo_initonce
.export __debug_get_filename
.export __debug_new_block
.export __debug_set_file
.export __debug_set_name
.export __debug_store_line
.export __debug_push_block
.export __debug_pop_block

.export __debuginfo_set_seg_id
.export __debuginfo_get_fileid

.ifdef vic20
.RODATA
.endif

__debug_addr2line:        JUMP FINAL_BANK_DEBUG, addr2line
__debug_line2addr:        JUMP FINAL_BANK_DEBUG, line2addr
__debug_end_block:        JUMP FINAL_BANK_DEBUG, end_block
__debuginfo_init:         JUMP FINAL_BANK_DEBUG, init
__debuginfo_initonce:     JUMP FINAL_BANK_DEBUG, initonce
__debug_get_filename:     JUMP FINAL_BANK_DEBUG, get_filename
__debug_new_block:        JUMP FINAL_BANK_DEBUG, new_block
__debug_set_file:         JUMP FINAL_BANK_DEBUG, set_file
__debug_set_name:         JUMP FINAL_BANK_DEBUG, set_name
__debug_store_line:       JUMP FINAL_BANK_DEBUG, store_line
__debug_push_block:       JUMP FINAL_BANK_DEBUG, push_block
__debug_pop_block:        JUMP FINAL_BANK_DEBUG, pop_block
__debuginfo_get_fileid:   JUMP FINAL_BANK_DEBUG, get_fileid
__debuginfo_set_seg_id:   JUMP FINAL_BANK_DEBUG, set_seg_id

.segment "DEBUGINFO_VARS"

;*******************************************************************************
numblocks:  .byte 0	; number of blocks that we have debug info for
block_open: .byte 0	; if !0, we are creating a block, when this is set
			; creating a new block will first close the open one
freeptr:    .word 0	; pointer to next available address in debuginfo
blocksp:    .byte 0	; stack pointer for block stack

; file table (stored as table of 0-terminated filenames)
.export __debug_filenames
__debug_filenames:
filenames: .res MAX_FILES * MAX_FILENAME_LEN

;*******************************************************************************
; BSS
.segment "DEBUGINFO_BSS"

blockstack: .res $100	; stack for line program state machine

; table of headers for each block
.export blockheaders
blockheaders: .res MAX_BLOCKS*SIZEOF_BLOCK_HEADER

; table of line program's start addresses for each block.
; these will map to somewhere in the debuginfo buffer
blockaddresseslo: .res MAX_FILES
blockaddresseshi: .res MAX_FILES

.segment "DEBUGINFO_CODE"

;*******************************************************************************
; INITONCE
; Clears state that should only be cleared on boot (file table)
.proc initonce
	lda #$00
	sta numfiles

	; fall through to init
.endproc

;*******************************************************************************
; INIT
; Clears the debug info state that is valid for a single assembled program
; This is the line table and line program information, which is regenerated
; from scratch each time a program is assembled
.proc init
	; init the address for the next free line program location
	lda #<debuginfo
	sta freeptr
	lda #>debuginfo
	sta freeptr+1

	; init debugger state variables
	lda #$00
	sta blocksp
	sta numblocks
	sta block_open
	rts
.endproc

;*******************************************************************************
; PUSH BLOCK
; Pushes the current state machine values for the line program
; This allows a new block to be created and then the current block restored
; OUT:
;   - .C: set on stack overflow
.proc push_block
	ldx #$00
	ldy blocksp
@l0:	lda zp::debug,x
	sta blockstack,y
	iny
	beq @overflow
	inx
	cpx #$06
	bne @l0
	lda file
	sta blockstack,y
	iny
	sty blocksp	; save new stack pointer
	RETURN_OK	; success

@overflow:
	RETURN_ERR ERR_STACK_OVERFLOW
.endproc

;*******************************************************************************
; POP BLOCK
; Pops the last pushed values (push_block) for the line program state machine
; OUT:
;   - .C: set on stack underflow
.proc pop_block
	ldx #$06
	ldy blocksp
	lda blockstack-1,y	; file
	sta file
	dey
@l0:	lda blockstack-1,x
	sta zp::debug-1,y
	dey
	cpy #$ff
	beq @underflow
	dex
	bne @l0
	sty blocksp	; save new stack pointer
	RETURN_OK	; success

@underflow:
	RETURN_ERR ERR_STACK_UNDERFLOW
.endproc

;*******************************************************************************
; NEW BLOCK
; Creates a new block of debug information
; Blocks are created any time the file is changed or the address is changed
; (as with a .ORG directive)
; e.g.
;   .org $1000  ; block 0
;   asl		; block 1
;   .inc "a"	; block 2 (everything in file "a")
;   asl		; block 3
;   .org $1200	; block 4
; IN:
;   - .XY:     the start address of the block
;   - srcline: the base line for the new block
;   - file:    the ID of the file for this block
;   - seg_id:  the SEGMENT id for the file
; OUT:
;   - addr: the base address for the new block
;   - line: the address of the line program for the new block
;   - .C: set if an error occurred
.proc new_block
@tmp=r0
	stxy addr		; init addr pointer

	lda block_open		; is there a block already open?
	beq @cont		; continue to create new block if not
	jsr end_block		; end the open block if there is one

@cont:	; get offset to block header (BLOCK_HEADER_SIZE * numblocks)
	lda numblocks
	jsr header_addr
	stxy block

	lda freeptr
	sta progstart
	sta line
	adc #$02
	sta progstop

	lda freeptr+1
	sta progstart+1
	sta line+1
	adc #$00
	sta progstop+1

	lda addr
	sta blockstart
	lda addr+1
	sta blockstart+1

	; store the address that this block's line program will live at
	ldy #BLOCK_LINE_PROG
	lda line
	STOREB_Y block
	iny
	lda line+1
	STOREB_Y block

	; store the base address the block maps to
	ldy #BLOCK_START_ADDR
	lda addr
	STOREB_Y block
	iny
	lda addr+1
	STOREB_Y block

	; initialize the base line to the value of srcline
	ldy #BLOCK_LINE_BASE
	lda srcline
	STOREB_Y block
	sta blocklinebase
	sta linestop
	iny
	lda srcline+1
	STOREB_Y block
	sta blocklinebase+1
	sta linestop+1

	; store the file ID
	ldy #BLOCK_FILE_ID
	lda file
	STOREB_Y block

	; store the SEGMENT ID
	ldy #BLOCK_SEGMENT_ID
	lda seg_id
	STOREB_Y block

	; initialize the line count to 0
	ldy #BLOCK_LINE_COUNT
	lda #$00
	STOREB_Y block
	iny
	STOREB_Y block

	; initialize program to empty (2 $00 bytes)
	tay
	sta (line),y
	iny
	sta (line),y

	inc numblocks
	inc block_open	; flag that we are creating a new block
	RETURN_OK
.endproc

;*******************************************************************************
; END BLOCK
; Updates values that may have changed with calls to storeline
; This includes the end address and number of lines in the block
; This is called when we are done working on a given block.
; IN:
;   - .XY: the address to end the block at
.proc end_block
@numlines=debugtmp
	lda numblocks
	beq @done	; no blocks exist, nothing to "end"

	lda block_open	; is there an open block?
	beq @done	; if not, nothing to "end"

	; check if block was empty (start address == end address), if so, just
	; delete it
	lda blockstart
	cmp blockstop
	bne :+
	lda blockstart+1
	cmp blockstop+1
	bne :+

	; delete the block (decrement block count and reset freeptr)
	dec numblocks
	lda progstart
	sta freeptr
	lda progstart+1
	sta freeptr+1
	jmp @close

:	; write updated address end
	ldy #BLOCK_STOP_ADDR
	lda addr
	STOREB_Y block
	iny
	lda addr+1
	STOREB_Y block

	; compute number of lines and write updated value
	; numlines = (linestop - linebase) + 1
	lda srcline
	sec
	sbc blocklinebase
	sta @numlines
	lda srcline+1
	sbc blocklinebase+1
	sta @numlines+1
	incw @numlines

	ldy #BLOCK_LINE_COUNT
	lda @numlines
	STOREB_Y block
	iny
	lda @numlines+1
	STOREB_Y block

	; store new end of line program
	ldy #BLOCK_PROG_STOP_ADDR
	lda progstop
	STOREB_Y block	; write LSB of stop address to block header
	iny
	lda progstop+1
	STOREB_Y block	; write MSB of stop address to block header

	; check if progstop > freeptr and set freeptr to progstop if it is
	; TODO: won't this always be the case?
	cmp freeptr+1
	bcc @close	; progstop is < freeptr, this isn't the new free ptr
	bne @set_freetop

	; check the LSB
	lda progstop
	cmp freeptr
	bcc @close	; progstop < freeptr, don't set freeptr

@set_freetop:
	; freeptr is > progstop, set freeptr to progstop
	lda progstop+1
	sta freeptr+1
	lda progstop
	sta freeptr

@close: dec block_open	; flag that there is no open block

@done:	rts
.endproc

;*******************************************************************************
; STORE LINE
; Write an instruction to the active line program to map the given line to the
; given address within the active block.
; IN:
;  - .XY: line number
;  - r0:  address corresponding to the given line number
.proc store_line
@addr=r0
@line=r2
@dline=r4
@daddr=r6
@isize=r8
.macro INC_LINE
	jsr inc_line_and_count
.endmacro

.macro DEC_LINE
.ifdef ultimem
.local @done
	lda line
	bne @done
	dec line+1
	cmp #$20
	bcs @done

	lda #$20
	sta line+1	; limit line+1 to $20
	dec $9ff8
@done:	dec line
.else
	decw line
.endif
	rts
.endmacro
	stxy @line

.ifdef ultimem
	; TODO: add (line % $2000) to get the bank for line
.endif

	; get the line and address delta for our new line (dline = new - prev)
	lda @line
	sec
	sbc srcline
	sta @dline
	lda @line+1
	sbc srcline+1
	sta @dline+1

	; set new srcline value to the line we're moving to
	stxy srcline

	ldy #$00
	sty @isize

	; get the relative address from the block's base address
	lda @addr
	sec
	sbc addr
	sta @daddr
	lda @addr+1
	sbc addr+1
	sta @daddr+1

	; check if address is encodable in basic instruction
	; in order to use a basic instruction, both must be in range [$01, $0f]
	ora @dline+1
	bne @extended	; if either delta's MSB is !0, need extended

	lda @dline
	ora @daddr
	bne :+
	rts		; no line/addr change, no instruction needed
:	and #$f0	; if either's LSB is >= $10, need extended
	bne @extended

@basic:	; encode the instruction ((daddr << 4) | dline)
	lda @daddr
	asl
	asl
	asl
	asl
	ora @dline
	STOREB_Y line		; write the encoded instruction
	INC_LINE
	jmp @done

@extended:
	lda @dline
	ora @dline+1
	beq @advanceaddr	; skip if line delta is 0

@advanceline:
	lda #$00		; extended instruction opcode byte 0
	STOREB_Y line

	; get 16 bit signed offset for target line
	INC_LINE
	lda #OP_ADVANCE_LINE	; extended instruction opcode byte 1
	STOREB_Y line

	INC_LINE
	lda @dline
	STOREB_Y line		; write LSB of line delta
	INC_LINE
	lda @dline+1
	STOREB_Y line		; write MSB of line delta

	INC_LINE

@advanceaddr:
	lda @daddr
	ora @daddr+1
	beq @done	; no need for instruction if addr delta is 0 - skip

	lda #$00		; extended instruction opcode byte 0
	STOREB_Y line

	INC_LINE
	lda #OP_ADVANCE_ADDR	; extended instruction opcode byte 1
	STOREB_Y line

	; get 16 bit signed offset for target address
	INC_LINE
	lda @daddr
	STOREB_Y line	; write LSB
	INC_LINE
	lda @daddr+1
	STOREB_Y line	; write MSB
	INC_LINE


@done:  ; update progstop
	lda @isize
	clc
	adc progstop
	sta progstop	; progstop += size of instruction
	bcc :+
	inc progstop+1

	; write new program terminator ($00 $00)
	lda #$00
	STOREB_Y line
	INC_LINE
	STOREB_Y line

	; fix line to point at start of new terminator
	DEC_LINE

	; if this line is > line top, set as new top
	ldxy srcline
	cmpw linestop
	bcc :+
	stxy linestop

:	; udpate addr
	lda addr
	clc
	adc @daddr
	sta addr
	lda addr+1
	adc @daddr+1
	sta addr+1
	rts
.endproc

;*******************************************************************************
; INSERT LINE
; Insers a line at the given line in the given file, shifting all lines below
; it down
; IN:
;   - .A:  the file ID to shift
;   - .XY: the line to shift everything below
.proc insert_line
	; find all blocks that need to be updated
	jsr blocks_below_line

	; update each block that needs to be updated

	; if the block STARTS after the line we're seeking, just increment its
	; base

	; if not, run the line program until we find a value >=
	; the line-to-insert below
	; then increment the instruction that advanced to or past that line
	; if the instruction cannot be incremented,
	ldxy zp::line
	jsr addr2line

	; delete it
.endproc

;*******************************************************************************
; DELETE LINE
; Deletes the debug info associated with the given line and shifts all lines
; below it up.
; If there is no information at the given line, just shifts without deleting
; IN:
;   - .A:  the file ID to shift
;   - .XY: the line to shift everything below
.proc delete_line
@blocks=mem::spare
@num=debugtmp
@line=debugtmp+1
	stxy @line

	; find all blocks that need to be updated
	jsr blocks_below_line
	sta @num

	; update each block that needs to be updated
@l0:	ldx @num
	beq @done		; no blocks
	dex
	lda @blocks,x
	pha

	jsr get_block_by_id	; load the block
	ldxy blocklinebase	; get its start line
	cmpw @line		; compare
	bcs @incbase

@find:	; find where the block crosses our sought line
	; jsr find_closest

	; modify the instruction to increment 1 less line

@incbase:
	; base is >= line to delete, just decrement the base
	decw blocklinebase

@save:	pla
	jsr save_block
	dec @num
	bne @l0

	; if the block STARTS after the line we're seeking, just decrement its
	; base

	; if not, run the line program until we find a value >=
	; the line-to-delete
	; then decrement the instruction that advanced to or past that line
	ldxy zp::line
	jsr addr2line

	; delete it

@done:	rts
.endproc

;*******************************************************************************
; ADDR2LINE
; returns the filename and address that correspond
; to the given address
; IN:
;  - .XY: the address to get the location of
; OUT:
;  - .A:  file-id of the address
;  - .XY: line number of the address
;  - .C:  set on error
.proc addr2line
@addr=r2
@cnt=r4
	stxy @addr
	lda #$00
	sta @cnt

@l0:	; load the next block to search for the address in
	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND	; no next block, address isn't mapped

; is the address we're looking for in the range [blockstart, blockstop]?
@checkstop:
	lda @addr+1
	cmp blockstop+1
	bcc @checkstart
	beq :+
	bcs @next	; addr > blockstop, skip to next block
:	lda @addr
	cmp blockstop
	beq @checkstart
	bcs @next	; if addr > blockstop, skip to next block

@checkstart:
	lda @addr+1
	cmp blockstart+1
	bcc @next	; addr < blockstart, skip to the next block
	beq :+
	bcs @findline 	; blockstart <= addr < blockstop, find the line #
:	lda @addr
	cmp blockstart
	bcs @findline   ; blockstart <= addr < blockstop, find the line #

@next:	inc @cnt
	bne @l0
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program until we find the line that is mapped to the given addr
@findline:
	lda addr
	cmp @addr
	bne @nextline
	lda addr+1
	cmp @addr+1
	bne @nextline

@found:	ldxy srcline	; return the line that the program ended on
	lda file	; and file
	RETURN_OK

@nextline:
	jsr advance
	bcc @findline
	lda #ERR_LINE_NOT_FOUND
	;sec
@done:	rts
.endproc

;*******************************************************************************
; LINE2ADDR
; Returns the address of the given line.
; IN:
;  - .XY: the line to get the address of
;  - .A:  the file ID of the file containing the line
; OUT:
;  - .XY: the address of the given line
;  - .C:  set if no address is mapped to the current line
.proc line2addr
@line=r2
@cnt=r4
@file=r5
@closest=r7		; nearest line < the one we're looking for
	sta @file
	stxy @line
	lda #$00
	sta @cnt

; find the first block that contains the file and line we are looking for
@l0:	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND

; does the file ID match the file we're looking for?
	lda file
	cmp @file
	bne @next	; if the file doesn't match, try the next block

; is the line we're looking for in the range [blockstart, blockstop]?
@checkstop:
	lda @line+1
	cmp linestop+1
	bcc @checkstart
	beq :+
	bcs @next	; line > (linebase+numlines), skip to next block
:	lda @line
	cmp linestop
	beq @findaddr	; line == (linebase+numlines), find the line #
	bcs @next

@checkstart:
	lda addr+1
	cmp blocklinebase+1
	bcc @next	; line < linebase, skip to the next block
	beq :+
	bcs @findaddr	; linebase <= line < (linebase+numlines), find address
:	lda addr
	cmp blocklinebase
	bcs @findaddr	; linebase <= line < (linebase+numlines), find address

@next:	inc @cnt
	lda @cnt
	cmp numblocks
	bcc @l0	 	; repeat until we've checked all blocks
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program for the block until we find a match for our line
@findaddr:
	lda srcline
	cmp @line
	bne @nextline

	lda srcline+1
	cmp @line+1
	bne @nextline

@found:	ldxy addr
	RETURN_OK

@nextline:
	jsr advance
	bcc @findaddr
@done:	rts
.endproc

;*******************************************************************************
; GET FILEID
; Returns the ID for the given file name by looking it up in the file table
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID
;  - .C: set if there was no match
.proc get_fileid
@other=zp::str0
@filename=zp::str2
@buff=$120
@cnt=debugtmp
	stxy @filename

	; copy the filename to compare to a temp buffer
	lda #FINAL_BANK_MAIN	; copy from MAIN bank
	stxy zp::bankaddr0	; copy from given address
	ldxy #@buff
	stxy zp::bankaddr1	; copy to temp buffer
	jsr ram::copyline	; copy the filename to a buffer in shared RAM
	lda @buff
	beq @notfound		; if string is 0-length, return with "not found" flag
	lda numfiles
	beq @notfound		; if no files are stored, return with not found

	lda #$00
	sta @cnt

; foreach file in the filetable, check if the name matches the one requested
@l0:	lda @cnt
	jsr get_filename_addr	; get the filename to compare
	stxy @other
	jsr strcompare		; @filename == @other?
	bne @next		; if not, try the next filename

@found: ldxy @filename		; restore .XY
	lda @cnt		; get the file ID
	RETURN_OK		; file found

@next:	inc @cnt
	lda @cnt
	cmp numfiles
	bne @l0

@notfound:
	ldxy @filename	; restore .XY
	sec		; not found
	rts
.endproc

;*******************************************************************************
; GET FILENAME ADDR
; Returns the address of the filename for the given file ID
; IN:
;  - .A: the file ID to get the filename of
; OUT:
;  - .XY: address of filename for the given file ID
;  - .C:  set if there is no filename for the given file (XY will STILL
;         point to the address the filename WOULD exist at)
.proc get_filename_addr
	pha
	asl
	asl
	asl
	asl
	adc #<filenames
	tax
	lda #>filenames
	adc #$00
	tay
	pla
	cmp numfiles		; set .C if file ID is >= numfiles
	rts
.endproc

;*******************************************************************************
; GET FILENAME
; Copies the filename for the given ID to a scratch buffer and returns the
; address to it
; IN:
;  - .A: the file ID to get the filename of
; OUT:
;  - .XY: address of filename for the given file ID
;  - .C:  set if there is no filename for the given file (XY will STILL
;         point to the address the filename WOULD exist at)
.ifndef vic20
get_filename = get_filename_addr
.else
.proc get_filename
@buff=$140
	cmp numfiles		; set .C if file ID is >= numfiles
	bcs @done
	jsr get_filename_addr
	stxy zp::bankaddr0
	ldxy #@buff
	stxy zp::bankaddr1
	lda #FINAL_BANK_DEBUG	; copy from DEBUG bank
	jsr ram::copyline	; copy the filename to a buffer in shared RAM
	ldxy #@buff
	clc
@done:	rts
.endproc
.endif

;*******************************************************************************
; SET SEG ID
; Sets the active segment-id to the given ID
; IN:
;   - .A: the SEGMENT id to set for the block
.proc set_seg_id
	sta seg_id
	rts
.endproc

;*******************************************************************************
; SET FILE
; Sets the active file-id to the the ID for given filename.
; If no file-id exists for the provided filename, one is first created.
; The filename given is assumed to be in the main bank
; IN:
;   - .XY: the 0-terminated file to set as the current file
; OUT:
;   - .A: the ID of the file that was activated
;   - .C: set on error
.proc set_file
	jsr get_fileid	; get the file ID (if the file is already stored)
	bcc :+		; if an ID was found, no need to copy filename
	jsr set_name	; copy the filename and get its new ID
:	sta file	; store the ID as the active file ID
	rts
.endproc

;*******************************************************************************
; SET NAME
; Renames the entry for the given ID in the file table to the given name
; IN:
;   - .A:  the debug file ID of the handle to (re)name
;   - .XY: the filename to set for the handle
;   - .C:  set if there is no file for the given ID
.proc set_name
.ifdef vic20
@filename = ram::src
@dst      = ram::dst
.else
@filename=r2
@dst=r4
.endif
	stxy @filename
	jsr get_filename_addr	; get the destination address for ID
	stxy @dst
	ldxy @filename		; restore filename
	CALLMAIN str::len

.ifdef vic20
	; bytes to copy = strlen(@filename)+1
	tax
	inx
	ldy #$00
	lda #FINAL_BANK_DEBUG			; dest bank
	sta ram::dst+2
	lda #FINAL_BANK_MAIN			; source bank
	sta ram::src+2
	CALLMAIN ram::copy
.else

:	lda (@filename),y
	sta (@dst),y
	dey
	bpl :-
.endif
	lda numfiles	; get ID of new file
	inc numfiles
	RETURN_OK
.endproc

;*******************************************************************************
; BLOCKS BELOW LINE
; Returns a list of all the blocks that CONTAIN lines at or below the the given
; line in the given file
; This is useful for, e.g. shifting all lines below a given line up or down
; IN:
;   - .A: the file id of blocks to match
;   - .XY: the line to search for
; OUT:
;   - mem::spare: the list of block IDs that match
;   - .A:         the # of matches
.proc blocks_below_line
@vars=zp::tmp10
@file=@vars
@line=@vars+1
@block=@vars+3
@numfound=@vars+5
@tmp=@vars+7
@result=mem::spare
	sta @file
	stxy @line

	ldxy #blockheaders
	stxy @block

	ldx #$00
	cpx numblocks
	beq @done		; if no blocks- we're done
	stx @numfound

	; check each block for a file match
@l0:	ldy #BLOCK_FILE_ID
	LOADB_Y @block
	cmp @file
	bne @next

	; check if the line is >= the one we're seeking
	; (line < (header[BLOCK_LINE_BASE]+header[BLOCK_LINE_COUNT])
	ldy #BLOCK_LINE_BASE
	LOADB_Y @block
	sta @tmp
	iny
	LOADB_Y @block
	sta @tmp+1

	ldy #BLOCK_LINE_COUNT
	LOADB_Y @block
	clc
	adc @tmp
	sta @tmp
	iny
	lda (@block),y
	adc @tmp+1
	sta @tmp+1

	cmp @line+1		; is MSB >= line we're seeking's?
	beq :+			; == -> check LSB
	bcc @next		; <  -> definitely not a match
	bcs @found		; >  -> definitely a match
:	lda @tmp
	cmp @line		; is LSB >= line we're seeking's?
	bcc @next		; if not, not a match

@found:	; this block contains a line >= the one we're seeking, append it
	; to our result
	ldy @numfound
	txa
	sta @result,y
	inc @numfound

@next:	lda @block
	clc
	adc #SIZEOF_BLOCK_HEADER
	sta @block
	bcc :+
	inc @block+1
:	inx
	cpx numblocks
	bne @l0

@done:	lda @numfound
	rts
.endproc

;*******************************************************************************
; HEADER ADDR
; Returns the address of the block header for the given ID
; IN:
;   - .A: the ID of the block to get
; OUT:
;   - .XY: the address of the requested header
; CLOBBERS:
;   - r0-r1
.proc header_addr
@tmp0=r0
@tmp1=r1
	; multiply block number by SIZEOF_BLOCK_HEADER
	asl			; *2
	sta @tmp0
	asl			; *4
	sta @tmp1
	asl			; *8
	adc @tmp1		; *12
	adc @tmp0		; *14

	adc #<blockheaders
	tax
	lda #>blockheaders
	adc #$00
	tay
	rts
.endproc

;*******************************************************************************
; SAVE BLOCK
; Copies the state of the work variables (zp::block) to the given block ID
; IN:
;   - .A: the ID of the block to save to.
.proc save_block
@block=r0
	jsr header_addr
	stxy @block

; save the header state for the block:
; start addr, stop addr, base line, # lines, file id, program addr
	ldy #SIZEOF_BLOCK_HEADER-1
@copy:	LOADB_Y block
	STOREB_Y @block
	dey
	bpl @copy

	rts
.endproc

;*******************************************************************************
; GET BLOCK BY ID
; Returns the start and stop addresses of a block by its ID.
; ID's are sequential, so to iterate over all blocks, you can call this
; routine with an incrementing counter over the range [0, numblocks)
; IN:
;  .A: the ID of the block to get
; OUT:
;  block:      the address of the block data
;  blockstart: the start address of the block
;  blockstop:  the stop address of the block
;  file:       the file ID used by the block
;  line:       the address of the line data for the block
;  numlines:   the number of lines in the block
;  srcline:    initialized to the first line in the block
;  .C:         set if the block doesn't exist, clear on success
.proc get_block_by_id
	cmp numblocks
	bcs @done	; return error

	; multiply block number by sizeof(blockdata)
	jsr header_addr
	stxy block

; copy the header state for the block:
; start addr, stop addr, base line, # lines, file id, program addr
	ldy #SIZEOF_BLOCK_HEADER-1
@copy:	LOADB_Y block
	sta blockstate,y
	dey
	bpl @copy

	; initialize line/address values to the base for the block
	lda blockstart
	sta addr
	lda blockstart+1
	sta addr+1

	lda blocklinebase
	sta srcline
	lda blocklinebase+1
	sta srcline+1

	; initialize line program for the block
	lda progstart
	sta line
	lda progstart+1
	sta line+1

	; compute linestop by adding numlines to linebase and subtracting 1
	lda linestop		; numlines
	clc
	adc blocklinebase
	sta linestop
	lda linestop+1		; numlines+1
	adc blocklinebase+1
	sta linestop+1
	decw linestop

	clc	; OK
@done:	rts
.endproc

;*******************************************************************************
; ADVANCE
; Runs one command in the active line program
; Updates the line pointer to point to the next line in the active file.
; OUT:
;  - line: set to the location of the next line in the code
;  - addr: set to the line that's mapped to line
;  - .C:   set if there are no lines left in the active block (block)
.proc advance
	ldy #$00
	LOADB_Y line
	beq @extended_i	; if opcode is 0, this is an extended instruction

@basic_i:
	; decode the opcode; top 4 bits contain PC offset and bottom 4 line
	pha		; save instruction
	and #$0f	; get line offest and add it to the current line
	clc
	adc srcline
	sta srcline
	bcc :+
	inc srcline+1

:	pla		; restore instruction
	lsr		; get PC offset and add it to the current address
	lsr
	lsr
	lsr
	clc
	adc addr
	sta addr
	jcc @ok
	inc addr+1
	jcs @ok		; branch always

@extended_i:
	jsr inc_line	; move line program PC to byte 2 of opcode
	LOADB_Y line	; get byte 2 of opcode
	jeq @end	; if byte 2 is 0, we're at the end of the program

	jsr inc_line	; move to operand

	cmp #OP_SET_ADDR
	bne :+
@setaddr:
	LOADB_Y line
	sta addr
	jsr inc_line
	LOADB_Y line
	sta addr+1
	jmp @ok

:	cmp #OP_ADVANCE_LINE
	bne :+
@adv_line:
	LOADB_Y line		; LSB of amount to advance
	clc
	adc srcline		; + current line value (LSB)
	sta srcline
	jsr inc_line
	LOADB_Y line		; MSB of amount to advance
	adc srcline+1		; + current line (MSB)
	sta srcline+1
	jmp @ok

:	cmp #OP_ADVANCE_ADDR
	bne :+
@adv_addr:
	LOADB_Y line
	clc
	adc addr
	sta addr
	jsr inc_line
	LOADB_Y line
	adc addr+1
	sta addr+1
	jmp @ok

:	cmp #OP_SET_PC
	bne @end	; invalid opcode
@set_pc:
	LOADB_Y line
	sta addr
	jsr inc_line
	LOADB_Y line
	sta addr+1
@ok:	jsr inc_line
	RETURN_OK

@end:	sec		; flag end of porgram
	rts
.endproc

;*******************************************************************************
; INC LINEOP
; Increments the operation at the cursor to point to the line after the one
; it is currently on.
; IN:
;   - line: points to the instruction in the line program to update
; OUT:
;   - .C: set if the operation could not be updated (or is not a line opcode)
; TODO: this is unused and probably broken
.proc inc_lineop
	ldy #$00
	LOADB_Y line	; get the opcode
	beq @extended	; if 0, handle extended opcode

@basic: and #$0f	; mask the line offset
	cmp #$0f	; do we need to insert another operation?
	bcs @expand

@noexpand:
	; just increment the operation and return
	; clc
	adc #$01
	STOREB_Y line
	;clc
	rts

@expand:
	; shift the whole program up
	; shift (freeptr-(line+1)) bytes up from line+1 to line+2
	ldxy line
	inx
	bne :+
	iny
:	stxy r2		; src = line+1
	inx
	stx r4		; dst = line+2
	bne :+
	iny
:	sty r4+1

	lda freeptr	; size = freeptr-(line+1)
	;sec
	sbc r2		; r2=line+1
	tax
	lda freeptr+1
	sbc line+1
	tay
	jsr ram::copy	; move the memory

	; now that we have opened a gap,
	; insert a new basic instruction to increment line
	lda #$01
	tay
	STOREB_Y line	; line++
	RETURN_OK

@extended:
	ldy #$01
	LOADB_Y line
	cmp #OP_ADVANCE_LINE
	bne @err		; not a valid instruction

	; increment the operand
	iny
	LOADB_Y line
	;sec
	adc #$00
	STOREB_Y line
	iny
	LOADB_Y line
	adc #$00
	STOREB_Y line
	rts

@err:	sec
	rts
.endproc

;*******************************************************************************
; COMPARE
; Compares the strings in (str0) to the buffer @ $120
; IN:
;  zp::str0: one of the strings to compare
;  $120:     the string to compare to
; OUT:
;  .Z: set if the strings are equal
.proc strcompare
@cnt=zp::bankval
@buff=$120
	ldy #$00
@l0:	lda (zp::str0),y
	cmp @buff,y		; compare it with the string in this bank
	bne @done		; not equal

@next:	cmp #$00		; are we at the terminating 0?
	beq @done		; if so, return
	iny			; next char
	bne @l0
	lda #$ff		; not equal
@done:	rts
.endproc

;*******************************************************************************
; DUMP
; Dumps the debug info in memory to the open file
.export __debuginfo_dump
.proc __debuginfo_dump
@name=r0
@i=r0
@dbgi=r2
@progstart=r4
	ldxy #filenames
	stxy @name

;--------------------------------------
; dump the filenames
	ldx numfiles
	beq @filesdone
@fnames:
	; write the filename (with terminating 0) to the object file
	ldy #$00
:	LOADB_Y @name
	jsr krn::chrout
	iny
	cmp #$00
	bne :-

	lda @name
	clc
	adc #MAX_FILENAME_LEN
	sta @name
	bcc :+
	inc @name+1
:	dex
	bne @fnames		; repeat for next filename

@filesdone:
	lda #$00
	jsr krn::chrout		; write 0 to terminate filename list

;--------------------------------------
; dump the BLOCK headers
	; write the number of blocks
	lda numblocks
	jsr krn::chrout

	ldxy #blockheaders
	stxy @dbgi
	ldx numblocks

	; dump each header
	ldx numblocks
	beq @done		; if no BLOCKS, we're done
	ldy #$00
@headers:
	LOADB_Y @dbgi
	jsr krn::chrout
	iny
	cpy #SIZEOF_BLOCK_HEADER_OBJ
	bne @headers

	; calculate the size of the line program for the block
	; (progstop-progstart)
	LOADB_Y @dbgi
	sta @progstart
	iny
	LOADB_Y @dbgi
	sta @progstart+1
	iny
	LOADB_Y @dbgi
	sbc @progstart
	php
	jsr krn::chrout		; write the LSB of the size
	iny
	LOADB_Y @dbgi
	plp
	sbc @progstart+1
	jsr krn::chrout		; write the MSB of the size

	lda @dbgi
	clc
	adc #SIZEOF_BLOCK_HEADER
	sta @dbgi
	bcc :+
	inc @dbgi+1
:	dex
	bne @headers

;--------------------------------------
; dump the line program data for the object file
@progdata:
	; write all bytes between debuginfo and freeptr
	lda #<debuginfo
	sta @dbgi
	lda #>debuginfo
	sta @dbgi+1
	bne @chk		; check if bot == top before looping

:	ldy #$00
	LOADB_Y @dbgi
	jsr krn::chrout
	incw @dbgi
@chk:	ldxy @dbgi
	cmpw freeptr
	bne :-

@done:	RETURN_OK
.endproc

;*******************************************************************************
; LOAD
; Loads the debug info from file to the current debug info state.
; Addresses are offset by a given base address to support relocation.
; This procedure is intended for use during linking, so some link context is
; assumed to be set up:
;  - map of local (object context) segment id's to segment names
;  - map of global (linker context) segment names to id's
;  - map of global (linker context) base addresses for segments
; IN:
;   - dbgi::loadaddr: offset to apply to all BLOCKS loaded
;   - .A:             relocate flag: !0=relocate segments, 0=absolute load
; OUT:
;   - .C: set on error
.export __debuginfo_load
.proc __debuginfo_load
@i        = r0
@header   = r0
@freeptr  = r0
@segname  = r0
@offset   = r0
@block_i  = zp::tmp10
@relocate = zp::tmp12
@filemap  = $100
@filename = $100+MAX_FILES
	sta @relocate

;--------------------------------------
; load the file table and map ids to global ids
@load_files:
	lda #$00
	sta @i
@mapfile:
	; read a filename
	ldy #$00
:	jsr krn::chrin
	sta @filename,y
	iny
	cmp #$00
	bne :-
	cpy #$01		; was filename empty?
	beq @load_blocks	; if so, we're at end of list

	ldxy #@filename
	jsr set_file
	ldx @i
	sta @filemap,x
	inc @i
	bne @mapfile

;--------------------------------------
; load the BLOCK data
@load_blocks:
	jsr krn::chrin		; read number of blocks
	sta numblocks
	cmp #$00
	bne :+
	RETURN_OK		; no blocks

:	lda #$00
	sta @block_i
@load_block:
	; load the BLOCK header
	ldy #$00
:	jsr krn::chrin
	sta blockstate,y
	iny
	cpy #SIZEOF_BLOCK_HEADER_OBJ
	bne :-

	; load the line program size and compute its stop address based on
	; current freeptr
	lda freeptr
	sta progstart
	lda freeptr+1
	sta progstart+1
	jsr krn::chrin		; read size LSB
	clc
	adc freeptr
	sta progstop		; store stop address LSB
	php
	jsr krn::chrin		; read size MSB
	plp
	adc freeptr+1
	sta progstop+1		; store stop address MSB

	; check if we should do relocation (for linking)
	lda @relocate
	beq @relocate_done

	; map local file id to the global one
	ldx file
	lda @filemap,x
	sta file

	; look up global SEGMENT id and replace the LOCAL one with it
	lda seg_id
	clc
	adc #$01		; get 1-based id
	CALL FINAL_BANK_LINKER, obj::get_segment_name_by_id
	bcc :+
	rts			; not found
:	CALL FINAL_BANK_LINKER, link::segid_by_name
	sta seg_id

	; look up the address offset for this SEGMENT
	CALL FINAL_BANK_LINKER, link::segaddr_for_file_by_id
	stxy @offset

	; add the offset for the SEGMENT to the value from the header
	lda blockstart
	clc
	adc @offset
	sta blockstart
	lda blockstart+1
	adc @offset+1
	sta blockstart+1

	; also add the SEGMENT address base to the stop address
	lda blockstop
	clc
	adc @offset
	sta blockstop
	lda blockstop+1
	adc @offset+1
	sta blockstop+1

@relocate_done:
	lda freeptr
	sta progstart
	lda freeptr+1
	sta progstart+1

	lda freeptr
	clc
	adc progstop
	sta progstop
	lda freeptr+1
	clc
	adc progstop+1
	sta progstop+1

	; write the modified header to its stable location
	lda @block_i
	jsr header_addr
	stxy @header
	ldy #SIZEOF_BLOCK_HEADER-1
:	lda blockstate,y
	STOREB_Y @header
	dey
	bpl :-

;--------------------------------------
; load/write the line program data
	ldxy freeptr
	stxy @freeptr
	ldy #$00
:	jsr krn::chrin		; load byte
	STOREB_Y @freeptr	; store it
	incw @freeptr

	; repeat for all bytes
	lda @freeptr
	cmp progstop
	bne :-
	sta freeptr
	lda @freeptr+1
	cmp progstop+1
	bne :-
	sta freeptr+1

	inc @block_i
	lda @block_i
	cmp numblocks
	beq @done
	jmp @load_block

@done:	RETURN_OK
.endproc

;*******************************************************************************
; INC LINE AND COUNT
; Falls through to inc_line, first incrementing a counter set by the caller
; in r8
.proc inc_line_and_count
	inc r8

	; fall through
.endproc

;*******************************************************************************
; INC LINE
; Increments the line pointer to point to the next location
; Used to scan through the program when seeking an address/line (addr2line
; line2addr) and when appending a line to a program (store_line)
; Also increments a counter set by the caller in r8
.proc inc_line
.ifdef ultimem
	pha
	inc line
	bne @done

	inc line+1
	lda line+1
	cmp #$40	; overflow?
	bne @done	; no -> continue

	; current bank is full, move to next one and reset
	; pointers to base of BLK1 ($2000)
	lda #$20
	sta line+1
	inc $9ff8	; next bank

@done:	pla
	rts
.else
	incw line
	rts
.endif
.endproc
