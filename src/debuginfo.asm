;*******************************************************************************
; DEBUGINFO.ASM
; This file contains definitions for procedures to store debug info for
; assembled programs.  This info is the map of file and line-number to address.
; Refer to docs/debug-info.md for more information about the format of the
; debug information
;*******************************************************************************
.include "errors.inc"
.include "kernal.inc"
.include "labels.inc"
.include "linker.inc"
.include "macros.inc"
.include "object.inc"
.include "string.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.include "limits.inc"
.macpack longbranch

; Debug info constants
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
; FREE          = $03
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

.export __debuginfo_top
__debuginfo_top:
freeptr:    .word 0	; pointer to next available address in debuginfo

.segment "DEBUGINFO"

;*******************************************************************************
; DEBUGINFO
; This large block of memory stores the line programs for each block.
; The location of each block is found by summing the sizes of each block before
; the block in question
.export debuginfo

debuginfo:
.ifdef vic20
.res $4800
.else
.res $6000
.endif
debuginfo_end:

.segment "DEBUGINFO_CODE"

.export __debug_addr2line
.export __debug_end_block
.export __debug_line2addr
.export __debuginfo_init
.export __debuginfo_initonce
.export __debug_get_filename
.export __debug_new_block
.export __debug_rename_file
.export __debug_set_file
.export __debug_store_line

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
__debug_rename_file:      JUMP FINAL_BANK_DEBUG, rename_file
__debug_set_file:         JUMP FINAL_BANK_DEBUG, set_file
__debug_store_line:       JUMP FINAL_BANK_DEBUG, store_line
__debuginfo_get_fileid:   JUMP FINAL_BANK_DEBUG, get_fileid
__debuginfo_set_seg_id:   JUMP FINAL_BANK_DEBUG, set_seg_id

.segment "DEBUGINFO_VARS"

;*******************************************************************************
numblocks:  .byte 0	; number of blocks that we have debug info for
block_open: .byte 0	; if !0, we are creating a block, when this is set
			; creating a new block will first close the open one

; file table (stored as table of 0-terminated filenames)
.export __debug_filenames
__debug_filenames:
filenames: .res MAX_FILES * MAX_FILENAME_LEN

; map of local (object file) file IDs to global file IDs (used during load)
filemap: .res MAX_FILES

;*******************************************************************************
; BSS
.segment "DEBUGINFO_BSS"

; table of headers for each block
.export blockheaders
blockheaders: .res MAX_BLOCKS*SIZEOF_BLOCK_HEADER

.segment "DEBUGINFO_CODE"

;*******************************************************************************
; INITONCE
; Clears state that should only be cleared on boot (the file table)
; The file table is NOT cleared by init: file IDs must remain stable across
; assemblies because they are stored externally (e.g. breakpoints)
.proc initonce
	lda #$00
	sta numfiles	; clear the file table
	; fall through to init
.endproc

;*******************************************************************************
; INIT
; Clears the debug info state that is valid for a single assembled program
; This is the line table and line program information, which is regenerated
; from scratch each time a program is assembled
; The file table persists (see initonce); set_file reuses the existing ID
; when a filename is already stored, so IDs are stable across assemblies
.proc init
	; init the address for the next free line program location
	lda #<debuginfo
	sta freeptr
	lda #>debuginfo
	sta freeptr+1

	; init debugger state variables
	lda #$00
	sta numblocks
	sta block_open
	rts
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
	jsr end_block		; end the open block at new block's start addr

@cont:	; make sure there is room for another block header
	lda numblocks
	cmp #MAX_BLOCKS-1	; is the block table full?
	jcs @err		; if so -> error

	; make sure there is room for the empty program (2 byte terminator)
	ldxy freeptr
	cmpw #debuginfo_end-2
	beq :+
	jcs @err		; freeptr > end-2: out of memory

:	; get offset to block header (BLOCK_HEADER_SIZE * numblocks)
	lda numblocks
	jsr header_addr
	stxy block

	lda freeptr
	sta progstart
	sta line
	;clc
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
	STOREB_Y line
	iny
	STOREB_Y line

	inc numblocks
	inc block_open	; flag that we are creating a new block
	RETURN_OK

@err:	RETURN_ERR ERR_OOM
.endproc

;*******************************************************************************
; END BLOCK
; Updates values that may have changed with calls to storeline
; This includes the end address and number of lines in the block
; This is called when we are done working on a given block.
; IN:
;   - .XY: the (exclusive) address to end the block at
.proc end_block
@numlines=debugtmp
	stxy blockstop	; set the end address for the block

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
	lda blockstop
	STOREB_Y block
	iny
	lda blockstop+1
	STOREB_Y block

	; compute number of lines and write updated value
	; numlines = (linestop - linebase) + 1
	; linestop is the highest line stored (see store_line); srcline may be
	; lower if lines were stored out of order (e.g. by a macro)
	lda linestop
	sec
	sbc blocklinebase
	sta @numlines
	lda linestop+1
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
; OUT:
;  - .C: set if the debug info buffer is full
.proc store_line
@addr=r0
@line=r2
@dline=r4
@daddr=r6
@isize=r8
@tmp=debugtmp
	; make sure there is room for the largest possible instruction
	; sequence (2 extended instructions (8 bytes) + terminator (2 bytes))
	lda line
	clc
	adc #$0a
	sta @tmp
	lda line+1
	adc #$00
	cmp #>debuginfo_end
	bcc @ok			; line+10 < end of buffer -> continue
	bne @err		; line+10 > end of buffer -> error
	lda @tmp
	cmp #<debuginfo_end
	bcc @ok			; line+10 < end -> continue
	beq @ok			; line+10 == end -> still fits
@err:	RETURN_ERR ERR_OOM

@ok:	stxy @line

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
	clc		; no line/addr change, no instruction needed
	rts
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
	lda #$01		; advance 1 byte
	sta @isize
	bne @done

@extended:
	ldy #$00
@advanceline:
	lda @dline
	ora @dline+1
	beq @advanceaddr	; skip if line delta is 0

	lda #$00		; extended instruction opcode byte 0
	STOREB_Y line

	; get 16 bit signed offset for target line
	iny
	lda #OP_ADVANCE_LINE	; extended instruction opcode byte 1
	STOREB_Y line

	iny
	lda @dline
	STOREB_Y line		; write LSB of line delta
	iny
	lda @dline+1
	STOREB_Y line		; write MSB of line delta

	iny

	lda #$04		; advance 4 bytes
	sta @isize

@advanceaddr:
	lda @daddr
	ora @daddr+1
	beq @done	; no need for instruction if addr delta is 0 - skip

	lda #$00		; extended instruction opcode byte 0
	STOREB_Y line

	iny
	lda #OP_ADVANCE_ADDR	; extended instruction opcode byte 1
	STOREB_Y line

	; get 16 bit signed offset for target address
	iny
	lda @daddr
	STOREB_Y line	; write LSB
	iny
	lda @daddr+1
	STOREB_Y line	; write MSB

	lda #$04	; advance 4 bytes
	clc
	adc @isize
	sta @isize

@done:  ; update progstop
	lda @isize
	clc
	adc progstop
	sta progstop	; progstop += size of instruction
	bcc :+
	inc progstop+1

	; update line pointer by instruction size
:	lda @isize
	clc
	adc line
	sta line
	bcc :+
	inc line+1

:	; write new program terminator ($00 $00)
	lda #$00
	tay
	STOREB_Y line
	iny
	STOREB_Y line

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
	clc		; ok (the add sets .C when the delta is negative)
	rts
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
@addr         = r2
@cnt          = r4
@match_found  = r5
@matched_line = r6
	stxy @addr
	lda #$00
	sta @match_found
	sta @cnt

@l0:	; load the next block to search for the address in
	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND	; no next block, address isn't mapped

; is the address we're looking for in the range [blockstart, blockstop)?
@checkstop:
	lda @addr+1
	cmp blockstop+1
	bcc @checkstart
	beq :+
	bcs @next	; addr > blockstop, skip to next block
:	lda @addr
	cmp blockstop
	bcs @next	; if addr >= blockstop, skip to next block

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

@found:	lda #$01
	sta @match_found	; flag that we found a matching line
	ldxy srcline
	stxy @matched_line	; save latest line that matches the target
	jmp @advance		; continue until the line no longer matches

@nextline:
	lda @match_found	; did a previous line match?
	bne @return_found	; if so, return it

@advance:
	jsr advance
	bcc @findline

	; ran out of lines in this block's program; if we had a match, return
	; it, otherwise continue searching the remaining blocks
	lda @match_found	; did a previous line match?
	beq @next		; if not, try the next block

@return_found:
	lda file		; and file
	ldxy @matched_line
	RETURN_OK
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
	sta @file
	stxy @line
	lda #$00
	sta @cnt

; find the first block that contains the file and line we are looking for
@l0:	lda @cnt
	jsr get_block_by_id
	bcc @checkfile
	RETURN_ERR ERR_LINE_NOT_FOUND

; does the file ID match the file we're looking for?
@checkfile:
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
	lda @line+1
	cmp blocklinebase+1
	bcc @next	; line < linebase, skip to the next block
	beq :+
	bcs @findaddr	; linebase <= line < (linebase+numlines), find address
:	lda @line
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

	; the line isn't mapped in this block's program; continue searching
	; the remaining blocks
	bcs @next	; branch always
.endproc

;*******************************************************************************
; GET FILEID
; Returns the ID for the given file name by looking it up in the file table
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID (or the ID the file would get if added - see set_name)
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
	lda numfiles	; .A = the ID the file would be assigned if added
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
	lsr
	lsr
	lsr
	lsr
	tay			; .Y=MSB of (ID * 16)
	pla
	pha			; restore the file ID
	asl
	asl
	asl
	asl			; LSB of (ID * 16)

	clc
	adc #<filenames
	tax
	tya
	adc #>filenames
	tay

	pla			; restore the file ID
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
	bcs @done	; if we failed to name the file, return the error
:	sta file	; store the ID as the active file ID
	clc		; ok
@done:	rts
.endproc

;*******************************************************************************
; SET NAME
; Adds a new entry to the file table with the given filename
; IN:
;   - .XY: the filename to add to the file table
; OUT:
;   - .A: the ID assigned to the new file
;   - .C: set if the file table is full or the filename is empty or too long
.proc set_name
	lda numfiles		; the ID for the new entry
	cmp #MAX_FILES		; is the file table full?
	bcs @toomany		; if so -> error
	jsr write_name		; write the name to the new entry's slot
	bcs @err		; -> error (invalid filename)
	lda numfiles		; get ID of new file
	inc numfiles
	RETURN_OK

@toomany:
	RETURN_ERR ERR_MAX_FILES_EXCEEDED
@err:	rts
.endproc

;*******************************************************************************
; RENAME FILE
; Renames an existing entry in the file table to the given name.
; Unlike set_name, no new file ID is created; debug info that references the
; given ID (blocks, breakpoints, etc.) now maps to the new name.
; IN:
;   - .A:  the debug file ID of the entry to rename
;   - .XY: the new filename for the entry
; OUT:
;   - .A: the ID of the file that was renamed
;   - .C: set if the ID doesn't exist or the filename is empty or too long
.proc rename_file
	cmp numfiles		; does an entry exist for the given ID?
	bcs @invalid		; if not -> error
	pha			; save the file ID
	jsr write_name		; write the new name to the entry's slot
	bcs @err
	pla			; restore the file ID
	RETURN_OK

@err:	tax			; save the error code
	pla			; clean up the saved file ID
	txa			; restore the error code
	sec
	rts

@invalid:
	RETURN_ERR ERR_FILE_NOT_FOUND
.endproc

;*******************************************************************************
; WRITE NAME
; Writes the given filename to the file table slot for the given file ID
; IN:
;   - .A:  the file ID of the slot to write to
;   - .XY: the filename to write (must reside in the MAIN bank)
; OUT:
;   - .C: set if the filename is empty or too long
.proc write_name
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

	; make sure the filename is not empty (an empty name terminates the
	; file list in dumped debug info)
	cmp #$00
	beq @invalid

	; make sure the filename (and terminating 0) fits in the file table
	cmp #MAX_FILENAME_LEN
	bcs @invalid

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
	tay			; .Y = index of terminating 0 (copy len..0)
:	lda (@filename),y
	sta (@dst),y
	dey
	bpl :-
.endif
	RETURN_OK

@invalid:
	RETURN_ERR ERR_FILENAME_TOO_LONG
.endproc

;*******************************************************************************
; HEADER ADDR
; Returns the address of the block header for the given ID
; IN:
;   - .A: the ID of the block to get
; OUT:
;   - .XY: the address of the requested header
; CLOBBERS:
;   - r0-r1, debugtmp
.proc header_addr
@id2=r0		; ID*2
@id4=debugtmp	; ID*4
	ldx #$00
	stx @id2+1

	; multiply block number by SIZEOF_BLOCK_HEADER (16-bit result)
	asl			; *2
	sta @id2
	rol @id2+1

	asl			; *4
	sta @id4
	lda @id2+1
	rol
	sta @id4+1

	lda @id4
	asl			; *8
	tax
	lda @id4+1
	rol
	tay			; .Y = MSB of ID*8
	txa			; .A = LSB of ID*8

	;clc
	adc @id4		; *12
	bcc :+
	iny			; carry into MSB
	clc
:	adc @id2		; *14
	bcc :+

	iny			; MSB++
	clc

:	; add the base address of the header table
	adc #<blockheaders
	tax			; .X = LSB of header address
	tya			; .Y = ID*8  (MSB)
	adc @id4+1		; .Y = ID*12 (MSB)
	adc @id2+1		; .Y = ID*14 (MSB)
	adc #>blockheaders
	tay			; .Y = MSB of header address
	rts
.endproc

;*******************************************************************************
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
	incw line	; move line program PC to byte 2 of opcode
	LOADB_Y line	; get byte 2 of opcode
	jeq @end	; if byte 2 is 0, we're at the end of the program

	incw line	; move to operand

	cmp #OP_SET_ADDR
	bne :+
@setaddr:
	LOADB_Y line
	sta addr
	incw line
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
	incw line
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
	incw line
	LOADB_Y line
	adc addr+1
	sta addr+1
	jmp @ok

:	cmp #OP_SET_PC
	bne @end	; invalid opcode
@set_pc:
	LOADB_Y line
	sta addr
	incw line
	LOADB_Y line
	sta addr+1
@ok:	incw line
	RETURN_OK

@end:	sec		; flag end of porgram
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
@dbgi=r2
@progstart=r4
@cnt=r6
	ldxy #filenames
	stxy @name

;-------------------------------------------------------------------------------
; dump the filenames
	ldx numfiles
	beq @filesdone
@fnames:
	; write the filename (with terminating 0) to the object file
	; the filename table lives in local RAM (see set_name), so read directly
	ldy #$00
:	lda (@name),y
	jsr krn::chrout
	iny
	cmp #$00
	bne :-

	; move @name pointer to the next filename
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

;-------------------------------------------------------------------------------
; dump the BLOCK headers
	; write the number of blocks
	lda numblocks
	jsr krn::chrout

	; dump each header
	tax			; .X = numblocks
	beq @done		; if no BLOCKS, we're done
	sta @cnt

	lda #$00
	jsr header_addr
	stxy @dbgi

;-------------------------------------------------------------------------------
; dump the header for the block
@dump_block_header:
	ldy #$00
:	LOADB_Y @dbgi
	jsr krn::chrout
	iny
	cpy #SIZEOF_BLOCK_HEADER_OBJ
	bne :-

	; calculate the size of the line program for the block
	; (progstop-progstart)

	;ldy #BLOCK_LINE_PROG
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

:	dec @cnt
	bne @dump_block_header

;-------------------------------------------------------------------------------
; dump the line program data for the BLOCK
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
; If the relocate flag is set (linking), the loaded BLOCKs are appended to
; the existing debug info and their addresses are offset by their SEGMENT
; base addresses.  Some link context is assumed to be set up for this:
;  - map of local (object context) segment id's to segment names
;  - map of global (linker context) segment names to id's
;  - map of global (linker context) base addresses for segments
; If the relocate flag is clear (absolute load), all existing debug info is
; replaced by the loaded debug info.  The file table persists in both cases
; (see initonce); loaded filenames are merged into it.
; IN:
;   - .A: relocate flag: !0=relocate segments, 0=absolute load
; OUT:
;   - .C: set on error
.export __debuginfo_load
.proc __debuginfo_load
@header    = r0
@freeptr   = r0
@segname   = r0
@offset    = r0
@block_i   = zp::tmp10
@nblocks   = zp::tmp11
@relocate  = zp::tmp12
@progstart = zp::tmp14
@i         = zp::tmp16
@filemap   = filemap
@filename  = $100	; NOTE: must not overlap get_fileid's buffer ($120)
	sta @relocate
	cmp #$00
	bne :+			; if relocating (linking), append to existing
	jsr init		; absolute load: replace existing debug info

:	ldxy freeptr
	stxy @progstart

;-------------------------------------------------------------------------------
; load the file table and map ids to global ids
@load_files:
	lda #$00
	sta @i
@mapfile:
	; read a filename into the filename buffer
	ldy #$00
:	jsr krn::chrin
	sta @filename,y
	iny
	cmp #$00
	beq :+			; if 0, we're at the end of the filename
	cpy #MAX_FILENAME_LEN	; is there room for more chars?
	bcc :-			; keep reading if so
	RETURN_ERR ERR_FILENAME_TOO_LONG

:	cpy #$01		; was filename empty?
	beq @load_blocks	; if so, we're at end of list

	; generate the ID for the file
	ldxy #@filename
	jsr set_file
	jcs @ret		; return error (e.g. too many files)
	ldx @i
	cpx #MAX_FILES		; is there room in the file map?
	bcc :+			; (guards against malformed file input)
	RETURN_ERR ERR_MAX_FILES_EXCEEDED
:	sta @filemap,x
	inc @i
	bne @mapfile

;-------------------------------------------------------------------------------
; load the BLOCK data
@load_blocks:
	jsr krn::chrin		; read number of blocks in this file
	cmp #$00
	bne :+
	RETURN_OK		; no blocks

:	; append this file's blocks to any that are already loaded
	ldx numblocks
	stx @block_i		; start at the first free block index
	sta @nblocks		; save the number of blocks left to load
	clc
	adc numblocks		; new total = existing + count for this file
	jcs @toomany		; if more than 255 total blocks -> error
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
	sta progstop		; store stop address LSB (BLOCK_PROG_STOP_ADDR)
	php
	jsr krn::chrin		; read size MSB
	plp
	adc freeptr+1
	jcs @toomany		; if we wrapped past $ffff -> error
	sta progstop+1		; store stop address MSB

	; make sure the line program will fit in the debug info buffer
	ldxy progstop
	cmpw #debuginfo_end
	beq @commit		; == end of buffer: fits exactly
	jcs @toomany		; > end of buffer -> out of memory

@commit:
	; the program fits; commit the new freeptr
	lda progstop
	sta freeptr
	lda progstop+1
	sta freeptr+1

	; map the local (object file) file id to the global one
	; this is needed even for absolute loads: the file table persists, so
	; global file ids may not match the ids local to the loaded file
	ldx file
	cpx @i			; is the local file id one that was loaded?
	jcs @badfile		; if not -> corrupt debug info
	lda @filemap,x
	sta file

	; check if we should do relocation (for linking)
	lda @relocate
	beq @relocate_done

	lda seg_id
	cmp #SEG_ABS
	beq @relocate_done

	; look up global SEGMENT id and replace the LOCAL one with it
	CALL FINAL_BANK_LINKER, obj::get_segment_name_by_id
	bcc :+
	rts			; not found
:	CALL FINAL_BANK_LINKER, link::segid_by_name
	bcc :+
	rts			; unknown segment; return error
:	sta seg_id

	; look up the address offset for this SEGMENT
	CALL FINAL_BANK_LINKER, link::segaddr_by_id
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
	; write the modified header to its stable location
	lda @block_i
	jsr header_addr
	stxy @header
	ldy #SIZEOF_BLOCK_HEADER-1
:	lda blockstate,y
	STOREB_Y @header
	dey
	bpl :-

	; the header is written; the block may now be safely counted
	inc numblocks

@next_block:
	inc @block_i
	dec @nblocks		; any more blocks to load for this file?
	jne @load_block

	; where freeptr ended is the new top of the program
	ldxy freeptr
	stxy progstop

	; make sure the line program data fits in the debug info buffer
	cmpw #debuginfo_end
	beq @load_program	; progstop == end of buffer: fits exactly
	bcc @load_program	; progstop < end of buffer: fits
	RETURN_ERR ERR_OOM

;-------------------------------------------------------------------------------
; load the line program data for the BLOCKs
@load_program:
	ldxy @progstart
	stxy @freeptr
	cmpw progstop
	beq @done		; no line program data -> done

	ldy #$00
:	jsr krn::chrin		; load byte
	STOREB_Y @freeptr	; store it

	; check if we're at the end (@freeptr == progstop)
	; and if we're not, repeat til we are
	incw @freeptr
	lda @freeptr
	cmp progstop
	bne :-
	lda @freeptr+1
	cmp progstop+1
	bne :-

@done:	RETURN_OK

@toomany:
	RETURN_ERR ERR_OOM	; too many blocks / debug info too big
@badfile:
	RETURN_ERR ERR_FILE_NOT_FOUND	; block references an invalid file id
@ret:	rts		; return with error from set_file
.endproc
