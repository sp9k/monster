;*******************************************************************************
; FILE.ASM
; This file contains procedures to read/write to files to/from source or binary.
; The pattern for file I/O is similar to the C standard or CBM DOS:
;
; ldx #<filename
; ldy #>filename
; jsr file::open
; lda #'3'
; jsr $ffd2
; ...
; jsr file::close
;
; We open a file, perform whatever reads/writes we wish, and then close it.
;
; File I/O behaves in two modes: binary and source. The mode is determined by
; the isbin flag, internal to this file, but which is set by the procedure
; that corresponds to the mode.  e.g. file::savebin writes using memory
; and file::savesrc writes the active source file.
;  - When reading binary files, the data goes directly to file::loadaddress
;  - When readings source files, the data is read into whatever the active
;    source file is (see source.asm)
;  - When writing binary files, data is written to the file at file::saveaddress
;  - When writing source files, data is written from the active source file.
;
; Note that the binary that gets written (file::writeb) comes from the virtual
; memory (vmem.asm) and not the main program memory, which is where the editor
; lives.
;*******************************************************************************

.include "config.inc"
.include "errors.inc"
.include "io.inc"
.include "kernal.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "target.inc"
.include "vmem.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
MAX_OPEN_FILES = 10
FIRST_FILE_ID  = 4

files        = $259	; KERNAL open file table
file_devices = $261	; KERNAL device ID table
kernal_sas   = $26d	; KERNAL secondary address table

isvirtual = zp::tmp16	; flag for binary load to VIRTUAL memory
isbin     = zp::tmp17	; flag for binary save/load to memory

;*******************************************************************************
; The address to load from during a binary LOAD
; VOLATILE should be set immediately before calling
.export __file_load_address
__file_load_address = rb

; The address to save to during a binary SAVE
; VOLATILE should be set immediately before calling
.export __file_save_address
__file_save_address     = rb
.export __file_save_address_end
__file_save_address_end = rd

.export __file_load_address_end
__file_load_address_end = rd

.BSS
;*******************************************************************************
secondaryaddr:	.byte 0	; secondary address to use on file open

.export __file_eof
__file_eof:	.byte 0	; if !0, EOF; only valid after call to readb or getline

.CODE
;*******************************************************************************
; MAIN-bank entry points
; NOTE: READB is in the main CODE bank as it is called frequently
; (per-byte in .incbin/.include)
.export __file_load_binv
.export __file_load_bin
.export __file_load_src
.export __file_save_bin
.export __file_save_src
.export __file_getline
.export __file_scratch
.export __file_open_w
.export __file_open_r
.export __file_open_r_prg
.export __file_open
.export __file_exists
.export __file_close
.export __file_closeall
.export __file_init_drive
.export __file_geterr
.export __file_write_banner

.if .defined(CART) .and .defined(c64)
__file_load_binv:    JUMP FINAL_BANK_FILEDIR, loadbinv
__file_load_bin:     JUMP FINAL_BANK_FILEDIR, loadbin
__file_load_src:     JUMP FINAL_BANK_FILEDIR, loadsrc
__file_save_bin:     JUMP FINAL_BANK_FILEDIR, savebin
__file_save_src:     JUMP FINAL_BANK_FILEDIR, savesrc
__file_getline:      JUMP FINAL_BANK_FILEDIR, fgetline
__file_scratch:      JUMP FINAL_BANK_FILEDIR, fscratch
__file_open_w:       JUMP FINAL_BANK_FILEDIR, openw
__file_open_r:       JUMP FINAL_BANK_FILEDIR, openr
__file_open_r_prg:   JUMP FINAL_BANK_FILEDIR, openrprg
__file_open:         JUMP FINAL_BANK_FILEDIR, fopen
__file_exists:       JUMP FINAL_BANK_FILEDIR, fexists
__file_close:        JUMP FINAL_BANK_FILEDIR, fclose
__file_closeall:     JUMP FINAL_BANK_FILEDIR, fcloseall
__file_init_drive:   JUMP FINAL_BANK_FILEDIR, init_drive
__file_geterr:       JUMP FINAL_BANK_FILEDIR, fgeterr
__file_write_banner: JUMP FINAL_BANK_FILEDIR, writebanner
.else
__file_load_binv    = loadbinv
__file_load_bin     = loadbin
__file_load_src     = loadsrc
__file_save_bin     = savebin
__file_save_src     = savesrc
__file_getline      = fgetline
__file_scratch      = fscratch
__file_open_w       = openw
__file_open_r       = openr
__file_open_r_prg   = openrprg
__file_open         = fopen
__file_exists       = fexists
__file_close        = fclose
__file_closeall     = fcloseall
__file_init_drive   = init_drive
__file_geterr       = fgeterr
__file_write_banner = writebanner
.endif

BANKED_CODE "FILEDIR", FINAL_BANK_FILEDIR

;*******************************************************************************
; LOADBINV
; loads the given file into the given virtual memory address
; IN:
;  .A:             the file handle to load from
;  file::loadaddr: the virtual address to load the file to
; OUT:
;  - .C: set on error
.proc loadbinv
	ldx #$01
	stx isvirtual
	stx isbin
	bne load
.endproc

;*******************************************************************************
; LOADBIN
; loads the given file into the given memory address
; IN:
;  .A:                      the file handle to load from
;  file::loadaddr: the address to load the file to
; OUT:
;  - .C: set on error
.proc loadbin
	ldx #$00
	stx isvirtual
	inx
	stx isbin
	bne load
.endproc

;*******************************************************************************
; LOAD SOURCE
; loads the given file into a new source buffer
; IN:
;  - .A: the file handle to load from
; OUT:
;  - .C: set on error
loadsrc:
	ldx #$00
	stx isbin		; not binary (load to source buff)
	sta secondaryaddr	; use handle as secondary address

	; fall through

;*******************************************************************************
; LOAD
; loads the given file into memory or a source buffer
; IN:
;  - .A  the file handle
;  - rb: the address to load the file to
; OUT:
;  - .C: set on error, clear on success
;  - .A: if .C is set, the error code
OSPROC load
	tax
	jsr krn::chkin	; CHKIN (file in .X now used as input)
@l0: 	jsr krn::readst	; call READST (read status byte)
	bne @err_or_eof	; either EOF or read error
	jsr krn::chrin	; call CHRIN (get a byte from file)
	jsr putb	; write the byte to source/memory
	bcc @l0
	rts		; failed to write byte return err
@err_or_eof:
	eor #$40
	beq @eof	; if EOF, return
@error:	jmp fgeterr
@eof:	RETURN_OK
ENDOSPROC

;*******************************************************************************
; SAVEBIN
; Saves the binary from the given address, to the given filename.
; NOTE: the address refers to the virtual memory address not the physical
; one.
; IN:
;  - .A:                      the file to save to
;  - .XY:                     the start address to write from
;  - __file_save_address_end: the end of the address range to save
; OUT:
;  .C: set on error, clear on success
OSPROC savebin
	stxy __file_save_address
	tax
	jsr krn::chkout	; CHKOUT (file in .X now uesd as output)
	lda #$01
	sta isbin	; flag that we're saving binary
	jmp dosave
ENDOSPROC

;*******************************************************************************
; SAVE SRC
; Saves the active source buffer to the given (already open) file
; IN:
;  .A: the handle of the file to write the source buffer to
; OUT:
;  .C: set on error, clear on success

OSPROC savesrc
	tax
	jsr krn::chkout	; CHKOUT (file in .X now uesd as output)
	ldx #$00
	stx isbin	; not binary

	; fall through to dosave
ENDOSPROC

;*******************************************************************************
; DOSAVE
; Saves the source buffer or binary block to the given file.
; This is called by __file_save and __file_savebin after those routines
; initialize the flags necessary to save source or memory respectively.
; IN:
;  - the file to write out should be open (file::open_w)
; OUT:
;  - .C: set on error, clear on success
OSPROC dosave
@save:  jsr krn::readst	; READST (read status byte)
	bne @error	; error

@chout: jsr getb
	bcs @done	; EOF or end of save range
	jsr krn::chrout	; write byte to file
	jmp @save

@done:	lda #$00	; no error
	RETURN_OK	; done

@error:	jmp fgeterr
ENDOSPROC

.CODE
SET_CUR_BANK BANK_NONE

;*******************************************************************************
; READB
; Reads a byte from the open file
; OUT:
;  - .C:  set on error (EOF returns .C clear)
;  - .A:  the byte that was read
;  - eof: set if READST returns eof
.export __file_readb
OSPROC __file_readb
	lda #$00
	sta __file_eof
	jsr krn::readst	; call READST (read status byte)
	bne @eof	; either EOF or read error
	jsr krn::chrin	; call CHRIN (get a byte from file)
	bcc @ok

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	and #$40
	beq @err
	inc __file_eof
@ok:	RETURN_OK

@err:	jmp fgeterr
ENDOSPROC

BANKED_CODE "FILEDIR"
SET_CUR_BANK FINAL_BANK_FILEDIR

;*******************************************************************************
; GETLINE
; Reads the file handle given in .A until EOF or a newline ($0d or $0a) is
; encountered and stores the data at the address given in .XY
; will read a maximum of 255 bytes
; IN:
;  - .XY: the address to read the line into
;  - .A: the file ID
; OUT:
;  - .A: contains the # of bytes read OR the error code
;  - .C: is set if an error occurred
OSPROC fgetline
	stxy __file_load_address
	tax
	jsr krn::chkin	; CHKIN (file in .X now used as input)

	ldy #$00
@l0:	jsr __file_readb
	bcs @ret	; return read error
	ldx __file_eof
	bne @done	; EOF: return what we have of the final line

	cmp #$0d
	beq @done
	cmp #$0a
	beq @done	; end of line

	sta (__file_load_address),y
	iny
	bne @l0

@done:	; check for a line-continuation marker (<-) as the last character
	cpy #$00
	beq @term		; empty line, nothing to check
	dey
	lda (__file_load_address),y
	cmp #LINE_CONT
	bne @noext		; normal end of line

	lda __file_eof
	bne @term		; EOF right after the marker: drop it and finish
	jmp @l0			; continuation: next line overwrites the marker

@noext:	iny			; restore the count (undo the peek)
@term:	lda #$00
	sta (__file_load_address),y	; 0-terminate the string
	tya		; put # of bytes read in .A
@ok:	clc		; no error
@ret:	rts
ENDOSPROC

;*******************************************************************************
; SCRATCH
; Deletes the given file
; IN:
;  - .XY: the 0-terminated filename of the file to open for deletion
; OUT:
;   - .C: set on error
OSPROC fscratch
	stxy r0
	jsr init_drive

	ldx #<@s_colon
	ldy #>@s_colon
	jsr str::cat		; s:<filename>
	lda #15			; SA (command channel)
	jsr fopen
	bcs @err
	jsr fclose	; close logical file of deleted file
	jmp fgeterr	; return the drive's response to the scratch

@err:	rts			; return error from open

@s_colon:
	.byte "s:",0
ENDOSPROC

;*******************************************************************************
; OPEN_W
; Opens a file for writing
; IN:
;  - .XY: the 0-terminated filename to open for writing
; OUT:
;  - .A: the file handle
;  - .C: set on error
.proc openw
	lda #<@p_w
	sta r0
	lda #>@p_w
	sta r0+1
	jsr str::cat	; filename + ",p,w"
	bcc :+
	rts		; filename too long

	; fall through to openr
:
@p_w:	.byte ",p,w",0
.endproc

;*******************************************************************************
; OPEN_R
; Opens a file for reading
; IN:
;  - .XY: the 0-terminated filename to open for reading
; OUT:
;  - .A: the file handle
;  - .C: set on error
.proc openr
	lda #$ff	; flag use file handle as SA
	skw

	; fall through to fopen
.endproc

;*******************************************************************************
; OPEN_R_PRG
; Opens a file for reading as a .PRG
OSPROC openrprg
	lda #$00

	; fall through to fopen
ENDOSPROC

;*******************************************************************************
; OPEN
; Opens a file from disk, opens a channel and returns the handle to it.
; you may call the read and write operations with this handle
; IN:
;  - .XY: the 0-terminated filename of the file to open
;  - .A:  the secondary address
; OUT:
;  - .A: the file handle
;  - .C: set on error
OSPROC fopen
@file=r2
@filename=r3
	sta secondaryaddr
	lda zp::numfiles
	cmp #MAX_OPEN_FILES
	bcc :+
	lda #ERR_MAX_FILES_EXCEEDED
	;sec
	rts

:	stxy @filename
	jsr str::len
	pha

	; find a free file ID in KERNAL's file table
	lda #FIRST_FILE_ID-1
	sta @file
@l0:	inc @file
	lda @file	; get an ID to try
	ldx zp::numfiles
@l1:	dex
	bmi @found	; file ID not found in table, it's free
	cmp files,x	; is file ID already in table?
	bne @l1		; check all entries
	beq @l0		; if file ID matches another entry, try a new ID

@found:	lda secondaryaddr
	bpl :+
	; if secondaryaddr < 0, use file handle as SA
	lda @file
	sta secondaryaddr

:	pla			; get length of filename
	ldxy @filename
	jsr krn::setnam		; SETNAM

	lda @file		; file handle
	ldx zp::device		; last used device number
	ldy secondaryaddr	; SA
	jsr krn::setlfs 	; SETLFS
	jsr krn::open 		; call OPEN
	bcc @ok

@getopenerr:
	pha			; save the KERNAL error code
	lda @file
	jsr krn::close
	pla			; restore the KERNAL error code
	cmp #$05		; DEVICE NOT PRESENT?
	bne :+
	ldxy #strings::device_not_present
	jmp io::seterr

:	jmp fgeterr
@ok:	lda @file		; get file ID that we opened
	rts			; return it
ENDOSPROC

;*******************************************************************************
; EXISTS
; Opens, reads a byte, and closes a file of the given name to check for its
; existence.
; IN:
;  - .XY: the filename to check if exists
; OUT:
;  - .A: 0 if file exists
;  - .C: set on error
;  - .Z: set if the file exists; clear if it does
OSPROC fexists
@file=r0
	jsr openr
	bcs @ret

	sta @file
	tax
	jsr krn::chkin		; CHKIN (file in .X now used as input)
	jsr __file_readb	; read one byte
	jsr krn::readst		; call READST (read status byte)
	cmp #20
	bcs :+
	lda #$00		; ignore errors 0-19 (file exists)
:	pha

	; close the file we were testing
	lda @file
	jsr fclose

	pla
	bne @done
	clc			; ok
@ret:	rts

@done:	jmp fgeterr
ENDOSPROC

;*******************************************************************************
; CLOSE
; Closes the file with the given handle.
; IN:
;  - .A: the file handle to close
OSPROC fclose
	jmp krn::close		; CLOSE
ENDOSPROC

;*******************************************************************************
; CLOSE ALL
; Closes all open files/channels
OSPROC fcloseall
	jmp krn::clall
ENDOSPROC

;*******************************************************************************
; INIT DRIVE
; Initializes the drive
OSPROC init_drive
	ldxy #@i
	lda #1
	jsr krn::setnam	; SETNAM

	lda #15
	ldx zp::device
	tay
	jsr krn::setlfs	; SETLFS
	jsr krn::open	; OPEN
	lda #15
	jsr krn::close	; CLOSE
	jmp krn::clrchn
@i:	.byte $49	; 'I'
ENDOSPROC

;*******************************************************************************
; GETB
; During SAVE: reads the next byte to be saved.
; OUT:
;  - .A: the character that was read
;  - .C: set if EOF (source) or end of save address range (binary)
.proc getb
	lda isbin	; are we reading from memory or source
	bne @use_bin

@use_src:
	jsr src::end
	beq @eof	; if EOF, return with .C set
	jmp src::next	; else, return the next byte
@eof:	sec
	rts

@use_bin:
	ldxy __file_save_address
	cmpw __file_save_address_end	; are we at the end of the save range?
	bcs @done			; if so, we're done

	jsr vmem::load
	incw __file_save_address
	clc
@done:	rts
.endproc

;*******************************************************************************
; GETERR
; Reads the error channel and maps the DOS error code to the internal one
; IN:
;  - .A: the error code to map e.g. #$3e (62)
; OUT:
;  - .A: the internal error code e.g. ERR_FILE_NOT_FOUND
;  - .C: set if the drive returns an error
.proc fgeterr
	jsr io::readerr
	txa
	cpx #20
	bcc @ret		; err < 20 -> no error

	cpx #73			; 73 means "power up OK" -> not an error
	bne :+
	RETURN_OK

:	lda #ERR_IO_ERROR	; default (unknown) file error
	cpx #$3e		; err code $3e (file not found)?
	sec
	bne @ret
	lda #ERR_FILE_NOT_FOUND
@ret:	rts
.endproc

;*******************************************************************************
; PUTB
; Outputs a byte to the __file_load_address (if isbin is !0) or to the current
; source if not.
; If loading a binary file, isvirtual determines if it will be loaded into
; physical memory (isvirtual == 0) or virtual (isvirtual != 0)
; IN:
;  - .A: the byte to output
; OUT:
;  - .C: set if the byte could not be written to the destination
.proc putb
	ldx isbin
	bne @mem
@src:	jmp src::insert_on_load
@mem:	ldxy __file_load_address
	cmpw __file_load_address_end
	bcc :+

	; load address has reached designated stop address
	lda #ERR_FILE_TOO_BIG
	rts

:	; check if we should write to physical or virtual memory
	pha			; save byte to write
	lda isvirtual
	beq @phys

@virt:	; write to virtual memory
	pla
	jsr vmem::store
	jmp @done

@phys:	ldy #$00
	pla
	sta (__file_load_address),y
@done:	incw __file_load_address
	RETURN_OK
.endproc

;*******************************************************************************
; WRITE BANNER
; Outputs a "banner" to the currently open file
.proc writebanner
	ldx #20
	lda #'-'
:	jsr krn::ciout
	dex
	bne :-
	lda #$0d
	jmp krn::ciout
.endproc
