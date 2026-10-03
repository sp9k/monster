;*******************************************************************************
; BUILD.ASM
; This file contains procedures for building a program from a manifest.
; This allows assembly of input -> output files in order.
;*******************************************************************************

.include "asm.inc"
.include "debuginfo.inc"
.include "edit.inc"
.include "errlog.inc"
.include "errors.inc"
.include "file.inc"
.include "kernal.inc"
.include "linker.inc"
.include "limits.inc"
.include "log.inc"
.include "macros.inc"
.include "memory.inc"
.include "object.inc"
.include "ram.inc"
.include "runtime.inc"
.macpack longbranch

.segment "BUILD_VARS"

;*******************************************************************************
linebuffer = mem::spare+$200

;*******************************************************************************
build_filename:  .res 17
source_filename: .res 17
obj_filename:    .res 17

;*******************************************************************************
.export __build_line
__build_line: .word $0000

.export __build_completed
__build_completed: .byte $00

;*******************************************************************************
manifest_file: .byte $00
output_file:   .byte $00
at_eof:        .byte $00
skip_lf:       .byte $00
position:      .byte $00
result:        .byte $00
list_end:      .word $0000
list_count:    .byte $00

BANKED_SEG "LINKER_AUX", FINAL_BANK_LINKER_AUX

;*******************************************************************************
; BUILD OBJECTS
; Reads quoted source/object pairs and rebuilds each object in order
; IN:
;   - .XY: manifest filename accessible in the current bank
; OUT:
;   - .C:               set on failure or cancellation
;   - .A:               error code on failure; zero on cancellation
;   - build::completed: number of objects successfully closed
;   - build::line:      current manifest line
.export __build_objects
.proc __build_objects
@name=r0
	stxy @name
	lda asm::mode
	pha
	lda zp::gendebuginfo
	pha
	lda zp::verify
	pha

	lda #$00
	sta __build_completed
	sta __build_line
	sta __build_line+1
	sta at_eof
	sta skip_lf
	sta manifest_file
	sta zp::gendebuginfo
	sta zp::verify

	ldy #$00
@copy:	lda (@name),y
	beq @named
	cpy #$10
	jeq @long
	jsr uppercase
	sta build_filename,y
	iny
	bne @copy

@named: sta build_filename,y		; 0-terminate
	cpy #$00
	jeq @missing

	; make sure BUILD file is not an object file (.o)
	ldxy #build_filename
	jsr object_suffix
	jeq @syntax

	; open the BUILD file
	CALLMAIN run::install_sigint
	ldxy #build_filename
	jsr filename
	CALLMAIN file::open_r
	jcs @finish
	sta manifest_file

@next:	lda edit::sigint
	jne @cancel
	lda at_eof
	bne @done

	; read line from the BUILD manifest
	jsr read_line
	jcs @finish

	lda file::eof
	sta at_eof		; assembly and nested includes use the same EOF flag

	incw __build_line
	lda __build_line
	ora __build_line+1
	beq @large

	jsr parse_pair		; parse the source/object pair
	jcs @finish
	beq @next		; blank or comment-only line

	lda __build_completed
	cmp #MAX_OBJS
	bcs @large

	jsr assemble		; assemble the source file
	jcs @finish
	jsr save_object		; save the assembled object code
	jcs @finish
	inc __build_completed
	jmp @next		; repeat for next source file

;-------------------------------------------------------------------------------
@done:	lda __build_completed
	beq @syntax
	lda #$00
	clc
	bcc @finish

@cancel:
	lda #$00
	sec
	bcs @finish

@large: lda #ERR_FILE_TOO_BIG
	bne @error
@long:	lda #ERR_FILENAME_TOO_LONG
	bne @error
@missing:
	lda #ERR_NO_FILENAME
	bne @error

@syntax:
	lda #ERR_SYNTAX_ERROR
@error:	sec
@finish:
	sta result
	php
	lda manifest_file
	beq @closed
	CALLMAIN file::close

;-------------------------------------------------------------------------------
@closed:
	plp
	pla
	sta zp::verify
	pla
	sta zp::gendebuginfo
	pla
	sta asm::mode

	lda result
	rts
.endproc

;*******************************************************************************
; LINK MANIFEST
; Links the completed build's objects in manifest order using the LINK layout
; IN:
;   - build_filename: filename retained by build::objects
; OUT:
;   - .C: set on failure or cancellation
;   - .A: error code on failure, zero on cancellation
;   - image::mode: $02 after successful linking
.export __build_link
.proc __build_link
	; parse the LINK file
	CALL FINAL_BANK_LINKER, link::init
	CALL FINAL_BANK_LINKER, link::parse
	jcs @ret

	lda #$00
	sta at_eof
	sta skip_lf
	sta list_count
	sta __build_line
	sta __build_line+1
	ldxy #link::objfiles
	stxy list_end

	ldxy #build_filename
	jsr filename
	CALLMAIN file::open_r
	bcs @ret
	sta manifest_file

@next:	; read the next object file to link
	lda edit::sigint
	bne @cancel
	lda at_eof
	bne @done
	jsr read_line
	bcs @close
	lda file::eof
	sta at_eof
	incw __build_line
	jsr parse_pair		; parse the source/object pair
	bcs @close
	beq @next
	jsr append_object	; append object to link to the list
	bcs @close
	inc list_count
	jmp @next

@done:	lda list_count
	beq @empty
	clc
	bcc @close

@empty: lda #ERR_SYNTAX_ERROR
	skw
@cancel:
	lda #$00
	sec

@close: sta result
	php
	lda manifest_file
	CALLMAIN file::close
	plp
	lda result
	bcs @ret

	; link the list of object files
	CALL FINAL_BANK_LINKER, link::link
@ret:	rts
.endproc

;*******************************************************************************
; APPEND OBJECT
; Adds a unique output filename to the linker's packed list
; IN:
;   - obj_filename: zero-terminated object filename
;   - list_end:     next free list byte
; OUT:
;   - list_end: updated next free byte
;   - .C:       set on duplicate filename or list overflow
;   - .A:       error code on failure
.proc append_object
@entry=r0
	lda list_count
	cmp #MAX_OBJS
	bcs @full
	ldxy #link::objfiles
	stxy @entry

@scan:	ldxy @entry
	cmpw list_end
	beq @append

	ldy #$00
@match: lda (@entry),y
	cmp obj_filename,y
	bne @skip
	cmp #$00
	beq @duplicate
	iny
	bne @match

@skip:	lda (@entry),y
	beq @advance
	iny
	bne @skip

@advance:
	iny
	tya
	clc
	adc @entry
	sta @entry
	bcc @scan
	inc @entry+1
	bne @scan

@append:
	ldy #$00
@copy:	lda obj_filename,y
	sta (@entry),y
	iny
	cmp #$00
	bne @copy
	sta (@entry),y		; terminate the filename list
	tya
	clc
	adc @entry
	sta list_end
	lda @entry+1
	adc #$00
	sta list_end+1
	RETURN_OK

@duplicate:
	RETURN_ERR ERR_DUPLICATE_NAME
@full:	RETURN_ERR ERR_TOO_MANY_OBJECTS
.endproc

;*******************************************************************************
; READ LINE
; Reads a bounded manifest line without consuming assembler continuation syntax
; IN:
;   - manifest_file: open manifest handle
; OUT:
;   - linebuffer: zero-terminated line of at most 80 bytes
;   - file::eof:  nonzero at end of file
;   - .C:         set on error
;   - A:          error code on read failure or oversized input
.proc read_line
	ldx manifest_file
	jsr krn::chkin
	bcs @io
	ldy #$00

@next:	CALLMAIN file::readb
	bcs @ret
	ldx file::eof
	bne @done
	ldx skip_lf
	beq @character
	ldx #$00
	stx skip_lf
	cmp #$0a
	beq @next

@character:
	cmp #$0d
	beq @cr
	cmp #$0a
	beq @done
	cpy #80
	bcs @long
	sta linebuffer,y
	iny
	bne @next

@cr:	inc skip_lf
@done:	lda #$00
	sta linebuffer,y
	clc
@ret:	rts
@long:	RETURN_ERR ERR_LINE_TOO_LONG
@io:	RETURN_ERR ERR_IO_ERROR
.endproc

;*******************************************************************************
; PARSE PAIR
; Reads one manifest line: "source.s" "object.o", optionally followed by ;
; IN:
;   - linebuffer: zero-terminated manifest line
; OUT:
;   - source_filename, obj_filename: parsed filenames
;   - .Z:    set for a blank or comment-only line
;   - .C:    set on error
;   - .A:    error code (if .C set)
.proc parse_pair
	lda #$00
	sta position
	jsr whitespace
	beq @empty
	cmp #';'
	beq @empty
	ldxy #source_filename
	jsr quoted_name
	bcs @ret
	cpx #$10
	beq @long
	ldxy #source_filename
	jsr object_suffix
	beq @bad		; source names cannot be object output names
	jsr whitespace
	ldxy #obj_filename
	jsr quoted_name
	bcs @ret
	ldxy #obj_filename
	jsr object_suffix
	bne @bad
	jsr whitespace
	beq @pair
	cmp #';'
	bne @bad

@pair:	lda #$01
	RETURN_OK
@empty:
	lda #$00
	clc
@ret:	rts
@bad:	RETURN_ERR ERR_SYNTAX_ERROR
@long:	RETURN_ERR ERR_FILENAME_TOO_LONG
.endproc

;*******************************************************************************
; QUOTED NAME
; Copies a quoted literal filename, excluding DOS command and wildcard syntax
; IN:
;   - .XY:      destination filename buffer
;   - position: opening quote in linebuffer
; OUT:
;   - .X:       filename length
;   - position: first character after the closing quote
;   - .C:       set on error
;   - .A:       error code for invalid or oversized names
.proc quoted_name
@dest=r0
	stxy @dest
	ldy position
	lda linebuffer,y
	cmp #'"'
	bne @bad
	ldx #$00

@next:	iny
	beq @bad
	lda linebuffer,y
	cmp #'"'
	beq @done
	cmp #$20
	bcc @bad
	cmp #':'
	beq @bad
	cmp #','
	beq @bad
	cmp #'@'
	beq @bad
	cmp #'?'
	beq @bad
	cmp #'*'
	beq @bad
	jsr uppercase
	sty position
	ldy #$00
	sta (@dest),y
	incw @dest
	ldy position
	inx
	cpx #$11
	bcc @next
	RETURN_ERR ERR_FILENAME_TOO_LONG

@done:	cpx #$00
	beq @bad
	iny
	sty position
	ldy #$00
	tya
	sta (@dest),y
	RETURN_OK
@bad:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc

;*******************************************************************************
; WHITESPACE
; Skips spaces and tabs in the manifest line
; IN:
;   - position: input offset
; OUT:
;   - .A, position: next non-space byte and its offset
.proc whitespace
	ldy position
@next:	lda linebuffer,y
	cmp #' '
	beq @skip
	cmp #$09
	bne @done
@skip:	iny
	bne @next
@done:	sty position
	cmp #$00
	rts
.endproc

;*******************************************************************************
; OBJECT SUFFIX
; Tests whether a filename ends in .O
; IN:
;   - .XY: zero-terminated filename
; OUT:
;   - .Z: set if the name ends in .O
.proc object_suffix
@name=r0
	stxy @name
	ldy #$00
@scan:	lda (@name),y
	beq @end
	iny
	bne @scan
@end:	cpy #$02
	bcc @no
	dey
	dey
	lda (@name),y
	cmp #'.'
	bne @ret
	iny
	lda (@name),y
	cmp #$4f		; O in disk filenames
@ret:	rts
@no:	lda #$01
	rts
.endproc

;*******************************************************************************
; UPPERCASE
; Normalizes ASCII lowercase filename characters
; IN:
;   - .A: character
; OUT:
;   - .A: normalized character
.proc uppercase
	cmp #$61
	bcc @ret
	cmp #$7b
	bcs @ret
	and #$df
@ret:	rts
.endproc

;*******************************************************************************
; FILENAME
; Copies a persistent filename into shared RAM for cross-bank file operations
; IN:
;   - .XY: zero-terminated filename
; OUT:
;   - .XY: mem::filename
.proc filename
@name=r0
	stxy @name
	ldy #$00
@copy:	lda (@name),y
	sta mem::filename,y
	beq @done
	iny
	bne @copy

@done:	ldxy #mem::filename
	rts
.endproc

;*******************************************************************************
; ASSEMBLE
; Runs both source passes and checks their final state before object output
; IN:
;   - source_filename: source filename
; OUT:
;   - .C: set on error
;   - .A: error code on failure, zero on cancellation
.proc assemble
	CALLMAIN dbgi::init
	CALLMAIN errlog::reset
	lda #$01
	sta asm::mode
@pass:	CALLMAIN asm::startpass

.ifdef c64
	; keep object bytes outside simulated CPU port registers
	lda #$02
	sta zp::asmresult
.endif
	ldxy #source_filename
	jsr filename
	CALLMAIN asm::include
	bcs @ret

	lda edit::sigint
	bne @cancel
	lda errlog::asmerrors
	bne @errors
	CALLMAIN asm::endpass
	bcs @ret

	CALLMAIN obj::close_section
	bcs @ret

	lda zp::pass
	cmp #$02
	beq @ok

	lda #$02
	bne @pass
@ok:	RETURN_OK
@errors:
	RETURN_ERR ERR_SYNTAX_ERROR
@cancel:
	lda #$00
	sec
@ret:	rts
.endproc

;*******************************************************************************
; SAVE OBJECT
; Replaces the output only after successful assembly and checks drive status
; IN:
;   - obj_filename:     output filename
;   - object workspace: completed assembly
; OUT:
;   - .C: set on error
;   - .A: error code on failure
.proc save_object
	ldxy #obj_filename
	jsr filename
	CALLMAIN file::exists
	bcc @replace
	cmp #ERR_FILE_NOT_FOUND
	bne @error
	beq @open

@replace:
	ldxy #obj_filename
	jsr filename
	CALLMAIN file::scratch
	bcs @ret

@open:	ldxy #obj_filename
	jsr filename
	CALLMAIN file::open_w			; open output file

	bcs @ret
	sta output_file
	tax
	jsr krn::chkout
	bcs @io_error
	CALL FINAL_BANK_LINKER, obj::dump	; write the object file
	bcs @failed

	lda edit::sigint
	bne @cancel
	jsr krn::readst
	bne @io_error

	lda output_file
	CALLMAIN file::close
	CALLMAIN file::geterr
	bcs @remove
	RETURN_OK

@io_error:
	lda #ERR_IO_ERROR
	bne @failed
@cancel:
	lda #$00
@failed:
	sta result
	lda output_file
	CALLMAIN file::close
	lda result

;-------------------------------------------------------------------------------
; delete the object file (if closing failed); it's probably a splat file
@remove:
	sta result
	ldxy #obj_filename
	jsr filename
	CALLMAIN file::scratch
	lda result
@error:	sec
@ret:	rts
.endproc
