;*******************************************************************************
; BUILD.ASM
; This file contains procedures for building a program from a manifest.
; This allows assembly of input -> output files in order.
;*******************************************************************************

.include "asm.inc"
.include "source.inc"
.include "screen.inc"
.include "text.inc"
.include "key.inc"
.include "keycodes.inc"
.include "debuginfo.inc"
.include "drivecfg.inc"
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

; manifest header and source/object pair flags
OPTION_DEBUG      = $01
OPTION_PAIRS      = $02
OPTION_OUTPUT     = $04
OPTION_BUILDONLY  = $08

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
manifest_file:    .byte $00
output_file:      .byte $00
at_eof:           .byte $00
skip_lf:          .byte $00
position:         .byte $00
result:           .byte $00
list_end:         .word $0000
list_count:       .byte $00
debug_info:       .byte $00

;*******************************************************************************
; parsed header and source/object pair flags
.export __build_options
__build_options:
manifest_started: .byte $00
object_device:    .byte $00
cache_bank:       .byte $00
cache_end:        .word $0000
cache_column:     .byte $00

.import __src_data
CACHE_LIMIT = __src_data+$2000

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
	sta manifest_started
	sta cache_bank
	sta zp::verify
	lda drivecfg::output_device
	bne :+
	lda zp::device
:	sta object_device
	lda #$01
	sta debug_info

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
	jsr cache_manifest
	jcs @finish

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

	jsr parse_pair		; parse the mode header or source/object pair
	jcs @finish
	beq @next		; blank or comment-only line

	lda __build_completed
	cmp #MAX_OBJS
	bcs @large

	ldxy #source_filename
	jsr __build_require_file
	jcs @finish
	jsr assemble		; assemble the source file
	jcs @finish
	jsr save_on_device	; save the assembled object code
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
	lda cache_bank
	beq :+
	CALLMAIN src::release_bank

	lda #$00
	sta cache_bank
:	plp
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
;   - image::mode: IMAGE_MODE_READY after successful linking
.export __build_link
.proc __build_link
	lda drivecfg::output_device
	bne :+
	lda zp::device

:	sta object_device
	lda #$00
	sta cache_bank
	CALL FINAL_BANK_LINKER, link::init
	ldxy #@layout
	jsr __build_require_file
	jcs @ret

	; parse the LINK file
	CALL FINAL_BANK_LINKER, link::parse
	jcs @ret

	lda #$00
	sta at_eof
	sta skip_lf
	sta list_count
	sta manifest_started
	sta __build_line
	sta __build_line+1
	lda #$01
	sta debug_info
	ldxy #link::objfiles
	stxy list_end

	ldxy #build_filename
	jsr __build_require_file
	jcs @ret
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
	jsr parse_pair		; parse the mode header or source/object pair
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
	lda debug_info
	eor #$01
	sta link::no_debug_info
	lda zp::device
	pha
	lda object_device
	sta zp::device
	CALL FINAL_BANK_LINKER, link::link
	sta result
	pla
	sta zp::device
	lda result
@ret:	rts
@layout: .byte "link",$00
.endproc

;*******************************************************************************
; FIND MANIFEST
; Selects the default BUILD manifest and checks that it exists
; OUT:
;   - build_filename: default manifest filename
;   - .C:             set if the manifest could not be opened
;   - .A:             error code (if .C set)
.export __build_find_manifest
.proc __build_find_manifest
	ldy #$05
@copy:	lda @name,y
	sta build_filename,y
	dey
	bpl @copy

	ldxy #build_filename
	jsr filename
	JUMPMAIN file::exists

;-------------------------------------------------------------------------------
@name:	.byte "build",0
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
	lda cache_bank
	bne @cached
	ldx manifest_file
	jsr krn::chkin
	bcs @io
	ldy #$00

@cached:
	ldy #$00
@next:	sty cache_column
	jsr manifest_byte
	ldy cache_column
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
@done:	; preserve EOI before another channel selection clears KERNAL status
	lda cache_bank
	bne :+
	jsr krn::readst
	sta file::eof
:	lda #$00
	sta linebuffer,y
	clc
@ret:	rts
@long:	RETURN_ERR ERR_LINE_TOO_LONG
@io:	RETURN_ERR ERR_IO_ERROR
.endproc

;*******************************************************************************
; PARSE PAIR
; Reads a build header or a quoted source/object pair with comments
; IN:
;   - linebuffer: zero-terminated manifest line
; OUT:
;   - source_filename, obj_filename: parsed filenames
;   - debug_info: updated by DEBUG or NODEBUG
;   - object_device: updated by OUTPUT
;   - manifest_started: updated header and pair flags
;   - .Z:    set for a mode header, blank, or comment-only line
;   - .C:    set on error
;   - .A:    error code (if .C set)
.proc parse_pair
	lda #$00
	sta position
	jsr whitespace
	beq @empty

	cmp #';'
	beq @empty
	cmp #'"'
	beq @source
	jmp parse_mode

@source:
	lda manifest_started
	ora #OPTION_PAIRS
	sta manifest_started

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
; PARSE MODE
; Reads DEBUG, NODEBUG, OUTPUT, or BUILDONLY before the source/object pairs
; IN:
;   - linebuffer:       contains line of the BUILD file
;   - position:         first nonspace character offset in the line
;   - manifest_started: previously parsed header and source-pair flags
; OUT:
;   - debug_info:       1 for DEBUG, 0 for NODEBUG
;   - object_device:    output drive from OUTPUT
;   - manifest_started: updated header flags
;   - .Z:               set on success (no source/object pair)
;   - .C:               set on error
;   - .A:               error code (if .C set)
.proc parse_mode
@option=r2
@value=r3
	; reject headers after the first source/object pair
	lda manifest_started
	and #OPTION_PAIRS
	jne @bad
	ldx #$00

	; match the header against each keyword without case sensitivity
@keyword:
	ldy position
@match:	lda @words,x
	beq @matched
	lda linebuffer,y
	jsr uppercase
	cmp @words,x
	bne @skip
	inx
	iny
	bne @match

	; advance past the unmatched keyword and its option bit
@skip:	lda @words,x
	inx
	cmp #$00
	bne @skip
	inx		; skip option bit
	lda @words,x
	bne @keyword
	beq @bad	; no options to check left -> error

	; reject a header whose option bit is already set
@matched:
	inx			; .X = offset to option bit
	lda @words,x		; read the option we parsed
	sta @option
	and manifest_started
	bne @bad

	; parse device number for OUTPUT
	sty position
	lda @option
	cmp #OPTION_OUTPUT
	bne @tail

	; require a space or tab after OUTPUT
	lda linebuffer,y
	cmp #' '
	beq :+
	cmp #$09
	bne @bad		; no whitespace -> error

	; skip whitespace and require the first decimal digit
:	jsr whitespace
	cmp #$30
	bcc @bad
	cmp #$3a
	bcs @bad

	; store the first digit and check for a second
	and #$0f
	sta @value
	inc position
	jsr whitespace_digit
	bcc @device		; decimal device # found -> continue

	; get decimal value (first * 10 + second)
	lda @value
	asl
	asl
	adc @value
	asl
	sta @value
	lda linebuffer,y
	and #$0f
	clc
	adc @value
	sta @value
	inc position

@device:
	; validate that IEC device numbers is between 8 and 30
	lda @value
	cmp #$08
	bcc @bad
	cmp #$1f
	bcs @bad
	sta object_device

	; allow only whitespace or a comment after the header
@tail:	jsr whitespace
	beq @commit
	cmp #';'
	bne @bad

	; record the header and check whether it selects debug output
@commit:
	lda @option
	ora manifest_started
	sta manifest_started
	lda @option
	cmp #OPTION_DEBUG
	bne @ok

	; enable debug output for DEBUG at offset 6, disable it for NODEBUG
	lda #$00
	cpx #$06
	bne :+
	lda #$01
:	sta debug_info

	; return with Z set and carry clear for success
@ok:	lda #$00
	clc
	rts

	; report an invalid, duplicate, or misplaced header
@bad:	RETURN_ERR ERR_SYNTAX_ERROR

;-------------------------------------------------------------------------------
; pair each header keyword with its option bit
@words: .byte "debug",    $00, OPTION_DEBUG
	.byte "nodebug",  $00, OPTION_DEBUG
	.byte "output",   $00, OPTION_OUTPUT
	.byte "buildonly",$00, OPTION_BUILDONLY, $00
.endproc

;*******************************************************************************
; WHITESPACE DIGIT
; Checks if the next character is a decimal digit
; IN:
;   - position: character offset
; OUT:
;   - .Y: character offset
;   - .C: set if decimal digit
.proc whitespace_digit
	ldy position
	lda linebuffer,y
	cmp #$30
	bcc @ret
	cmp #$3a
	bcs @no
	sec
@ret:	rts
@no:	clc
	rts
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
	lda debug_info
	sta zp::gendebuginfo
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
@ok:	; close the final line-mapping block before writing the object
	lda debug_info
	beq :+
	ldxy zp::virtualpc
	CALLMAIN dbgi::endblock
:	RETURN_OK
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
	jne @error
	beq @open

@replace:
	ldxy #obj_filename
	jsr filename
	CALLMAIN file::scratch
	jcs @ret

@open:	ldxy #obj_filename
	jsr filename
	CALLMAIN file::open_w			; open output file
	bcc :+
	cmp #ERR_DISK_FULL
	beq @remove
	sec
	rts
:
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
	beq @remove
	CALLMAIN file::geterr
	cmp #ERR_DISK_FULL
	bne :+
	sta result
:	lda result

;-------------------------------------------------------------------------------
; delete the object file (if closing failed); it's probably a splat file
@remove:
	sta result
	ldxy #obj_filename
	jsr filename
	CALLMAIN file::scratch
	bcs @ret
	lda result
@error:	sec
@ret:	rts
.endproc

;*******************************************************************************
; CACHE MANIFEST
; Copies the BUILD file into a temporary source-pool bank
; IN:
;   - manifest_file: open BUILD handle
; OUT:
;   - cache_bank: allocated bank, released by build::objects
;   - list_end:   next cached byte
;   - .C:         set on read failure or a manifest larger than 8 KiB
;   - .A:         error code on failure
.proc cache_manifest
	CALLMAIN src::reserve_bank
	bcc :+
	RETURN_ERR ERR_OOM

:	sta cache_bank
	ldxy #__src_data
	stxy list_end
	ldx manifest_file
	jsr krn::chkin
	jcs @io

;-------------------------------------------------------------------------------
@load:	; read the BUILD file into the source buffer we're borrowing
	lda edit::sigint
	jne @cancel
	CALLMAIN file::readb
	jcs @ret
	ldx file::eof
	bne @done
	pha
	ldxy list_end
	cmpw #CACHE_LIMIT
	pla
	bcs @large
.ifdef c64
	pha
	stxy reu::reuaddr
	lda cache_bank
	sta reu::reuaddr+2
	pla
	CALLMAIN reu::store1
.else
	sta zp::bankval
	ldxy list_end
	lda cache_bank
	CALLMAIN ram::store
.endif
	incw list_end
	jsr krn::readst
	beq @load

	and #$bf			; EOI?
	bne @io				; if not, some other error occurred

;-------------------------------------------------------------------------------
@done:	ldxy list_end
	stxy cache_end
	ldxy #__src_data
	stxy list_end

	lda manifest_file
	CALLMAIN file::close		; close the BUILD file
	lda #$00
	sta manifest_file
	clc
@ret:	rts
@cancel:
	lda #$00
	sec
	rts

;-------------------------------------------------------------------------------
@large:	RETURN_ERR ERR_FILE_TOO_BIG
@io:	RETURN_ERR ERR_IO_ERROR
.endproc

;*******************************************************************************
; MANIFEST BYTE
; Reads the next byte from the cached BUILD or its live channel during relinking
; IN:
;   - cache_bank: temporary bank or zero for the live channel
; OUT:
;   - .A: next character
;   - file::eof: nonzero after the final cached byte
;   - .C: set on a read error
.proc manifest_byte
	lda cache_bank
	bne @cached
	JUMPMAIN file::readb
@cached:
	ldxy list_end
	cmpw cache_end
	beq @eof
.ifdef c64
	stxy reu::reuaddr
	lda cache_bank
	sta reu::reuaddr+2
	CALLMAIN reu::load1
.else
	lda cache_bank
	CALLMAIN ram::load
.endif
	incw list_end
	ldx #$00
	stx file::eof
	clc
	rts

@eof:	lda #$01
	sta file::eof
	clc
	rts
.endproc

;*******************************************************************************
; REQUIRE FILE
; Prompts for missing input media before opening source or object channel
; IN:
;   - .XY:        0-terminated filename
;   - zp::device: input drive
; OUT:
;   - .XY: shared filename buffer on success
;   - .C:  set on failure or cancellation
;   - .A:  error code on failure, 0 on cancellation
.export __build_require_file
.proc __build_require_file
@name=r0
	stxy @name

	ldy #$00
@copy:	lda (@name),y
	sta source_filename,y
	beq @exists
	iny
	bne @copy

@exists:
	ldxy #source_filename
	jsr filename
	CALLMAIN file::exists
	bcc @ok
	cmp #ERR_FILE_NOT_FOUND
	jne @error		; i/oerror

	; file not found, ask user to insert disk
	lda #$00
	jsr disk_prompt
	bcs @ret
	bcc @exists

@ok:	ldxy #source_filename
	jsr filename
	clc
@ret:	rts
@error:	sec
	rts
.endproc

;*******************************************************************************
; OPEN READ
; Opens an input file after prompting for its disk if necessary
; IN:
;   - .XY: filename
;   - zp::device: input drive
; OUT:
;   - .A: file handle, error code, or zero on cancellation
;   - .C: set on failure or cancellation
.export __build_open_read
.proc __build_open_read
	jsr __build_require_file
	jcs @ret
	CALLMAIN file::open_r
@ret:	rts
.endproc

;*******************************************************************************
; SAVE ON DEVICE
; Writes the assembled object to the configured output drive, retrying full media
; IN:
;   - object_device: output drive
; OUT:
;   - .C: set on failure or cancellation
;   - .A: error code on failure, zero on cancellation
.proc save_on_device
	lda zp::device
	pha
	lda object_device
	sta zp::device
@try:	jsr save_object
	bcc @done
	cmp #ERR_DISK_FULL
	bne @error
	lda #$01
	jsr disk_prompt
	bcs @done
	jmp @try
@error:	sec
@done:	sta result
	pla
	sta zp::device
	lda result
	rts
.endproc

;*******************************************************************************
; DISK PROMPT
; Displays the required disk and waits for the user to confirm (RETURN) or
; cancel (RUN/STOP)
; IN:
;   - .A:              0 for an input filename, nonzero for a new output disk
;   - source_filename: missing input filename
;   - zp::device:      drive requiring a disk
; OUT:
;   - .C: set if user cancelled
;   - .A: 0 if user cancelled (RUN/STOP)
.proc disk_prompt
@message=$100
	pha
	CALLMAIN scr::unblank
	pla
	ldx #$00
	cmp #$00
	beq @source

;-------------------------------------------------------------------------------
; copy the "enter new output disk" prompt
@output:
	lda @output_text,x
	sta @message,x
	beq @drive
	inx
	bne @output

;-------------------------------------------------------------------------------
; copy the "enter disk with" prompt
@source:
	lda @source_text,x
	sta @message,x
	beq @name
	inx
	bne @source
@name:	ldy #$00
@copy:	lda source_filename,y
	beq @drive
	sta @message,x
	inx
	iny
	bne @copy

;-------------------------------------------------------------------------------
@drive:	ldy #$00
@append:
	lda @drive_text,y
	beq @number
	sta @message,x
	inx
	iny
	bne @append
@number:
	lda zp::device
	ldy #$30
@tens:	cmp #10
	bcc @units
	sbc #10
	iny
	bne @tens

@units:
	pha
	tya
	sta @message,x
	inx
	pla
	ora #$30
	sta @message,x
	inx
	lda #$00
	sta @message,x
	ldxy #@message
	lda edit::status_row
	sec
	sbc #$01
	CALLMAIN text::print

;-------------------------------------------------------------------------------
; print the user instructions (confirm/cancel)
	ldx #$00
@help:	lda @help_text,x
	sta @message,x
	beq @show
	inx
	bne @help

@show:	ldxy #@message
	lda edit::status_row
	CALLMAIN text::print
	CALLMAIN key::flush
@wait:	CALLMAIN key::waitch
	cmp #K_QUIT
	beq @cancel
	ldx edit::sigint
	bne @cancel
	cmp #K_RETURN
	bne @wait
	clc
	skb
@cancel:
	sec				; flag that user canceled
@finish:
	php
	lda #$00
	sta @message
	ldxy #@message
	lda edit::status_row
	sec
	sbc #$01
	CALLMAIN text::print

	ldxy #@message
	lda edit::status_row
	CALLMAIN text::print
	CALLMAIN scr::blank
	plp
	lda #$00
	rts

;-------------------------------------------------------------------------------
@source_text: .byte "enter disk with ",$00
@output_text: .byte "enter new output disk",$00
@drive_text:  .byte " on #",$00
@help_text:   .byte "return: retry   run/stop: cancel",$00
.endproc
