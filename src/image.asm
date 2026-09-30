;*******************************************************************************
; IMAGE.ASM
; Shared 24-bit image storage using the VIC-20 and C64 backends.
; All calls execute in FINAL_BANK_LINKER_AUX.
;*******************************************************************************

.include "image.inc"
.include "errors.inc"
.include "macros.inc"
.include "ram.inc"
.include "asm.inc"
.include "file.inc"
.include "kernal.inc"
.include "vmem.inc"
.import __image_backend_load, __image_backend_store, __image_backend_fill

.segment "DATA"
.export __image_mode
__image_mode: .byte $00 ; 0 = no linked image, 1 = building, 2 = ready
.export __image_mapped
__image_mapped: .byte $00 ; 0 = CPU-addressed image, 1 = explicit OFFSET layout

.segment "IMAGE_VARS"
.export __image_cursor, __image_length, __image_page
__image_cursor: .res 3
__image_length: .res 3
.export __image_start, __image_copy_address, __image_copy_size
__image_start: .res 3            ; first byte to save
__image_copy_address: .word $0000
__image_copy_size: .res 3
copy_end: .res 3
fill_byte: .byte $00
store_byte: .byte $00
__image_page: .byte $00
ready: .res IMAGE_PAGES

BANKED_SEG "LINKER_AUX", FINAL_BANK_LINKER_AUX

;*******************************************************************************
; INIT
; Initializes an empty image with the given fill byte.
; IN:
;   - .A: fill byte.
; OUT:
;   - .C: clear.
.export __image_init
.proc __image_init
	sta fill_byte
	lda #$00
	ldx #IMAGE_PAGES-1

@clear:	sta ready,x
	dex
	cpx #$ff
	bne @clear
	sta __image_cursor
	sta __image_cursor+1
	sta __image_cursor+2
	sta __image_length
	sta __image_length+1
	sta __image_length+2
	sta __image_start
	sta __image_start+1
	sta __image_start+2
	clc
	rts
.endproc

;*******************************************************************************
; CHECK CURSOR
; Checks all 24 bits of the cursor against the image capacity.
; IN:
;   - image::cursor: 24-bit image offset.
; OUT:
;   - .C: set and .A = ERR_FILE_TOO_BIG if outside the image capacity.
.proc check_cursor
	lda __image_cursor+2
	cmp #.bankbyte(IMAGE_CAPACITY)
	bcc @ok
	bne @bad
	lda __image_cursor+1
	cmp #>IMAGE_CAPACITY
	bcc @ok
	bne @bad
	lda __image_cursor
	cmp #<IMAGE_CAPACITY
	bcc @ok

@bad:	lda #ERR_FILE_TOO_BIG
	sec
	rts

@ok:	clc
	rts
.endproc

;*******************************************************************************
; SELECT PAGE
; Selects the 8 KiB page containing the cursor.
; IN:
;   - image::cursor: validated image offset.
; OUT:
;   - .Y, __image_page: page index.
.proc select_page
	lda __image_cursor+2
	asl
	asl
	asl
	sta __image_page
	lda __image_cursor+1
	lsr
	lsr
	lsr
	lsr
	lsr
	ora __image_page
	sta __image_page
	tay
	rts
.endproc

;*******************************************************************************
; LOAD
; Reads a byte from the image, including lazily filled pages.
; IN:
;   - image::cursor: 24-bit image offset.
; OUT:
;   - .A: byte, or error code when .C is set.
.export __image_load
.proc __image_load
	jsr check_cursor
	bcs @done
	jsr select_page
	lda ready,y
	bne @read
	lda fill_byte
	clc
	rts

@read:	jsr __image_backend_load
	clc

@done:	rts
.endproc

;*******************************************************************************
; STORE
; Writes a byte and extends the image length when needed.
; IN:
;   - .A: byte
;   - image::cursor: 24-bit image offset
; OUT:
;   - .A: stored byte, or error code when .C is set.
;   - image::length: exclusive end of the image.
.export __image_store
.proc __image_store
	sta store_byte
	jsr check_cursor
	bcs @done
	jsr select_page
	lda ready,y
	bne @write
	lda fill_byte
	jsr __image_backend_fill
	ldy __image_page
	lda #$01
	sta ready,y

@write:	lda store_byte
	jsr __image_backend_store

	; Extend the exclusive end only when writing at or beyond it.
	lda __image_cursor+2
	cmp __image_length+2
	bcc @ok
	bne @extend
	lda __image_cursor+1
	cmp __image_length+1
	bcc @ok
	bne @extend
	lda __image_cursor
	cmp __image_length
	bcc @ok

@extend:
	ldx #$02

@copy:	lda __image_cursor,x
	sta __image_length,x
	dex
	bpl @copy
	inc __image_length
	bne @ok
	inc __image_length+1
	bne @ok
	inc __image_length+2

@ok:	lda store_byte
	clc

@done:	rts
.endproc

;*******************************************************************************
; NEXT
; Advances the image cursor by one byte.
; IN:
;   - image::cursor: 24-bit image offset.
; OUT:
;   - image::cursor: next offset
;   - .C: set and .A = error code at capacity
.export __image_next
.proc __image_next
	jsr check_cursor
	bcs @done
	inc __image_cursor
	bne @ok
	inc __image_cursor+1
	bne @ok
	inc __image_cursor+2

@ok:	clc

@done:	rts
.endproc

;*******************************************************************************
; SAVE
; Writes the completed linked image, or the current standalone assembly, to a file.
; IN:
;   - .A: output file handle
;   - image::mode: linked image state (0 selects standalone assembly)
; OUT:
;   - .C: set and .A = error code on failure.
.export __image_save
.proc __image_save
	pha
	lda __image_mode
	bne @image
	lda asm::has_output
	bne @flat
	pla
	RETURN_OK

@flat:	ldxy asm::top
	stxy file::save_address_end
	ldxy asm::origin
	pla
	JUMPMAIN file::savebin

@image:	cmp #$02
	beq @ready
	pla
	RETURN_ERR ERR_INVALID_COMMAND

@ready:	pla
	tax
	jsr krn::chkout
	bcs @ioerror
	ldx #$02
@start:	lda __image_start,x
	sta __image_cursor,x
	dex
	bpl @start

@loop:	jsr krn::readst
	bne @ioerror
	lda __image_cursor
	cmp __image_length
	bne @byte
	lda __image_cursor+1
	cmp __image_length+1
	bne @byte
	lda __image_cursor+2
	cmp __image_length+2
	beq @done

@byte:	jsr __image_load
	bcs @return
	jsr krn::chrout
	jsr __image_next
	bcs @return
	jmp @loop

@done:	RETURN_OK

@ioerror:
	RETURN_ERR ERR_IO_ERROR

@return:
	rts
.endproc

;*******************************************************************************
; CHECK PROGRAM
; Checks whether the current result has one unambiguous CPU address layout.
; IN:
;   - image::mode, image::mapped: current result state and placement
; OUT:
;   - .C: set and .A = error code if incomplete or explicitly mapped
.export __image_check_program
.proc __image_check_program
	lda __image_mode
	beq @ok                  ; standalone assembly/debug-file result
	cmp #$02
	bne @bad
	lda __image_mapped
	bne @bad
@ok:	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_COMMAND
.endproc

;*******************************************************************************
; LOAD PROGRAM
; Loads the completed CPU-addressed image into simulated memory.
; IN:
;   - image::start, image::length: initialized image range
;   - image::mode: ready; image::mapped: zero
; OUT:
;   - .C: set and .A = error code if no CPU-addressed image is ready
.export __image_load_program
.proc __image_load_program
	lda __image_mode
	cmp #$02
	bne @bad
	jsr __image_check_program
	bcs @done
	lda __image_start
	sta __image_cursor
	sta __image_copy_address
	lda __image_start+1
	sta __image_cursor+1
	sta __image_copy_address+1
	lda __image_start+2
	sta __image_cursor+2
	sec
	lda __image_length
	sbc __image_start
	sta __image_copy_size
	lda __image_length+1
	sbc __image_start+1
	sta __image_copy_size+1
	lda __image_length+2
	sbc __image_start+2
	sta __image_copy_size+2
	jmp __image_to_vmem
@bad:	RETURN_ERR ERR_INVALID_COMMAND
@done:	rts
.endproc

;*******************************************************************************
; TO VMEM
; Copies a checked image slice to simulated memory after linking is complete.
; IN:
;   - image::cursor: 24-bit source offset
;   - image::copy_size: 24-bit byte count (up to $10000)
;   - image::copy_address: 16-bit simulated destination address
; OUT:
;   - .C: set and .A = error code on invalid state or bounds
;   - image::cursor, image::copy_address: first address past the copied bytes
;   - image::copy_size: zero after a successful copy
.export __image_to_vmem
.proc __image_to_vmem
	lda __image_mode
	cmp #$02
	beq :+
	RETURN_ERR ERR_INVALID_COMMAND
:	clc
	lda __image_copy_address
	adc __image_copy_size
	sta copy_end
	lda __image_copy_address+1
	adc __image_copy_size+1
	sta copy_end+1
	lda __image_copy_size+2
	adc #$00
	bcs @bad
	beq @source
	cmp #$01
	bne @bad
	lda copy_end
	ora copy_end+1
	bne @bad                  ; exclusive CPU endpoint may equal $10000

@source:
	clc
	lda __image_cursor
	adc __image_copy_size
	sta copy_end
	lda __image_cursor+1
	adc __image_copy_size+1
	sta copy_end+1
	lda __image_cursor+2
	adc __image_copy_size+2
	sta copy_end+2
	bcs @bad
	lda __image_length
	cmp copy_end
	lda __image_length+1
	sbc copy_end+1
	lda __image_length+2
	sbc copy_end+2
	bcc @bad

@loop:	lda __image_copy_size
	ora __image_copy_size+1
	ora __image_copy_size+2
	beq @done
	jsr __image_load
	bcs @return
	ldxy __image_copy_address
	jsr vmem::store
	inc __image_copy_address
	bne :+
	inc __image_copy_address+1
:	jsr __image_next
	bcs @return
	lda __image_copy_size
	bne @decrement
	lda __image_copy_size+1
	bne :+
	dec __image_copy_size+2
:	dec __image_copy_size+1
@decrement:
	dec __image_copy_size
	jmp @loop
@done:	RETURN_OK
@bad:	RETURN_ERR ERR_SEGMENT_OUT_OF_RANGE
@return:
	rts
.endproc
