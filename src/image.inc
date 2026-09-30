;*******************************************************************************
; IMAGE.INC
; Interface to the shared banked output image store.
;*******************************************************************************

.ifndef IMAGE_INC
IMAGE_INC = 1
.ifdef vic20
.include "vic20/ultimem/banks.inc"
.else
.include "c64/image.inc"
.endif
.global __image_init, __image_load, __image_store, __image_next
.global __image_cursor, __image_length, __image_mode, __image_save
.global __image_start, __image_mapped, __image_copy_address, __image_copy_size, __image_to_vmem, __image_load_program
.global __image_check_program
.scope image
	mode     = __image_mode
	mapped   = __image_mapped
	start    = __image_start
	copy_address = __image_copy_address
	copy_size = __image_copy_size
	to_vmem  = __image_to_vmem
	load_program = __image_load_program
	check_program = __image_check_program
	save     = __image_save
	init     = __image_init
	load     = __image_load
	store    = __image_store
	next     = __image_next
	cursor   = __image_cursor
	length   = __image_length
	capacity = IMAGE_CAPACITY
.endscope
.endif
