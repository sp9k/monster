.ifndef FLASHIO_INC
FLASHIO_INC = 1

.import __flasher_open_image
.import __flasher_open_named
.import __flasher_read_image_byte
.import __flasher_printz
.import __flasher_screen
.import __flasher_status
.import __flasher_filename
.import __flasher_file_open
.import __flasher_eof_seen
.import __flasher_write_byte
.import __flasher_image_blocks
.import __flasher_image_last
.import __flasher_block_no
.import __flasher_decimal_value
.import __flasher_print_dec4
.import __flasher_program_byte

.scope flashio
	open           = __flasher_open_image
	open_named     = __flasher_open_named
	read           = __flasher_read_image_byte
	message        = __flasher_printz
	screen         = __flasher_screen
	status         = __flasher_status
	filename       = __flasher_filename
	file_open      = __flasher_file_open
	eof            = __flasher_eof_seen
	byte           = __flasher_write_byte
	blocks         = __flasher_image_blocks
	last           = __flasher_image_last
	block          = __flasher_block_no
	decimal        = __flasher_decimal_value
	print_dec4     = __flasher_print_dec4
	program_byte   = __flasher_program_byte
.endscope

.endif
