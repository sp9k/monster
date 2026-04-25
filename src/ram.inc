.ifndef RAM_INC
RAM_INC = 1

.include "zeropage.inc"

.ifdef vic20
.include "vic20/ram.inc"
.endif

.ifdef c64
.include "c64/ram.inc"
.endif

.import __ram_call
.import __ram_copy
.import __ram_memcpy
.import __ram_copy_line
.import __ram_store_byte
.import __ram_bank_store_rel
.import __ram_load_byte
.import __ram_load_byte_off

.ifdef c64
.importzp __ram_src
.importzp __ram_dst
.else
.import __ram_src
.import __ram_dst
.endif

.scope ram
	call      = __ram_call
	copyline  = __ram_copy_line

	store     = __ram_store_byte
	store_off = __ram_bank_store_rel
	load      = __ram_load_byte
	load_off  = __ram_load_byte_off

	src       = __ram_src
	dst       = __ram_dst
	copy      = __ram_memcpy
.endscope

.endif
