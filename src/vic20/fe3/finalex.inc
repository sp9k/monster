.ifndef VIC20_FINALEX_INC
VIC20_FINALEX_INC=1

.import __ram_load_byte
.import __ram_load_byte_off
.import __ram_memcpy
.import __ram_copy_line
.import __ram_bank_store_rel
.import __ram_memcpy_bank
.import __ram_get_byte
.import __ram_store_byte

.import __fe3_bank
.import __fe3_boot
.import __fe3_init0
.import __fe3_init1
.import __fe3_push_bank
.import __fe3_pop_bank

;*******************************************************************************
.scope fe3
	init0 = __fe3_init0
	init1 = __fe3_init1
.endscope

.scope exp
	bank      = __fe3_bank
	push_bank = __fe3_push_bank
	pop_bank  = __fe3_pop_bank
.endscope

;*******************************************************************************
; SELECT BANK
; Activates the bank from the given string
.macro SELECT_BANK str
.endmacro

;*******************************************************************************
; SELECT BANK A
; Activates the bank in .A
.macro SELECT_BANK_A
	sta $9c02
.endmacro

.endif
