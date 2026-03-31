.ifndef ULTIMEM_ULTIMEM_INC
ULTIMEM_ULTIMEM_INC = 1

.include "banks.inc"

.import __ultimem_bank
.import __ultimem_push_bank
.import __ultimem_pop_bank
.import __ultimem_select_bank
.import __ultimem_select_prog00
.import __ram_memcpy
.import __ram_copy_line
.import __ram_load_byte
.import __ram_load_byte_off
.import __ram_get_byte
.import __ram_store_byte
.import __ram_bank_store_rel
.import __ultimem_init
.import __ultimem_reset_blk5

.scope exp
	bank        = __ultimem_bank
	push_bank   = __ultimem_push_bank
	pop_bank    = __ultimem_pop_bank
	copy        = __ram_memcpy
	copyline    = __ram_copy_line
	load        = __ram_load_byte
	load_off    = __ram_load_byte_off
	get_byte    = __ram_get_byte
	store       = __ram_store_byte
	store_off   = __ram_bank_store_rel
.endscope

.scope ultim
	init            = __ultimem_init
	reset_blk5      = __ultimem_reset_blk5
	select_prog00   = __ultimem_select_prog00
.endscope

;*******************************************************************************
; SELECT BANK
.macro SELECT_BANK bank
.if .xmatch ("SYMBOLS", bank)
	lda #FINAL_BANK_SYMBOLS
	jsr __ultimem_select_bank
.elseif .xmatch("SYMBOL_NAMES", bank)
	lda #FINAL_BANK_SYMBOL_NAMES
	jsr __ultimem_select_bank
.else
	.error "invalid bank"
.endif
.endmacro

;*******************************************************************************
; SELECT BANK A
; Activates the bank in .A
.macro SELECT_BANK_A
	jsr __ultimem_select_bank
.endmacro

.endif
