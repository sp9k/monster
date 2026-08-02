.ifndef C64_REU_INC
C64_REU_INC = 1

.import __reu_copy_y
.import __reu_move
.import __reu_compare
.import __reu_c64_addr
.import __reu_find
.import __reu_init
.import __reu_load
.import __reu_load_blk
.import __reu_load_delayed
.import __reu_load1
.import __reu_loadb
.import __reu_loadb_off
.import __reu_loadw
.import __reu_move
.import __reu_reu_addr
.import __reu_restoreprog00
.import __reu_saveprog00
.import __reu_swap
.import __reu_store
.import __reu_store_blk
.import __reu_store_delayed
.import __reu_store1
.import	__reu_storeb
.import __reu_storeb_off
.import	__reu_storew
.import __reu_txlen
.import __reu_zero

.import __reu_move_src
.import __reu_move_dst
.import __reu_move_size

; address of virtual memory state
REU_VMEM_ADDR           = $fe0000
REU_VMEM_IO             = $fd0000
REU_VMEM_ROM            = $fc0000

; backup of Monster's state while debugging
REU_BACKUP_ADDR         = $fb0000
REU_BACKUP_IO           = $fa0000

; address in the REU to save/restore C64 data to
REU_TMP_ADDR            = $ff0000

.scope reu
	c64addr        = __reu_c64_addr
	copy_y         = __reu_copy_y
	compare        = __reu_compare
	find           = __reu_find
	load           = __reu_load
	load_blk       = __reu_load_blk
	load_delayed   = __reu_load_delayed
	loadb          = __reu_loadb
	load1          = __reu_load1
	loadb_off      = __reu_loadb_off
	loadw          = __reu_loadw
	init           = __reu_init
	move           = __reu_move
	reuaddr        = __reu_reu_addr
	save_prog00    = __reu_saveprog00
	restore_prog00 = __reu_restoreprog00
	store          = __reu_store
	store_blk      = __reu_store_blk
	store_delayed  = __reu_store_delayed
	store1         = __reu_store1
	storeb         = __reu_storeb
	storeb_off     = __reu_storeb_off
	storew         = __reu_storew
	swap           = __reu_swap
	txlen          = __reu_txlen
	move_src       = __reu_move_src
	move_dst       = __reu_move_dst
	move_size      = __reu_move_size
	zero           = __reu_zero
.endscope

;*******************************************************************************
; STORE16
; Copies the data at the given source address to the destination
; one in the REU.
; The most-significant byte of the REU address is given as the "bank"
.macro STORE16 bank, dst16, src16, size16
	.if .match( .left( 1, {bank} ), # )
		lda #^(.right( .tcount( {bank} )-1, {bank} ))
		sta __reu_reu_addr+2
	.else
		lda bank
		sta __reu_reu_addr+2
	.endif

	.if .match( .left( 1, {dst16} ), # )
		lda #<(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_reu_addr
		lda #>(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_reu_addr+1
	.else
		lda dst16
		sta __reu_reu_addr
		lda dst16+1
		sta __reu_reu_addr+1
	.endif

	.if .match( .left( 1, {src16} ), # )
		lda #<(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_c64_addr
		lda #>(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_c64_addr+1
	.else
		lda src16
		sta __reu_c64_addr
		lda src16+1
		sta __reu_c64_addr+1
	.endif

	.if .match( .left( 1, {txlen} ), # )
		lda #<(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen
		lda #>(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen+1
	.else
		lda size16
		sta __reu_txlen
		lda size16+1
		sta __reu_txlen+1
	.endif
.endmacro

;*******************************************************************************
; STORE
; Copies the data at the given source address to the destination one.
.macro STORE dst24, src16, size16
	.if .match( .left( 1, {dst24} ), # )
		COPY16 #^(.right( .tcount( {dst24} )-1, {dst24} )), #(.right( .tcount( {dst24} )-1, {dst24} ) & $ffff), src16, size16
	.else
		COPY16 ^dst24, dst24 & $ffff, src16, size16
	.endif
.endmacro

;*******************************************************************************
; LOAD16
; Copies the data from the source REU address to the C64 destination one
; The most-significant byte of the REU address is given as the "bank"
.macro LOAD16 bank, dst16, src16, size16
	.if .match( .left( 1, {bank} ), # )
		lda #^(.right( .tcount( {bank} )-1, {bank} ))
		sta __reu_reu_addr+2
	.else
		lda bank
		sta __reu_reu_addr+2
	.endif

	.if .match( .left( 1, {dst16} ), # )
		lda #<(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_c64_addr
		lda #>(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_c64_addr+1
	.else
		lda dst16
		sta __reu_c64_addr
		lda dst16+1
		sta __reu_c64_addr+1
	.endif

	.if .match( .left( 1, {src16} ), # )
		lda #<(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_reu_addr
		lda #>(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_reu_addr+1
	.else
		lda src16
		sta __reu_reu_addr
		lda src16+1
		sta __reu_reu_addr+1
	.endif

	.if .match( .left( 1, {txlen} ), # )
		lda #<(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen
		lda #>(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen+1
	.else
		lda size16
		sta __reu_txlen
		lda size16+1
		sta __reu_txlen+1
	.endif
.endmacro
.endif
