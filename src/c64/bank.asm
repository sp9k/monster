.include "ram.inc"

;*******************************************************************************
; SELECT BANK SYMBOLS
; Sets the RAM bank to the symbols one
.export select_bank_symbols
.proc select_bank_symbols
	pha
	lda #FINAL_BANK_SYMBOLS
	sta reu::reuaddr+2
	pla
	rts
.endproc

;*******************************************************************************
; SELECT BANK SYMBOL NAMES
; Sets the RAM bank to the symbol names one
.export select_bank_symbol_names
.proc select_bank_symbol_names
	pha
	lda #FINAL_BANK_SYMBOL_NAMES
	sta reu::reuaddr+2
	pla
	rts
.endproc

;*******************************************************************************
; SELECT BANK SYMBOLS
; Sets the RAM bank to the debug info one
.export select_bank_debuginfo
.proc select_bank_debuginfo
	pha
	lda #FINAL_BANK_DEBUG
	sta reu::reuaddr+2
	pla
	rts
.endproc

;*******************************************************************************
; SELECT BANK LINKER
; Sets the RAM bank to the linker one
.export select_bank_linker
.proc select_bank_linker
	pha
	lda #FINAL_BANK_LINKER
	sta reu::reuaddr+2
	pla
	rts
.endproc

;*******************************************************************************
; SELECT BANK COPYBUFF
; Sets the RAM bank to the copy buffer one
.export select_bank_copybuff
.proc select_bank_copybuff
	pha
	lda #FINAL_BANK_BUFF
	sta reu::reuaddr+2
	pla
	rts
.endproc

;*******************************************************************************
; SELECT BANK MACRO
; Sets the RAM bank to the macro one
.export select_bank_macro
.proc select_bank_macro
	pha
	lda #FINAL_BANK_MACROS
	sta reu::reuaddr+2
	pla
	rts
.endproc
