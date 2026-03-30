.ifndef C64_C64_INC
C64_C64_INC = 1

.import select_bank_linker
.import select_bank_symbols
.import select_bank_symbol_names
.import select_bank_debuginfo
.import select_bank_debuginfo
.import select_bank_copybuff
.import select_bank_macro

.import __screen_crowslo
.import __screen_crowshi
.import __screen_rowslo
.import __screen_rowshi

.include "bsp.inc"
.include "debug.inc"

.scope c64
	rowslo = __screen_rowslo
	rowshi = __screen_rowshi
	crowslo = __screen_crowslo
	crowshi = __screen_crowshi
.endscope

.macro OSPROC fn
.proc fn
.endmacro

.macro ENDOSPROC fn
.endproc
.endmacro

.macro SELECT_BANK bank
.if .xmatch ("SYMBOLS", bank)
	jsr select_bank_symbols
.elseif .xmatch ("SYMBOL_NAMES", bank)
	jsr select_bank_symbol_names
.elseif .xmatch("DEBUGINFO", bank)
	jsr select_bank_debuginfo
.elseif .xmatch("LINKER", bank)
	jsr select_bank_linker
.elseif .xmatch("COPYBUFF", bank)
	jsr select_bank_copybuff
.elseif .xmatch("MACRO", bank)
	jsr select_bank_macro
.else
	.error "invalid bank"
.endif
.endmacro

.endif
