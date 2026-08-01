.import __ram_mem01

.macro IO_BEGIN
	php
	sei
	pha
	lda #$36	; make I/O space visible
	sta $01
	pla
.endmacro

.macro IO_DONE
	lda __ram_mem01	; restore the current context's memory config
	sta $01
	plp
.endmacro
