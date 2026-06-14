.ifndef VIC20_EXPANSION_INC
VIC20_EXPANSION_INC = 1

.if .defined(ultimem)
	.include "ultimem/banks.inc"
	.include "ultimem/ultimem.inc"
.endif

.endif
