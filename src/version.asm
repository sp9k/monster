;*******************************************************************************
; VERSION
; Emits the UltiMem firmware header at the cartridge's reserved header location.
; Included by vic20/boot.asm; VERSION contains the five ASCII release digits.
; The packager checks the bank count against the linked image and fills the sum.
.include "firmware.inc"

.assert * = $a000+FIRMWARE_OFFSET, error, "firmware header moved"
.byte $4d, $55, $50, $31	; "MUP1"
.incbin "VERSION", 0, 5
.word $0020		; 32 populated 8 KiB banks
.word $0000		; checksum filled after linking
.assert * = $a000+FIRMWARE_OFFSET+FIRMWARE_SIZE, error, "firmware header size"
