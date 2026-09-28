.ifndef FIRMWARE_INC
FIRMWARE_INC = 1

;*******************************************************************************
FIRMWARE_OFFSET = 9			; offset in image of version header
FIRMWARE_SIZE = 13			; size of version header
FIRMWARE_VERSION = FIRMWARE_OFFSET+4	; offset of version # (ASCII, 5 bytes)
FIRMWARE_BLOCKS = FIRMWARE_OFFSET+9	; offset of # of blocks used in image
FIRMWARE_CHECKSUM = FIRMWARE_OFFSET+11	; offset of version header checksum

.endif
