.ifndef VIC20_ULTIMEM_BANKS_INC
VIC20_ULTIMEM_BANKS_INC = 1

.import src_data

;*******************************************************************************
;VIRTUAL BANK IDs
FINAL_BANK_MAIN          = $01
FINAL_BANK_DEBUG         = $02
FINAL_BANK_UDGEDIT       = $03
FINAL_BANK_EXPR          = $04
FINAL_BANK_HELP          = $04
FINAL_BANK_MACROS        = $05
FINAL_BANK_LINKER        = $06
FINAL_BANK_BUFF          = $07
FINAL_BANK_COPYBUFF      = $07
FINAL_BANK_CONSOLE       = $08
FINAL_BANK_MONITOR       = $08
FINAL_BANK_SYMBOLS       = $09
FINAL_BANK_SYMBOL_NAMES  = $0a
FINAL_BANK_CTX           = $0b
FINAL_BANK_VSCREEN       = $0c
FINAL_BANK_VSYS          = $0d
FINAL_BANK_SIM           = $0d
FINAL_BANK_FASTCOPY      = $0d
FINAL_BANK_FTXT          = $0e
FINAL_BANK_FASTTEXT      = $0e
FINAL_BANK_USER          = $0f
NUM_BANKS                = $0f

ERRORS_BANK        = 15	; BLOB0
SIMDBG_BANK        = 35	; from "VSYS"
SIMRAM_00_BANK     = 33	; from "VSYS" $0000-$0400
VMEM_IO_BANK       = 33	; from "VSYS" $0000-$0400 and $9000-$9800 ($1400 bytes)
VMEM_RAM123_BANK   = 34 ; $0400-$1000 & $1000-$2000 ($1c00 bytes)
VMEM_BLK1_BANK     = 36 ; $2000-$4000 ($2000 bytes)
VMEM_BLK2_BANK     = 37 ; $4000-$6000 ($2000 bytes)
VMEM_BLK3_BANK     = 38	; $6000-$8000 ($2000 bytes)
VMEM_BLK5_BANK     = 39	; $a000-$c000 ($2000 bytes)

FINAL_BANK_SOURCE0 = 40
.endif
