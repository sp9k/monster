.ifndef VIC20_ULTIMEM_BANKS_INC
VIC20_ULTIMEM_BANKS_INC = 1

.import src_data

;*******************************************************************************
;VIRTUAL BANK IDs
FINAL_BANK_MAIN            = $01
FINAL_BANK_ASM             = FINAL_BANK_MAIN
FINAL_BANK_EDIT            = FINAL_BANK_MAIN
FINAL_BANK_DEBUG           = $02
FINAL_BANK_UDGEDIT         = $03
FINAL_BANK_EXPR            = $03
FINAL_BANK_FP              = FINAL_BANK_EXPR

FINAL_BANK_HELP            = $04
FINAL_BANK_MEMCFG          = $04
FINAL_BANK_MACROS          = $05
FINAL_BANK_LINKER          = $06
FINAL_BANK_BUFF            = $07
FINAL_BANK_COPYBUFF        = $07
FINAL_BANK_CONSOLE         = $08
FINAL_BANK_MONITOR         = $08
FINAL_BANK_SYMBOLS         = $09
FINAL_BANK_SYMBOL_NAMES    = $0a
FINAL_BANK_CTX             = $0b
FINAL_BANK_VSCREEN         = $0c
FINAL_BANK_VSYS            = $0d
FINAL_BANK_SIM             = $0d
FINAL_BANK_FASTCOPY        = $0d
FINAL_BANK_FTXT            = $0e
FINAL_BANK_FASTTEXT        = $0e
FINAL_BANK_USER            = $0f
FINAL_BANK_FASTSCROLL_UP   = $10
FINAL_BANK_FASTSCROLL_DOWN = $11
NUM_BANKS                  = $11

ERRORS_BANK        = 15	; BLOB0
SIMDBG_BANK        = 35	; from "VSYS"
SIMRAM_00_BANK     = 33	; from "VSYS" $0000-$0400
VMEM_IO_BANK       = 33	; from "VSYS" $0000-$0400 and $9000-$9800 ($1400 bytes)
VMEM_RAM123_BANK   = 34 ; $0400-$1000 & $1000-$2000 ($1c00 bytes)
VMEM_BLK1_BANK     = 36 ; $2000-$4000 ($2000 bytes)
VMEM_BLK2_BANK     = 37 ; $4000-$6000 ($2000 bytes)
VMEM_BLK3_BANK     = 38	; $6000-$8000 ($2000 bytes)
VMEM_BLK5_BANK     = 39	; $a000-$c000 ($2000 bytes)

FINAL_BANK_SOURCE0 = 40	; 8 source buffers * 3 blocks each (40-63)
FINAL_BANK_LOG     = 64

;*******************************************************************************
; MEMORY CONFIGURATION BLOCKS
; Bits (per region) of the VIC's address space that the UltiMem can map/unmap
; for the user's program.  See memcfg.asm.
MEMCFG_RAM123 = $01	; $0400-$0fff (the "3K" expansion)
MEMCFG_BLK1   = $02	; $2000-$3fff
MEMCFG_BLK2   = $04	; $4000-$5fff
MEMCFG_BLK3   = $08	; $6000-$7fff
MEMCFG_BLK5   = $10	; $a000-$bfff

MEMCFG_ALL = MEMCFG_RAM123|MEMCFG_BLK1|MEMCFG_BLK2|MEMCFG_BLK3|MEMCFG_BLK5
MEMCFG_NUM_BLOCKS = 5
.endif
