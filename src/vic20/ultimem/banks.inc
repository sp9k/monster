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
FINAL_BANK_MEMCFG          = $15
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
FINAL_BANK_ERRLOG          = $12
FINAL_BANK_FLASH           = $13
FINAL_BANK_LINKER_AUX      = $14
FINAL_BANK_VIEWERS         = $15
NUM_BANKS                  = $15

;*******************************************************************************
; Physical RAM layout (decimal bank numbers, 8 KiB each):
;   0..19   shared RAM
;  20..22   assembler contexts
;  23..25   virtual screen
;  26..33   simulator/debugger/user memory
;  34..60   source pool
;  61..63   error log
;  64..94   symbol store
;      95   flash validation
;  96..127  link image
; These are physical RAM banks, independent of virtual bank IDs and ROM banks.

;*******************************************************************************
; contiguous symbol storage: physical RAM blocks 64..94 (248 KiB).
SYMBOL_META_BANK    = 64
SYMBOL_RECORDS_BANK = 65   ; seven blocks, $0010 + 4096 twelve-byte records
SYMBOL_NODES_BANK   = 72   ; three blocks, $0004 + 4096 four-byte nodes
SYMBOL_INDEXES_BANK = 75   ; four blocks, four 4096-word indexes
SYMBOL_NAMES_BANK   = 79   ; sixteen blocks, 128 KiB packed name pool
SYMBOL_END_BANK     = 95   ; exclusive
.assert SYMBOL_END_BANK <= 128, error, "symbol storage exceeds UltiMem RAM"

;*******************************************************************************
ERRORS_BANK        = 15	; BLOB0
SIMDBG_BANK        = 29	; from "VSYS"
SIMRAM_00_BANK     = 27	; from "VSYS" $0000-$0400
VMEM_IO_BANK       = 27	; from "VSYS" $0000-$0400 and $9000-$9800 ($1400 bytes)
VMEM_RAM123_BANK   = 28 ; $0400-$1000 & $1000-$2000 ($1c00 bytes)
VMEM_BLK1_BANK     = 30 ; $2000-$4000 ($2000 bytes)
VMEM_BLK2_BANK     = 31 ; $4000-$6000 ($2000 bytes)
VMEM_BLK3_BANK     = 32	; $6000-$8000 ($2000 bytes)
VMEM_BLK5_BANK     = 33	; $a000-$c000 ($2000 bytes)

FINAL_BANK_SOURCE0 = 34	; pool of source banks shared by all buffers (34-60)
SOURCE_POOL_SIZE   = 27
ERRLOG_RAM_BANK    = 61
ERRLOG_ROM_BANK    = 28
FLASH_CHECK_RAM_BANK = 95 ; private disk-validation workspace

;*******************************************************************************
; output area for linker
IMAGE_BANK       = 96
IMAGE_PAGES      = 32
IMAGE_CAPACITY   = IMAGE_PAGES * $2000
.assert IMAGE_BANK + IMAGE_PAGES <= 128,   error, "image exceeds UltiMem RAM"

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
