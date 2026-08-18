.ifndef VIC20_ULTIMEM_BANKS_INC
VIC20_ULTIMEM_BANKS_INC = 1

.import src_data

;*******************************************************************************
;VIRTUAL BANK IDs
FINAL_BANK_MAIN            = $01
FINAL_BANK_DEBUG           = $02
FINAL_BANK_UDGEDIT         = $03
FINAL_BANK_EXPR            = $04
FINAL_BANK_HELP            = $04
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
FINAL_BANK_SIM_CAP         = $12	; frame-capture code generator
FINAL_BANK_SIM_VIS         = $13	; frame-capture visibility analysis
NUM_BANKS                  = $13

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
FINAL_BANK_LOG     = 64	; log buffer, 3 blocks (64-66)

; FRAME CAPTURE BUFFERS
CAPTURE_BANK       = 67	; chain[0]
CAPTURE_BANK2      = 68	; chain[1]
CAPTURE_BANK3      = 69	; chain[2]
CAPTURE_BANK4      = 70	; chain[3]
CAPTURE_BANK5      = 71	; chain[4]
CAPTURE_BANK6      = 72	; chain[5]

; GENERATED REPLAY BUFFER
OUTPUT_BANK        = 73	; set A, BLK1 ($2000-$3FFF)
OUTPUT_BANK2       = 74	; set A, BLK2 ($4000-$5FFF)
OUTPUT_BANK3       = 75	; set A, BLK3 ($6000-$7FFF)
OUTPUT_B_BANK      = 76	; set B, BLK1 ($2000-$3FFF)
OUTPUT_B_BANK2     = 77	; set B, BLK2 ($4000-$5FFF)
OUTPUT_B_BANK3     = 78	; set B, BLK3 ($6000-$7FFF)

; FRAME CAPTURE BUFFERS (continued)
; 7-byte records hold 1170 per bank; the interlaced worst case is one store
; every 4 cycles over 34125 cycles = 8531 records, so 8 banks (9360) keeps
; the chain structurally impossible to overflow.
CAPTURE_BANK7      = 79	; chain[6]
CAPTURE_BANK8      = 80	; chain[7]

; REPLAY ZEROPAGE STASH
; Holds Monster's page 0 while the replay runs with the identity zeropage
; installed (see zp_install).  dbg00 cannot serve here: it lives at $b410, in
; the same BLK5 window as the capture generator's own code, so mapping it in
; would swap the caller out from under itself.
ZPSAVE_BANK        = 82

; PIXEL-0 STATE (generation-time scratch; never mapped during replay)
;   $4000-$540F  shadow: pixel-0 value per address in the captured ranges
;   $5410-$5691  dirty bitmap: address was written during the frame
;   $5692-$5913  rmw bitmap:   that first write is a read-modify-write, so the
;                              address must be restored before the budget runs
;                              out -- the re-emitted RMW reads it back
;   $5914-$5B95  wseen bitmap: exact-window scratch; address already written
;                              inside the strip, which is what makes the rmw
;                              bitmap window-scoped there rather than frame-wide
;   $5B96-$5E17  fetch bitmap: the VIC reads this address somewhere in the frame.
;                              A clear bit means no write to it is observable, so
;                              it is neither replayed nor restored (see fetch_walk)
SHADOW_BANK        = 81

; CONFIRMATION (generation-time scratch)
;   $6000-$6491  one bit per capture record: a read of its address confirmed it
;   $6492-$6C91  two bytes per colour index: its unconfirmed write
;   $6C92-$6F13  rewind bitmap: addresses the pass leaves off their pixel-0 value
; Mapped into BLK3 for the whole of generation.  Nothing else holds BLK3 across
; the generation pass -- gen_emit borrows all three windows for the output banks
; and puts them back, so the table survives underneath it.
ANALYSIS_BANK      = 83

; UNCONFIRMED WRITES, screen half (generation-time scratch)
;   $2000-$3FFF  two bytes per screen index: the ordinal of the write nothing has
;                read yet, or 0.  Mapped into BLK1 only while the analysis runs;
;                cap_read_record borrows that window a record at a time and puts
;                it back, so the table survives underneath it.  Dead by the time
;                the scheduling loop wants BLK1 for the OUTPUT bank.
PENDING_BANK       = 84

.endif
