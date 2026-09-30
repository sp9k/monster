;*******************************************************************************
; SYMBOLPAGES.ASM
; This file contains procedures/data for paged symbol lookup.  On the Vic-20
; symbols are stored across several Ultimem banks. On the C64, they are stored
; in the REU
;*******************************************************************************

.include "config.inc"
.include "limits.inc"
.include "memory.inc"
.include "macros.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.include "symbolpagesconst.inc"

.export __sympage_load
.export __sympage_store
.export __sympage_copy_record
.export __sympage_name_buffer
.export __sympage_scope_buffer

;*******************************************************************************
; mirrors labels.asm.
flags = zp::labels+2

SIZEOF_LABEL       = 12

;*******************************************************************************
; Memory pool for symbol records, nodes, and indices
.segment "SYMBOL_RECORD_STORAGE"
.res $10 + MAX_LABELS*SIZEOF_LABEL
.segment "SYMBOL_NODE_STORAGE"
.res 4 + MAX_LABELS*4
; Stores one object-owner byte per symbol after the hash nodes.
.assert SYM_LINK_OWNERS = 4 + MAX_LABELS*4, error, "link ownership offset"
.res MAX_LABELS
.assert SYM_LINK_OWNERS + MAX_LABELS <= $6000, error, "link ownership exceeds node pool"
.segment "SYMBOL_INDEX_STORAGE"
.ifdef c64
.res $2000		; C64 offset by $2000
.endif
.res MAX_LABELS*8

.segment "SYMBOL_NAME_STORAGE0"
.res $ffff
.byte $00
.segment "SYMBOL_NAME_STORAGE1"
.res $ffff
.byte $00

;*******************************************************************************
.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "DATA"
.endif
page_command: .byte 0
page_value:   .byte 0
page_x:       .byte 0
page_y:       .byte 0
page_extra:   .byte 0
page_mapping: .byte 0

BANKED_SEG "LABELS", FINAL_BANK_SYMBOLS

;*******************************************************************************
; SYMBOL PAGE LOAD
; Read one byte from a paged pool. Use PAGE_LOAD: the two bytes following
; the JSR encode the source zero-page pointer and the SYM_* pool number.
; Names use word offsets; all other pointers and Y offsets are in bytes.
; IN:
;   - inline byte 0: ZP pointer to the pool offset
;   - inline byte 1: SYM_RECORDS, SYM_NODES, SYM_INDEXES or SYM_NAMES
;   - .Y:            offset to load by
; OUT:
;   - .A: byte read; N/Z reflect this byte
; PRESERVES:
;   - X/Y, all flags except N/Z, the pool offset, and the caller's mapping
; CLOBBERS:
;   - zp::bankaddr0/1, private page scratch; C64 REU transfer registers
.proc __sympage_load
	php
	sei
	pha
	lda #$91
	bne symbol_page_access	; branch always
.endproc

;*******************************************************************************
; SYMBOL PAGE STORE
; Write one byte using the pointer/pool encoding as PAGE_LOAD.
; The caller's pointer is never advanced, including across physical pages.
; IN:
;   - .A:            byte to write
;   - .Y:            offset to add after decoding the pool pointer
;   - inline byte 0: ptr - ZP address to pool offset
;   - inline byte 1: pool - SYM_RECORDS, SYM_NODES, SYM_INDEXES or SYM_NAMES
; PRESERVES:
;   - A/X/Y, all flags, the pool offset, and the caller's mapping
; CLOBBERS:
;   - zp::bankaddr0/1, private page scratch; C64 REU transfer registers
.proc __sympage_store
	php
	sei
	pha
	lda #$90

	; fall through to symbol_page_access
.endproc

;*******************************************************************************
; SYMBOL PAGE ACCESS
; Shared entrypoint for LOAD or STORE.
; IN:
;   - .A:    $91 to read or $90 to write;
;   - stack: byte to write (if STORE)
; CLOBBERS:
;   - zp::bankaddr0/1, private page scratch; C64 REU transfer registers
.proc symbol_page_access
@inline = zp::bankaddr0
@data = zp::bankaddr1
	sta page_command
	stx page_x
	sty page_y
	tsx

	; recover the caller's A and the low byte of the JSR return address
	lda $0101,x
	sta page_value
	lda $0103,x
	sta @inline

	; save the original return address and advance the stacked address by two
	; so RTS skips the inline pointer/pool bytes
	clc
	adc #$02
	sta $0103,x
	lda $0104,x
	sta @inline+1
	adc #$00
	sta $0104,x

	; inline byte 0 names the zero-page pair containing the pool offset
	ldy #$01
	lda (@inline),y
	tax
	lda $00,x	; get LSB of pointer
	sta @data
	lda $01,x	; get MSB of pointer
	sta @data+1
	; inline byte 1 selects the pool
	iny
	lda (@inline),y
	tax

	lda #$00
	sta page_extra
	cpx #SYM_NAMES
	bne @offset

	; decode a word-offset name reference into a 17-bit byte address.
	asl @data
	rol @data+1
	rol page_extra

@offset:
	clc
	lda @data
	adc page_y
	sta @data
	bcc :+
	inc @data+1
	bne :+
	inc page_extra
:
.ifdef vic20
	; banks' base is an 8 KiB physical block. Index pointers start at
	; $2000, so their base is biased down by one block.
	lda $9ffc
	sta page_mapping
	lda page_extra
	asl
	asl
	asl
	sta page_extra

	lda @data+1
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc page_extra
	adc page_banks,x
	sta $9ffc

	lda @data+1
	and #$1f
	ora #$60
	sta @data+1
	ldy #$00
	lda page_command
	cmp #$91
	beq @read
	lda page_value
	sta (@data),y
	jmp @restore

@read:	lda (@data),y
	sta page_value

@restore:
	lda page_mapping
	sta $9ffc
.else
	lda $01
	sta page_mapping
	ora #$06		; expose I/O without hiding our cartridge ROM
	sta $01
	lda @data
	sta $df04
	lda @data+1
	sta $df05
	txa
	clc
	adc #SYMBOL_RECORDS_REU_BANK
	adc page_extra
	sta $df06

	lda #<page_value
	sta $df02
	lda #>page_value
	sta $df03
	lda #$01
	sta $df07
	lda #$00
	sta $df08
	sta $df0a

	lda page_command
	sta $df01
	lda page_mapping
	sta $01
.endif
	ldx page_x
	ldy page_y
	lda page_command
	cmp #$91
	beq @loaded

	pla
	plp
	rts

@loaded:
	pla
	plp
	lda page_value
	rts
.endproc

;*******************************************************************************
.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "DATA"
.endif

__sympage_name_buffer  = __mem_spare+$200
__sympage_scope_buffer = __mem_spare+$300
.assert SPARESIZE >= $400, error, "symbol scratch requires four shared pages"
.ifndef vic20
.assert MAX_OBJS*17+1 <= $200, error, "object filenames overlap symbol scratch"
.endif

page_mapping2: .byte 0
page_count:    .byte 0
page_pool:     .byte 0

;*******************************************************************************
; COPY RECORD
; Loads the 12-byte label record.
; IN:
;   - .XY: byte offset of the record in SYM_RECORDS (not its symbol ID)
; OUT:
;   - zp::labels+2 .. zp::labels+13: label data for the record
BANKED_SEG "LABELS", FINAL_BANK_SYMBOLS
.proc __sympage_copy_record
@src = zp::bankaddr1
@dst = zp::bankaddr0
	stxy @src
	lda #<flags
	sta @dst

	lda #>flags
	sta @dst+1

	lda #SIZEOF_LABEL
	sta page_count

	lda #SYM_RECORDS
	jmp copy_symbol
.endproc

;*******************************************************************************
; COPY SYMBOL
; Copy a record/name for the given symbol offset in the given pool (RECORDS or
; NAMES)
; IN:
;   - .A:            SYM_RECORDS or SYM_NAMES
;   - zp::bankaddr1: source byte offset (RECORDS), or word offset (SYM_NAMES)
;   - zp::bankaddr0: destination address to copy to (in shared RAM)
;   - page_count:    number of bytes (12 for a record, 32 for a name)
; CLOBBERS:
;   - A/X/Y, zp::bankaddr1, private page scratch
;   - REU transfer registers (C64)
.proc copy_symbol
@src = zp::bankaddr1
@dst = zp::bankaddr0
	php
	sei
	sta page_pool
	tax
	lda #$00
	sta page_extra
	cpx #SYM_NAMES
	bne :+
	asl @src
	rol @src+1
	rol page_extra

:
.ifdef vic20
	lda $9ffa
	sta page_mapping2
	lda $9ffc
	sta page_mapping

	lda page_extra
	asl
	asl
	asl
	sta page_extra

	; configure BLK2 and BLK3
	lda @src+1
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc page_extra
	adc page_banks,x
	sta $9ffa
	clc
	adc #$01
	sta $9ffc

	lda @src+1
	and #$1f
	ora #$40
	sta @src+1
	ldy #$00
:	lda (@src),y
	sta (@dst),y
	iny
	cpy page_count
	bcc :-

	lda page_mapping2
	sta $9ffa
	lda page_mapping
	sta $9ffc
.else
	lda $01
	sta page_mapping
	ora #$06
	sta $01
	lda @src
	sta $df04
	lda @src+1
	sta $df05
	lda page_pool
	clc
	adc #SYMBOL_RECORDS_REU_BANK
	adc page_extra
	sta $df06

	lda @dst
	sta $df02
	lda @dst+1
	sta $df03

	lda page_count
	sta $df07
	lda #$00
	sta $df08
	sta $df0a

	lda #$91
	sta $df01
	lda page_mapping
	sta $01
.endif
	plp
	rts

;-------------------------------------------------------------------------------
.ifdef vic20
::page_banks:
	.byte SYMBOL_RECORDS_BANK, SYMBOL_NODES_BANK
	.byte SYMBOL_INDEXES_BANK-1, SYMBOL_NAMES_BANK
.endif
.endproc

;*******************************************************************************
; LINK OWNER PTR
; Converts a symbol ID to its object-owner offset in the node pool.
; IN:
;   - .XY: symbol ID
; OUT:
;   - r0: offset of the symbol's owner byte
;   - .Y: zero
; PRESERVES:
;   - .A
BANKED_SEG "LABELS", FINAL_BANK_SYMBOLS
.proc link_owner_ptr
@owner=r0
	stxy @owner
	pha
	clc
	lda @owner
	adc #<SYM_LINK_OWNERS
	sta @owner
	lda @owner+1
	adc #>SYM_LINK_OWNERS
	sta @owner+1
	pla
	ldy #0
	rts
.endproc

;*******************************************************************************
; LINK OWNER SET
; Records the object that defines a symbol during the first link pass.
; IN:
;   - .XY: symbol ID
;   - .A: one-based object ID
; OUT:
;   - .C: clear
.export __sympage_link_owner_set
.proc __sympage_link_owner_set
@owner=r0
	jsr link_owner_ptr
	jsr __sympage_store
	.byte @owner, SYM_NODES
	clc
	rts
.endproc

;*******************************************************************************
; LINK OWNER GET
; Reads the object that defines a symbol.
; IN:
;   - .XY: symbol ID
; OUT:
;   - .A: one-based object ID
.export __sympage_link_owner_get
.proc __sympage_link_owner_get
@owner=r0
	jsr link_owner_ptr
	jsr __sympage_load
	.byte @owner, SYM_NODES
	rts
.endproc
