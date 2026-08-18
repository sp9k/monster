;*******************************************************************************
; ULTIMEM.ASM
; This file contains utilities for selecting RAM/ROM configurations in the
; Ultimem
; At a high level, RAM123 and IO 2/3 are generally used as shared RAM across
; all banks.
; BLK's 1, 2, 3, 5 are generally configured to be ROM containing code for the
; bank.
;*******************************************************************************

.include "banks.inc"
.include "../../zeropage.inc"

.segment "ULTIREGS"
;*******************************************************************************
; BANK
; This virtual register contains the current "bank", which is used to
; select the appropriate configuration of Ultimem registers by looking them
; up in the tables (see ultim::select_bank)
.export __ultimem_bank
__ultimem_bank: .byte 0

.segment "ULTICFG"

;*******************************************************************************
; SELECT PROG 00
; Maps the prog00 virtual RAM area to BLK5, leaving all other regions as is
.export __ultimem_select_prog00
.proc __ultimem_select_prog00
	; bank in the area containing prog00
	lda #SIMRAM_00_BANK
	sta $9ffe
	lda #$d5
	sta $9ff2
	rts
.endproc

;*******************************************************************************
; RESET BLK5
; Restores BLK5 to the default ROM bank. All other banks are unaffected
.export __ultimem_reset_blk5
.proc __ultimem_reset_blk5
	; reset BLK5 RAM area
	lda #$04
	sta $9ffe
	lda #$55
	sta $9ff2
	rts
.endproc

;*******************************************************************************
; SELECT BANK
; Selects the given logical bank, configuring BLK 1,2,3, and 5 with the
; preset RAM/ROM configuration for that "bank".
; Takes 69 cycles on stock Ultimem or 19 with Ultimme + profile extension
; IN:
;   - .A: the bank to activate
.export __ultimem_select_bank
.proc __ultimem_select_bank
	cmp #NUM_BANKS+1	; 2 (2)
	bcc :+			; 3 (5)

	; if id is above last virutal bank index, treat as raw bank value
	; this is used for source buffers
	sta $9ff8
	;sec
	adc #$00
	sta $9ffa
	adc #$01
	sta $9ffc
	lda #$7f		; RAM in BLK1,2,3
	sta $9ff2
	rts

:	sta __ultimem_bank	; 4 (9)
.ifdef ultimem_p
	sta $9ff3		; 4 (13) UltiMem-P: load this bank's profile
	rts			; 6 (19)
.else
	stx @savex		; 4 (13)
	sta @savea		; 4 (17)
	tax			; 2 (19)

	lda cfg-1,x		; 4 (23)
	sta $9ff2		; 4 (27)
	lda blk1-1,x		; 4 (31)
	sta $9ff8		; 4 (35)
	lda blk2-1,x		; 4 (39)
	sta $9ffa		; 4 (43)
	lda blk3-1,x		; 4 (47)
	sta $9ffc		; 4 (51)
	lda blk5-1,x		; 4 (55)
	sta $9ffe		; 4 (59)

@savex=*+1
	ldx #$00		; 2 (61)
@savea=*+1
	lda #$00		; 2 (63)
	rts			; 6 (69)
.endif
.endproc

.ifdef ultimem_p
;*******************************************************************************
; INIT PROFILES
; Populates the UltiMem-P profile SRAM with one register profile per virtual
; bank plus one profile per source buffer
; Each profile contains a configuration for the complete range $9ff1,...,$9fff.
; NOTE: all profiles keep RAM123 and IO2/3 as shared RAM (bank 0).
.export __ultimem_init_profiles
.proc __ultimem_init_profiles
	; enable SAVEMODE - writes to $9ff3 now save the live registers into
	; the addressed profile instead of loading from it
	lda $9ff0
	ora #$20		; set bit5 (savemode)
	and #$bf		; keep bit6 (reset) clear
	sta $9ff0

	; trigger the profile command with the $55,$aa sequence
	lda #$55
	sta $9ff3
	lda #$aa
	sta $9ff3

	; write register values shared by every profile
	lda #$3f
	sta $9ff1		; RAM123, IO2, IO3 all RAM r/w
	lda #$00
	sta $9ff4		; RAM123 bank = 0
	sta $9ff5
	sta $9ff6		; IO bank = 0
	sta $9ff7
	sta $9ff9		; BLK1/2/3/5 bank MSB's
	sta $9ffb
	sta $9ffd
	sta $9fff

	ldx #$01		; profile index
@l0:	lda cfg-1,x
	sta $9ff2		; BLK 1/2/3/5 RAM/ROM config
	lda blk1-1,x
	sta $9ff8		; BLK1 bank
	lda blk2-1,x
	sta $9ffa		; BLK2 bank
	lda blk3-1,x
	sta $9ffc		; BLK3 bank
	lda blk5-1,x
	sta $9ffe		; BLK5 bank
	txa
	sta $9ff3		; save profile
	inx			; move to next profile
	cpx #NUM_BANKS+1
	bne @l0			; repeat for all profiles

	; generate source-buffer profiles (consecutive area in BLKs 1/2/3
	lda #$7f
	sta $9ff2		; BLK1/2/3 RAM r/w, BLK5 ROM
	lda #$04
	sta $9ffe		; BLK5 = MAIN ROM bank

	ldx #FINAL_BANK_SOURCE0				; .X=BLK1 for src bank 0
	ldy #(FINAL_BANK_LOG-FINAL_BANK_SOURCE0)/3+1	; .Y=# of source buffers

@l1:	stx $9ff8		; BLK1 = buffer base bank
	inx
	stx $9ffa		; BLK2 = base+1
	inx
	stx $9ffc		; BLK3 = base+2
	txa
	sec
	sbc #$02		; .A = base bank
	sta $9ff3		; snapshot live registers -> profile <base>
	inx			; advance to next buffer base
	dey
	bne @l1

	; turn SAVEMODE off: writes to $9ff3 now load profiles again
	lda $9ff0
	and #$df		; clear bit5 (savemode)
	sta $9ff0

	; restore the boot ROM mapping so that the init procedure can continue
	lda #$00
	sta $9ffe		; set BLK5 to boot ROM (bank 0)
	lda #$55
	sta $9ff2		; BLK 1/2/3/5 config (all ROM)
	rts
.endproc
.endif	; ultimem_p

;*******************************************************************************
; VIRTUAL BANK CONFIG MAP
; generated by genmapper.py
blk1: .byte $01, $02, $05, $05, $08, $0b, $0e, $11, $14, $17, $1a, $1d, $21, $10, $24, $14, $18, $21, $21
blk2: .byte $02, $03, $06, $06, $09, $0c, $0f, $12, $15, $18, $1b, $1e, $22, $11, $25, $15, $19, $22, $22
blk3: .byte $03, $04, $07, $07, $0a, $0d, $10, $13, $16, $19, $1c, $1f, $23, $12, $26, $16, $1a, $23, $23
blk5: .byte $04, $05, $06, $06, $07, $08, $09, $0a, $0b, $0b, $0c, $0d, $0e, $13, $27, $17, $1b, $1c, $1d
cfg:  .byte $55, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $55, $ff, $55, $55, $7f, $7f
