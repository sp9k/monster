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
; Takes 69+6=75 cycles (counting the JSR to call this routine)
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
.endproc

;*******************************************************************************
;VIRTUAL BANK CONFIG MAP
blk1: .byte $01, $02, $05, $05, $08, $0b, $0e, $11, $14, $17, $1a, $1d, $21, $10, $24
blk2: .byte $02, $03, $06, $06, $09, $0c, $0f, $12, $15, $18, $1b, $1e, $22, $11, $25
blk3: .byte $03, $04, $07, $07, $0a, $0d, $10, $13, $16, $19, $1c, $1f, $23, $12, $26
blk5: .byte $04, $05, $06, $06, $07, $08, $09, $0a, $0b, $0b, $0c, $0d, $0e, $13, $27
cfg:  .byte $55, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f, $55, $ff

