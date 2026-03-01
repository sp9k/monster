.ifndef C64_RAM_INC
C64_RAM_INC = 1

.include "macros.inc"
.include "reu.inc"

.import __ram_call

;******************************************************************************
; BANKS
; These are the data banks for various logical components
FINAL_BANK_EXPR    = $00
FINAL_BANK_ROM0	   = $00
FINAL_BANK_MAIN    = $00

FINAL_BANK_USER    = $02
FINAL_BANK_DEBUG   = $02
FINAL_BANK_SOURCE0 = $03
FINAL_BANK_SOURCE1 = $04
FINAL_BANK_SOURCE2 = $05
FINAL_BANK_SOURCE3 = $06
FINAL_BANK_SOURCE4 = $07
FINAL_BANK_SOURCE5 = $08
FINAL_BANK_SOURCE6 = $09
FINAL_BANK_SOURCE7 = $0a
FINAL_BANK_MACROS  = $0b
FINAL_BANK_UDGEDIT = $0c
FINAL_BANK_HELP    = $0c
FINAL_BANK_LINKER  = $0d
FINAL_BANK_CONSOLE = $0e
FINAL_BANK_BUFF    = $0f
FINAL_BANK_SYMBOLS = $10
FINAL_BANK_SYMVIEW = $11
FINAL_BANK_MONITOR = $12
FINAL_BANK_CTX     = $13

.macro CALLMAIN proc
	jsr proc
.endmacro

.macro CALL bank, proc
	jsr __ram_call
	.byte bank
	.word proc
.endmacro

.macro JUMP bank, proc
	jsr __ram_call
	.byte bank
	.word proc
	rts
.endmacro

.macro JUMPMAIN proc
	jmp proc
.endmacro

;******************************************************************************
; LDA24
; Loads the value from the given 24 bit address and returns it in .A
.macro lda24 bank, addr
	; set REU (load) address
	IO_BEGIN
	lda bank
	sta $df06	; MSB of REU address
	lda addr+1
	sta $df05	; middle byte of REU address
	lda addr
	sta $df04	; LSB of REU address

	ldx #$00
	stx $df0a

	; set c64 (target) address (load into banktmp)
	lda #zp::banktmp
	sta $df02
	stx $df03

	; set transfer size to 1 byte
	stx $df08
	inx
	stx $df07

	; execute the REU command
	lda #$91	; load 1 byte from REU -> C64
	sta $df01
	IO_DONE

	; read the loaded data
	lda zp::banktmp
.endmacro

;******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
; CLOBBERS:
;  zp::banktmp
.macro sta24 bank, addr
	sta zp::banktmp
	IO_BEGIN

	ldx #$00

	; set REU (load) address
	lda addr
	sta $df04	; LSB of REU address
	lda addr+1
	sta $df05	; middle byte of REU address
	lda bank
	sta $df06	; MSB of REU address

	; set c64 (target) address
	lda #zp::banktmp
	sta $df02
	stx $df03

	; set transfer size to 1 byte
	stx $df08
	inx
	stx $df07

	; execute the REU command
	lda #$90	; store 1 byte from C64 -> REU
	sta $df01
	IO_DONE

	lda zp::banktmp	; restore byte that we stored
.endmacro

;******************************************************************************
; MOV
; Moves the value from the given 24-bit source to the given 24-bit destination
.macro mov dst24, src24
	.if (.match (.left (1, {src24}), #))
		; immediate mode, just load .A with the immediate value
		lda src24
	.else
		lda24 src
	.endif
	sta24 dst24
.endmacro

;*******************************************************************************
; LOADB Y
; Load a byte from memory with Y offset
; IN:
;  - addr: the zeropage address to load from
;  - .Y:   offset to apply to load address
; OUT:
;  - .A: the value that was read
;  - .Z: set if the value read was 0
;  - .N: set if the value read is negative
.macro LOADB_Y addr
	jsr reu::loadb_off
	.byte addr
.endmacro

;*******************************************************************************
; LOADB
; Load a byte from the given address
; IN:
;  - addr: the zeropage address to load from
; OUT:
;  - .A: the value that was read
;  - .Z: set if the value read was 0
;  - .N: set if the value read is negative
.macro LOADB addr
	jsr reu::loadb
	.byte addr
.endmacro

;*******************************************************************************
; LOADW
; Read a word from the given address
; IN:
;   - addr: the zeropage address to load from
; OUT:
;   - .XY: the value that was read
.macro LOADW addr
	jsr reu::loadw
	.byte addr
.endmacro

;*******************************************************************************
; STOREB Y
; Store a byte to memory with Y offset
; IN:
;   - .A: byte to write
;   - .Y: offset
;   - addr: zeropage address to write to
.macro STOREB_Y addr
	jsr reu::storeb_off
	.byte addr
.endmacro

;*******************************************************************************
; STOREB
; Stores the value in .A to the target address
; IN:
;   - .A:   value to store
;   - addr: target address
.macro STOREB addr
	jsr reu::storeb
	.byte addr
.endmacro

;*******************************************************************************
; STOREW
; Write a word to memory
; IN:
;   - .XY:  value to write
;   - addr: address to store to
.macro STOREW addr
	jsr reu::storew
	.word addr
.endmacro

;*******************************************************************************
; SWAPB_Y
; Swaps 2 bytes at the given addresses offset by the value of .Y
; IN:
;   - addr1: one address to swap
;   - addr2: the other address to swap
.macro SWAPB_Y addr1, addr2
	LOADB_Y addr1
	pha
	LOADB_Y addr2
	STOREB_Y addr1
	pla
	STOREB_Y addr2
.endmacro

;*******************************************************************************
; SWAPW_Y
; Swaps 2 words at the given addresses offset by the value of .Y
; IN:
;   - addr1: one address to swap
;   - addr2: the other address to swap
.macro SWAPW_Y addr1, addr2
	SWAPB_Y
	iny
	SWAPB_Y
.endmacro

;*******************************************************************************
; COPY_Y
; Copy the number of bytes in .Y from src to dst
; IN:
;   - src:  source address
;   - dst:  destination address
;   - .Y:   the number of bytes to copy
.macro COPY_Y src, dst
.local size
	jsr reu::copy_y
	.byte src
	.byte dst
.endmacro

;*******************************************************************************
; COPYxx
; Copy a chunk of memory (count in Y register, decrementing)
; IN:
;   - src:  source address
;   - dst:  destination address
;   - size: size (0-255)
.macro COPYxx src, dst, size
	ldy #size
	COPY_Y
.endmacro

;*******************************************************************************
; FILL_Y
; Fill memory with a single byte value
; IN:
;   - addr: start address to fill
;   - .A:   byte value
;   - .Y:   number of bytes to fill
.macro FILL_Y addr, size, val
	jsr reu::fill
	.word addr
	.byte size
	.byte val
.endmacro

.endif
