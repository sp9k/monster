.ifndef C64_RAM_INC
C64_RAM_INC = 1

.include "cartbanks.inc"
.include "macros.inc"
.include "reu.inc"

.import __ram_call
.import __ram_init

;*******************************************************************************
; BANKS
; These are the data banks for various logical components
FINAL_BANK_ROM0	        = $00
FINAL_BANK_MAIN         = $00

FINAL_BANK_USER         = $02
FINAL_BANK_DEBUG        = $02
FINAL_BANK_SOURCE0      = $03
FINAL_BANK_SOURCE1      = $04
FINAL_BANK_SOURCE2      = $05
FINAL_BANK_SOURCE3      = $06
FINAL_BANK_SOURCE4      = $07
FINAL_BANK_SOURCE5      = $08
FINAL_BANK_SOURCE6      = $09
FINAL_BANK_SOURCE7      = $0a
FINAL_BANK_MACROS       = $0b
FINAL_BANK_UDGEDIT      = $0c
FINAL_BANK_HELP         = $0c
FINAL_BANK_LINKER       = $0d
FINAL_BANK_CONSOLE      = $0e
FINAL_BANK_BUFF         = $0f
FINAL_BANK_SYMBOLS      = $10
FINAL_BANK_SYMBOL_NAMES = $11
FINAL_BANK_SYMVIEW      = $12
FINAL_BANK_MONITOR      = $13
FINAL_BANK_CTX          = $14
FINAL_BANK_LOG          = $15
FINAL_BANK_DBGUI        = $16
FINAL_BANK_FILEDIR      = $17
FINAL_BANK_ASM          = $18
FINAL_BANK_EXPR         = $18
FINAL_BANK_EDIT         = $19

;*******************************************************************************
; CUR_BANK
; The logical bank (the top 8 bits of the 24-bit address space) that the code
; currently being assembled runs from.  CALL/JUMP compare a target's bank
; against this to decide whether a call is "near" (same bank -> emit a bare
; jsr/jmp with no bank switching at all) or "far" (route through the __ram_call
; trampoline, which switches the REU data bank and, on the cart build, the
; cartridge ROM bank around the call and restores them afterwards).
;
; CUR_BANK is a redefinable (.set) symbol so that a module can declare its bank
; once near the top of the file, and multi-bank modules can re-declare it before
; each .segment they switch to.
;
; Defaults, chosen so that a module that has NOT opted in behaves exactly as it
; did before this mechanism existed:
;  - cart build: BANK_NONE, so every call stays far (the previous behavior was
;    to always trampoline).  A module opts into near calls with its own
;    `CUR_BANK .set FINAL_BANK_x`.
;  - disk build: MAIN, since all code is resident there and only the REU data
;    bank is ever switched.  This keeps CALLMAIN (and any same-bank call) a bare
;    jsr, exactly as the old `.else jsr proc` path did.
BANK_NONE = $ff
.ifdef CART
CUR_BANK .set BANK_NONE
.else
CUR_BANK .set FINAL_BANK_MAIN
.endif

;*******************************************************************************
; SET_CUR_BANK
; Declares the bank the following code runs from, so that same-bank CALL/JUMP
; targets collapse to a bare jsr/jmp.  A no-op except on the c64 cart build: on
; the disk build all code is resident (CUR_BANK stays MAIN) and on the vic20 the
; c64 bank ids do not apply.  Put it right after the .segment/BANKED_CODE that
; begins a banked body, and reset with `SET_CUR_BANK BANK_NONE` before any
; resident (.CODE) block that follows.
.macro SET_CUR_BANK bank
.if .defined(CART)
CUR_BANK .set bank
.endif
.endmacro

;*******************************************************************************
; CALL
; Calls `proc`, whose code lives in logical bank `bank` (the top 8 bits of its
; 24-bit address).  Near targets (bank == CUR_BANK) become a plain jsr; far
; targets go through the trampoline.
.macro CALL bank, proc
; the :: forces global-scope lookup: inside a .proc an unqualified reference to
; a file-scope constant is not resolved as a constant, which .if requires
.if ::bank = ::CUR_BANK
	jsr proc
.else
	jsr __ram_call
	.byte bank
	.word proc
.endif
.endmacro

;*******************************************************************************
; JUMP
; Tail-call form of CALL (control returns to CALL's caller, not here).
.macro JUMP bank, proc
.if ::bank = ::CUR_BANK
	jmp proc
.else
	jsr __ram_call
	.byte bank
	.word proc
	rts
.endif
.endmacro

;*******************************************************************************
; CALLMAIN / JUMPMAIN
; Convenience forms for targets in the MAIN bank (resident code, C64 RAM data).
; These are just CALL/JUMP with an implicit MAIN bank: on the cart build a
; banked caller trampolines into MAIN; a resident (MAIN) caller emits a bare
; jsr/jmp; on the disk build everything is MAIN so they are always bare.
.macro CALLMAIN proc
	CALL FINAL_BANK_MAIN, proc
.endmacro

.macro JUMPMAIN proc
	JUMP FINAL_BANK_MAIN, proc
.endmacro

;*******************************************************************************
; LDA24
; Loads the value from the given 24 bit address and returns it in .A
.macro lda24 bank, addr
	lda bank
	; set REU (load) address
	IO_BEGIN
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

;*******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
; CLOBBERS:
;  zp::banktmp
.macro sta24 bank, addr
	sta zp::banktmp
	lda bank
	IO_BEGIN

	sta $df06	; MSB of REU address

	ldx #$00

	; set REU (load) address
	lda addr
	sta $df04	; LSB of REU address
	lda addr+1
	sta $df05	; middle byte of REU address

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

;*******************************************************************************
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
