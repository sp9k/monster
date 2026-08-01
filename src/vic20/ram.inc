.ifndef VIC20_RAM_INC
VIC20_RAM_INC = 1

.import __ram_call

.include "../zeropage.inc"
.include "ultimem/banks.inc"

;*******************************************************************************
; LDA24
; Loads the given 24 bit address into .A
.macro lda24 bank, addr, offset
.ifnblank offset
	lda offset
	sta zp::bankval
.endif
.ifnblank addr
	ldxy addr
.endif
	lda bank

.ifblank offset
	jsr __ram_load_byte
.else
	lda bank
	jsr __ram_load_byte_off
.endif
.endmacro

;*******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
.macro sta24 bank, addr
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __ram_store_byte
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
;  - addr: the address to load from
;  - .Y:   offset to apply to load address
; OUT:
;  - .A: the value that was read
;  - .Z: set if the value read was 0
;  - .N: set if the value read is negative
.macro LOADB_Y addr
	lda (addr),y
.endmacro

;*******************************************************************************
; LOADB
; Load a byte from the given address
; IN:
;  - addr: the address to load from
; OUT:
;  - .A: the value that was read
;  - .Z: set if the value read was 0
;  - .N: set if the value read is negative
.macro LOADB addr
	ldy #$00
	LOADB_Y addr
.endmacro

;*******************************************************************************
; LOADW
; Read a word from the given address
; IN:
;   - addr: source address
; OUT:
;   - .XY: the value that was read
.macro LOADW addr
	ldy #$00
	lda (addr),y
	tax
	iny
	lda (addr),y
	tay
.endmacro

;*******************************************************************************
; STOREB Y
; Store a byte to memory with Y offset
; IN:
;   - .A: byte to write
;   - .Y: offset
;   - addr: address to write to
.macro STOREB_Y addr
	sta (addr),y
.endmacro

;*******************************************************************************
; STOREB
; Stores the value in .A to the target address
; IN:
;   - .A:   value to store
;   - addr: target address
.macro STOREB addr
	ldy #$00
	STOREB_Y addr
.endmacro

;*******************************************************************************
; STOREW_Y
; Write a word to memory at the target address offset by the value in .Y
; IN:
;   - val:  value to write
;   - addr: address to store to
; OUT:
;   - .Y: the input value of .Y + 2
.macro STOREW_Y val, addr
	lda val
	STOREB_Y addr
	iny
	STOREB_Y addr
	iny
.endmacro

;*******************************************************************************
; STOREW
; Write a word to memory
; IN:
;   - .XY:  value to write
;   - addr: address to store to
.macro STOREW addr
	tya
	ldy #$01
	sta (addr),y
	txa
	dey
	sta (addr),y
.endmacro

;*******************************************************************************
; SWAPB_Y
; Swaps 2 bytes at the given addresses offset by the value of .Y
; IN:
;   - addr1: one address to swap
;   - addr2: the other address to swap
.macro SWAPB_Y addr1, addr2
	lda (addr1),y
	tax
	lda (addr2),y
	sta (addr1),y
	txa
	sta (addr2),y
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
.local @loop
@loop:	lda (src),y
	sta (dst),y
	dey
	bpl @loop
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
.macro FILL_Y addr
.local @loop
@loop:	sta (addr),y
	dey
	bpl @loop
.endmacro

;*******************************************************************************
; CUR_BANK
; The logical bank the code currently being assembled runs from.  CALL/JUMP
; compare a target's bank against this to decide whether a call is "near" (same
; bank -> a bare jsr/jmp, no bank switch) or "far" (route through __ram_call).
; See the fuller explanation in c64/ram.inc.
;
; The default is BANK_NONE so that every call stays far (the historical
; behavior) until a module opts in with its own `CUR_BANK .set FINAL_BANK_x`.
BANK_NONE = $ff
CUR_BANK .set BANK_NONE

;*******************************************************************************
; SET_CUR_BANK
; Declares the bank the following code runs from
.macro SET_CUR_BANK bank
.endmacro

;*******************************************************************************
; CALL
; calls the banked routine
; params:
;  bank: the bank of the procedure to call
;  proc: the procedure to call
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
; CALLMAIN
; equivalent to CALL FINAL_BANK_MAIN, proc
.macro CALLMAIN proc
	CALL FINAL_BANK_MAIN, proc
.endmacro

;*******************************************************************************
; JUMP
; jumps to the banked routine
; params:
;  bank: the bank of the procedure to jump to
;  proc: the procedure to jump to
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
; JUMPMAIN
; equivalent to JUMP FINAL_BANK_MAIN, proc
; params:
;  proc: the procedure to jump to
.macro JUMPMAIN proc
	JUMP FINAL_BANK_MAIN, proc
.endmacro

.endif
