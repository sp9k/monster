.include "banks.inc"
.include "../ram.inc"
.include "../../kernal.inc"
.include "../../macros.inc"

.import __IRQ_LOAD__
.import __IRQ_RUN__
.import __IRQ_SIZE__

.import __BANKCODE_LOAD__
.import __BANKCODE_RUN__
.import __BANKCODE_SIZE__

.import __BANKCODE2_LOAD__
.import __BANKCODE2_RUN__
.import __BANKCODE2_SIZE__

.import __INTS_LOAD__
.import __INTS_RUN__
.import __INTS_SIZE__

.import __ULTICFG_LOAD__
.import __ULTICFG_RUN__
.import __ULTICFG_SIZE__

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.import __BSS_LOAD__
.import __BSS_SIZE__

.import enter

.segment "CART"

;*******************************************************************************
; RELOC
; relocates code from 1 address to another
; IN:
;  - r0r1: source address
;  - r2r3: dest address
;  - r4:   number of bytes to copy
.macro reloc
@src=r0
@dst=r2
@size=r4
	lda @size+1
	beq @lastpage

	ldy #$00
@pageloop:
	lda (@src),y
	sta (@dst),y
	iny
	bne @pageloop
	inc @src+1
	inc @dst+1
	dec @size+1
	bne @pageloop

@lastpage:
	ldy @size
	beq @done
	dey
:	lda (@src),y
	sta (@dst),y
	dey
	cpy #$ff
	bne :-
@done:
.endmacro

;*******************************************************************************
; INIT
; Copies bank-indepenedent code to its runtime position in shared RAM
.export __ultimem_init
.proc __ultimem_init
@cnt=r7
@relocs=r8
	; configure Ultimem so that RAM123 and IO2/3 use RAM0
	lda #$3f
	sta $9ff1

	; init bank registers for shared RAM spaces
	lda #$00
	sta $9ff4	; set RAM123 to bank 0
	sta $9ff5
	sta $9ff6	; also set I/O to bank 0
	sta $9ff7

	; init MSB's of bank selection registers and I/O bank
	sta $9ff9
	sta $9ffb
	sta $9ffd
	sta $9fff

	lda #num_relocs
	sta @cnt
	ldxy #relocs
	stxy @relocs

@reloc:	ldy #$00
	lda (@relocs),y
	sta r0
	iny
	lda (@relocs),y
	sta r0+1

	; destination
	iny
	lda (@relocs),y
	sta r2
	iny
	lda (@relocs),y
	sta r2+1

	; size
	iny
	lda (@relocs),y
	sta r4
	iny
	lda (@relocs),y
	sta r4+1

	reloc

	lda @relocs
	clc
	adc #$06
	sta @relocs
	bcc :+
	inc @relocs+1
:	dec @cnt
	bne @reloc

	; initialize the JMP vector
	lda #$4c		; JMP
	sta zp::jmpaddr

	; clean up files
	jsr krn::clall

	lda #$80
	sta $028a	; repeat all characters
	sta $0291	; don't swap charset on C= + SHIFT

;--------------------------------------
; zero the BSS segment
	lda #>__BSS_LOAD__
	sta r0+1
	ldy #<__BSS_LOAD__
	lda #$00
	sta r0
@zerobss:
	sta (r0),y
	iny
	bne :+
	inc r0+1
:	cpy #<(__BSS_LOAD__+__BSS_SIZE__-1)
	bne @zerobss
	ldx r0+1
	cpx #>(__BSS_LOAD__+__BSS_SIZE__-1)
	bne @zerobss

	; enable ROM in BLK 1, 2, and 3
	lda #$55
	sta $9ff2

	sei
	lda #$7f
	sta $911e

	; set default device number
	lda #DEFAULT_DEVICE
	sta zp::device

	; restore default KERNAL vectors
	jsr $fd52

	; init some BASIC variables that are used (keyboard ptrs/delay)
	lda #<$ebdc	; get keyboard decode logic pointer low byte
	sta $028f	; set keyboard decode logic pointer low byte
	lda #>$ebdc	; get keyboard decode logic pointer high byte
	sta $0290	; set keyboard decode logic pointer high byte
	lda #$0a
	sta $0289	; set maximum size of keyboard buffer
	sta $028c	; set repeat delay counter
	lda #$04
	sta $028b	; set repeat timer

	; activate the main CODE bank and begin the app
	lda #$4c		; JMP
	sta zp::jmpaddr
	sta zp::bankjmpaddr
	JUMP FINAL_BANK_MAIN, enter
.endproc

;*******************************************************************************
; RELOCS
; Table of start and target addresses for segments that need to be relocated
relocs:
; BANK CODE
.word __BANKCODE_LOAD__, __BANKCODE_RUN__, __BANKCODE_SIZE__

; BANK CODE 2
.word __BANKCODE2_LOAD__, __BANKCODE2_RUN__, __BANKCODE2_SIZE__

; IRQ
.word __IRQ_LOAD__, __IRQ_RUN__, __IRQ_SIZE__

; INTS
.word __INTS_LOAD__, __INTS_RUN__, __INTS_SIZE__

; DATA
.word __DATA_LOAD__, __DATA_RUN__, __DATA_SIZE__

; ULICFG
.word __ULTICFG_LOAD__, __ULTICFG_RUN__, __ULTICFG_SIZE__

num_relocs=(*-relocs)/6
