.include "../../config.inc"
.include "../../edit.inc"
.include "../../memory.inc"
.include "../../monitor.inc"
.include "../../source.inc"
.include "banks.inc"
.include "../ram.inc"
.include "../../kernal.inc"
.include "../../macros.inc"

.ifndef JIFFYDOS
JIFFYDOS = 0
.endif

.import __IRQ_LOAD__
.import __IRQ_RUN__
.import __IRQ_SIZE__
.import __irq_keydecode

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

.if JIFFYDOS
.import __JIFFY_GATE_LOAD__, __JIFFY_GATE_RUN__, __JIFFY_GATE_SIZE__
.endif

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.import __BSS_LOAD__
.import __BSS_SIZE__
.import __VIEW_SHARED_RUN__, __VIEW_SHARED_SIZE__
.import __SHAREBSS_RUN__, __SHAREBSS_SIZE__
.import __SHAREBSS2_RUN__, __SHAREBSS2_SIZE__
.import __BSS_NOINIT_RUN__, __BSS_NOINIT_SIZE__
.import __SRCVARS_RUN__, __SRCVARS_SIZE__
.import __LINEBUFF_RUN__, __LINEBUFF_SIZE__
.import __ultimem_bank

.import enter
.ifdef ultimem_p
.import __ultimem_init_profiles
.endif

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
	; restart the unlock sequence and expose hidden UltiMem registers
	bit $9ff0
	bit $9f55
	bit $9faa
	bit $9f01

	; turn on LED
	lda #$01
	sta $9ff0

	; configure Ultimem so that RAM123 and IO2/3 use RAM0
	lda #$3f
	sta $9ff1

	; init bank registers for shared RAM spaces
	lda #$00
	sta $9ff4	; set RAM123 to bank 0
	sta $9ff5
	sta $9ff6	; also set I/O to bank 0
	sta $9ff7

	; clear bank high bytes and select the boot ROM in BLK5
	sta $9ff9
	sta $9ffb
	sta $9ffd
	sta $9ffe
	sta $9fff

	; Map BLK1/2/3 to the MAIN bank's ROM
	lda #$01
	sta $9ff8	; BLK1 = ROM bank 1
	lda #$02
	sta $9ffa	; BLK2 = ROM bank 2
	lda #$03
	sta $9ffc	; BLK3 = ROM bank 3
	lda #$55	; enable ROM in BLK 1, 2, and 3
	sta $9ff2

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

	jsr clear_cold_state

	; reset shared UI state
	lda #$00
	sta edit::debugging
	sta edit::sigint
	sta mon::windowed
	sta mem::coloron
	sta src::sp
	lda #CUR_BLINK_SPEED
	sta zp::curtmr

	; initialize the JMP vector
	lda #$4c		; JMP
	sta zp::jmpaddr

	lda #$00
	sta $98		; clear file count

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
:	cpy #<(__BSS_LOAD__+__BSS_SIZE__)
	bne @zerobss
	ldx r0+1
	cpx #>(__BSS_LOAD__+__BSS_SIZE__)
	bne @zerobss

	; clear viewport state, GUI line backup, bank stack, and status text
.export __ultimem_clear_view_shared
__ultimem_clear_view_shared:
	.assert __VIEW_SHARED_SIZE__ > 0, lderror, "empty shared RAM"
	.assert __VIEW_SHARED_SIZE__ < 256, lderror, "shared RAM exceeds one page"
	lda #0
	ldx #<__VIEW_SHARED_SIZE__
:	dex
	sta __VIEW_SHARED_RUN__,x
	bne :-

	sei
	lda #$7f
	sta $911e

	; set default device number
	lda #DEFAULT_DEVICE
	sta zp::device

	; restore default KERNAL vectors
	jsr $fd52

	; init some BASIC variables that are used (keyboard ptrs/delay)
	lda #<__irq_keydecode	; custom decode logic (maps CTRL+key combos)
	sta $028f		; set keyboard decode logic pointer low byte
	lda #>__irq_keydecode
	sta $0290		; set keyboard decode logic pointer high byte
	lda #$0a
	sta $0289	; set maximum size of keyboard buffer
	sta $028c	; set repeat delay counter
	lda #$04
	sta $028b	; set repeat timer

.ifdef ultimem_p
	jsr __ultimem_init_profiles
.endif

	; activate the main CODE bank and begin the app
	lda #$00
	sta zp::banksp
	lda #FINAL_BANK_MAIN
	sta __ultimem_bank
	lda #$4c		; JMP
	sta zp::jmpaddr
	sta zp::bankjmpaddr
	JUMP FINAL_BANK_MAIN, enter
.endproc

;*******************************************************************************
; CLEAR COLD STATE
; Clears shared state from "cold" (non-reset) state
.proc clear_cold_state
@dst=r0
@size=r2
	; see if signature exists to determine if cold reset needed
	ldx #<(boot_init_sig_len-1)
:	lda boot_init_sig,x
	cmp mem::init_sig,x
	bne @cold
	dex
	bpl :-
	rts

@cold:	ldx #$00
@l0:	; get base and size of a region of state to clear/init
	lda cold_regions,x
	sta @dst
	lda cold_regions+1,x
	sta @dst+1
	lda cold_regions+2,x
	sta @size
	lda cold_regions+3,x
	sta @size+1
	jsr clear_region

	; move to next region's pointers
	txa
	clc
	adc #$04
	tax
	cpx #cold_regions_size
	bne @l0

	rts
.endproc

;*******************************************************************************
; CLEAR REGION
; Clears the givven range
; IN:
;  - r0/r1: address to clear
;  - r2/r3: size in bytes to clear
.proc clear_region
@dst=r0
@size=r2
	ldy #$00
	lda @size+1
	beq @tail		; skip to last page if MSB is 0

	lda #$00
@page:	sta (@dst),y
	iny
	bne @page
	inc @dst+1
	dec @size+1
	bne @page

@tail:	; clear the final page
	ldy @size
	beq @done
	lda #$00
:	dey
	sta (@dst),y
	cpy #$00
	bne :-

@done:	rts
.endproc

;*******************************************************************************
; BOOT INIT SIGNATURE
; expected recovery signature in the boot ROM bank, matching init_sig
boot_init_sig:
	.byte $06,$05,$06,$00
boot_init_sig_len=*-boot_init_sig

;*******************************************************************************
; COLD REGIONS
; Addresses and sizes to clear on "cold" boot
cold_regions:
.word __SHAREBSS_RUN__, __SHAREBSS_SIZE__
.word __SHAREBSS2_RUN__, __SHAREBSS2_SIZE__
.word __BSS_NOINIT_RUN__, __BSS_NOINIT_SIZE__
.word __SRCVARS_RUN__, __SRCVARS_SIZE__
.word __LINEBUFF_RUN__, __LINEBUFF_SIZE__
cold_regions_size=*-cold_regions

;*******************************************************************************
; RELOCS
; Table of start and target addresses for segments that need to be relocated
relocs:
.if JIFFYDOS
.word __JIFFY_GATE_LOAD__, __JIFFY_GATE_RUN__, __JIFFY_GATE_SIZE__
.endif

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
