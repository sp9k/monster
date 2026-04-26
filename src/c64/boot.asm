.include "bsp.inc"
.include "layout.inc"
.include "reu.inc"
.include "../asm.inc"
.include "../config.inc"
.include "../copybuff.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../draw.inc"
.include "../edit.inc"
.include "../irq.inc"
.include "../labels.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../monitor.inc"
.include "../runtime.inc"
.include "../settings.inc"
.include "../screen.inc"
.include "../source.inc"
.include "../vmem.inc"
.include "../watches.inc"
.include "../zeropage.inc"

.import __BSS_LOAD__
.import __BSS_SIZE__

.segment "SETUP"
;*******************************************************************************
; BASIC header: SYS 2061
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "2061"
@next: .word 0
start:
	sei

	; enable all RAM
	lda #$34
	sta $01

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

	sta zp::banksp		; zero out bank stack pointer

        jsr irq::on

	lda #<start
	sta $0316		; BRK
	lda #>start
	sta $0317		; BRK

	; initialize the status row reverse
	lda #DEFAULT_RVS
	ldx #STATUS_ROW
	jsr draw::hline

	jsr reu::init
	jsr asm::reset
	jsr src::init
	jsr src::new

	; initialize bitmap
	jsr scr::init
	jsr edit::clear

	jsr dbgi::initonce
	jsr asm::reset
	jsr buff::clear		; clear copy buffer

	; save the current machine state
	jsr run::clr
	jsr mon::init

	lda #$80
	sta $028a	; repeat all characters
	sta $0291	; don't swap charset on C= + SHIFT

	jsr dbgi::initonce

	lda #$4c
	sta zp::jmpaddr

	lda #DEFAULT_DEVICE
	sta zp::device

	lda #$00
	sta dbg::numbreakpoints	; clear breakpoints
	sta watch::num		; clear watches

	; clear row colors
	lda #DEFAULT_900F
	ldx #24-1
:	lda #DEFAULT_900F
	sta mem::rowcolors,x
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	dex
	bpl :-

	jmp edit::init
