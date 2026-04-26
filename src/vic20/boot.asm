;*******************************************************************************
; BOOT.ASM
; This is the entrypoint.  Performs necessary initialization and jumps to the
; editor main loop.
; If CART is defined, copies code from cartridge to RAM.  If it isn't defined,
; loads it from disk.
;*******************************************************************************
.include "expansion.inc"
.include "../asm.inc"
.include "../config.inc"
.include "../monitor.inc"
.include "../copybuff.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../draw.inc"
.include "../edit.inc"
.include "../irq.inc"
.include "../io.inc"
.include "../kernal.inc"
.include "../key.inc"
.include "../labels.inc"
.include "../log.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../prefs.inc"
.include "../ram.inc"
.include "../runtime.inc"
.include "../screen.inc"
.include "../settings.inc"
.include "../sim6502.inc"
.include "../source.inc"
.include "../text.inc"
.include "../vmem.inc"
.include "../watches.inc"
.include "../zeropage.inc"

.include "fastcopy.inc"
.include "vic20.inc"

.import __BSS_LOAD__
.import __BSS_SIZE__

.ifndef CART	; DISK
.segment "SETUP"
;*******************************************************************************
; BASIC header: SYS 4621
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0
	lda #DEFAULT_900F
	sta $900f		; white/white
	lda #$c0
	sta $9005		; screen @ $1000
	lda #00			; # of columns and rows
	sta $9002
	sta $9003
	jmp __boot_start

;*******************************************************************************
; CART header and boot code
.else ; CART
.segment "CART"
.word cart_start	; Entry point for power up
.word edit::init	; Entry point for warm start (RESTORE)

; cartridge header
.byte "a0",$C3,$C2,$CD	; "A0CBM"

; copy cart binary ($0000-$6000) to RAM
cart_start:
	ldx #$ff
	txs

	jsr $fd52	; init vectors
	jsr $fdf9	; init I/O

	sei
	lda #$7f
	sta $911e

	lda #DEFAULT_900F
	sta $900f		; white/white
	lda #$c0
	sta $9005		; screen @ $1000
	lda #00			; # of columns and rows
	sta $9002
	sta $9003

.ifdef ultimem
	jmp ultim::init
.endif

;-----------------------
.segment "SETUP"
.endif	; CART

.ifndef ultimem
;*******************************************************************************
; LOWINIT
; Code that is sensitive to initialization order
; This code loads the app and sets up various banked code.
; Once the initialization in complete, jumps to enter to begin the app
.proc lowinit
	lda #CUR_BLINK_SPEED
	sta zp::curtmr

.ifdef CART
	jsr fe3::init1
	jmp enter
.else
; DISK init code; load the application from file
	; load the app and enter it
	lda #FINAL_BANK_MAIN
	SELECT_BANK_A
	ldxy #@mainprg
	lda #@mainprg_len
	jsr $ffbd	; SETNAM
	lda #$01
	ldx $ba		; last used device
	bne :+
	ldx #$0a	; default to #10
:	ldy #$01	; load to address stored in file
	jsr $ffba	; SETLFS

	lda #$00	; load (not verify)
	jsr $ffd5	; LOAD
	jmp enter
@mainprg: .byte "masm.prg"
@mainprg_len=*-@mainprg
.endif
.endproc

;*******************************************************************************
; START
; Entrypoint to program
.export __boot_start
.proc __boot_start
	sei
	lda #$7f
	sta $911e

	; set default device number
	lda #DEFAULT_DEVICE
	sta zp::device

	; enable all memory
	lda #FINAL_BANK_MAIN
	SELECT_BANK_A

	; restore default KERNAL vectors
	jsr $fd52
	ldxy #$eb15
	stxy $0314

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

.ifdef fe3
	jsr fe3::init0
.endif

	; perform the machine-specific initialization
	jsr vic20::init

	; initialize the JMP vector
	lda #$4c		; JMP
	sta zp::jmpaddr

	; clean up files
	jsr $ffe7		; CLALL (close all files)

	lda #$80
	sta $028a	; repeat all characters
	sta $0291	; don't swap charset on C= + SHIFT

	; clear row colors
	lda #DEFAULT_900F
	ldx #24-1
:	lda #DEFAULT_900F
	sta mem::rowcolors,x
	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x
	dex
	bpl :-

	jmp lowinit
.endproc
.endif

.CODE
;*******************************************************************************
; ENTER
; Entrypoint after initialization, from here on we're safe to use the bitmap
; address space ($1000-$2000) as a bitmap
.export enter
.proc enter
	lda #$00
	sta zp::banksp		; init bank stack pointer
	sta $c6			; clear keyboard buffer
	sta dbg::numbreakpoints	; clear breakpoints
	sta watch::num		; clear watches

	lda #$4c
	sta zp::bankjmpaddr	; write the JMP instruction

	sei

.ifdef CART
@detect_reset:
	ldx #init_sig_len-1
:	lda init_sig,x
	cmp mem::init_sig,x
	bne @init		; not a match -> complete initialization
	dex
	bpl :-

@recover:
	; do some basic init to allow displaying recovery message
	jsr scr::init
        jsr irq::on

	; signature intact, ask user if they wish to recover
	ldxy #recover_reset
	lda #10
	jsr text::print
	jsr key::waitch
	pha
        jsr irq::off
	pla
	cmp #$79		; Y
	beq @enter
	cmp #$6e		; N
	bne @recover
.endif

@init:
	jsr src::init
	jsr src::new
	jsr dbgi::initonce
	jsr asm::reset
	jsr buff::clear		; clear copy buffer
	jsr edit::clear

	lda #$00
	sta mem::linebuffer
	CALL FINAL_BANK_MONITOR, mon::init

@enter:
.ifndef TEST
.ifdef CART
	; write the "initialized" signature
	ldx #init_sig_len-1
:	lda init_sig,x
	sta mem::init_sig,x
	dex
	bpl :-
.endif

	jsr run::clr		; init user state (run BASIC startup proc)

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

	; initialize the status row reverse
	ldx #23
	jsr draw::hiline

	; load default preferences
	; jsr gprefs::load

        jsr irq::on
	jmp edit::init
.else
	.import testsuite
	jmp testsuite
.endif
.endproc

.ifdef CART
.PUSHSEG
.RODATA
init_sig:
	.byte 6,5,6,0	; magic value to detect reset for recovery
init_sig_len=*-init_sig

recover_reset:
	.byte " reset detected - restore state? (y/n)",0
.POPSEG
.endif
