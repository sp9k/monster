.include "bsp.inc"
.include "layout.inc"
.include "ram.inc"
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

.ifdef CART
.import __DATA_LOAD__,        __DATA_RUN__,        __DATA_SIZE__
.import __IRQ_LOAD__,         __IRQ_RUN__,         __IRQ_SIZE__
.import __BANKCODE2_LOAD__,   __BANKCODE2_RUN__,   __BANKCODE2_SIZE__
.import __CODE_LOAD__,        __CODE_RUN__,        __CODE_SIZE__
.import __RODATA_LOAD__,      __RODATA_RUN__,      __RODATA_SIZE__
.import __GUICODE_LOAD__,     __GUICODE_RUN__,     __GUICODE_SIZE__
.import __DEBUGGER_LOAD__,    __DEBUGGER_RUN__,    __DEBUGGER_SIZE__
.import __ERRORS_LOAD__,      __ERRORS_RUN__,      __ERRORS_SIZE__
.import __HELP_LOAD__,        __HELP_RUN__,        __HELP_SIZE__
.import __NMI_HANDLER_LOAD__, __NMI_HANDLER_RUN__, __NMI_HANDLER_SIZE__
.import __TRAMPOLINE_LOAD__,  __TRAMPOLINE_RUN__,  __TRAMPOLINE_SIZE__
.import __BOOTLDR_LOAD__,     __BOOTLDR_RUN__,     __BOOTLDR_SIZE__
.endif

.segment "SETUP"

.ifdef CART
;*******************************************************************************
; CARTRIDGE HEADER
; Magic Desk: 8K at $8000, autostarted by the KERNAL via the CBM80 signature
.word coldstart			; cold start vector
.word coldstart			; warm start vector
.byte $c3, $c2, $cd, $38, $30	; "CBM80"

;*******************************************************************************
; COLDSTART
; Reset entry point.  Initializes the KERNAL, then copies the boot loader to
; RAM and runs it there so that it can switch ROM banks out from under itself.
.proc coldstart
	sei
	ldx #$ff
	txs
	cld

	; set the port data register before the DDR: the output latch is 0 at
	; power-on, so writing the DDR first would unmap the cartridge ROM
	lda #$37
	sta $01
	lda #$2f
	sta $00

	jsr $fda3	; init I/O
	jsr $fd50	; RAMTAS
	jsr $fd15	; restore default I/O vectors
	jsr $ff5b	; init screen
	sei

	; copy the boot loader to RAM ($200 bytes covers loader + reloc table)
	ldx #$00
:	lda __BOOTLDR_LOAD__,x
	sta __BOOTLDR_RUN__,x
	lda __BOOTLDR_LOAD__+$100,x
	sta __BOOTLDR_RUN__+$100,x
	inx
	bne :-
	jmp loader
.endproc

.segment "BOOTLDR"
;*******************************************************************************
; LOADER
; Runs from RAM.  Zeroes the RAM regions that the disk build gets zeroed by
; its image fill, then streams the resident image from the flash banks to its
; run addresses.  The flash is laid out linearly starting at bank 1 (see
; link-c64-cart.config); the bank and window offset for each segment are
; computed from its 16-bit load address.
.proc loader
@src=r0
@dst=r2
@cnt=r4
@bank=r6
@reloc=r7
@count=r9
	lda #$34
	sta $01		; all RAM

	; zero the line buffers ($0800-$08ff)
	lda #$00
	tay
:	sta $0800,y
	iny
	bne :-

	; zero $0d00-$7fff
	sta @dst
	ldx #$0d
	stx @dst+1
@z0:	sta (@dst),y
	iny
	bne @z0
	inc @dst+1
	bpl @z0

	; zero $c000-$feff
	ldx #$c0
	stx @dst+1
@z1:	sta (@dst),y
	iny
	bne @z1
	inc @dst+1
	ldx @dst+1
	cpx #$ff
	bne @z1

	; stream the resident image
	lda #$37
	sta $01		; cartridge ROM at $8000 + I/O

	lda #<relocs
	sta @reloc
	lda #>relocs
	sta @reloc+1
	lda #num_relocs
	sta @count

@entry:	ldy #$00
	lda (@reloc),y	; load address (lo)
	sta @src
	iny
	lda (@reloc),y	; load address (hi)
	pha
	lsr		; bank = 1 + (load address >> 13)
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$01
	sta @bank
	sta $de00
	pla
	and #$1f
	ora #$80	; window offset -> $8000-$9fff
	sta @src+1
	iny
	lda (@reloc),y	; run address
	sta @dst
	iny
	lda (@reloc),y
	sta @dst+1
	iny
	lda (@reloc),y	; size
	sta @cnt
	iny
	lda (@reloc),y
	sta @cnt+1

	ldy #$00
@copy:	lda @cnt
	ora @cnt+1
	beq @next
	lda (@src),y
	sta (@dst),y
	inc @dst
	bne :+
	inc @dst+1
:	inc @src
	bne @deccnt
	inc @src+1
	lda @src+1
	cmp #$a0	; reached the end of the window?
	bne @deccnt
	inc @bank	; continue in the next bank
	lda @bank
	sta $de00
	lda #$80
	sta @src+1
@deccnt:
	lda @cnt
	bne :+
	dec @cnt+1
:	dec @cnt
	jmp @copy

@next:	lda @reloc
	clc
	adc #$06
	sta @reloc
	bcc :+
	inc @reloc+1
:	dec @count
	bne @entry

	lda #$00
	sta $de00	; leave bank 0 selected
	lda #$34
	sta $01
	jmp start
.endproc

;*******************************************************************************
; RELOCS
; (load, run, size) for each segment of the resident image
relocs:
.word __DATA_LOAD__,        __DATA_RUN__,        __DATA_SIZE__
.word __IRQ_LOAD__,         __IRQ_RUN__,         __IRQ_SIZE__
.word __BANKCODE2_LOAD__,   __BANKCODE2_RUN__,   __BANKCODE2_SIZE__
.word __CODE_LOAD__,        __CODE_RUN__,        __CODE_SIZE__
.word __RODATA_LOAD__,      __RODATA_RUN__,      __RODATA_SIZE__
.word __GUICODE_LOAD__,     __GUICODE_RUN__,     __GUICODE_SIZE__
.word __DEBUGGER_LOAD__,    __DEBUGGER_RUN__,    __DEBUGGER_SIZE__
.word __ERRORS_LOAD__,      __ERRORS_RUN__,      __ERRORS_SIZE__
.word __HELP_LOAD__,        __HELP_RUN__,        __HELP_SIZE__
.word __NMI_HANDLER_LOAD__, __NMI_HANDLER_RUN__, __NMI_HANDLER_SIZE__
.word __TRAMPOLINE_LOAD__,  __TRAMPOLINE_RUN__,  __TRAMPOLINE_SIZE__
num_relocs=(*-relocs)/6

.CODE

.else	; disk build

;*******************************************************************************
; BASIC header: SYS 2061
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "2061"
@next: .word 0

.endif

;*******************************************************************************
; START
; Common initialization.  On the disk build this runs (once) from the BOOT
; region; on the cartridge build it is resident and also serves as the BRK
; (warm start) handler.
start:
	sei

	; enable all RAM
	lda #$34
	sta $01

;-------------------------------------------------------------------------------
; zero the HIRAM BSS ($d000-$dfff)
	lda #$00
	tay
	sta r0
	ldx #$d0
	stx r0+1

@zerobss:
	sta (r0),y
	iny
	bne @zerobss		; 256 bytes per page
	inc r0+1
	ldx r0+1
	cpx #$e0		; stop at $e000 (KERNAL-area code is loaded there)
	bne @zerobss

	sta zp::banksp		; zero out bank stack pointer
	jsr __ram_init		; init execution context state

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
	CALL FINAL_BANK_MONITOR, mon::init

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
