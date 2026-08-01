.include "../macros.inc"

.import __ram_mem01

.segment "IRQ"

;*******************************************************************************
; IRQ OFF
.export __irq_off
.proc __irq_off
	sei
	rts
.endproc

;*******************************************************************************
; IRQ ON
; Syncs to the configured scanline and sets up an IRQ that will trigger whenever
; that location is reached.
.export __irq_on
.proc __irq_on
	sei

	lda #$34		; make all RAM available
	sta $01

	ldxy #sys_update
	stxy $0314		; software vector
	ldxy #hw_irq_handler
	stxy $fffe		; hardware vector

	; restore the caller's memory context (this may be called from banked
	; code on the cart build, e.g. via scr::unblank)
	lda __ram_mem01
	sta $01

	cli
	rts
.endproc

;*******************************************************************************
.proc hw_irq_handler
	pha
	txa
	pha
	tya
	pha
	tsx
	lda $104,x
	and #$10		; BRK?
	beq sys_update		; if not -> continue

@brk:	jmp *
.endproc

;*******************************************************************************
; SYS_UPDATE
; This is the main IRQ for this program. It handles updating the beeper.
; It is relocated to a place where it may be called from any bank
.proc sys_update
	lda $01
	pha

	lda #$36	; make KERNAL ($e000-$ffff) available
	sta $01

	; save $f5-$f6
        lda $f5
	sta @savef5
        lda $f6
	sta @savef6

	jsr $ea87	; scan the keyboard

@keydone:
@savef5=*+1
	lda #$00
        sta $f5
@savef6=*+1
	lda #$00
        sta $f6

	lda $dc0d	; ack interrupts

	; keep VIC in bank 0 (screen @ $0400 / charset @ $1800)
	lda $dd00
	ora #$03
	sta $dd00

	pla
	sta $01

	pla
	tay
	pla
	tax
	pla
	rti
.endproc
