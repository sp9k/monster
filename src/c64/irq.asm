.include "../macros.inc"
.include "../keycodes.inc"
.include "sidplay.inc"

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
; Install the editor IRQ and restore the current tune's raster IRQ source.
.export __irq_on
.proc __irq_on
	sei

	lda #$34		; make all RAM available
	sta $01

	ldxy #sys_update
	stxy $0314		; software vector
	ldxy #hw_irq_handler
	stxy $fffe		; hardware vector

.ifdef CART
	ldxy #keydecode
	stxy $028f		; KERNAL keyboard decode vector
	lda #$36
	sta $01
	jsr __sid_config_irq
.endif

	; restore the caller's memory context (this may be called from banked
	; code on the cart build, e.g. via scr::unblank)
	lda __ram_mem01
	sta $01

	cli
	rts
.endproc

.ifdef CART
;*******************************************************************************
; KEYDECODE
; Check for C= + function (music) keys
.proc keydecode
	lda $028d
	cmp #$02		; C= held?
	bne @normal
	ldy $cb
	cpy #$04		; f1
	beq @view
	cpy #$05		; f3
	beq @toggle
	cpy #$06		; f5
	bne @normal

	lda #K_SID_RESTART
	bne @buffer
@toggle:
	lda #K_SID_TOGGLE
	bne @buffer
@view:
	lda #K_SID_VIEW
@buffer:
	jmp $eae4		; normal non-modified key
@normal:
	jmp $eb48		; normal modifier/table selection
.endproc
.endif

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
; Dispatch video-frame music separately from CIA keyboard/music events.
; It is relocated to a place where it may be called from any bank
.proc sys_update
	lda $01
	pha

	lda #$36	; make KERNAL ($e000-$ffff) available
	sta $01

	; service music player (if active)
	cld
	lda $d019
	and $d01a
	and #1
	beq @cia
	lda #1
	sta $d019
	jsr __sid_tick

@cia:	lda $dc0d		; read/ack CIA sources once
	and #1			; timer A: keyboard, and CIA-timed music
	beq @done
	jsr __sid_cia_tick

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

@done:
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
