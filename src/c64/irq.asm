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

	ldxy #keydecode
	stxy $028f		; KERNAL keyboard decode vector
.ifdef CART
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
.segment "KEYDECODE"
.endif
;*******************************************************************************
; KEYDECODE
; Decodes CTRL shortcuts and C= + function (music) keys
.proc keydecode
	lda $028d
	and #$04		; CTRL held?
	beq @commodore

	lda $cb
	ldy #ctrl_count-1
@find:	cmp ctrlkeys,y
	beq @ctrl
	dey
	bpl @find
	lda #$ff		; unassigned CTRL chords produce no input
	bne @buffer
@ctrl:	lda ctrlcodes,y
@buffer:
	ldy $cb			; repeat handling compares the matrix index
	jmp $eae4		; KERNAL repeat handling and keyboard buffer

@commodore:
.ifdef CART
	lda $028d
	cmp #$02		; C= held without SHIFT?
	bne @normal
	lda $cb
	sec
	sbc #$04		; f1/f3/f5 have consecutive matrix indexes
	cmp #$03
	bcs @normal
	adc #K_SID_VIEW		; carry clear: $ec/$ed/$ee
	bne @buffer
.assert K_SID_TOGGLE = K_SID_VIEW+1, error, "SID key order"
.assert K_SID_RESTART = K_SID_VIEW+2, error, "SID key order"
.endif
@normal:
	jmp $eb48		; normal modifier/table selection
.endproc

;*******************************************************************************
; CTRL KEYS
; C64 matrix indexes and codes
ctrlkeys:
	.byte $38,$3b,$08,$0b,$10,$13,$18,$1b	; 1 2 3 4 5 6 7 8
	.byte $1d,$2a,$22,$25,$21		; H L J K I
	.byte $32,$2d,$28,$2b			; ; : + -
ctrl_count=*-ctrlkeys
ctrlcodes:
	.byte K_GOTO_BUFF1,K_GOTO_BUFF2,K_GOTO_BUFF3,K_GOTO_BUFF4
	.byte K_GOTO_BUFF5,K_GOTO_BUFF6,K_GOTO_BUFF7,K_GOTO_BUFF8
	.byte K_PREV_BUFF,K_NEXT_BUFF,K_WIN_DOWN,K_WIN_UP,$09
	.byte K_NEXT_BANNER,K_PREV_BANNER,K_NEXT_PAL,K_PREV_PAL
.assert * - ctrlcodes = ctrl_count, error, "CTRL key table size"
.segment "IRQ"

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
