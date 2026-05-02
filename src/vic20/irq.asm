.include "expansion.inc"
.include "prefs.inc"
.include "settings.inc"
.include "../beep.inc"
.include "../key.inc"
.include "../layout.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../util.inc"

.macpack longbranch

;*******************************************************************************
; CONSTANTS

.ifdef PAL
LINES           = 312
CYCLES_PER_LINE = 71
IRQ_START_LINE  = $1c
.else ;NTSC
LINES           = 261
CYCLES_PER_LINE = 65

.ifdef soft4x8
IRQ_START_LINE = $0e
.else
IRQ_START_LINE = $11
.endif

.endif

; timer value for stable raster interrupt
TIMER_VALUE     = LINES * CYCLES_PER_LINE - 2
CYCLES_PER_ROW  = 8 * (CYCLES_PER_LINE - 2) - 25

;*******************************************************************************
; There is only room for 1 custom char in the memory map.
; We will redefine it at runtime between: empty (all 0's) and a breakpoint
; character, like this:
;   **
;  ****
;  ****
;   **
NUM_COLS    = 20
SCREEN_ROWS = 24
DYNAMIC_CHAR_ADDR     = $10f0
DYNAMIC_CHAR_ADDR_BOT = $1000

;*******************************************************************************
.segment "SHAREBSS"
rowcnt:    .byte 0
savebank:  .byte 0
savebank2: .byte 0

.segment "IRQ"
;*******************************************************************************
; SYS_UPDATE
; This is the main IRQ for this program. It handles updating the beeper.
; It is relocated to a place where it may be called from any bank
.proc sys_update
.ifndef ultimem ; fe3
	lda exp::bank		; 4 (4)
	sta savebank		; 4 (8)
	lda #FINAL_BANK_MAIN	; 2 (10)
	SELECT_BANK_A		; 4 (14)
.else
	; kill 14 cycles
	pha
	pla
	pha
	pla
.endif
	jsr stable_handler	; 6 (20) / (6 - ultimem)

.ifndef ultimem
	lda savebank
	SELECT_BANK_A
.endif
	jmp $eb15
.endproc

.proc row_interrupt
.ifndef ultimem
	lda exp::bank
	sta savebank2
	lda #FINAL_BANK_MAIN
	SELECT_BANK_A
.else
	; kill 14 cycles
	pha
	pla
	pha
	pla
.endif
	jsr row_handler
.ifndef ultimem
	lda savebank2
	SELECT_BANK_A
.endif
	jmp $eb15
.endproc

;*******************************************************************************
.ifndef ultimem
.CODE
.endif

;*******************************************************************************
; STABLE_HANDLER
.export stable_handler
.proc stable_handler
	cld
	sec
.ifdef PAL
	lda #$44
.else
	; base phase value minus all instructions
	; executed in handler before this
	lda #$15-4-3-2-4-6
.endif
	sbc $9124
	cmp #$0a
	bcc @s0
	rts

@s0:	sta @s1+1
@s1:	bcc @s1
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop


	; clear the character area for the breakpoint
	lda #$55
	sta $10f0
	sta $10f1
	sta $10f2
	sta $10f3
	sta $10f4
	sta $10f5
	sta $10f6
	sta $10f7
	sta $10f8
	sta $10f9
	sta $10fa
	sta $10fb
	sta $10fc
	sta $10fd
	sta $10fe
	sta $10ff

	; restore screen row 0 (repurposed for breakpoint rendering at bottom of
	; display)
	lda #$0f
	sta $1000
	lda #$10
	sta $1001
	lda #$1c
	sta $1002
	lda #$28
	sta $1003
	lda #$34
	sta $1004
	lda #$40
	sta $1005
	lda #$4c
	sta $1006
	lda #$58
	sta $1007
	lda #$64
	sta $1008
	lda #$70
	sta $1009
	lda #$7c
	sta $100a

	; set up sub-interrupt that executes every character row to draw
	; breakpoints on any line that has one
	; we only do this when at least one row has color to save cycles in the
	; average case (no color)
	lda mem::coloron
	beq @cont
	lda #$80|$20|$c0
	sta $912e		; enable T2 interrupts
	ldxy #row_interrupt
	stxy $0314
	ldxy #105
	stx $9128
	sty $9129
	cli
	lda #$00

@cont:	sta rowcnt		; rowcnt = 0

	lda #$88
	sta $100b
	lda #$94
	sta $100c
	lda #$a0
	sta $100d
	lda #$ac
	sta $100e
	lda #$b8
	sta $100f

	; save $f5-$f6
        lda $f5
	pha
        lda $f6
	pha

	jsr $eb1e               ; scan keyboard

	; inject TAB ($09) into keyboard buffer if the CTRL key is pressed
	lda $028d		; get CTRL flag reg
	cmp $028e		; debounce
	beq @keydone
	and #$04		; is bit 2 (CTRL) pressed?
	beq @keydone		; if 0, no
	ldx #$09		; TAB
	jsr $ebba		; store to keyboard table

@keydone:
	pla
	sta $f6
	pla
        sta $f5
	jmp beep::update
.endproc

;*******************************************************************************
; ROW HANDLER
; Handles the sub-interrupt responsible for drawing breakpoints
; For correct timing, the branches in this routine should not cross a page
.export row_handler
.proc row_handler
	cld
	sec

	;lda #$4+3+4+2+4+6+2+2+8+$10+24
	lda #$be

	sbc $9128	; add signed overflow value from timer
	cmp #$0a
	bcc @s0
	rts

@s0:	sta @s1+1
@s1:	bcc @s1
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop

@done:	; reload timer
	ldxy #CYCLES_PER_ROW-52-10
	stxy $9128

	; set the color for the next row
	ldx rowcnt
	lda mem::rowcolors,x
	sta $900f

	cpx #$04
	bne @handle_brkpts

	;----------------------------------------------------------------------
	; clear character 0
	; we will use this for breakpoints on row 11 (the last screen row)
	lda #$55
	sta DYNAMIC_CHAR_ADDR_BOT+$00
	sta DYNAMIC_CHAR_ADDR_BOT+$01
	sta DYNAMIC_CHAR_ADDR_BOT+$02
	sta DYNAMIC_CHAR_ADDR_BOT+$03
	sta DYNAMIC_CHAR_ADDR_BOT+$04
	sta DYNAMIC_CHAR_ADDR_BOT+$05
	sta DYNAMIC_CHAR_ADDR_BOT+$06
	sta DYNAMIC_CHAR_ADDR_BOT+$07
	sta DYNAMIC_CHAR_ADDR_BOT+$08
	sta DYNAMIC_CHAR_ADDR_BOT+$09
	sta DYNAMIC_CHAR_ADDR_BOT+$0a
	sta DYNAMIC_CHAR_ADDR_BOT+$0b
	sta DYNAMIC_CHAR_ADDR_BOT+$0c
	sta DYNAMIC_CHAR_ADDR_BOT+$0d
	sta DYNAMIC_CHAR_ADDR_BOT+$0e
	sta DYNAMIC_CHAR_ADDR_BOT+$0f

@handle_brkpts:
	ldy #$03
	cpx #SCREEN_HEIGHT-2
	bcs @botrows

	;----------------------------------------------------------------------
	; copy either the empty character (no breakpoint)
	; or the breakpoint character data if there is one
	lda mem::breakpoint_rows,x
	beq @empty
@breakpoint:
	lda @breakpoint_char,y
	sta DYNAMIC_CHAR_ADDR+$02,y
	lda @breakpoint_char+4,y
	sta DYNAMIC_CHAR_ADDR+$02+8,y
	dey
	bpl @breakpoint
	jmi @nextrow			; branch always
@empty:
	lda @empty_char,y
	sta DYNAMIC_CHAR_ADDR+$02,y
	lda @empty_char+4,y
	sta DYNAMIC_CHAR_ADDR+$02+8,y
	dey
	bpl @empty
	bmi @nextrow			; branch always

@botrows:
	cpx #SCREEN_HEIGHT-2
	bne :+
;------------------------------------------------------------------------------
; before the last row is drawn, we need to restore the screen codes for the
; bitmap (characters: $10f0-$10fa) - 60 cycles total
@restore_screen:
	lda #$7b
	sta $10f0
	lda #$87
	sta $10f1
	lda #$93
	sta $10f2
	lda #$9f
	sta $10f3
	lda #$ab
	sta $10f4
	lda #$b7
	sta $10f5
	lda #$c3
	sta $10f6
	lda #$cf
	sta $10f7
	lda #$db
	sta $10f8
	lda #$e7
	sta $10f9
	lda #$f3
	sta $10fa
	lda #$ff
	sta $10fb

:	lda mem::breakpoint_rows,x
	beq @empty_last
@breakpoint_last:
	lda @breakpoint_char,y
	sta DYNAMIC_CHAR_ADDR_BOT+$02,y
	lda @breakpoint_char+4,y
	sta DYNAMIC_CHAR_ADDR_BOT+$02+8,y
	dey
	bpl @breakpoint_last
	bmi @nextrow				; branch always
@empty_last:
	lda @empty_char,y
	sta DYNAMIC_CHAR_ADDR_BOT+$02,y
	lda @empty_char+4,y
	sta DYNAMIC_CHAR_ADDR_BOT+$02+8,y
	dey
	bpl @empty_last

	lda #$55
	sta $10fd
	sta $10fe
	sta $10ff

@nextrow:
	cpx #SCREEN_HEIGHT-1
	inc rowcnt
	bcc @ret

@main: ; reinstall main IRQ handler
	lda #$00
	sta rowcnt
	ldxy #sys_update
	stxy $0314
	lda #$00|$20		; disable T2 interrupts
	sta $912e
@ret:	rts

; NOTE: there are only 4 pixel rows each. the top and bottom rows of each char
;       are assumed to never be changed
@breakpoint_char:	.byte $7d,$7d,$7d,$7d
			.byte $7d,$7d,$7d,$7d
@empty_char:		.byte $55,$55,$55,$55
			.byte $55,$55,$55,$55
.endproc

.CODE

;*******************************************************************************
; IRQ OFF
; Turns off the interrupt until reenabled.  Disables all VIA generated interrupts
; and replaces the IRQ ($0314) with the KERNAL's default one
; CLOBBERS:
;  - .A
;  - $0314-$0315
;  - $912e, $911e
; Preserves .X, .Y, and the .C flag
.export __irq_off
.proc __irq_off
	sei

	; bit $9124; pla; tay; pla; tax; pla; rti
	lda #<$eb15
	sta $0314
	lda #>$eb15
	sta $0314+1

	lda prefs::normal_color
	sta $900f

	; disable all interrupts
	lda #$00|$7f
	sta $911e
	sta $912e

	cli
	rts
.endproc

;*******************************************************************************
; BRK HANDLER
.proc brk_handler
	lda #FINAL_BANK_MAIN
	SELECT_BANK_A
	jmp @brk

.PUSHSEG
.RODATA
@brk:	lda #$f0
	sta $9005

	lda #$18
	sta $9000
	lda #$40
	sta $9001

	lda #$2e
	sta $9003

	lda #20|$80
	sta $9002

	lda #$66|$08
	sta $900f

	ldx #$00
:	lda #$20
	sta $1e00,x
	sta $1f00,x
	lda #1
	sta $9600,x
	dex
	bne :-

	lda #$3a	; :
	sta $1e00
	lda #$28	; (
	sta $1e01

	; show BRK address on stack
	tsx
	lda $102,x
	jsr util::hextostr
	sty $1e00+20
	stx $1e00+21
	tsx
	lda $101,x
	jsr util::hextostr
	sty $1e00+22
	stx $1e00+23

	; show address of last procedure
	tsx
	lda $104,x
	jsr util::hextostr
	sty $1e00+(20*2)
	stx $1e00+(20*2)+1
	tsx
	lda $103,x
	jsr util::hextostr
	sty $1e00+(20*2)+2
	stx $1e00+(20*2)+3

	jmp *
.POPSEG
.endproc

;*******************************************************************************
; IRQ ON
; Syncs to the configured scanline and sets up an IRQ that will trigger whenever
; that location is reached.
.export __irq_on
.proc __irq_on
        sei
	ldxy #sys_update
	stxy $0314

	ldxy #brk_handler
	stxy $0316

	ldy #IRQ_START_LINE

	lda #<TIMER_VALUE
	sta $9124
@i0:	cpy $9004
	bne @i0
	iny
	iny
@i1:	cpy $9004
	bne @i1
	jsr @i6
	iny
	cpy $9004
	beq @i2
	nop
	nop
@i2:	jsr @i6
	nop
	iny
	cpy $9004
	beq @i3
	bit $24
@i3:	jsr @i6
	nop
	iny
	cpy $9004
	bne @i4
@i4:	ldx #$06	; position
@i5:	dex
	bne @i5

.ifndef PAL
	nop
	nop
.endif
	lda #>TIMER_VALUE
	sta $9125

	; enable T2 interrupts
	lda #$80|$40
	sta $912e

	cli
	rts

@i6:
.ifdef PAL
	ldx #$19	; delay
.else
	ldx #$17	; delay
.endif
@i7:	dex
	bne @i7
	nop
	rts
.endproc
