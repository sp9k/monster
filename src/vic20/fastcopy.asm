.include "expansion.inc"
.include "../macros.inc"
.include "../ram.inc"
.include "../screen.inc"
.include "../zeropage.inc"

.import TRAMPOLINE_ADDR

RETURN_ADDR     = TRAMPOLINE_ADDR-1
JMP_RETURN_ADDR = RETURN_ADDR-5

.export prog1000
.export prog9000
.export prog9110
.export prog9400
.export dbg9000
.export dbg9400

.ifdef ultimem
.segment "SIMRAM_INTERNAL"
.else
.SEGMENT "FASTCOPY_BSS"
.endif

;*******************************************************************************
; PROG
; backup for the user's program during debug
progsave:
; PROG1000 (SCREEN)
prog1000: .res $1000		; $1000-$2000

; PROG9000, PROG91000 (VIC and VIAs)
.ifdef ultimem
.segment "SIMRAM_IO"
.endif

prog9000: .res $10		; $9000-$9010

.ifdef ultimem
.res $9110-$9010		; padding between the VIC and VIA images
.endif
prog9110: .res $20		; $9110-$9130

; PROG9400 (COLOR RAM)
.ifdef ultimem
.res $9400-$9130		; padding between VIAs and color RAM
.endif
prog9400: .res $400		; $9400-$9800

;*******************************************************************************
; DBG
; backup for debugger/editor memory
; we back up less for debug because we can just re-init some state
.ifdef ultimem
.segment "SIMDBG"
.endif
dbg1000: .res $1000	; $1000-$2000
dbg9000: .res $10	; $9000-$9010
dbg9400: .res $400	; $9400-$9800

.CODE

;*******************************************************************************
; SAVE DEBUG STATE
; Saves the state of the debugger's zeropage
.export __fastcopy_save_debug_state
.proc __fastcopy_save_debug_state
	JUMP FINAL_BANK_FASTCOPY, save_debug_state
.endproc

;*******************************************************************************
; SAVE PROG STATE
; Saves memory clobbered by the debugger (screen, VIC registers and color)
.export __fastcopy_save_prog_state
.proc __fastcopy_save_prog_state
	JUMP FINAL_BANK_FASTCOPY, save_prog_state
.endproc

;*******************************************************************************
; SAVE PROG VISUAL
; Saves the visible program state (screen, VIC and color)
.export __fastcopy_save_prog_visual
.proc __fastcopy_save_prog_visual
	JUMP FINAL_BANK_FASTCOPY, save_prog_visual
.endproc

;*******************************************************************************
; RESTORE PROG STATE
; Restores the saved program state
.export __fastcopy_restore_prog_state
.proc __fastcopy_restore_prog_state
	JUMP FINAL_BANK_FASTCOPY, restore_prog_state
.endproc

;*******************************************************************************
; RESTORE PROG VISUAL
; Restores the user program state that affects the screen ($1000-$2000 and
; the VIC registers at $9000-$9010)
.export __fastcopy_restore_prog_visual
.proc __fastcopy_restore_prog_visual
	JUMP FINAL_BANK_FASTCOPY, restore_prog_visual
.endproc

;*******************************************************************************
; RESTORE DEBUG STATE
; Restores the saved debugger state
.export restore_debug_visual
restore_debug_visual:
.export __fastcopy_restore_debug_state
.proc __fastcopy_restore_debug_state
	JUMP FINAL_BANK_FASTCOPY, restore_debug_state
.endproc

.ifdef ultimem
.segment "SIM"
.else
.SEGMENT "FASTCOPY"
.endif

;*******************************************************************************
; RESTORE DEBUG STATE
; Restores the saved debugger state
.export restore_debug_state
.proc restore_debug_state
@vicsave=dbg9000
@colorsave=dbg9400
@src=r0
@dst=r2
	; disable NMI/IRQs (we will be clobbering their vectors)
	lda #$7f
	sta $911e
	sta $912e

	ldy #$10
:	lda @vicsave-1,y
	sta $9000-1,y
	dey
	bne :-

	; blank screen while we draw the bitmap.
	sty $9003		; $9003 = 0 (# of rows)

; save $9400-$9500
:	lda @colorsave,y
	sta $9400,y
	lda @colorsave+$100,y
	sta $9500,y
	lda @colorsave+$200,y
	sta $9600,y
	lda @colorsave+$300,y
	sta $9700,y
	dey
	bne :-
	sty @dst

; restore $1000-$2000
	lda #>dbg1000
	sta @src+1
	lda #<dbg1000
	sta @src
	lda #$10
	sta @dst+1		; start from $1000

:	lda (@src),y
	sta (@dst),y
	dey
	bne :-
	inc @src+1		; next page
	inc @dst+1
	lda @dst+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	; reinit the bitmap and return
	JUMPMAIN scr::init
.endproc

;*******************************************************************************
; SAVE DEBUG STATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.export save_debug_visual
save_debug_visual:
.export save_debug_state
.proc save_debug_state
@vicsave=dbg9000
@colorsave=dbg9400
@src=r0
@dst=r2
	ldy #$10
@savevic:
	lda $9000-1,y
	sta @vicsave-1,y
	dey
	bne @savevic

@savecolor:
	lda $9400,y
	sta @colorsave,y
	lda $9400+$100,y
	sta @colorsave+$100,y
	lda $9400+$200,y
	sta @colorsave+$200,y
	lda $9400+$300,y
	sta @colorsave+$300,y
	dey
	bne @savecolor

	sty @src

; save $1000-$2000
@savescreen:
	lda #>dbg1000
	sta @dst+1
	lda #<dbg1000
	sta @dst
	lda #$10
	sta @src+1		; start from $1000

:	lda (@src),y
	sta (@dst),y
	dey
	bne :-
	inc @src+1		; next page
	inc @dst+1
	lda @src+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	rts
.endproc

;*******************************************************************************
; RESTORE PROG STATE
; restores the saved program state
.export restore_prog_state
.proc restore_prog_state
	; restore VIA2 ($9120-$9130)
	ldx #$10
:	lda prog9110+$10-1,x
	sta $9120-1,x
	dex
	bne :-

	; fall through to RESTORE PROG VISUAL
.endproc

;*******************************************************************************
; RESTORE PROG VISUAL
.proc restore_prog_visual
; restore $9000-$9010
@src=r0
@dst=r2
; restore VIC state
	ldy #$10
:	lda prog9000-1,y
	sta $9000-1,y
	dey
	bne :-

	; set LSB of @dst to 0
	sty @dst

; restore $1000-$2000
	lda #>prog1000
	sta @src+1
	lda #<prog1000
	sta @src
	lda #$10
	sta @dst+1		; start from $1000

:	lda (@src),y
	sta (@dst),y
	dey
	bne :-

	inc @src+1		; next page
	inc @dst+1
	lda @dst+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

; restore $9400-$9800
	lda #>prog9400
	sta @src+1
	lda #<prog9400
	sta @src
	lda #$94
	sta @dst+1
@restorecolor:
	lda (@src),y
	sta (@dst),y
	dey
	bne @restorecolor
	inc @src+1
	inc @dst+1
	lda @dst+1
	cmp #$98
	bne @restorecolor
	rts
.endproc

;*******************************************************************************
; SAVE PROG MEM
; Saves the program memory clobbered by the debugger: the screen ($1000-$2000)
; and color RAM ($9400-$9800)
.proc save_prog_mem
@src=r0
@dst=r2
; save $1000-$2000
@savescreen:
	lda #$10
	sta @src+1		; start from $1000
	lda #>prog1000
	sta @dst+1
	lda #<prog1000
	sta @dst

	ldy #$00
	sty @src

:	lda (@src),y
	sta (@dst),y
	dey
	bne :-

	inc @src+1		; next page
	inc @dst+1
	lda @src+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

; save $9400-$9800
	lda #$94
	sta @src+1
	lda #>prog9400
	sta @dst+1
	lda #<prog9400
	sta @dst
@savecolor:
	lda (@src),y
	sta (@dst),y
	dey
	bne @savecolor
	inc @src+1
	inc @dst+1
	lda @src+1
	cmp #$98
	bne @savecolor
	rts
.endproc

;*******************************************************************************
; SAVE PROG VISUAL
; Saves the visible state (screen, color and VIC)
.export save_prog_visual
.proc save_prog_visual
	jsr save_prog_mem
	jmp save_vic_state
.endproc

;*******************************************************************************
; SAVE PROG STATE
; Saves memory clobbered by the debugger (screen, color, VIC and VIA)
.export save_prog_state
.proc save_prog_state
	jsr save_prog_mem
	ldx #$10
:	lda $9120-1,x
	sta prog9110+$10-1,x
	dex
	bne :-

	; fall through to save_vic_state
.endproc

;*******************************************************************************
; SAVE VIC STATE
; Saves the VIC
.proc save_vic_state
@vicsave=prog9000
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic
	rts
.endproc
