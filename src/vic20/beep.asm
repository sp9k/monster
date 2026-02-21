.include "../macros.inc"
.include "settings.inc"

.BSS
;******************************************************************************
beep_tmr: .byte 0

.CODE
;******************************************************************************
; BEEP_SHORT
; Plays a short beep. Call beep::update to run the sounds player
; Because beeps are typically returned for errors, the .C flag is returned SET
; OUT:
;   - .C: set
.export __beep_short
.proc __beep_short
	lda #SHORT_BEEP_DURATION
	skw
	; fallthrough
.endproc

;******************************************************************************
; BEEP_LONG
; Plays a long beep. Call beep::update to run the sounds player
; Because beeps are typically returned for errors, the .C flag is returned SET
; OUT:
;   - .C: set
.export __beep_long
.proc __beep_long
	lda #LONG_BEEP_DURATION
	sta beep_tmr

	lda #BEEP_VOL
	ora $900e
	sta $900e

	lda #BEEP_TONE
	sta $900b
	sec
	rts
.endproc

.ifdef ultimem
.segment "BANKCODE"
.endif
;******************************************************************************
; BEEP_UPDATE
; Runs one step of the sound player. Call this every frame if you are playing
; sounds.
.export __beep_update
.proc __beep_update
	lda beep_tmr
	beq @done
	dec beep_tmr
	bne @done

	; turn off volume / clear aux color (not used by editor)
	lda #$00
	sta $900e

@done:	rts
.endproc
