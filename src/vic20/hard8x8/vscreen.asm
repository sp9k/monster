;*******************************************************************************
; VSCREEN.ASM
; Off-screen backup of the physical character matrix for the hard8x8 port.
;*******************************************************************************

.include "layout.inc"
.include "../../zeropage.inc"
.import __vscreen_rowslo, __vscreen_rowshi

SCREEN_ADDR = $1800			; base of the physical character matrix
SCREEN_SIZE = PHYS_COLS * SCREEN_HEIGHT	; whole matrix (gutter column included)

src = r0
dst = r2

;*******************************************************************************
.segment "VSCREEN_BSS"
backup: .res SCREEN_SIZE

;*******************************************************************************
.segment "VSCREEN"

;*******************************************************************************
; SAVE
; Copies the physical character matrix into the off-screen backup buffer.
.export __vscreen_save
.proc __vscreen_save
	lda #<SCREEN_ADDR
	sta src
	lda #>SCREEN_ADDR
	sta src+1
	lda #<backup
	sta dst
	lda #>backup
	sta dst+1
	jmp copy
.endproc

;*******************************************************************************
; RESTORE
; Copies the off-screen backup buffer back to the physical character matrix.
.export __vscreen_restore
.proc __vscreen_restore
	lda #<backup
	sta src
	lda #>backup
	sta src+1
	lda #<SCREEN_ADDR
	sta dst
	lda #>SCREEN_ADDR
	sta dst+1
	; fall through to copy
.endproc

;*******************************************************************************
; COPY
; Copies SCREEN_SIZE bytes from (src) to (dst).
.proc copy
	ldy #$00
	ldx #>SCREEN_SIZE	; number of whole pages to copy
	beq @rem
@page:	lda (src),y
	sta (dst),y
	iny
	bne @page
	inc src+1
	inc dst+1
	dex
	bne @page

@rem:	ldx #<SCREEN_SIZE	; remaining bytes (< 256)
	beq @done
	ldy #$00
@l0:	lda (src),y
	sta (dst),y
	iny
	dex
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; RESTORE ROW
; Restores a row's content columns, retaining the current gutter marker
; IN:
;   - .A: screen row to restore
.export __vscreen_restore_row
.proc __vscreen_restore_row
	jsr row_ptrs
	ldx #$01
@swap:	lda src,x
	ldy dst,x
	sta dst,x
	sty src,x
	dex
	bpl @swap
	jmp copy_row
.endproc

;*******************************************************************************
; SAVE ROW
; Saves a row's content columns in the existing full-screen backup
; IN:
;   - .A: screen row to save
.export __vscreen_save_row
.proc __vscreen_save_row
	jsr row_ptrs
	; fall through to copy_row
.endproc

;*******************************************************************************
; COPY ROW
; Copies the content columns between a screen row and its backup
; IN:
;   - r0: source address
;   - r2: destination address
.proc copy_row
	ldy #SCREEN_WIDTH-1
@byte:	lda (src),y
	sta (dst),y
	dey
	bpl @byte
	rts
.endproc

;*******************************************************************************
; ROW PTRS
; Locates a row's content columns in the screen and backup buffer
; IN:
;   - .A: screen row
; OUT:
;   - r0: screen address
;   - r2: backup address
.proc row_ptrs
	tax
	lda __vscreen_rowslo,x
	sta src
	clc
	adc #<(backup-SCREEN_ADDR)
	sta dst
	lda __vscreen_rowshi,x
	sta src+1
	adc #>(backup-SCREEN_ADDR)
	sta dst+1
	rts
.endproc
