;*******************************************************************************
; BITMAP.ASM
; This file contains procedures for initializing and writing to the the bitmap
; display.  The bitmap is configured as 20 columns of 12 rows of double-height
; user-defined characters for a total of 40 columns and 24 rows of 4x8
; characters.
; This configuration is popularly known as MINIGRAFIK, created by Mike
;*******************************************************************************

.include "layout.inc"
.include "../expansion.inc"
.include "../prefs.inc"
.include "../fastcopy.inc"
.include "../../draw.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../ram.inc"
.include "../../settings.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

MAX_SHIFT = NUM_COLS

;*******************************************************************************
; CONSTANTS
BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

SCREEN_ADDR = $1000
PHYS_COLS   = 21	; number of PHYSICAL columns (breakpoints use one)
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

.CODE
;*******************************************************************************
; INIT
; Initializes the screen layout
.export __screen_init
.proc __screen_init
@dst=r0
@col=r2
	ldy #$00
	sty @dst
	ldx #$10		; $10 is also our initial value for bitmap char
	stx @dst+1
	stx @col

@l0:	; column 0 is character $0f for first 10 rows
	; on last row, it switches to $00, which will be dynamically modified
	; and also used for breakpoint rendering
	lda #$0f
	sta (@dst),y
	iny

	; set up one row of the bitmap
	lda @col
	ldx #PHYS_COLS-1
@l1:	sta (@dst),y
	clc
	adc #$0c
	iny
	dex
	bne @l1

	inc @col
	cpy #PHYS_COLS*SCREEN_ROWS-1
	bcc @l0

	; set leftmost column of last row to char 0
	lda #$00
	sta $10e7

	; configure VIC registers
	ldy #$05
@l2:	clc
	lda $ede4,y
	adc inittab,y
	sta $9000,y
	dey
	bpl @l2

	; set aux color
	lda #$02<<4
	sta $900e

	; clear the aux character
	ldx #$10
	lda #$55
:	sta $10f0-1,x
	dex
	bne :-

	lda prefs::normal_color
	sta $900f
	rts
.endproc

;*******************************************************************************
; CLR
; Clears the screen
.export __screen_clr
.proc __screen_clr
@bm=r0
	ldxy #$1100
	stxy @bm

	lda #$00
	tay
:	sta (@bm),y
	iny
	bne :-
	inc @bm+1
	ldx @bm+1
	cpx #>$2000
	bne :-

	; fall through to clrcolor
.endproc

;*******************************************************************************
; CLRCOLOR
; Reverts all color memory to the given color
; IN:
;  - .A: the color to fill the screen with
.export __screen_clrcolor
.proc __screen_clrcolor
	ldy #PHYS_COLS*SCREEN_ROWS
	ldx #PHYS_COLS
@l0:    lda prefs::text_color
	sta COLMEM_ADDR-1,y
	ora #$80
	dex
	bne :+
	; column 0, use multicolor mode
	ora #$08
	ldx #PHYS_COLS
:	sta COLMEM_ADDR-1,y
        dey
        bne @l0
        rts
.endproc

;*******************************************************************************
; CLR_PART
; Clears all rows below the given offset in every column of the bitmap
; IN:
;  - .A: the character row to start clearing at
.export __screen_clr_part
.proc __screen_clr_part
@screen=r0
@offset=r2
	asl
	asl
	asl
	sta @offset
	clc
	adc #<BITMAP_ADDR
	sta @screen
	lda #>BITMAP_ADDR
	sta @screen+1

	; get # of pixels to clear (192 - offset)
	lda #192
	sec
	sbc @offset
	sta @offset

	ldx #20			; number of columns

@l0:	ldy @offset
	dey
	lda #$00
;clear the character memory (bitmap)
@l1:    sta (r0),y
        dey
        bne @l1

	lda @screen
	clc
	adc #$c0
	sta @screen
	bcc :+
	inc @screen+1
:	dex
	bne @l0

        rts
.endproc

;*******************************************************************************
; BM CLRLINE
; Clears the given character row
; IN:
;  - .A: the row to clear
.export __screen_clrline
.proc __screen_clrline
@dst=r0
	jsr __screen_char_addr
	stx @dst
	sty @dst+1

	ldx #(SCREEN_WIDTH/2)
@l0:	ldy #$07
	lda #$00
@l1:	sta (@dst),y
	dey
	bpl @l1
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0
	rts
.endproc

;*******************************************************************************
; RVSLINE PART
; Reverses the given number of character (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
;  - .Y: the first column to reverse
;  - .X: the last column to reverse
.export __screen_rvsline_part
.proc __screen_rvsline_part
@dst=r0
@odd=r2		; !0 if the character to end at is odd
@start=r3
@stop2=r4
@start2=r5
	asl
	asl
	asl
	cpx #40
	bcc :+
	ldx #40

:	; swap Y and X if (X < Y)
	sty @start
	cpx @start
	pha		; save character row

	bcs :+		; if stop > start, swap
	txa
	ldx @start
	tay

:	sty @start2
	stx @stop2
	lda #$00
	sta @odd

	; check whether the start column is even/odd
	tya
	lsr
	sta @start
	ror @odd

	; get the first column to reverse
	tay
	pla
	adc __screen_columnslo,y
	sta @dst
	lda __screen_columnshi,y
	adc #$00
	sta @dst+1

	lda @odd
	beq @cont

@odd0:	; reverse right half of the first column
	ldy #$07
@col0:	lda (@dst),y
	eor #$0f
	sta (@dst),y
	dey
	bpl @col0

	; move to next column
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	inc @start	; first column is done
	inc @start2

	lda @start2
	cmp @stop2
	beq @done

@cont:	; divide character # by 2 to get bitmap column
	txa
	lsr
	tax

	; check if end column is even or odd
	lda #$00
	rol
	sta @odd

	cpx @start
	beq @lastcol
	bcc @done

@l0:	ldy #$07
@l1: 	lda (@dst),y
	eor #$ff
	sta (@dst),y
	dey
	bpl @l1
	lda @dst
	clc
	adc #$c0
	sta @dst
	lda @dst+1
	adc #$00
	sta @dst+1
	dex
	cpx @start
	bne @l0

	; check if we need to do the odd column
	lda @odd
	beq @done

@lastcol:
	; reverse half of the last column
	ldy #$07
@l2:	lda (@dst),y
	eor #$f0
	sta (@dst),y
	dey
	bpl @l2

@done:	rts
.endproc

;*******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer. It may then be restored with a call
; to scr::restore
.export __screen_save
.proc __screen_save
	CALL FINAL_BANK_VSCREEN, save

	; save colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_idx,x
	sta mem::rowcolors_save,x

	lda prefs::normal_color
	sta mem::rowcolors,x

	lda #COLOR_NORMAL
	sta mem::rowcolors_idx,x

	dex
	bpl :-

	jmp __screen_init
.endproc

;*******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
@buff=r0
@bm=r2
	CALL FINAL_BANK_VSCREEN, restore

	; restore the per-row colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors_idx,x
	dex
	bpl :-
	jmp draw::refresh_colors
.endproc

;*******************************************************************************
; CHAR ADDR
; Returns the bitmap address for the "character row" of the given row.
; Characters are 8 pixels tall, so this is BITMAP_ADDR+(8*row) where row is
; the provided row.
; IN:
;  - .A: the character row to get the bitmap address of
; OUT:
;  - .XY: the bitmap address
.export __screen_char_addr
.proc __screen_char_addr
	asl
	asl
	asl
	adc #<BITMAP_ADDR
	tax
	ldy #>BITMAP_ADDR
	rts
.endproc

.RODATA
;*******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

.export __screen_columnslo
.export __screen_columnshi
__screen_columnslo: .lobytes cols
__screen_columnshi: .hibytes cols

inittab:	.byte $00	; +$0c (PAL) +$05 (NTSC) $9000
		.byte $fe	; +$26 (PAL) +$19 (NTSC) $9001
		.byte $ff	; +$16                   $9002
		.byte $eb	; +$2e                   $9003
		.byte $00	; +$00                   $9004
		.byte $0c	; +$c0                   $9005

.segment "VSCREEN"

;*******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer. It may then be restored with a call
; to scr::restore
.proc save
@buff=r0
@bm=r2
	ldxy #backbuff
	stxy @buff
	ldxy #$1100
	stxy @bm

	; save bitmap to back-buffer
	ldy #$00
:	lda (@bm),y
	sta (@buff),y
	iny
	bne :-
	inc @bm+1
	inc @buff+1
	lda @bm+1
	cmp #>$2000
	bne :-
	rts
.endproc

;*******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; to scr::restore
.proc restore
@buff=r0
@bm=r2
	ldxy #backbuff
	stxy @buff
	ldxy #$1100
	stxy @bm

	; restore screen from the backbuff
	ldy #$00
:	lda (@buff),y
	sta (@bm),y
	iny
	bne :-
	inc @bm+1
	inc @buff+1
	lda @bm+1
	cmp #>$2000
	bne :-
	rts
.endproc

;*******************************************************************************
; SHL
; Shifts the CHARACTER data of the screen to the left.
; See SHR for documentation
; The bottom 2 rows are NOT shifted
.export __scr_shl
.proc __scr_shl
@src=r0
@dst=r2
	lda shiftamount
	cmp #MAX_SHIFT
	bcs @done

	ldxy #SCREEN_ADDR
	stxy @dst
	inx
	stxy @src

	ldx #NUM_ROWS

@l0:	ldy #$00
	lda (@dst),y
	pha

@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #NUM_COLS-1
	bne @l1

	; wrap character at column 0 to last column
	pla
	sta (@dst),y

	lda @dst
	adc #NUM_COLS-1	; .C always set
	sta @dst
	sta @src
	inc @src	; src = dst+1

	dex
	bne @l0

	inc shiftamount
@done:	rts
.endproc

;*******************************************************************************
; SHR
; Shifts the CHARACTER data of the screen to the right.  This means that the
; bitmap addresses for each column will shift by $c0
; So the default bitmap address arrangement:
; | $1100 | $11c0 |  ...  |
; will now be:
; | $1f40 | $1100 | $11c0 |  ...  |
; after the shift
; The bottom 2 character rows are NOT shifted
.export __scr_shr
.proc __scr_shr
@src=r0
@dst=r2
	lda shiftamount
	cmp #$01
	beq @done

	ldxy #SCREEN_ADDR
	stxy @src
	inx
	stxy @dst

	ldx #NUM_ROWS

@l0:	ldy #NUM_COLS-2
	lda (@dst),y	; save rightmost char
	pha

@l1:	lda (@src),y	; get char to shift
	sta (@dst),y	; shift it right
	dey
	bpl @l1

	; last character: wrap around
	iny
	pla		; get rightmost char
	sta (@src),y	; store it in leftmost position

	; move to the next row
	lda @src
	clc
	adc #NUM_COLS
	sta @src
	sta @dst
	inc @dst

	dex
	bne @l0

;--------------------------------------
; now copy the bitmap data in
	; get column of bitmap data to copy's address
	ldx shiftamount
	lda vcolumnslo,x
	sta @src
	lda vcolumnshi,x
	sta @src+1

	; get address to copy bitmap column to
	dec shiftamount
	ldx shiftamount
	lda bm_columnslo,x
	sta @dst
	lda bm_columnshi,x
	sta @dst+1

	; copy the column in
	ldy #NUM_ROWS*16-1
:	lda #$ff ;(@src),y
	sta (@dst),y
	dey
	bne :-

	; copy the last byte
	lda #$ff ;(@src),y
	sta (@dst),y

@done:	rts
.endproc

shiftamount: .byte NUM_COLS	; number of columns the screen is shifted

; these pointers are one less than the real addresses they reference
bmptr:	.word BITMAP_ADDR-1

vcolumnslo:
.repeat (VSCREEN_WIDTH/2), i
	.byte <(screen + ($c0*i))
.endrepeat

vcolumnshi:
.repeat VSCREEN_WIDTH/2, i
	.byte >(screen + ($c0*i))
.endrepeat

.linecont +
.define vcols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40, \
$1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

bm_columnslo: .lobytes vcols
bm_columnshi: .hibytes vcols

;*******************************************************************************
; RESTORE
; Initializes the screen using shiftamount to determine the layout
; IN:
;  - .A: the number of columns to shift the screen
;.proc restore
;@scr=r0
;@row=r2
;	lda #$00
;	sta @row
;	ldxy #SCREEN_ADDR
;	stxy @scr
;
;@l0:	lda shiftamount
;	beq @done
;	lda #NUM_COLS
;	sec
;	sbc shiftamount
;	tay
;
;	lda #$10
;	clc
;	adc @row
;
;	ldx #NUM_COLS
;@l1:	sta (@scr),y
;	iny
;	cpy #NUM_COLS
;	bcc :+
;	ldy #$00
;:	clc
;	adc #SCREEN_ROWS
;	dex
;	bne @l1
;
;	; move to next screen row
;	lda @scr
;	clc
;	adc #NUM_COLS
;	sta @scr
;
;	inc @row
;	lda @row
;	cmp #NUM_ROWS
;	bne @l0
;@done:	rts
;.endproc

.segment "VSCREEN_BSS"
;*******************************************************************************
; SCREEN
; The "virtual" screen.  This is a continuation of the bitmap at address $1000
screen:   .res $2000	; bitmap for 200 columns
backbuff: .res $f00	; backup for 1 bitmap screen
