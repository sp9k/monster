;*******************************************************************************
; SYMVIEW.ASM
; This file contains the code for the symbol viewer, which allows the user to
; see all (non-local/anonymous) symbols in their program.
; Using debug information, the user can navigate to the location of the symbol
;*******************************************************************************

.include "debuginfo.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "screen.inc"
.include "settings.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.include "ram.inc"

;*******************************************************************************
; CONSTANTS
HEIGHT     = SCREEN_HEIGHT-1

SORT_ALPHA = 0	; sort by label name alphabetically
SORT_ADDR  = 1	; sort by label address

;*******************************************************************************
; ZEROPAGE/MEMORY LOCATIONS
lbl      = zp::editortmp	; ID of the current line's label
addr     = zp::editortmp+2	; address corresponding to current line's label
filename = zp::tmp10		; the filename for the current line
line     = zp::tmp12		; the line number for the current line
sortby   = zp::tmp14		; the sort order (ALPHA, ADDR)
mode     = zp::tmp15		; mode (0=ZP, 1=ABS)
tmp      = zp::tmp16
name     = $100

;*******************************************************************************
.DATA
; these strings are modified depending on the address mode
; ESCAPE_BYTE replaces the ESCAPE_VALUE for zeropage symbols
sym_line:
.byte ESCAPE_VALUE_DEC, ESCAPE_GOTO, 5, "$", ESCAPE_VALUE, " ", ESCAPE_STRING, " ", ESCAPE_GOTO, 22, ESCAPE_STRING
.byte " ", "l:", ESCAPE_VALUE_DEC, 0
sym_line_no_file:
.byte ESCAPE_VALUE_DEC, ESCAPE_GOTO, 5, "$", ESCAPE_VALUE, " ", ESCAPE_STRING, 0

;*******************************************************************************
.RODATA
sort_by_name_msg: .byte "f1 sort by name",0
sort_by_addr_msg: .byte "f1 sort by addr",0

.CODE

;*******************************************************************************
; GET ITEM
; Returns the label ID at the given index based on the current sortby value.
; IN:
;   - .XY: the item index
; OUT:
;   - lbl:      the ID of the label at a given index
;   - addr:     the address of the label at the requested index
;   - name:     the name of the symbol
;   - filename: the name of the file containing the symbol (if any)
;   - line:     the line number that contains the symbol
;   - .XY:      the ID of the label at the given index (determined by sortby)
.proc get_item
	; destination buffer for getname
	lda #<$100
	sta r0
	lda #>$100
	sta r0+1

	lda sortby
	beq @sortalpha

@sortaddr:
	jsr lbl::idbyaddrindex	; lookup via sorted addresses

@sortalpha:
	stxy lbl		; store the ID for the label
	jsr lbl::getname	; read the symbol name into buffer ($100)
	ldxy lbl
	jsr lbl::addr_and_mode	; get the symbol address
	stxy addr
	sta mode

	; default filename to nothing
	lda #$00
	sta filename
	sta filename+1

	jsr dbgi::addr2line	; get file and line #
	bcs @done		; if no mapping, skip
				; (this will filter out constants)
	stxy line
	jsr dbgi::get_filename
	bcs @done
	stxy filename
@done:	rts
.endproc

;*******************************************************************************
; PRINT ITEM
; Prints the item at the given line.  The pointers are set by the most recent
; call to get_item
; IN:
;   - .A: the line to draw the item at
.proc print_item
@row=tmp+1
	sta @row

	ldxy #sym_line_no_file

	lda filename
	bne :+
	lda filename+1
	beq :++		; no filename

:	ldxy #sym_line
	lda line	; push line #
	pha
	lda line+1
	pha

	lda filename+1	; push filename
	pha
	lda filename
	pha

:	lda #>name	; push the symbol name (written by getname)
	pha
	lda #<name
	pha

	; check the mode
	lda addr
	pha
	lda mode
	bne @abs
@zp:	lda #ESCAPE_BYTE	; 1 byte (zeropage)
	bne :+			; branch always

@abs:	lda addr+1
	pha
	lda #ESCAPE_VALUE	; 2 bytes (absolute)
:	sta sym_line+4
	sta sym_line_no_file+4

@print: ; push the label's id
	lda lbl
	pha
	lda lbl+1
	pha

	lda @row
	jsr text::print
	rts
.endproc

;*******************************************************************************
; ENTER
; Enters the symbol viewer.
.export __symview_enter
.proc __symview_enter
@tmp=r5
@scroll=r7
@row=rb
@selection=tmp
	jsr scr::save
	lda #$00
	sta sortby
	sta @selection

	ldx #HEIGHT
	jsr draw::hiline	; highlight the bottom row

@start: lda #$00
	sta @scroll
	sta @scroll+1

@l0:	jsr edit::clear

	; if we are sorting by name, use the sort by addr msg
	; else sorting by addr -> use the sort by name msg
	ldxy #sort_by_addr_msg
	lda sortby
	cmp #SORT_ALPHA
	beq :+
	ldxy #sort_by_name_msg
:	lda #HEIGHT
	jsr text::print

	lda lbl::num
	ora lbl::num+1
	beq @done		; no labels

	lda #$00
	sta @row

@l1:	ldxy @scroll
	jsr get_item	; get the item for this row (@scroll)
	lda @row
	jsr print_item

	inc @row
	lda @row
	cmp #HEIGHT
	beq @done		; end of screen
@nextitem:
	incw @scroll
	ldxy @scroll
	cmpw lbl::num
	bne @l1
	decw @scroll

; the screen has been drawn, enter the main user loop
@done:  ; @scroll is now set to the index of the item at the bottom
@menu:	ldx @selection
	jsr draw::hiline	; highlight the current selection

@menuloop:
	jsr key::waitch		; wait for a key

	pha
	ldx @selection
	jsr draw::resetline
	pla

	jsr key::isdown
	beq @down
	jsr key::isup
	beq @up
	cmp #K_RETURN		; RETURN
	beq @select
	cmp #$85		; F1 (change sort order)
	beq @changesort
	cmp #K_QUIT		; <-
	bne @menu
	jmp scr::restore

@down:	inc @selection
	lda @selection
	cmp @row
	bcc @menu

	; if (scroll+1) <= lbl::num, don't allow scroll
	lda @scroll
	; sec
	adc #$00	; +1
	tax
	lda @scroll+1
	adc #$00
	tay
	cmpw lbl::num
	bcc @scrolldown

	; can't scroll, we're at the end of labels
	dec @selection
	jmp @menu

@scrolldown:
	stxy @scroll
	lda #$00
	sta @selection
	jmp @l0

@up:	dec @selection
	bpl @menu
	inc @selection
	lda @scroll+1
	bne @scrollup
	lda @scroll
	cmp #HEIGHT
	bcc @menu

@scrollup:
	; @scroll -= (@row + HEIGHT-1)
	lda @scroll
	sec
	sbc @row
	bcs :+
	dec @scroll+1
:	sbc #HEIGHT-1
	sta @scroll
	bcs :+
	dec @scroll+1

:	; set selected row to last row on screen
	lda #HEIGHT-1
	sta @selection
	jmp @l0

@select:
	jsr scr::restore

	lda @row
	clc			; subtract an extra 1
	sbc @selection
	sta @selection

	lda @scroll
	sec
	sbc @selection
	tax
	lda @scroll+1
	sbc #$00
	tay
	jsr get_item
	ldxy addr
@exit:	jmp dbg::gotoaddr	; go to the line of the symbol definition

@changesort:
	; toggle sort order from alpha to addr or vise-versa
	lda sortby
	eor #$01
	sta sortby
	jmp @start
.endproc
