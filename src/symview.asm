;*******************************************************************************
; SYMVIEW.ASM
; This file contains the code for the symbol viewer, which allows the user to
; see all (non-local/anonymous) symbols in their program.
; The user can navigate to the definition location stored on each symbol.
;*******************************************************************************

.include "debuginfo.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "fp.inc"
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
.ifdef fe3
.segment "FE3CONST"
.endif
; these strings are modified depending on the address mode
; ESCAPE_BYTE replaces the ESCAPE_VALUE for zeropage symbols
sym_line:
.byte ESCAPE_VALUE_DEC, ESCAPE_GOTO, 5, "$", ESCAPE_VALUE, ESCAPE_GOTO, $b, ESCAPE_STRING, " ", ESCAPE_GOTO, 22, ESCAPE_STRING
.byte " ", "l:", ESCAPE_VALUE_DEC, 0
sym_line_no_file:
.byte ESCAPE_VALUE_DEC, ESCAPE_GOTO, 5, "$", ESCAPE_VALUE, ESCAPE_GOTO, $b, ESCAPE_STRING, 0
.ifdef vic20
sym_line_float:
.byte ESCAPE_VALUE_DEC, ESCAPE_GOTO, 5, ESCAPE_STRING, " = ", ESCAPE_STRING, 0
.endif

;*******************************************************************************
.RODATA
sort_by_name_msg: .byte "f1 sort by name",0
sort_by_addr_msg: .byte "f1 sort by addr",0

.CODE
.ifdef fe3
; The formats live in MAIN on FE3. Patch them in that bank, which is also
; where text::render reads them; EXPR's BLK3 is a different physical RAM.
.proc set_value_format
	sta sym_line+4
	sta sym_line_no_file+4
	rts
.endproc
.endif

;*******************************************************************************
; MAIN-bank entry point
.export __symview_enter

.if .defined(CART) .and .defined(c64)
__symview_enter: JUMP FINAL_BANK_DBGUI, enter
.else
__symview_enter = enter
.endif

BANKED_CODE "DBGUI", FINAL_BANK_DBGUI

.ifdef vic20
get_item:	JUMP FINAL_BANK_EXPR, get_item_impl
print_item:	JUMP FINAL_BANK_EXPR, print_item_impl
.pushseg
.segment "EXPR"
SET_CUR_BANK FINAL_BANK_EXPR
.else
get_item   = get_item_impl
print_item = print_item_impl
.endif

;*******************************************************************************
; GET ITEM
; Returns the label ID at the given index based on the current sortby value.
; IN:
;   - .XY: the item index
; OUT:
;   - lbl:      ID of the label at a given index
;   - addr:     address of the label at the requested index
;   - name:     pointer to name of the symbol
;   - filename: pointer to name of the file containing the symbol (if any)
;   - line:     line number that contains the symbol
;   - .XY:      ID of the label at the given index (determined by sortby)
;   - $100:     buffer containing symbol name
.proc get_item_impl
@namebuff=$100
	lda sortby
	beq @sortalpha

@sortaddr:
	CALLMAIN lbl::idbyaddrindex	; lookup via sorted addresses
	jmp @getinfo

@sortalpha:
	CALLMAIN lbl::id_by_alpha_index

@getinfo:
	stxy lbl			; store the ID for the label

	; destination buffer for getname
	lda #<$100
	sta r0
	sta filename		; default filename to nothing
	sta filename+1

	lda #>$100
	sta r0+1
	CALLMAIN lbl::getname	; read the symbol name into buffer ($100)

	ldxy lbl
	CALLMAIN lbl::get_line	; definition location, independent of symbol value
	stxy line
	cpx #$00
	bne @getfile
	cpy #$00
	beq @value		; no definition location
@getfile:
	CALLMAIN dbgi::get_filename
	bcs @value
	stxy filename
@value:
	ldxy lbl
.if FP_SUPPORTED
	CALLMAIN lbl::getsegment
	cmp #SEG_FLOAT
	bne @address
	ldxy lbl
	CALLMAIN lbl::getaddr

	jsr expr::fconst_get
	bcs @done
	lda #$02
	sta mode
	rts
@address:
	ldxy lbl
.endif
	CALLMAIN lbl::addr_and_mode	; get the symbol address
	stxy addr
	sta mode

@done:	rts
.endproc

;*******************************************************************************
; PRINT ITEM
; Prints the item at the given line.  The pointers are set by the most recent
; call to get_item
; IN:
;   - .A: the line to draw the item at
.proc print_item_impl
@row=r0
	sta @row
.ifdef vic20
	lda mode
	cmp #$02		; float?
	bne @integer		; if not, format as integer

	; format the value as a float
	jsr expr::float_format
	lda #>expr::floatstr
	pha
	lda #<expr::floatstr
	pha
	lda #>name
	pha
	lda #<name
	pha
	ldxy #sym_line_float
	jmp @print
@integer:
.endif

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
:
.ifdef fe3
	CALLMAIN set_value_format
.else
	sta sym_line+4
	sta sym_line_no_file+4
.endif

@print: ; push the label's id
	lda lbl
	pha
	lda lbl+1
	pha

	RENDER_STR		; .XY = the rendered line
	lda @row
	CALLMAIN text::print
	rts
.endproc

.ifdef vic20
.popseg
SET_CUR_BANK FINAL_BANK_DBGUI
.endif

;*******************************************************************************
; ENTER
; Enters the symbol viewer.
.proc enter
@scroll    = r8
@row       = tmp
@selection = tmp+1
.ifdef vic20
	jsr scr::savebuf
.else
	jsr scr::save
.endif
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
	CALLMAIN text::print

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
	pla			; restore key

	cmp #$85		; F1 (change sort order)
	bne :+
@changesort:
	; toggle sort order from alpha to addr or vise-versa
	lda sortby
	eor #$01
	sta sortby
	jmp @start

:	jsr key::isdown
	beq @down
	jsr key::isup
	beq @up
	cmp #K_RETURN		; RETURN
	beq @select
	cmp #K_QUIT		; RUN/STOP
	beq @quitview
	cmp #K_WIN_CLOSE	; C= + q (dismisses the viewer, as with windows)
	bne @menu		; unrecognized key
@quitview:
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
	sec
:	sbc #HEIGHT-1
	sta @scroll
	bcs :+
	dec @scroll+1

:	; set selected row to last row on screen
	lda #HEIGHT-1
	sta @selection
	jmp @l0

@select:
	lda lbl::num
	ora lbl::num+1
	bne :+
	jmp scr::restore		; nothing to select in an empty table
:
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
	ldxy lbl
	jmp dbg::gotolabel	; go to the stored definition, including constants
.endproc
