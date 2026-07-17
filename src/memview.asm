;*******************************************************************************
; VIEW.ASM
; This file contains the code for the memory viewer/editor.  This editor is
; invoked via the debugger and allows the user to inspect memory or change its
; contents through a visual interface.
; It is implemented as a CUSTOM window (see gui.asm): the window manager
; calls back into this file to draw the view and to interact with it.
;*******************************************************************************

.include "beep.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "flags.inc"
.include "gui.inc"
.include "guis.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "settings.inc"
.include "strings.inc"
.include "text.inc"
.include "ui.inc"
.include "util.inc"
.include "watches.inc"
.include "vmem.inc"
.include "zeropage.inc"


;*******************************************************************************
; CONSTANTS

.ifdef hard8x8
BYTES_TO_DISPLAY=4
COL_START = 5
.else
BYTES_TO_DISPLAY=8
COL_START = 7
.endif
COL_STOP  = COL_START+(3*BYTES_TO_DISPLAY)-1

DEFAULT_HEIGHT = MEMVIEW_STOP-MEMVIEW_START-1

; offset of the address within the title string ("memory[$....]")
TITLE_ADDR_START = 8

.BSS
;*******************************************************************************
.export __view_addr
__view_addr:
memaddr: .word 0

wintop: .byte 0		; first (top) row of the view's contents
winbot: .byte 0		; last (bottom) row of the view's contents

.RODATA
;*******************************************************************************
; WINDOW
; The window descriptor for the memory viewer
window:
.byte GUI_MEMVIEW		; id for the memory viewer
.byte GUI_CLASS_CUSTOM
.byte DEFAULT_HEIGHT		; initial height
.byte 2				; min height
.byte 12			; max height
.word strings::memview_title	; title
.word windraw			; draw handler
.word enter			; enter handler
.word 0				; unused

.CODE
;*******************************************************************************
; EDIT
; Opens the memory editor window and gives it focus
.export __view_edit
.proc __view_edit
	ldxy #window
	jmp gui::open
.endproc

;*******************************************************************************
; SELECT
; Makes the memory editor the active window without giving it focus.
; The window manager will draw it and (if focus is being handed over via
; GUI_RET_SWITCH) interact with it.
.export __view_select
.proc __view_select
	ldxy #window
	jmp gui::select
.endproc

;*******************************************************************************
; SETBOUNDS
; Limits the cursor to the view's contents
.proc setbounds
	ldx #COL_START
	ldy wintop
	jsr cur::setmin

	ldy winbot
	iny
	ldx #COL_STOP
	jmp cur::setmax
.endproc

;*******************************************************************************
; ENTER
; The window manager's "enter" handler: interacts with the memory editor
; until the user exits it
; IN:
;   - .A: the first row of the view's contents
;   - .X: the last row of the view's contents
; OUT:
;   - .A: the GUI_RET_x code for the manager
.proc enter
@dst=r0
@odd=r4
@dstoffset=r6
@src=r8
	sta wintop
	stx winbot
	jsr setbounds

	ldy wintop
	ldx #COL_START
	jsr cur::set

	lda #TEXT_REPLACE
	sta text::insertmode

; until user exits, get input and update memory
@edit:	jsr cur::on
	ldxy memaddr
	stxy @src

	jsr key::waitch
	pha
	jsr cur::off
	pla

	cmp #K_UP_ARROW
	bne :+
	jsr getset_addr
	jsr setbounds		; restore the cursor's bounds
	jsr __view_refresh	; redraw at the new address
	jmp @edit

:	cmp #K_QUIT
	beq @quit
	cmp #K_CLOSE_WINDOWS	; <- (done)
	bne @chkcycle
@quit:	jmp @done

@chkcycle:
	cmp #K_SWAP_WINS
	bne :+
	lda #GUI_RET_CYCLE	; activate the next window
	rts

:	cmp #K_WIN_GROW
	bne :+
	jsr gui::grow
	jmp @resize

:	cmp #K_WIN_MAXIMIZE
	bne :+
	jsr gui::maximize
	jmp @resize

:	cmp #K_WIN_SHRINK
	bne :+
	jsr gui::shrink
@resize:
	jsr setbounds
	; move the cursor back in bounds if the window shrank
	lda zp::cury
	cmp wintop
	bcs @edit
	ldy wintop
	ldx zp::curx
	jsr cur::set
	jmp @edit

:	jsr key::isup
	bne :+
@up:	jsr up
	jmp @edit

:	jsr key::isdown
	bne :+
@down:	jsr down
	jmp @edit

:	jsr key::isleft	; h or left
	bne :+
@retreat:
	jsr @prev_x
	jmp @edit

:	jsr key::isright
	bne :+
@right: jsr @next_x
	jmp @edit

:	cmp #K_FIND
	bne :+
	jmp @find

:	jsr key::ishex
	bcs @replace_val
	cmp #K_SET_WATCH
	beq @setwatch
	jmp @edit

@setwatch:
	jsr get_addr	; get the address of the byte under the cursor
	stxy r0		; also set as STOP address to this address
	txa
	pha
	tya
	pha

	lda #WATCH_STORE
	jsr watch::add

	ldxy #strings::watch_added
	lda #DEBUG_MESSAGE_LINE
	jsr text::print

	jsr beep::short	; beep to confirm add
	jmp @edit

@done:	lda #GUI_RET_QUIT
	rts

@replace_val:
	jsr @set_nybble	; replace the nybble under cursor
	jsr @next_x	; advance the cursor (if we can)
	jsr __view_refresh
	jmp @edit

;--------------------------------------
; get the address of the memory at the cursor position
@set_nybble:
	jsr util::chtohex
	pha

	; get the base address for the row that the cursor is on
	lda zp::cury
	sec
	sbc wintop
	asl		; *8 (each row is 8 bytes)
	asl
	asl
	adc @src
	sta @dst
	lda @src+1
	adc #$00
	sta @dst+1

	; get the offset from the row's base address using the curor's x pos
	; the offset is calcuated by: (zp::curx - COL_START) / 3
	ldy #$ff
	lda zp::curx
	sec
	sbc #COL_START
:	iny
	sbc #$03	; -3 (bytes are 3 cursor positions apart)
	bpl :-
	sty @dstoffset

	; get odd/even cursor column
	lda zp::curx
	and #$01
	sta @odd
	; bytes alternate odd/even columns for hi/lo nybble
	tya
	and #$01
	eor @odd
	beq @lownybble

;--------------------------------------
@hinybble:
	ldxy @dst
	lda @dstoffset
	jsr vmem::load_off

	and #$0f
	sta @odd
	pla
	asl
	asl
	asl
	asl
	ora @odd
	bcc @store	; branch always

;--------------------------------------
@lownybble:
	ldxy @dst
	lda @dstoffset
	jsr vmem::load_off

	and #$f0
	sta @odd
	pla
	ora @odd
@store:
	sta zp::bankval
	ldxy @dst
	lda @dstoffset
	jmp vmem::store_off

;--------------------------------------
; move cursor to the next x-position
@next_x:
	ldx zp::curx
@next_x2:
	inx
	txa
	ldy #@num_x_skips-1
:	cmp @x_skips,y
	beq @next_x2
	dey
	bpl :-
	ldy zp::cury
	jmp cur::set

;--------------------------------------
; move cursor to the previous x-position
@prev_x:
	ldx zp::curx
@prev_x2:
	dex
	txa
	ldy #@num_x_skips-1
:	cmp @x_skips,y
	beq @prev_x2
	dey
	bpl :-
	ldy zp::cury
	jmp cur::set

;--------------------------------------
; table of columns to skip in cursor movement
.PUSHSEG
.RODATA
@x_skips:
	.byte COL_START+2
	.byte COL_START+5
	.byte COL_START+8
	.byte COL_START+11
	.byte COL_START+14
	.byte COL_START+17
	.byte COL_START+20
@num_x_skips=*-@x_skips
.POPSEG

;--------------------------------------
@find:	pushcur
@len=r0
	lda #$00
	sta cur::minx
	sta zp::curx
	lda #EDITOR_HEIGHT-1
	sta zp::cury
	lda #CUR_NORMAL
	sta cur::mode
	lda #TEXT_INSERT
	sta text::insertmode

	ldxy #key::getch
	jsr edit::gets		; get the string to parse
	sta @len		; save the string len; 1-2: byte, >2: word

	popcur
	lda #CUR_SELECT
	sta cur::mode
	lda #TEXT_REPLACE
	sta text::insertmode

	jsr util::parsehex	; parse the user's given hex string
	bcs @find		; if invalid hex, retry
	lda @len
	cmp #$03
	bcs @word		; 3-4 characters -> find a word
	txa
	ldxy memaddr
	jsr find_byte		; find byte
	jmp @cont
@word:	jsr find_word		; find the word we're looking for
@cont:	bcs @reset
	stxy memaddr		; set address of word to memaddr
@reset:	jsr gui::refresh	; redraw (the find prompt may have hit a row)
	jsr setbounds		; restore the cursor's bounds
	jmp @edit
.endproc

;*******************************************************************************
; UP
; Handles the Up key, moving the cursor or scrolling if needed
.proc up
	; are we at the top of the editor?
	lda zp::cury
	cmp wintop
	bne :+

	; we're at the top, scroll
	lda memaddr
	sec
	sbc #$08	; # of bytes per row
	sta memaddr
	bcs @done
	dec memaddr+1
@done:	jmp __view_refresh	; refresh the display

:	dec zp::cury
	rts
.endproc

;*******************************************************************************
; DOWN
; Handles the Down key, moving the cursor or scrolling if needed
.proc down
	; are we at the bottom of the editor?
	lda zp::cury
	cmp winbot
	bcc :+

	; we're at the bottom, scroll
	lda memaddr
	clc
	adc #$08	; # of bytes per row
	sta memaddr
	bcc @done
	inc memaddr+1
@done:	jmp __view_refresh	; refresh the display

:	inc zp::cury
	rts
.endproc

;*******************************************************************************
; GETSET ADDR
; Gets an address from the user (as input in the memory title area) and updates
; the memory view to render that area of memory.
.proc getset_addr
	pushcur

	; copy title to linebuffer
	ldx #TITLE_ADDR_START-2
:	lda strings::memview_title,x
	sta mem::linebuffer,x
	dex
	bpl :-

	; clear the existing value
	lda #']'
	sta mem::linebuffer+TITLE_ADDR_START+4
	lda #$00
	sta mem::linebuffer+TITLE_ADDR_START+5

	lda #'$'
	sta mem::linebuffer+TITLE_ADDR_START-1

	; set bounds for the input
	lda #TITLE_ADDR_START
	sta cur::minx
	sta zp::curx
	lda #TITLE_ADDR_START+4
	sta cur::maxx

	; edit the address on the title row (directly above the contents)
	ldx wintop
	dex
	stx zp::cury

	ldxy #key::gethex
	jsr edit::gets

	ldxy #mem::linebuffer+TITLE_ADDR_START-1
	stxy zp::line
	jsr expr::eval
	bcs :+			; on invalid input, leave address unchanged
	stxy memaddr
:	popcur
	rts
.endproc

;*******************************************************************************
; REFRESH
; Redraws the contents of the memory view at its current position
.export __view_refresh
.proc __view_refresh
	lda wintop
	ldx winbot

	; fall through to draw
.endproc

;*******************************************************************************
; WINDRAW
; The window manager's draw handler: displays the contents of memory in the
; given rows, beginning with the address in memaddr.
; Also renders the current address into the window's title string.
; IN:
;   - .A: the first row to draw at
;   - .X: the last row to draw at
.proc windraw
@src=ra
@row=rd
	sta wintop
	stx winbot
	sta @row

	; render the current address into the title
	lda memaddr
	sta @src
	jsr util::hextostr
	stx strings::memview_title+TITLE_ADDR_START+3
	sty strings::memview_title+TITLE_ADDR_START+2

	lda memaddr+1
	sta @src+1
	jsr util::hextostr
	stx strings::memview_title+TITLE_ADDR_START+1
	sty strings::memview_title+TITLE_ADDR_START

@l0:	ldxy @src
	jsr ui::memline

	lda @row
	jsr text::print		; draw the row of rendered bytes
	ldx @row
	jsr draw::resetline

	; (ui::memline advanced @src to the next row's address)
	inc @row
	lda @row
	cmp winbot
	bcc @l0			; have we drawn all rows?
	beq @l0
	rts
.endproc

;*******************************************************************************
; GET_ADDR
; Gets the address of the byte under the cursor when editing memory
; IN:
;  - memaddr: the base address of the current view
; OUT:
;  - .XY: the address under the cursor
;  - r0: the address under the cursor
.proc get_addr
@dst=r0
	lda zp::cury
	sec
	sbc wintop
	asl		; *8 (each row is 8 bytes)
	asl
	asl
	adc memaddr
	sta @dst
	lda memaddr+1
	adc #$00
	sta @dst+1

	ldy #$ff
	lda zp::curx
	sec
	sbc #COL_START
:	iny
	sbc #$03
	bpl :-

	tya
	clc
	adc @dst
	sta @dst
	tax
	bcc :+
	inc @dst+1
:	ldy @dst+1
	rts
.endproc

;*******************************************************************************
; FIND WORD
; Seeks forward from the address in memaddr for the given WORD value.
; IN:
;  - .XY: the word to seek for
; OUT:
;  - .XY: the address of the first occurrence of the value
;  - .C:  set if the value was not found
.proc find_word
@val=r0
@addr=r2
	stxy @val
	ldxy memaddr
	stxy @addr

@l0:	lda @val
	ldxy @addr
	jsr find_byte		; look for the LSB
	bcs @done		; return with .C set

	stxy @addr		; store the address of the LSB
	lda #$01		; next byte
	jsr vmem::load_off
	cmp @val+1		; is the MSB a match our value's?
	beq @found		; if so, we found our word
@next:	incw @addr		; try from the next address
	ldxy @addr
	cmpw memaddr
	bne @l0
@notfound:
	;sec
	rts			; return with .C set

@found:	ldxy @addr
	clc
@done:	rts
.endproc

;*******************************************************************************
; FIND BYTE
; Searches for the given byte value starting at the given address and ending at
; the given address (wrapping around if needed)
; IN:
;  - .A:  the byte value to look for
;  - .XY: the start address
; OUT:
;  - .XY: the address of the first occurrence of the value
;  - .C:  set if the value was not found
.proc find_byte
@addr=r4
@val=r6
@start=r7
	stxy @start
	stxy @addr
	sta @val

@l0:	ldxy @addr	; get current address to seek at
	jsr vmem::load	; load the value at the next address
	cmp @val	; == val we're looking for?
	beq @found	; if so, we're done
	incw @addr	; move to the next address
	ldxy @addr	; get current address
	cmpw @start	; are we back at the start address?
	bne @l0
	sec		; not found
	rts

@found:	ldxy @addr
	RETURN_OK
.endproc
