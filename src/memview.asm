;*******************************************************************************
; VIEW.ASM
; This file contains the code for the memory viewer/editor.  This editor is
; invoked via the debugger and allows the user to inspect memory or change its
; contents through a visual interface.
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

TOTAL_BYTES = BYTES_TO_DISPLAY*(MEMVIEW_STOP-MEMVIEW_START)

.ifdef hard8x8
TITLE_ADDR_START = 12
.else
TITLE_ADDR_START = 18
.endif

.BSS
;*******************************************************************************
.export __view_addr
__view_addr:
memaddr: .word 0

.CODE
;*******************************************************************************
; EDIT
; Starts the memory editor
.export __view_edit
.proc __view_edit
@dst=r0
@odd=r4
@dstoffset=r6
@src=r8
	ldx #COL_START
	ldy #MEMVIEW_START+1
	jsr cur::setmin

	ldy #MEMVIEW_STOP
	ldx #COL_STOP
	jsr cur::setmax

	ldy #MEMVIEW_START+1
	ldx #COL_START
	jsr cur::set

	lda #TEXT_REPLACE
	sta text::insertmode

	jsr __view_mem

; until user exits (<- or RETURN), get input and update memory
@edit:
	jsr cur::on
	ldxy memaddr
	stxy @src

	jsr key::waitch
	pha
	jsr cur::off
	pla

	cmp #K_UP_ARROW
	bne :+
	jsr getset_addr
	jmp __view_edit	; reactivate editor at new address

:	cmp #K_QUIT	; <- (done)
	beq @done

	jsr key::isup
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
	bcc @edit

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

@done:	jmp cur::unlimit

@replace_val:
	jsr @set_nybble	; replace the nybble under cursor
	jsr @next_x	; advance the cursor (if we can)
	ldxy @src
	jsr __view_mem	; update the display
	jmp @edit

;--------------------------------------
; get the address of the memory at the cursor position
@set_nybble:
	jsr util::chtohex
	pha

	; get the base address for the row that the cursor is on
	lda zp::cury
	sec
	sbc #MEMVIEW_START+1
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
@reset: jmp __view_edit		; restart the viewer at the word's address
.endproc

;*******************************************************************************
; UP
; Handles the Up key, moving the cursor or scrolling if needed
.proc up
	; are we at the top of the editor?
	lda zp::cury
	cmp #MEMVIEW_START+1
	bne :+

	; we're at the top, scroll
	lda memaddr
	sbc #$08	; # of bytes per row (.C is always set)
	sta memaddr
	bcs @done
	dec memaddr+1
@done:	sta memaddr
	jmp __view_mem	; refresh the display

:	dec zp::cury
	rts
.endproc

;*******************************************************************************
; DOWN
; Handles the Down key, moving the cursor or scrolling if needed
.proc down
	; are we at the bottom of the editor?
	lda zp::cury
	cmp #MEMVIEW_STOP-1
	bcc :+

	; we're at the bottom, scroll
	lda memaddr
	adc #$07	; # of bytes per row - 1 (.C is always set)
	sta memaddr
	bcc @done
	inc memaddr+1
@done:	sta memaddr
	jmp __view_mem	; refresh the display

:	inc zp::cury
	rts
.endproc

;*******************************************************************************
; GETSET ADDR
; Gets an address from the user (as input in the memory title area) and updates
; the memory view to render that area of memory.
.proc getset_addr
	pushcur

	ldx #MEMVIEW_START
	jsr draw::hiline

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

	; set bounds for the input
	lda #18
	sta cur::minx
	sta zp::curx
	lda #18+4
	sta cur::maxx

	lda #MEMVIEW_START
	sta zp::cury

	ldxy #key::gethex
	jsr edit::gets

	ldxy #mem::linebuffer+17
	stxy zp::line
	jsr expr::eval
	stxy memaddr
	popcur
	rts
.endproc

;*******************************************************************************
; MEM
; Displays the contents of memory in a large block beginning with the
; address in memaddr
; The address is that which was set with the most recent call to mem::edit
.export __view_mem
.proc __view_mem
@src=ra
@col=rc
@row=rd
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

	; draw the title for the memory display
	ldxy #strings::memview_title
	lda #MEMVIEW_START
	jsr text::print
	ldx #MEMVIEW_START
	jsr draw::hiline

	lda #MEMVIEW_START+1
	sta @row

@l0:	ldxy @src
	jsr ui::memline

	lda @row
	jsr text::print		; draw the row of rendered bytes
	ldx @row
	jsr draw::resetline
	inc @row
	lda @row
	cmp #MEMVIEW_STOP	; have we drawn all rows?
	bcc @l0			; repeat til we have
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
	sbc #MEMVIEW_START+1
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
