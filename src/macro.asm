.include "asm.inc"
.include "config.inc"
.include "draw.inc"
.include "errors.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "limits.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "screen.inc"
.include "string.inc"
.include "strings.inc"
.include "target.inc"
.include "text.inc"
.include "zeropage.inc"

.export macro_addresses
.export macros

.import __MACROBSS_LOAD__

;*******************************************************************************
.segment "SHAREBSS"
.export __mac_num
__mac_num:
nummacros: .byte 0

.export __mac_top
__mac_top: .word 0

;*******************************************************************************
; VARS
.segment "MACRO_VARS"
macro_addresses: .res MAX_MACROS * 2

;*******************************************************************************
; BSS
.segment "MACROBSS"
macros: .res $6000 - (MAX_MACROS*2)

;*******************************************************************************
; MACRO FORMAT:
;
;    | size (bytes)  |  description              |
;    |-------------------------------------------|
;    |      0-16     | macro name                |
;    |       1       | number of parameters      |
;    |      0-16     | parameter 0 name          |
;    |      ...      | parameter n name          |
;    |      ...      | macro definition          |
;    |       2       | terminating 0,0           |

.segment "MACROCODE"

;*******************************************************************************
; MAC_INIT
; Initializes the macro state by removing all existing macros
.export __mac_init
.proc __mac_init
	lda #$00
	sta nummacros

	; init address for first macro that will be created
	ldxy #macros
	stxy __mac_top
	rts
.endproc

;*******************************************************************************
; MAC_ADD
; Adds the macro to the internal macro state.
; IN:
;  - .XY: pointer to the macro definition
;     This will not contain the .MAC but does end with .ENDMAC)
;  - .A: number of parameters
;  - r0: pointer to parameters as a sequence of 0-terminated strings
.export __mac_add
.proc __mac_add
@src=zp::macros
@dst=zp::macros+2
@addr=zp::macros+4
@params=r0
@numparams=r2
	sta @numparams

	lda nummacros
	cmp #MAX_MACROS
	bcc :+
	RETURN_ERR ERR_TOO_MANY_MACROS

:	stxy @src

	; write pointer for the macro to address we will write it to
	lda nummacros
	asl
	tax
	lda __mac_top
	sta @dst
	sta macro_addresses,x
	lda __mac_top+1
	sta @dst+1
	sta macro_addresses+1,x

	; copy the name of the macro (parameter 0)
	ldy #$00
@copyname:
	lda (@params),y
	STOREB_Y @dst
	php
	incw @dst
	incw @params
	plp
	bne @copyname

	; store the number of parameters
	dec @numparams	; decrement to get the # without the macro name
	lda @numparams
	STOREB_Y @dst
	incw @dst

	; store the parameters if there are any
	lda @numparams
	beq @paramsdone
@copyparams:
	lda (@params),y
	STOREB_Y @dst
	php
	incw @dst
	incw @params
	plp
	bne @copyparams
	dec @numparams
	bne @copyparams

; copy the macro definition byte-by-byte til we get to terminating 0,0
@paramsdone:
	ldx #$00		; previous character value
@l0:	ldy #$00
	lda (@src),y		; read a character
	bne :+			; if not zero-> continue to store it
	cpx #$00		; was previous char also 0?
	beq @done		; if so, we're done
:	STOREB_Y @dst		; store character for the macro
	tax			; save previous char read to check EOF state
	incw @src
	incw @dst
	bne @l0			; branch always

@done:  ; 0-terminate the macro definition
	lda #$00
	tay
	STOREB_Y @dst
	incw @dst
	lda @dst
	sta __mac_top
	lda @dst+1
	sta __mac_top+1
	inc nummacros
	RETURN_OK
.endproc

;*******************************************************************************
; ASM
; Expands the given macro using the provided parameters and assembles it.
; IN:
;  - .A:                id of the macro
;  - zp::mac0-zp::mac4: the macro parameters
.export __mac_asm
.proc __mac_asm
@params=zp::macros
@err=zp::macros+$0b
@errcode=zp::macros+$0c
@cnt=zp::macros+$0d
@macro=zp::macros+$0e
@numparams=zp::macros+$10
@tmplabel=$140
	; get the address of the macro from its id
	asl
	tax
	lda macro_addresses,x
	sta @macro
	lda macro_addresses+1,x
	sta @macro+1

	; read macro name for use as the new temporary scope
	ldy #$00
	sty @err	; init err to none
	dey		; pre-decrement ($ff)
:	iny
	LOADB_Y @macro
	sta @tmplabel,y
	bne :-

	; move @macro pointer past the name of macro
	tya
	clc
	adc @macro
	sta @macro
	bcc :+
	inc @macro+1

:	; set the label scope to the name of the macro
	ldxy #@tmplabel
	CALLMAIN lbl::setscope
	bcc :+
	rts		; return err

:	; define the macro params
	incw @macro
	ldy #$00
	LOADB_Y @macro	; get the number of parameters
	sta @numparams
	incw @macro	; move to the first parameter name

	lda #$00
	sta @cnt
@setparams:
	lda @cnt
	cmp @numparams
	beq @paramsdone

	; get the value to set the parameter to
	asl
	tax
	lda @params,x
	sta zp::label_value
	lda @params+1,x
	sta zp::label_value+1
	beq :+
	lda #$01		; ABS
:	sta zp::label_mode	; set the address mode for this label
	inc @cnt

	; set the parameter to its value
	; (copy the param name to a temp buffer so that it can be
	; seen in the label bank first)
	ldy #$ff		; -1
:	iny
	LOADB_Y @macro
	sta @tmplabel,y
	beq :+
	cmp #$0d
	bne :-

:	tya
	sec		; +1
	adc @macro
	sta @macro
	bcc :+
	inc @macro+1

:	ldxy #@tmplabel
	CALLMAIN lbl::set
	bcs @done
	bcc @setparams		; repeat for all params

@paramsdone:
	; assemble the macro line by line
@asm:	; get macro address and
	; save state that may be clobbered if we assemble another macro
	lda @macro
	pha
	tax
	lda @macro+1
	pha
	tay

	; assemble this line of the macro
	lda #FINAL_BANK_MACROS
	CALLMAIN asm::tokenize

	rol @err		; set error if .C was set
	sta @errcode		; store the error code

	; restore state (@cnt and @macro)
	pla
	sta @macro+1
	pla
	sta @macro

@chkerr:
	lda @err		; did an error occur?
	bne @done		; if yes, exit

@ok:	; move to the next line
	ldy #$00
:	incw @macro
	LOADB_Y @macro
	bne :-

	incw @macro
	LOADB_Y @macro		; at the end of the macro? (two 0's)
	bne @asm		; no, continue

@done:	CALLMAIN lbl::popscope	; pop the scope for this macro
	lsr @err		; set .C if error occurred
	lda @errcode
	rts
.endproc

;*******************************************************************************
; GET
; Returns the id of the macro corresponding to the given text
; IN:
;  - .XY: pointer to the text
; OUT:
;  - .A: the id of the macro (if any)
;  - .C: set if there is no macro for the given text, clear if there is
.export __mac_get
.proc __mac_get
@tofind=r0
@addr=r2
@name=r4
@cnt=r6
@tmp=r7
	stxy @tofind
	lda #<macro_addresses
	sta @addr
	lda #>macro_addresses
	sta @addr+1
	lda #$00
	sta @cnt
	cmp nummacros
	beq @notfound

@find:	; get the address of the macro (its name)
	ldy #$00
	lda (@addr),y
	sta @name
	iny
	lda (@addr),y
	sta @name+1
	dey

@compare:
	LOADB_Y @name
	sta @tmp

	lda (@tofind),y
	beq :+		; end of the string we're trying to find
	cmp #' '
	beq :+
	cmp #$09
	beq :+
	cmp @tmp
	bne @next
	iny
	bne @compare

:	LOADB_Y @name	; make sure the name length matches
	beq @found

@next:	incw @addr
	incw @addr
	inc @cnt
	ldx @cnt
	cpx nummacros
	bne @find
@notfound:
	sec		; not found
	rts

@found: ldy #$00
	lda @cnt
	RETURN_OK
.endproc

;*******************************************************************************
; VIEW
; Enters the macro viewer, which displays a list of all macros that have
; been loaded
MODE_MAIN = 0
MODE_DEF  = 1
.export __mac_view
.proc __mac_view
@name=r8
@row=ra
@select=rb
@cnt=rc			; number of files extracted from listing
@scrollmax=rd		; maximum amount to allow scrolling
@scroll=re
@i=rf
@mode=zp::tmp10
@dirbuff=mem::spare+40		; 0-40 will be corrupted by text routines
@namebuff=mem::spareend-40	; buffer for the file name
@lineptrslo=@namebuff-(128*2)	; room for 128 lines
@lineptrshi=@namebuff-(128)	; room for 128 lines

	; reset/save the screen
	CALLMAIN scr::save
	CALLMAIN scr::clr

	; start in MAIN mode
	ldx #$00
	stx @mode
	CALLMAIN draw::hiline	; highlight top row

	lda __mac_num
	beq @exit
	sta @cnt
	cmp #SCREEN_HEIGHT
	bcc :+
	lda #SCREEN_HEIGHT-1
:	sta @row

;--------------------------------------
; init viewer
@init:	lda @mode
	bne :+

	; reset MAIN mode
	lda __mac_num
	sta @cnt

	lda #$00
	ldxy #strings::macros
	CALLMAIN text::print

:	lda #$00
	sta @select
	sta @scroll
	jsr @refresh		; draw the initial state

	; max a user can scroll is (# of macros - SCREEN_HEIGHT-1)
	ldx #$00
	lda @cnt
	cmp #SCREEN_HEIGHT-1
	bcc :+
	;sec
	sbc #SCREEN_HEIGHT-1
	tax
:	stx @scrollmax

	; highlight the first item
	jsr highlight_selection

;--------------------------------------
; main viewer loop
@key:	CALLMAIN key::waitch
	cmp #K_QUIT
	bne @checkdown
	lda @mode			; are we in MAIN mode
	beq @exit			; if so, return to editor

	jsr unhighlight_selection
	dec @mode			; go back to MAIN mode
	bpl @init			; branch always

@exit:  JUMPMAIN scr::restore

; check the arrow keys (used to select a macro)
@checkdown:
	jsr isdown
	bne @checkup

@rowdown:
	jsr unhighlight_selection
	inc @select
	lda @select
	cmp @row
	bcc @hiselection
	dec @select

@scrolldown:
	lda @scroll
	cmp @scrollmax
	bcs @hiselection

	inc @scroll

	; scroll up and redraw the bottom line
	ldx #$01
	lda #SCREEN_HEIGHT-1-1
	CALLMAIN text::scrollup

	lda @scroll
	clc
	adc @select
	jsr @getline
	lda #SCREEN_HEIGHT-1-1			; bottom row
	CALLMAIN text::print
	jmp @hiselection

@checkup:
	jsr isup
	bne @checkret

@rowup:
	jsr unhighlight_selection
	dec @select
	bpl @hiselection
	inc @select		; lowest valid select value is 0
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the bottom line
	lda #1
	ldx #SCREEN_HEIGHT-1-1
	CALLMAIN text::scrolldown

	dec @scroll
	lda @scroll
	clc
	adc @select
	jsr @getline
	lda #1			; top row
	CALLMAIN text::print

@hiselection:
	jsr highlight_selection
@nextkey:
	jmp @key

; check the RETURN key (to display macro definition)
@checkret:
	cmp #K_RETURN
	bne @checkgototop
	lda @mode
	beq @showdef
	jmp @init		; if already in DEF mode, ignore

; if 'G', go to bottom of directory list
@checkgototop:
	cmp #$67		; 'g'
	bne @checkbottom
	CALLMAIN key::waitch
	cmp #$67		; gg?
	bne @nextkey

	jsr unhighlight_selection

	ldx #$00
	stx @select
	stx @scroll
	beq @redraw		; branch always

; if 'G', go to bottom of directory list
@checkbottom:
	cmp #$47		; 'G'
	bne @nextkey

	jsr unhighlight_selection

	; set scroll to scrollmax
	lda @scrollmax
	sta @scroll

	; set selection (row) to min(SCREEN_HEIGHT-2, @cnt)
	ldx @cnt
	cpx #SCREEN_HEIGHT-2
	bcc :+
	ldx #SCREEN_HEIGHT-2
:	dex
	stx @select
@redraw:
	jsr @refresh
	jmp @hiselection

; user selected a macro (RETURN), display it
@showdef:
	lda @select
	clc
	adc @scroll
	jsr @show_macro_def	; show the macro definition
	jmp @init		; re-enter main loop

;--------------------------------------
; refresh (redraw) all visible rows
@refresh:
	lda #$00
	sta @i

:	; print the macro name or line of the macro def (mode dependent)
	lda @i
	clc
	adc @scroll
	jsr @getline
	lda @i
	clc
	adc #$01
	CALLMAIN text::print

	inc @i
	lda @i
	cmp #SCREEN_HEIGHT
	bcs @refresh_done
	adc @scroll
	cmp @cnt
	bcc :-

@refresh_done:
	rts

;--------------------------------------
; loads @namebuff with either:
;   1. macro name at the given index (ID) - if in macros view
;   2. given line of macro definition  - if in macro definition view
@getline:
	ldx @mode
	cpx #MODE_DEF
	bne @getname

@getdef:
	tax
	lda @lineptrslo,x
	sta @name
	lda @lineptrshi,x
	sta @name+1
	jmp @copy

@getname:
	asl
	tax
	lda macro_addresses,x
	sta @name
	lda macro_addresses+1,x
	sta @name+1

@copy:	; copy the name of the macro to a temp buffer
	ldy #$00
:	LOADB_Y @name
	sta @namebuff,y
	iny
	cmp #$00
	bne :-
	ldxy #@namebuff
	rts

;--------------------------------------
; open a fullscreen view of the macro's definition
@show_macro_def:
@macro=r0
@numparams=r2
@tmp=r3
	; get the address of the macro from its id
	asl
	tax
	lda macro_addresses,x
	sta @macro
	lda macro_addresses+1,x
	sta @macro+1

	; read macro name for use as the new temporary scope
	ldy #$ff		; pre-decrement ($ff)
:	iny
	LOADB_Y @macro
	sta @namebuff,y
	bne :-
	lda #' '
	sta @namebuff,y
	lda #$00
	sta @namebuff+1,y

	; move @macro pointer past the name of macro
	tya
	tax
	clc
	adc @macro
	sta @macro
	bcc :+
	inc @macro+1

:	; append macro params to the macro name buffer
	incw @macro
	ldy #$00
	LOADB_Y @macro		; get the number of parameters
	sta @numparams
	incw @macro		; move to the first parameter name
	cmp #$00
	beq @getlines		; if no args, skip
	ldy #$ff		; -1

@copyargs:
	iny
	inx
	LOADB_Y @macro
	sta @namebuff,x
	beq @next
	cmp #$0d
	bne @copyargs
@next:	tya
	clc
	adc @macro
	sta @macro
	bcc :+
	inc @macro+1

:	ldy #$00
	lda #' '
	sta @namebuff,x
	dec @numparams
	bne @copyargs

	lda #$00
	sta @namebuff,x

@getlines:
	ldx #$00
	ldy #$00
	incw @macro

@line:	LOADB_Y @macro		; are we at the end of the definition?
	beq @end		; if so, we're done

	lda @macro
	sta @lineptrslo,x
	lda @macro+1
	sta @lineptrshi,x

:	; move to the next line
	incw @macro
	LOADB_Y @macro
	bne :-
	incw @macro
	inx
	bne @line		; branch always
@end:	stx @cnt

	; clear the screen, set the current mode to DEF, and return
	CALLMAIN scr::clr
	lda #MODE_DEF
	sta @mode

	; draw the macro name and its arguments
	jsr unhighlight_selection
	ldxy #@namebuff
	lda #$00
	CALLMAIN text::print
	rts
.endproc

;*******************************************************************************
; UNHIGHLIGHT SELECTION
; Unhighlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc unhighlight_selection
@select=rb
	ldx @select
	inx
	JUMPMAIN draw::resetline	; deselect the current selection
.endproc

;*******************************************************************************
; HIGHLIGHT SELECTION
; Highlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc highlight_selection
@select=rb
	ldx @select
	inx
	JUMPMAIN draw::hiline	; select the current selection
.endproc

;*******************************************************************************
; ISUP
; Checks if the given key is UP or 'k'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is UP or 'k'
.proc isup
	cmp #$6b	; 'k'
	beq :+
	cmp #K_UP
:	rts
.endproc

;*******************************************************************************
; ISDOWN
; Checks if the given key is DOWN or 'j'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is DOWN or 'j'
.proc isdown
	cmp #$6a	; 'j'
	beq :+
	cmp #K_DOWN
:	rts
.endproc
