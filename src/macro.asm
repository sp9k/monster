.include "asm.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "draw.inc"
.include "errors.inc"
.include "expr.inc"
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

MAX_MACRO_NAME_LEN = 16

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
macros_end:

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

BANKED_SEG "MACROCODE", FINAL_BANK_MACROS

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
@src=zp::tmp10
@dst=zp::tmp12
@addr=zp::tmp14
@params=r0
@numparams=r2
	sta @numparams

	lda nummacros
	cmp #MAX_MACROS
	bcc :+
	RETURN_ERR ERR_TOO_MANY_MACROS

:	; make sure there is room for the macro's header (name, params, ...)
	lda __mac_top+1
	cmp #>(macros_end-$100)
	bcc :+
	RETURN_ERR ERR_OOM

:	; write pointer for the macro to address we will write it to
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
	; handle the case of an empty macro
	CALLMAIN ctx::getline	; read a line of the macro definition
	cmp #$00
	bne :+
	lda #$00
	tay
	STOREB_Y @dst		; store first of 2 terminating 0's
	jmp @done		; continue to store the 2nd

@l0:	CALLMAIN ctx::getline	; read a line of the macro definition
	cmp #$00		; were any bytes read?
	beq @done		; if not, we're done
:	stxy @src

	; make sure there is room for the line (and the terminating 0,0)
	lda @dst+1
	cmp #>(macros_end-LINESIZE-3)
	bcc @copyline
	RETURN_ERR ERR_OOM

@copyline:
	ldy #$00
@l1:	lda (@src),y
	STOREB_Y @dst		; store character for the macro
	incw @src
	incw @dst
	cmp #$00		; at end of line?
	bne @l1			; if not, keep processing line
	beq @l0			; if so, loop to get another line

@done:  ; 0,0 terminate the macro definition
	lda #$00
	tay
	STOREB_Y @dst
	incw @dst
	STOREB_Y @dst

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
@argslen=zp::macros+$08	; 2*(number of args passed by the caller)
@err=zp::macros+$0b
@errcode=zp::macros+$0c
@cnt=zp::macros+$0d
@macro=zp::macros+$0e
@numparams=zp::macros+$10
@tmplabel=$140
@tmp=r0
	stx @argslen	; save the number of args (*2) that were passed
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

	; copy the param name to a temp buffer so that it can be
	; seen in the label bank (this also advances @macro past the name)
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

:	; if no value was passed for this parameter, leave it undefined
	lda @cnt
	asl
	cmp @argslen
	bcs @nextparam

	; get the value to set the parameter to
	tax
	lda @params,x
	sta zp::label_value
	lda @params+1,x
	sta zp::label_value+1
	beq :+
	lda #$01		; ABS
:	sta zp::label_mode	; set the address mode for this label

	; macro params are constants; they must not generate relocations
	lda #SEG_ABS
	sta zp::label_segmentid

	; set the parameter to its value
	ldxy #@tmplabel
	CALLMAIN lbl::set
	bcc @nextparam		; ok -> continue with the next parameter
	sta @errcode		; save the error code (e.g. invalid label)
	rol @err		; record the error (.C is set) for @done
	bne @done		; branch always (@err is now nonzero)

@nextparam:
	inc @cnt
	bne @setparams		; repeat for all params (branch always)

@paramsdone:
	; check if the macro is empty (begins with 2 0's)
	ldy #$00
	LOADB_Y @macro
	sta @tmp
	iny
	LOADB_Y @macro
	ora @tmp
	beq @done

@asm:	; assemble the macro line by line
	; get macro address and
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
@lineptrslo=@namebuff-(256*2)	; room for 128 lines
@lineptrshi=@namebuff-(256)	; room for 128 lines

	; reset/save the screen
	CALLMAIN scr::save
	CALLMAIN scr::clr

	; start in MAIN mode
	ldx #$00
	stx @mode
	CALLMAIN draw::hiline	; highlight top row

	lda __mac_num
	beq @exit

;--------------------------------------
; init viewer
@init:	lda #$00
	sta @select
	sta @scroll
	lda @mode
	bne :+

	; reset MAIN mode
	lda __mac_num
	sta @cnt

	lda #$00
	ldxy #strings::macros
	CALLMAIN text::print

	jsr highlight_selection

:	jsr @refresh		; draw the initial state

	; max a user can scroll is (# of macros - SCREEN_HEIGHT-1)
	ldx #$00
	lda @cnt
	cmp #SCREEN_HEIGHT-1
	bcc :+
	;sec
	sbc #SCREEN_HEIGHT-1
	tax
:	stx @scrollmax

	lda @cnt
	cmp #SCREEN_HEIGHT
	bcc :+
	lda #SCREEN_HEIGHT-1
:	sta @row

;--------------------------------------
; main viewer loop
@key:	CALLMAIN key::waitch
	cmp #K_QUIT
	bne @checkdown
	lda @mode			; are we in MAIN mode
	beq @exit			; if so, return to editor

	CALLMAIN scr::clr
	dec @mode			; go back to MAIN mode
	bpl @init			; branch always

@exit:  JUMPMAIN scr::restore

; check the arrow keys (used to select a macro)
@checkdown:
	jsr isdown
	bne @checkup

	lda @mode
	bne @scrolldown			; if in definition viewer, just scroll

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
	lda #SCREEN_HEIGHT-1
	CALLMAIN text::scrollup

	lda @scroll
	clc
	adc #SCREEN_HEIGHT-2
	jsr @getline
	lda #SCREEN_HEIGHT-1			; bottom row
	CALLMAIN text::print
	jmp @hiselection

@checkup:
	jsr isup
	bne @checkret

	lda @mode
	bne @scrollup			; if in definition viewer, just scroll

@rowup:
	jsr unhighlight_selection
	dec @select
	bpl @hiselection
	inc @select		; lowest valid select value is 0

@scrollup:
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the new top line
	lda #1
	ldx #SCREEN_HEIGHT-1
	CALLMAIN text::scrolldown

	dec @scroll
	lda @scroll
	jsr @getline
	lda #1			; top row
	CALLMAIN text::print

@hiselection:
	lda @mode
	bne @nextkey		; don't highlight if we're viewing the macro def
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

	; set selection (row) to min(SCREEN_HEIGHT-1, @cnt)
	ldx @cnt
	cpx #SCREEN_HEIGHT-1
	bcc :+
	ldx #SCREEN_HEIGHT-1
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
	cmp @cnt
	beq @refresh_done

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
	cmp #SCREEN_HEIGHT-1
	bcs @refresh_done
	adc @scroll
	cmp @cnt
	bcc :-

@refresh_done:
	rts

;-------------------------------------------------------------------------------
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

;-------------------------------------------------------------------------------
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

	incw @macro
	lda #$00
	sta @namebuff,x

@getlines:
	ldx #$00
	ldy #$00
@line:	LOADB_Y @macro		; are we at the end of the definition?
	cmp #$00
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
@end:
	stx @cnt

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

;*******************************************************************************
; IS VALID
; Checks if the given string is a valid macro name
; IN:
;  - .XY: the address of the label
; OUT:
;  - .C: set if the label is NOT valid
.export __mac_isvalid
.proc __mac_isvalid
	lda zp::line
	pha
	lda zp::line+1
	pha
	stxy zp::line
	ldy #$00

; first character must be a letter or '@'
@l0:	lda (zp::line),y
	iny
	jsr @iswhitespace
	beq @l0

	; check first non whitespace char
	cmp #'@'
	beq @cont
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

	; make sure string is not an opcode (opcodes are not valid macros)
	CALLMAIN asm::isopcode
	bcc @err

	; following characters must be between '0' and 'Z'
@cont:	ldx #$00
@l1:	inx
	cpx #(MAX_MACRO_NAME_LEN/2)+1
	bcs @toolong
	lda (zp::line),y
	jsr @is_separator
	beq @params
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l1
@err:	lda #ERR_ILLEGAL_LABEL
	skw
@toolong:
	lda #ERR_LABEL_TOO_LONG
	sec			; error
	bcs @done		; branch always

@params:
	; now validate that the operand(s) are all valid
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

	; is there a directive or opcode after the name? If so, this is a label
	; definition, not a macro invocation
:	jsr @process_ws
	ldy #$00
	lda (zp::line),y
	cmp #'.'
	beq @perr		; directive -> not a macro invocation
	CALLMAIN asm::isopcode
	bcc @perr		; opcode -> not a macro invocation

@paramloop:
	jsr @process_ws
	ldy #$00
	lda (zp::line),y
	jsr @is_end_of_line
	beq @ok
	cmp #'#'
	bne :+
	incw zp::line		; skip '#' (macro params may be immediate)
.ifdef vic20
:	CALL FINAL_BANK_UDGEDIT, expr::parse
.else
:	CALL FINAL_BANK_EXPR, expr::parse
.endif
	bcs @perr

	; if there is another arg, it must be separated by comma
	jsr @process_ws
	ldy #$00
	lda (zp::line),y
	jsr @is_end_of_line
	beq @ok			; no more args -> done
	cmp #','
	bne @perr		; no comma -> err
	incw zp::line
	bne @paramloop		; comma found, check next param

@perr:	sec
	skb			; return error
@ok:	clc
@done:	; restore zp::line
	pla
	sta zp::line+1
	pla
	sta zp::line
	lda #ASM_MACRO
	rts

;-------------------------------------------------------------------------------
@process_ws:
	ldy #$00
@l2:	lda (zp::line),y
	beq :+			; if end of line, we're done
	bmi @skip		; skip non-printing chars
	jsr @iswhitespace
	bne :+			; if not space, we're done
@skip:	incw zp::line
	bne @l2
:	rts

;-------------------------------------------------------------------------------
@is_end_of_line:
	cmp #';'
	beq :+
	cmp #$0d
	beq :+
	cmp #$00
:	rts

;-------------------------------------------------------------------------------
@is_separator:
@xsave=zp::util+2
	stx @xsave
	cmp #':'
	beq @yes
	jsr @is_null_return_space_comma_closingparen_newline
	bne :+
@yes:	rts

:	ldx #@numops-1
:	cmp @ops,x
	beq @end
	dex
	bpl :-
@end:	php
	ldx @xsave
	plp
	rts
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.', '<', '>'
@numops=*-@ops

;-------------------------------------------------------------------------------
@iswhitespace:
	.include "inline/is_ws.asm"

;-------------------------------------------------------------------------------
@is_null_return_space_comma_closingparen_newline:
	cmp #$00
	beq @done
	jsr @iswhitespace
	beq :+
	cmp #','
	beq :+
	cmp #')'
:	rts
.endproc
