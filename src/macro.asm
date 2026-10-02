.include "asm.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "context_tokens.inc"
.include "draw.inc"
.include "errors.inc"
.include "expr.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "limits.inc"
.include "lexer.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "screen.inc"
.include "string.inc"
.include "strings.inc"
.include "target.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"
.macpack longbranch

.export macro_addresses
.export macros

.import __MACROBSS_LOAD__
.import __mac_clear_mutables
.import __mac_verify_property

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
macros: .res $5000 - (MAX_MACROS*2) - $a0
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
;    |      ...      | definition (ctx records)  |
;    |       1       | terminating 0             |

BANKED_SEG "MACROCODE", FINAL_BANK_MACROS

;*******************************************************************************
; MAC_INIT
; Initializes the macro state by removing all existing macros
.export __mac_init
.proc __mac_init
	jsr __mac_clear_mutables
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
; The definition is read from the active context (see context_tokens.inc).
; IN:
;  - .A: number of parameters (including the macro's name)
;  - r0: pointer to the name and parameters as 0-terminated strings
; OUT:
;  - .C: set on error
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

; copy each record of the macro definition
@paramsdone:
@l0:	CALLMAIN ctx::getrecord
	bcs @ret
	cmp #$00
	beq @done
	; make sure there is room for the record and the terminating 0
	clc
	adc @dst
	tax
	lda @dst+1
	adc #$00
	cmp #>macros_end
	bcc @copyline
	bne @full
	cpx #<macros_end
	bcs @full
@copyline:
	STOREBLK8 CTX_TOKEN_BUFFER, @dst, CTX_TOKEN_BUFFER
	tya
	clc
	adc @dst
	sta @dst
	bcc @l0
	inc @dst+1
	bne @l0
@full:
	RETURN_ERR ERR_OOM
@done:
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
@ret:	rts
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
@body=mem::spare+40		; address of the macro's first record
@namebuff=mem::spare+$80	; same memory as mac::source

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
	cmp #K_WIN_CLOSE		; C= + q: close the viewer from any level
	beq @exit
	cmp #K_QUIT			; RUN/STOP: back one level
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
	jne @getname

@getdef:
	tax
	lda @body
	sta @name
	lda @body+1
	sta @name+1
	ldy #$00
@seek:
	cpx #$00
	beq @record
	LOADB_Y @name
	clc
	adc @name
	sta @name
	bcc :+
	inc @name+1
:	dex
	jmp @seek
@record:
	LOADB_Y @name
	tax
@tokens:
	LOADB_Y @name
	sta CTX_TOKEN_BUFFER,y
	iny
	dex
	bne @tokens
	CALL LEX_DECODE_BANK, lex::decode
	bcs @badline
	lda lex::cached
	bmi @render_values
	ldxy #mem::asmbuffer
	rts
@badline:
	lda #$00		; show a line that can't be decoded as empty
	sta mem::asmbuffer
	ldxy #mem::asmbuffer
	clc
	rts

; the line has values: print each one as hex in place of its placeholder
@render_values:
@sourcepos=r0
@outpos=r1
@low=r2
	lda #$00
	sta @sourcepos
	sta @outpos
@render:
	ldx @sourcepos
	CALL LEX_BANK, lex::value_at
	bcs @character
	stx @low
	tya
	pha
	lda #'$'
	jsr @emit
	pla
	jsr @hex
	lda @low
	jsr @hex
	jmp @render_next
@character:
	ldx @sourcepos
	lda mem::asmbuffer,x
	beq @render_done
	jsr @emit
@render_next:
	inc @sourcepos
	jmp @render
@render_done:
	ldx @outpos
	lda #$00
	sta @namebuff,x
	ldxy #@namebuff
	rts
@hex:
	CALLMAIN util::hextostr
	txa
	pha
	tya
	jsr @emit
	pla
@emit:
	ldx @outpos
	cpx #MAX_LINE_LEN
	bcs :+
	sta @namebuff,x
	inc @outpos
:	rts

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
	ldxy @macro
	stxy @body
	ldx #$00
	ldy #$00
@line:	LOADB_Y @macro		; are we at the end of the definition?
	cmp #$00
	beq @end		; if so, we're done

	; move to the next record
	clc
	adc @macro
	sta @macro
	bcc :+
	inc @macro+1
:	inx
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
;  - .X: (if .C is clear) 0 if no parameters were given, 1 some were
;        this is used for formatting
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
	tya
	pha			; save name offset (isopcode clobbers .Y)
	CALLMAIN asm::isopcode
	pla
	tay			; restore name offset
	bcc @err

	; following characters must be between '0' and 'Z'
@cont:	ldx #$00
@l1:	inx
	cpx #MAX_MACRO_NAME_LEN
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
	jcs @done		; branch always

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
	bne :+
	jsr __mac_verify_property
	bcs @perr
:
	CALLMAIN asm::isopcode
	bcc @perr		; opcode -> not a macro invocation

	; check the first parameter separately; if the line ends here, the
	; invocation has no parameters
	ldy #$00		; (isopcode clobbers .Y)
	lda (zp::line),y
	jsr @is_end_of_line
	beq @noparams
	bne @param		; branch always

@paramloop:
	jsr @process_ws
	ldy #$00
	lda (zp::line),y
	jsr @is_end_of_line
	beq @ok
@param:	cmp #','
	beq @comma
	cmp #'"'
	beq @string
	cmp #'#'
	bne :+
	incw zp::line		; skip '#' (macro params may be immediate)
:	CALL FINAL_BANK_EXPR, expr::parse
	bcs @perr

	; if there is another arg, it must be separated by comma
@checkcomma:
	jsr @process_ws
	ldy #$00
	lda (zp::line),y
	jsr @is_end_of_line
	beq @ok			; no more args -> done
	cmp #','
	bne @perr		; no comma -> err
@comma:
	incw zp::line
	jmp @paramloop
@string:
	incw zp::line
	ldy #$00
	lda (zp::line),y
	beq @perr
	cmp #'"'
	bne @string
	incw zp::line
	jmp @checkcomma

@perr:	sec
	bcs @done		; return error (branch always)

@noparams:
	ldx #$00		; no parameters were given
	clc
	bcc @done		; branch always

@ok:	ldx #$01		; 1 or more parameters were given
	clc

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
	beq :+
	jsr @iswhitespace
	beq :+
	cmp #','
	beq :+
	cmp #')'
:	rts
.endproc
