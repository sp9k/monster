.include "asm.inc"
.include "config.inc"
.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "string.inc"
.include "target.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
MAX_MACROS = 128

.export macro_addresses
.export macros

;*******************************************************************************
; VARS
.segment "MACRO_VARS"

;*******************************************************************************
nummacros:       .byte 0
macro_addresses: .res MAX_MACROS * 2

;*******************************************************************************
; BSS
.segment "MACROBSS"
macros:          .res $1400

;*******************************************************************************
; MACRO FORMAT:
;
;    | size (bytes)  |  description              |
;    |-------------------------------------------|
;    |      0-16     | macro name                |
;    |       1       | number of parameters      |
;    |      0-16     | parameter 0 name          |
;    |      ...      | parameter n name          |
;    |     0-255     | macro definition          |
;    |       1       | terminating 0             |

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
	stxy macro_addresses
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
	lda nummacros
	asl
	adc #<macro_addresses
	sta @addr
	lda #>macro_addresses
	adc #$00
	sta @addr+1

	; get the address to write the macro to
	ldy #$00
	lda (@addr),y
	sta @dst
	iny
	lda (@addr),y
	sta @dst+1

	; copy the name of the macro (parameter 0)
	dey
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
	STOREB_Y @dst

	; done copying use the end address as the start address for the next macro
	inc nummacros
	lda nummacros
	asl
	adc #<macro_addresses
	sta @addr
	lda #>macro_addresses
	adc #$00
	sta @addr+1
	ldy #$00
	lda @dst
	sta (@addr),y
	iny
	lda @dst+1
	sta (@addr),y
	;clc
	rts
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
