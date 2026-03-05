;*******************************************************************************
; UTIL.ASM
; This file contains definitions for various utility procedures.  This includes
; things like converting binary values to hex or copying memory.
;*******************************************************************************

.include "errors.inc"
.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"
.include "macros.inc"
.CODE

;*******************************************************************************
; CHTOHEX
; Returns the binary representation of the character given in .A
; IN:
;  - .A: the character to get the representation of
; OUT:
;  - .A: the binary representation of the given character
;  - .C: set if the character is not a valid hex digit
.export __util_chtohex
.proc __util_chtohex
	cmp #$66+1
	bcs @done
	cmp #$61
	bcc @upper
@lower:
	sbc #($61-$0a)
	RETURN_OK

@upper:
	cmp #$46+1
	bcs @done
	cmp #$41
	bcc @numeric
	sbc #($41-$0a)
	RETURN_OK

@numeric:
	cmp #'9'+1
	bcs @done
	cmp #'0'
	bcc @done
	;sec
	sbc #'0'
	RETURN_OK

@done:	sec
	rts
.endproc

;******************************************************************************
; HEXTOSTR
; Returns the string representation of the value in .A
; IN:
;  - .A: the value to get the string representation of
; OUT:
;  - .X: the character representation of the low nybble
;  - .Y: the character representation of the  high nybble
.export __util_hextostr
.proc __util_hextostr
	pha
	lsr
	lsr
	lsr
	lsr
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tay

	pla
	and #$0f
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tax
	rts
.endproc

;*******************************************************************************
; PARSE HEX
; Returns the 16-bit binary value of the given hex string.
; IN:
;  - .XY: the address of the string to parse
; OUT:
;  - .XY: the binary representation of the hex string
;  - .C:  set on error
.export __util_parsehex
.proc __util_parsehex
@str=zp::util
@result=zp::util+2
	lda #$00
	sta @result
	sta @result+1
	stxy @str

	; find the end of the string
	ldy #$ff
@l0:	iny
	lda (@str),y
	bne @l0

	dey			; go back to the last character (the LSB)
	ldx #$00
@l1:	lda (@str),y
	jsr __util_chtohex
	bcs @done		; return err
	sta @result,x
	dey
	bmi @ok
	lda (@str),y
	jsr __util_chtohex
	bcs @done
	asl
	asl
	asl
	asl
	ora @result,x
	sta @result,x
	inx
	dey
	bpl @l1

@ok:	clc
	ldxy @result
@done:	rts
.endproc

;*******************************************************************************
; ATOI
; Returns the value of the decimal string given in .XY.
; The string must be terminated by a \0, $0d (newline), or ','.
; IN:
;  - .XY: the address of the string to get the value of
; OUT:
;  - .A: the index after the last character
;  - .C: clear on success
;  - .XY: the 16 bit value of the string
; used in the given string
.export atoi
.proc atoi
@tmp=zp::tmp10
@str=zp::tmp12
@scale=zp::tmp14
@val=zp::tmp15
@tmp2=zp::tmp17
@offset=zp::util+1
	stxy @str
	ldy #$00
	sty @val
	sty @val+1

	; find terminating char
	dey			; .Y = $ff
:	iny
	lda (@str),y
	jsr __util_is_separator
	bne :-
@endfound:
	sty @offset
	dey

	ldx #$ff
	stx @scale
@l0:	inc @scale
	lda (@str),y
	cmp #'9'+1
	bcs @unexpectedchar
	cmp #'0'
	bcc @unexpectedchar
	sec
	sbc #'0'
	sta @tmp
	lda #$00
	sta @tmp+1
	ldx @scale
	beq @muldone
@l1:	jsr @mul10
	bcs @err	; oversized value
	dex
	bne @l1
@muldone:
	clc
	lda @tmp
	adc @val	; add the digit*(10**x)
	sta @val
	lda @val+1
	adc @tmp+1
	sta @val+1
	bcc :+
	lda #ERR_OVERSIZED_OPERAND
	;sec
	rts
:	dey
	bpl @l0

	ldx @val
	ldy @val+1
	lda @offset
	RETURN_OK

@unexpectedchar:
	lda #ERR_UNEXPECTED_CHAR
	sec
@err:	rts

;------------------
@mul10:
; multiply (in place) word in @tmp by 10
; .C is clear on success, set on failure
	lda @tmp+1
	sta @tmp2+1
	lda @tmp
	sta @tmp2

	asl		; *2
	rol @tmp+1
	bcs @mulerr
	asl		; *4
	rol @tmp+1
	bcs @mulerr
	asl		; *8
	rol @tmp+1
	bcs @mulerr

	adc @tmp2	; *9
	sta @tmp
	lda @tmp+1
	adc @tmp2+1
	bcs @mulerr
	sta @tmp+1

	lda @tmp	; *10
	adc @tmp2
	sta @tmp
	lda @tmp+1
	adc @tmp2+1
	bcs @mulerr
	sta @tmp+1
	RETURN_OK
@mulerr:
	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc

;*******************************************************************************
; TODEC
; Gets the decimal representation of the value given.
; IN:
;  - .XY: the value to get the decimal representation of
; OUT:
;  - mem::spare: contains the decimal representation fo the value.
;   The returned string is 0-terminated
.export __util_todec
.proc __util_todec
result=mem::spare
	lda #'0'
	sta result
	sta result+1
	sta result+2
	sta result+3
	sta result+4
@l1000s:
	sub16 #1000
	bcc @do100s
	inc result+1
	lda result+1
	cmp #'9'+1
	bcc @l1000s
	lda #'0'
	sta result+1
	inc result
	bne @l1000s

@do100s:
	add16 #1000
@l100s:	sub16 #100
	bcc @do10s
	inc result+2
	bne @l100s

@do10s:	add16 #100
@l10s:	sub16 #10
	bcc @do1s
	inc result+3
	bne @l10s

@do1s:	txa
	clc
	adc #10+'0'
	sta result+4

; skip leading zeroes
	ldx #$00
	ldy #$00
@format:
	lda result,x
	cmp #'0'
	bne @shift
	inx
	cpx #4
	bne @format
@shift:	lda result,x
	sta result,y
	iny
	inx
	cpx #5
	bcc @shift
	lda #$00
	sta result,y
	rts
.endproc

;*******************************************************************************
; TODEC8
; A small (faster) routine to convert a small number to decimal
; IN:
;  - .A: the number to convert
; OUT:
;  - YXA: the 100's, 10's and 1's place of the decimal # respectively
.export __util_todec8
.proc __util_todec8
	ldy #$2f
	ldx #$3a
	sec
:	iny
	sbc #100
	bcs :-
:	dex
	adc #10
	bmi :-
	adc #$2f
	rts
.endproc

;*******************************************************************************
; TODEC24
; Converts the given 24 bit binary value to a decimal string
; IN:
;  - .A: the MSB of the value
;  - .Y: the 2nd LSB of the value
;  - .X: the LSB of the value
; OUT:
;  - .XY: the address of the string to convert
.export __util_todec24
.proc __util_todec24
@input=r0		; 3 bytes
@result=r3	; 4 bytes
@str=$100
	sta @input+2
	sty @input+1
	stx @input

	lda #0
	ldx #8-1
:	sta @result+0,x
	dex
	bpl :-

	sed

	ldx #24		; 3 bytes
@l0:	asl @input
	rol @input+1
	rol @input+2
	ldy #$00
:	lda @result,y
	adc @result,y
	sta @result,y
	iny
	tya
	and #$04	; is bit 2 set (.Y == 8)
	beq :-		; loop until it is
	dex
	bne @l0

	cld

	ldx #$00
	ldy #8-1
:	lda @result,x
	pha
	clc
	and #$0f
	adc #'0'
	sta @str,y
	dey
	pla
	and #$f0
	lsr
	lsr
	lsr
	lsr
	adc #'0'
	sta @str,y
	dey
	inx
	cpx #4
	bcc :-

	ldxy #@str
	rts
.endproc

;*******************************************************************************
; IS WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.export __util_is_whitespace
.proc __util_is_whitespace
	.include "inline/is_ws.asm"
.endproc

;*******************************************************************************
; IS NULL SPACE COMMA CLOSINGPAREN NEWLINE
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is: 0,$0d,' ', ',', or ')'
.export __util_is_null_return_space_comma_closingparen_newline
.proc __util_is_null_return_space_comma_closingparen_newline
	cmp #$00
	beq @done
	jsr __util_is_whitespace
	beq @done
	cmp #','
	beq @done
	cmp #')'
@done:	rts
.endproc

;*******************************************************************************
; IS SEPARATOR
; Checks if the given byte represents a "separator". A separator is any of:
;  0, $0d, ' ', ',', ')', or any operator character
; IN:
;   - .A: the byte to check
; OUT:
;   - .Z: set if the given byte represents a "separator"
.export __util_is_separator
.proc __util_is_separator
	cmp #':'
	beq @yes
	jsr __util_is_null_return_space_comma_closingparen_newline
	bne :+
@yes:	rts
:	; fall through to __util_is_operator
.endproc

;*******************************************************************************
; IS OPERATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is an operator ('+', '-', etc.)
.export __util_isoperator
.proc __util_isoperator
@xsave=zp::util+2
	stx @xsave
	ldx #@numops-1
:	cmp @ops,x
	beq @end
	dex
	bpl :-
@end:	php
	ldx @xsave
	plp
	rts
.PUSHSEG
.RODATA
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.', '<', '>'
@numops = *-@ops
.POPSEG
.endproc

;*******************************************************************************
; IS ALPHANUM
; IN:
;  .A: the character to test for alphanumeric
; OUT:
;  - .C: clear if the given character is alpha ('A'-'z', or '0'-'9')
.export __util_is_alphanum
.proc __util_is_alphanum
	cmp #'0'
	bcc @no
	cmp #'9'+1
	bcc @yes
	cmp #$41
	bcc @no
	cmp #$7a+1
	bcs @no
@yes:	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; PARSE ENQUOTED LINE
; Calls util::parse_enquoted_string with zp::line and stores the result
; in $100
; IN:
;   - zp::line: the string to parse
; OUT:
;   - $100: the parsed string
;   - .C:   set if the given string was not valid
.export __util_parse_enquoted_line
.proc __util_parse_enquoted_line
	ldxy #$100
	stxy r0

	ldxy zp::line
	; fall through to __util_parse_enquoted_string
.endproc

;*******************************************************************************
; PARSE ENQUOTED STRING
; Parses an enquoted text string in returns the text that is within the quotes
; at the given destination address
; returns the length in .A ($ff if no string was found)
; IN:
;  - .XY: the address of the string to parse
;  - r0: the address to store the parsed string
; OUT:
;  - r0:  the text within the quotes (")
;  - .XY: the address where we stopped parsing
;  - .C:  set if the given string was not valid
.export __util_parse_enquoted_string
.proc __util_parse_enquoted_string
@src=r2
@dst=r0
	stxy @src
	ldy #$00
@eatws:	lda (@src),y
	cmp #' '
	bne :+
	incw @src
	bne @eatws
:	cmp #'"'
	bne @err

@l0:	incw @src
	lda (@src),y
	beq @err	; no closing quote
	cmp #'"'
	beq @done
	sta (@dst),y
	incw @dst
	bne @l0

@done:	lda #$00
	sta (@dst),y	; 0-terminate the string
	ldxy @src
	RETURN_OK
@err:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc
