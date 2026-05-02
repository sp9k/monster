.include "errors.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
UNSHIFTED_KEY_TABLE = $ec5e
SHIFTED_KEY_TABLE   = $ec9f
CBM_KEY_TABLE       = $ece0
CTRL_KEY_TABLE      = $eda3
CURSOR_LR_MASK      = 2

.CODE

;******************************************************************************
; GETCH
; Gets a key from the keyboard and returns it
; OUT:
;  - .A: the key code of the pressed key or 0 if no key is pressed
;  - .Z: set if no key is pressed
.export __key_getch
.proc __key_getch
	lda $c6		; get keyboard buffer length
	beq @ret	; if buffer empty, return

.ifdef vic20
	jsr $e5cf
	ldx $028b	; get repeat timer
	cpx #$04	; check if repeat was just reinitialized (to 4)
	bne :+
	; KERNAL resets repeat count to 4, lower it for faster repeat
	ldx #$01
	stx $028b
:
.else
	ldy $0277
	ldx #$00
:	lda $0277+1,x
	sta $0277,x
	inx
	cpx $c6
	bne :-
	dec $c6		; dec keyboard buffer index
	tya		; get key
.endif

	ldx #@num_translate
@transloop:
	cmp @to_translate,x
	bne :+
	lda @translated,x
	bne @cont	; branch always
:	dex
	bpl @transloop

@cont:	cmp #$41
	bcc @done
	cmp #$5a+1
	bcs :+
	eor #$20	; if alpha, EOR $20
	rts		; rts with .C clear

:	cmp #$c1
	bcc @done
	cmp #$da+1
	bcs @done
	eor #$80	; if control code, EOR $80

@done:	cmp #$00
@ret:	rts

.PUSHSEG
.RODATA
; these characters are translated to their corresponding character in
; @translated
;
@to_translate:
	.byte $5c	; £ (Pound)
	.byte $5e	; Arrow up
	.byte $dd	; SHIFT Minus
	.byte $5f	; Arrow left
	.byte $ba	; SHIFT @
	.byte $a9	; Shift-£
	.byte $c0	; Shift *
	;.byte $xx	; Shift left-arrow TODO: ?
@translated:
	.byte 92	; \
	.byte 94	; ^
	.byte 95	; _
	.byte 96	; `
	.byte 123	; {
	.byte 124	; |
	.byte 125	; }
	;.byte 126	; ~
@num_translate=*-@translated
.POPSEG
.endproc

;******************************************************************************
; WAITCH
; Waits for a keypress and returns the key that was pressed.
; OUT:
;   - .A: the key that was pressed
.export __key_waitch
.proc __key_waitch
:	jsr __key_getch
	beq :-
	rts
.endproc

;******************************************************************************
; GETHEX
; Gets a key from the keyboard, and returns its value ONLY
; if it is a hex value, a DELETE, a RETURN, or a QUIT (<-)
; Lowercase hex is converted to uppercase
; OUT:
;  - .A: the key pressed (if valid)
.export __key_gethex
.proc __key_gethex
	jsr __key_getch
	cmp #K_DEL	; allow delete
	beq :+
	cmp #K_RETURN
	beq :+
	cmp #K_QUIT
	beq :+
	jsr __key_ishex
	bcs :+
	lda #$00	; don't accept non-hex characters
	rts

:	cmp #$46+1
	bcc @done
	eor #$20	; convert to upper-case
@done:	rts
.endproc

;*******************************************************************************
; ISHEX
; Returns .C set if the given key is 0-9 or A-F
; IN:
;  - .A: the key to check if is hex
; OUT:
;  - .C: set if the given key is 0-9, a-f, or A-F
.export __key_ishex
.proc __key_ishex
	cmp #'0'
	bcc @done
	cmp #$66+1	; 'f'+1
	bcs @nothex
	cmp #$61	; 'a'
	bcs @done
	cmp #$46+1	; 'F' + 1
	bcs @nothex
	cmp #$41	; 'A'
	bcs @done
	cmp #'9'+1
	bcs @nothex
@ishex:	sec
	rts
@nothex:
	clc
@done:	rts
.endproc

;*******************************************************************************
; ISDEC
; Returns .C set if the given key is 0-9
; IN:
;  - .A: the key to check if is hex
; OUT:
;  - .C: set if the given key is 0-9
.export __key_isdec
.proc __key_isdec
	cmp #'0'
	bcc @done
	cmp #'9'+1
	bcs @notdec
	sec
	rts
@notdec:
	clc
@done:	rts
.endproc

;*******************************************************************************
; IS PRINTING
; Returns with .C set if the given character is printable
; IN:
;  - .A: the character to check for printability
; OUT:
;  - .C: set if the character is not printable
.export __key_is_printing
.proc __key_is_printing
	cmp #$09
	beq @yes
	cmp #' '
	bcc @no
	cmp #$7f
	bcs @done
@yes:	RETURN_OK
@no:	sec
@done:	rts
.endproc

;*******************************************************************************
; ISUP
; Checks if the given key is UP or 'k'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is UP or 'k'
.export __key_isup
.proc __key_isup
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
.export __key_isdown
.proc __key_isdown
	cmp #$6a	; 'j'
	beq :+
	cmp #K_DOWN
:	rts
.endproc

;*******************************************************************************
; ISLEFT
; Checks if the given key is LEFT or 'h'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is LEFT or 'h'
.export __key_isleft
.proc __key_isleft
	cmp #K_LEFT
	beq :+
	cmp #$68	; 'h'
:	rts
.endproc

;*******************************************************************************
; ISRIGHT
; Checks if the given key is RIGHT or 'l'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is RIGHT or 'l'
.export __key_isright
.proc __key_isright
	cmp #K_RIGHT
	beq :+
	cmp #$6c	; 'l'
:	rts
.endproc
