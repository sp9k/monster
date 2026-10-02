;*******************************************************************************
; MACROEXPAND.ASM
; Expands macro invocations: collects the arguments of the invocation, then
; assembles each line of the macro's body with its parameters filled in.
;*******************************************************************************

.include "asm.inc"
.include "config.inc"
.include "codes.inc"
.include "ctx.inc"
.include "context_tokens.inc"
.include "lexer.inc"
.include "errors.inc"
.include "expr.inc"
.include "rpn.inc"
.include "labels.inc"
.include "line.inc"
.include "limits.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.macpack longbranch
.import macro_addresses, macro_enabled
.import __asm_ifstack, __asm_ifdepth
.import __label_namespace_depth, __label_namespace_unwind

MAX_DEPTH = 4	; max nesting depth of macro invocations (each uses a scope)

;*******************************************************************************
; FRAME FORMAT
; $200 bytes (accessed with LOADB/STOREB):
;   $00-$1f:   header (see below)
;   $20-$7f:   spellings of all the arguments (96 bytes)
;   $80-$ff:   names declared with .LOCAL (8 slots of 16 bytes)
;   $100-$1ff: compiled arguments (see expr::compile)
F_BODY = 0		; address of the next body line
F_PARAMS = 2		; address of the parameter names
F_COUNT = 4		; number of parameters
F_SERIAL = 5		; invocation number (makes local names unique)
F_RAWSTART = 8		; offset of each argument's spelling (from F_RAW)
F_BOUNDSTART = 12	; offset of each compiled argument (from F_BOUND)
F_FLAGS = 16		; ARG_* flags for each argument
F_LOCALS = 20		; number of .LOCAL names
F_SCOPE = 21		; set if the macro's scope was pushed
F_IF = 22		; caller's .IF depth
F_IFSTATE = 23		; caller's .IF stack (MAX_IFS bytes)
F_NAMESPACE = 31	; caller's namespace depth
.assert F_IFSTATE+MAX_IFS <= F_NAMESPACE, error, "too many .IFs to save"
F_RAW   = 32
F_NAMES = 128
F_BOUND = 256

ARG_PRESENT   = 1
ARG_IMMEDIATE = 2
ARG_STRING    = 4

; Function selectors for emitparam and macro property lookup
FUNC_PLAIN   = $00
FUNC_ISIMM   = $01
FUNC_PRESENT = $02
FUNC_VALUE   = $03
FUNC_IDENT   = $04
FUNC_TEXT    = $05

.segment "MACROBSS"
frames: .res MAX_DEPTH*$200
mutable_ids: .res $80

.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "BSS_NOINIT"
.endif
.export __mac_source, __mac_depth
__mac_source = mem::spare+$80	; the line being assembled (original case)
__mac_depth: .byte 0

.segment "MACRO_VARS"
serial: .word 0
input = __mac_source

.export __mac_argrecord
__mac_argrecord = mem::spare+$d1
argrecord = __mac_argrecord
token = mem::spare+$122		; name scratchpad
.assert token+MAX_LINE_LEN+1 <= CTX_ITER_NAME, error, "token overlaps"
.assert argrecord+MAX_LINE_LEN+1 <= token,     error, "argument record overlaps"

begin:         .byte 0
finish:        .byte 0
argkind:       .byte 0
parens:        .byte 0
function:      .byte 0
localdef:      .byte 0
paramindex:    .byte 0
namelen:       .byte 0
savedscan:     .byte 0
savedid:       .word 0
dstbank:       .byte 0
srcbank:       .byte 0
parentarg:     .byte 0
mutable_count: .byte 0
cached_index:  .byte 0
cached_lo:     .byte 0
cached_hi:     .byte 0

frame = zp::macros
src   = zp::macros+2
dst   = zp::macros+4
param = zp::macros+6
other = zp::macros+8

scan       = zp::macros+$0a
arg        = zp::macros+$0b
rawtop     = zp::macros+$0c
boundtop   = zp::macros+$0d
input_base = zp::macros+$0e
outlen     = zp::macros+$0f
outlimit   = zp::macros+$10
.assert outlimit < zp::io_status, error, "macro zeropage overlaps KERNAL I/O"

BANKED_SEG "MACROCODE", FINAL_BANK_MACROS

;*******************************************************************************
; RESET EXPANSION
; Clears the invocation state at the start of an assembly pass.
; IN:
;  - None
; OUT:
;  - .C: clear
.export __mac_reset_expansion
.proc __mac_reset_expansion
	lda #$00
	sta __mac_depth
	sta lex::cached
	sta serial
	sta serial+1
	RETURN_OK
.endproc

;*******************************************************************************
; SELECT FRAME
; Sets the frame pointer to the current invocation's frame.
; IN:
;  - __mac_depth: the current depth (1 to MAX_DEPTH)
; OUT:
;  - frame: set to the address of the frame
.proc select_frame
	lda #<frames
	sta frame
	lda __mac_depth
	sec
	sbc #$01		; convert to base 0
	asl			; *2
	;clc
	adc #>frames
	sta frame+1
	rts
.endproc

;*******************************************************************************
; ASM
; Collects the arguments of a macro invocation and assembles each line of the
; macro's body. The caller's .IF, scope, and namespace state are restored
; however the expansion ends.
; IN:
;  - .A:       the id of the macro
;  - zp::line: points after the macro name in mem::asmbuffer
; OUT:
;  - .A: ASM_MACRO or an error code
;  - .C: set on error
.export __mac_asm
.proc __mac_asm
	pha			; save macro ID
	lda __mac_depth
	cmp #MAX_DEPTH
	bcc :+
	pla
	RETURN_ERR ERR_STACK_OVERFLOW

:	inc __mac_depth
	jsr select_frame
	lda #$00
	ldy #$1f
@clear: STOREB_Y frame
	dey
	bpl @clear

	; copy the .IF stack to the frame
	ldy #F_IF
	lda __asm_ifdepth
	STOREB_Y frame
	ldx #$00
@saveifs:
	lda __asm_ifstack,x
	iny
	STOREB_Y frame
	inx
	cpx #MAX_IFS
	bcc @saveifs

	; save namespace depth for frame
	lda __label_namespace_depth
	ldy #F_NAMESPACE
	STOREB_Y frame

	; set src to the address for the macro to assemble
	pla			; restore macro ID
	asl
	tax
	lda macro_addresses,x
	sta src
	lda macro_addresses+1,x
	sta src+1
	ldy #$00

@name:	LOADB_Y src
	incw src
	cmp #$00
	bne @name
	LOADB_Y src		; read # of params
	ldy #F_COUNT
	STOREB_Y frame		; store # of params to frame
	incw src
	ldy #F_PARAMS
	lda src
	STOREB_Y frame		; set param pointer for frame (LSB)
	iny
	lda src+1
	STOREB_Y frame		; set param pointer for frame (MSB)

	incw serial		; increment local label name counter
	lda serial
	ora serial+1
	bne :+
	lda #ERR_TOO_MANY_MACROS
	jmp @fail

:	; store the serial id for the invocation
	ldy #F_SERIAL
	lda serial
	STOREB_Y frame
	iny
	lda serial+1
	STOREB_Y frame

	; move the operands (in their original case) to the start of input
	lda zp::line
	sec
	sbc #<mem::asmbuffer
	sta input_base
	tax
	ldy #$00

@source:
	lda __mac_source,x
	sta input,y
	beq @collect
	inx
	iny
	cpy #MAX_LINE_LEN+1
	bcc @source
	lda #ERR_LINE_TOO_LONG
	jmp @fail

;-------------------------------------------------------------------------------
@collect:
	jsr collect
	jcs @fail

	; create a new symbol scope called "m"
	lda #'m'
	sta token
	lda #$00
	sta token+1
	ldxy #token
	CALLMAIN lbl::setscope
	jcs @fail
	ldy #F_SCOPE
	lda #$01
	STOREB_Y frame

	; skip over parameter names to get to the first body line
	ldy #F_PARAMS
	LOADB_Y frame
	sta src
	iny
	LOADB_Y frame
	sta src+1
	ldy #F_COUNT
	LOADB_Y frame
	tax
	ldy #$00
	cpx #$00
	beq @body

@skipparam:
	LOADB_Y src
	incw src
	cmp #$00
	bne @skipparam
	dex
	bne @skipparam

@body:	ldy #F_BODY
	lda src
	STOREB_Y frame
	iny
	lda src+1
	STOREB_Y frame

@next:	jsr select_frame
	ldy #F_BODY
	LOADB_Y frame
	sta src
	iny
	LOADB_Y frame
	sta src+1
	ldy #$00
	LOADB_Y src
	jeq @ok
	sta CTX_TOKEN_BUFFER
@copy:	LOADBLK8 src, CTX_TOKEN_BUFFER, CTX_TOKEN_BUFFER

@advance:
	tya
	clc
	adc src
	tax
	lda src+1
	adc #$00
	ldy #F_BODY+1
	STOREB_Y frame
	dey
	txa
	STOREB_Y frame
	CALL LEX_DECODE_BANK, lex::decode
	jcs @fail
	lda #$00
	sta input_base
	CALL FINAL_BANK_ASM, macro_enabled
	beq @disabled
	jsr expand
	bcs @fail
	CALLMAIN asm::assemble_tokens
	bcs @fail
	jmp @next

@disabled:
	; the line is in a false .IF block: assemble it without expanding it
	CALLMAIN asm::assemble_view
	bcs @fail
	jmp @next

@ok:	ldy #F_IF
	LOADB_Y frame
	cmp __asm_ifdepth
	beq :+
	lda #ERR_UNCLOSED_IF
	sec
	bcs @fail
:	ldy #F_NAMESPACE
	LOADB_Y frame
	cmp __label_namespace_depth
	beq :+
	lda #ERR_NO_MATCHING_SCOPE
	sec
	bcs @fail
:	lda #ASM_MACRO
	clc

;-------------------------------------------------------------------------------
@fail:	php
	pha
	jsr select_frame
	ldy #F_IF
	LOADB_Y frame
	sta __asm_ifdepth
	ldx #$00

;-------------------------------------------------------------------------------
; restore saved state from the frame
@restoreifs:
	iny
	LOADB_Y frame
	sta __asm_ifstack,x
	inx
	cpx #MAX_IFS
	bcc @restoreifs

	ldy #F_NAMESPACE
	LOADB_Y frame
	CALL FINAL_BANK_SYMBOLS, __label_namespace_unwind
	jsr select_frame

	ldy #F_SCOPE
	LOADB_Y frame
	beq :+
	CALLMAIN lbl::popscope

:	dec __mac_depth
	pla
	plp
	rts
.endproc

;*******************************************************************************
; NAMESPACE FLOOR
; Returns the namespace depth at entry to the current macro.
; IN:
;  - __mac_depth: nonzero
; OUT:
;  - .A: the namespace depth
.export __mac_namespace_floor
.proc __mac_namespace_floor
	jsr select_frame
	ldy #F_NAMESPACE
	LOADB_Y frame
	rts
.endproc

;*******************************************************************************
; TOUPPER
; Converts a lowercase ASCII letter to the case the assembler uses.
; IN:
;  - .A: the character
; OUT:
;  - .A: the converted character
.proc toupper
	cmp #$61
	bcc :+
	cmp #$7b
	bcs :+
	eor #$20
:	rts
.endproc

;*******************************************************************************
; WORDCHAR
; Checks if a character can be part of an identifier.
; IN:
;  - .A: the character
; OUT:
;  - .A: the character (case touppered)
;  - .C: clear if the character can be part of an identifier
.proc wordchar
	jsr toupper
	cmp #$2e
	beq @yes
	cmp #$40
	beq @yes
	cmp #$5f
	beq @yes
	cmp #$30
	bcc @no
	cmp #$3a
	bcc @yes
	cmp #$41
	bcc @no
	cmp #$5b
	bcc @yes
	cmp #$c1
	bcc @no
	cmp #$db
	bcs @no
@yes:	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; SKIPSPACE
; Moves scan past any spaces and tabs in input.
; IN:
;  - scan: index into input
; OUT:
;  - .A:   the first character that isn't a space
;  - .Z:   set if at the end of the line
;  - scan: the index of that character
.proc skipspace
	ldx scan
@next:
	lda input,x
	cmp #' '
	beq @skip
	cmp #$09
	bne @done
@skip:	inx
	bne @next
@done:	stx scan
	cmp #$00
	rts
.endproc

;*******************************************************************************
; PUT
; Appends a byte to the output buffer.
; IN:
;  - .A:       byte to write
;  - dst:      output buffer
;  - dstbank:  nonzero if dst is banked
;  - outlen:   index to write to
;  - outlimit: size of the buffer
; OUT:
;  - outlen: incremented
;  - .C:     set if the buffer is full
.proc put
	ldy outlen
	cpy outlimit
	bcc :+
	RETURN_ERR ERR_LINE_TOO_LONG
:	pha
	lda dstbank
	beq @native
	pla
	STOREB_Y dst
	jmp @written

@native:
	pla
	sta (dst),y

@written:
	inc outlen
	clc
	rts
.endproc

;*******************************************************************************
; TERMINATE
; Writes a 0 at the end of the output buffer without moving outlen.
; IN:
;  - dst, dstbank, outlen: the output buffer (see PUT)
; OUT:
;  - None
.proc terminate
	ldy outlen
	lda dstbank
	beq @native
	lda #$00
	STOREB_Y dst
	rts
@native:
	sta (dst),y
	rts
.endproc

;*******************************************************************************
; COLLECT
; Stores the invocation's arguments in the frame, preserving zp::line.
; IN:
;  - input: invocation's operands
;  - frame: current frame
; OUT:
;  - .A: error code (if any)
;  - .C: set on error
.proc collect
	lda zp::line
	pha
	lda zp::line+1
	pha
	jsr split
	tax
	pla
	sta zp::line+1
	pla
	sta zp::line
	txa
	rts
.endproc

;*******************************************************************************
; SPLIT
; Stores each comma-separated argument (without trailing whitespace) in the
; frame.
; IN:
;  - input: invocation's operands
;  - frame: current frame
; OUT:
;  - .A: error code (if any)
;  - .C: set on error
.proc split
	lda #$00
	sta scan
	sta arg
	sta rawtop
	sta boundtop
	jsr eat_ws
	jcs @ret
	cmp #LEX_END
	jeq @done
	cmp #';'
	jeq @done

;-------------------------------------------------------------------------------
@argument:
	ldy #F_COUNT
	LOADB_Y frame
	cmp arg
	jeq @bad
	jcc @bad
	jsr eat_ws
	jcs @ret
	ldx scan
	stx begin
	stx finish
	lda #$00
	sta parens

;-------------------------------------------------------------------------------
@scan:	; find the beginning (begin) and end (finish) offsets of the argument
	ldx scan
	jsr lexarg	; read next token (.A=kind, .X=size)
	bcs @ret
	cmp #LEX_END
	beq @end
	cmp #';'
	beq @end
	cmp #','
	bne :+
	ldy parens
	beq @end
:	cmp #'('
	bne :+
	inc parens
:	cmp #')'
	bne :+
	ldy parens
	beq @bad	; imbalanced parens? -> exit
	dec parens
:	pha
	txa		; scan += .X (token size)
	clc
	adc scan
	sta scan
	pla
	cmp #LEX_SPACE
	beq @scan
	lda scan
	sta finish
	jmp @scan

;-------------------------------------------------------------------------------
@end:	ldy parens
	bne @bad
	jsr store_argument	; store argument that was found
	bcs @ret
	inc arg
	ldx scan
	lda input,x
	cmp #','
	bne @done
	inc scan
	jmp @argument		; look for next arg

;-------------------------------------------------------------------------------
@done:	; store empty records for the arguments that weren't given
	ldy #F_COUNT
	LOADB_Y frame
	cmp arg
	beq @ok
	bcc @ok
	lda scan
	sta begin
	sta finish
	jsr store_argument	; store empty argument
	bcs @ret
	inc arg
	jmp @done

@ok:	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts
.endproc

;*******************************************************************************
; EAT WS
; Moves scan past any whitespace tokens.
; IN:
;  - scan: index into input
; OUT:
;  - .A: kind of the token at scan (or error code)
;  - .X: length of the token
;  - .C: set on error
.proc eat_ws
@next:
	ldx scan
	jsr lexarg
	bcs @ret
	cmp #LEX_SPACE
	beq @skip
	clc
@ret:	rts
@skip:	txa
	clc
	adc scan
	sta scan
	jmp @next
.endproc

;*******************************************************************************
; LEXARG
; Reads the token at the given index in input.
; IN:
;  - .X: index into input
; OUT:
;  - .A:       kind of the token (or error code)
;  - .X:       length of the token
;  - .C:       set on error
;  - zp::line: clobbered
.proc lexarg
	txa
	clc
	adc #<input
	sta zp::line
	lda #>input
	adc #$00
	sta zp::line+1
	CALL LEX_BANK, lex::peek
	rts
.endproc

;*******************************************************************************
; STORE ARGUMENT
; Stores an argument's flags, spelling, and compiled expression in the frame.
; If the argument is just an argument of an enclosing macro, that argument is
; copied as is.
; IN:
;  - begin:  index of the argument in input
;  - finish: index after the argument's last token
;  - scan:   index of the argument's terminator
;  - arg:    argument's number
; OUT:
;  - .C: set on error
.proc store_argument
	lda #$01
	sta dstbank

	; store index to argument @ frame[RAWSTART + arg_number]
	lda arg
	clc
	adc #F_RAWSTART
	tay
	lda rawtop
	STOREB_Y frame

	; store offset to compiled argument's area in the frame
	lda arg
	clc
	adc #F_BOUNDSTART
	tay
	lda boundtop
	STOREB_Y frame

	lda finish
	cmp begin
	jeq @spelling

	ldx begin
	jsr valued_at
	bcs @flags
	cmp #LEX_INTEGER
	beq @flags
	sta argkind

	lda finish
	sec
	sbc begin
	cmp #$01
	jeq clone_argument

	lda argkind
	cmp #LEX_IMMARG
	bne @flags
	lda #ARG_PRESENT|ARG_IMMEDIATE
	bne @setflags				; branch always

@flags: ldx begin
	jsr lexarg
	ldy #ARG_PRESENT
	bcs @flagged
	cmp #'#'
	bne :+
	ldy #ARG_PRESENT|ARG_IMMEDIATE
:	cmp #LEX_STRING
	bne @flagged
	txa
	clc
	adc begin
	cmp finish
	bne @flagged
	ldy #ARG_PRESENT|ARG_STRING

@flagged:
	tya
@setflags:
	pha
	lda arg
	clc
	adc #F_FLAGS
	tay
	pla
	STOREB_Y frame

@spelling:
	lda frame
	clc
	adc #F_RAW
	sta dst
	lda frame+1
	adc #$00
	sta dst+1
	lda rawtop
	sta outlen
	lda #F_NAMES-F_RAW
	sta outlimit

	ldx begin
@raw:	cpx finish
	beq @rawdone
	jsr cached_value
	bcc @raw
	cmp #$00
	bne @ret
	lda input,x
	jsr put
	bcs @ret
	inx
	bne @raw

@rawdone:
	lda #$00
	jsr put				; 0 terminate
	bcs @ret

	lda outlen
	sta rawtop

	lda finish
	cmp begin
	bne :+

	; start index == stop index (invalid)
	lda #ERR_INVALID_MACRO_ARGS
	jsr error_record
	jmp @bound

:	jsr compile_arg			; compile the argument to argrecord
@bound: lda #$01
	sta dstbank

	; dst = F_BOUND + frame
	lda frame
	sta dst
	lda frame+1
	clc
	adc #>F_BOUND
	sta dst+1

	; src = argrecord
	lda boundtop
	sta outlen
	lda #$ff
	sta outlimit
	ldxy #argrecord
	stxy src
	lda #$00
	sta srcbank

	; copy the compiled argument to the output buffer
	jsr copyrecord
	bcs @ret

	lda outlen
	sta boundtop
	clc
@ret:	rts
.endproc

;*******************************************************************************
; COMPILE ARG
; Compiles the argument (minus any leading '#') in the caller's scope.
; IN:
;  - begin:      index of the argument in input
;  - scan:       index of the argument's terminator
;  - input_base: offset of input in mem::asmbuffer
; OUT:
;  - argrecord: the compiled expression or an error record
.proc compile_arg
	lda begin
	clc
	adc input_base
	tax
	ldy begin
	lda input,y
	cmp #'#'
	bne :+
	inx
:	txa
	clc
	adc #<mem::asmbuffer
	sta zp::line
	lda #>mem::asmbuffer
	adc #$00
	sta zp::line+1

	ldxy #argrecord
	CALL FINAL_BANK_EXPR, expr::compile
	bcs error_record

	; make sure the whole argument was used by the expression
	lda zp::line
	sec
	sbc #<mem::asmbuffer
	tax
	lda zp::line+1
	sbc #>mem::asmbuffer
	bne @partial
	txa
	sec
	sbc input_base
	cmp scan
	bne @partial
	rts
@partial:
	lda #ERR_UNEXPECTED_CHAR
	; fall through
.endproc

;*******************************************************************************
; ERROR RECORD
; Stores an argument record that gives the error wherever it's used.
; IN:
;  - .A: error code
; OUT:
;  - argrecord: error record
.proc error_record
	sta argrecord+1
	lda #$ff
	sta argrecord
	rts
.endproc

;*******************************************************************************
; COPYRECORD
; Appends a compiled argument (or error) record to the output.
; IN:
;  - src:     record
;  - srcbank: nonzero if src is banked
;  - dst:     the output buffer (see PUT)
; OUT:
;  - src: address after the record
;  - .A:  ERR_EXPRESSION_TOO_COMPLEX if the output is full
;  - .C:  set if the output is full
.proc copyrecord
@size=r0
	ldy #$00
	jsr @load
	ldx #$02
	cmp #$ff
	beq @copy
	tay
	iny
	sta @size
	jsr @load
	clc
	adc @size
	adc #$02
	tax
@copy:	stx namelen
@next:	ldy #$00
	jsr @load
	jsr put
	bcs @full
	incw src
	dec namelen
	bne @next
	rts
@full:	lda #ERR_EXPRESSION_TOO_COMPLEX
	rts

@load:	lda srcbank
	beq :+
	LOADB_Y src
	rts
:	lda (src),y
	rts
.endproc

;*******************************************************************************
; CLONE ARGUMENT
; Copies an argument of an enclosing invocation to this frame. If it was
; passed as a value (LEX_ARG) the immediate flag is dropped.
; IN:
;  - cached_lo: depth of the argument's frame
;  - cached_hi: argument's number
;  - argkind:   LEX_ARG or LEX_IMMARG
;  - arg:       the number of the argument to store
; OUT:
;  - .C: set on error
.proc clone_argument
	ldx cached_lo
	ldy cached_hi
	jsr arg_frame
	bcc :+
	RETURN_ERR ERR_INVALID_MACRO_ARGS

:	sty parentarg
	tya
	clc
	adc #F_FLAGS
	tay
	LOADB_Y other
	ldx argkind
	cpx #LEX_IMMARG
	beq :+
	and #$ff-ARG_IMMEDIATE

:	pha
	lda arg
	clc
	adc #F_FLAGS
	tay
	pla
	STOREB_Y frame

	lda parentarg
	clc
	adc #F_RAWSTART
	tay
	LOADB_Y other
	clc
	adc #F_RAW
	adc other
	sta src
	lda other+1
	adc #$00
	sta src+1

	lda frame
	;clc
	adc #F_RAW
	sta dst
	lda frame+1
	adc #$00
	sta dst+1

	lda rawtop
	sta outlen
	lda #F_NAMES-F_RAW
	sta outlimit

	lda #$01
	sta srcbank
	jsr copystring
	bcs @ret

	lda #$00
	jsr put
	bcs @ret

	lda outlen
	sta rawtop
	lda parentarg
	clc
	adc #F_BOUNDSTART
	tay
	LOADB_Y other
	clc
	adc other
	sta src
	lda other+1
	adc #>F_BOUND
	sta src+1

	lda frame
	sta dst
	lda frame+1
	clc
	adc #>F_BOUND
	sta dst+1

	lda boundtop
	sta outlen

	lda #$ff
	sta outlimit
	jsr copyrecord
	bcs @ret

	lda outlen
	sta boundtop
	clc
@ret:	rts
.endproc

;*******************************************************************************
; ARG FRAME
; Points "other" at the frame for the given depth.
; IN:
;  - .X: the frame depth (1 is the outermost)
;  - .Y: the argument's number
; OUT:
;  - other: frame's address
;  - .C:    set if there is no such frame or argument
.proc arg_frame
	txa
	beq @bad
	cmp __mac_depth
	beq :+
	bcs @bad

:	cpy #$04
	bcs @bad
	sec
	sbc #$01
	asl
	clc
	adc #>frames
	sta other+1
	lda #<frames
	sta other
	clc
	rts
@bad:	sec
	rts
.endproc

;*******************************************************************************
; SPLICE ARG
; Appends an argument's compiled expression to expr's RPN list. Float literal
; indexes are moved past the literals expr already has.
; IN:
;  - .A:         number of float literal bytes expr already has
;  - .X:         depth of the argument's frame
;  - .Y:         argument's number
;  - zp::expr+2: index to write to in the RPN list
; OUT:
;  - zp::expr+2: updated index
;  - argrecord:  number of float literal bytes followed by the literals
;  - .A:         error code (if any)
;  - .C:         set on error
.export __mac_splice_arg
.proc __mac_splice_arg
@fltbase=r1
@rpnend=r2
@toktype=r3
@i=zp::expr+2
	sta @fltbase
	jsr arg_frame
	bcc :+
	RETURN_ERR ERR_INVALID_MACRO_ARGS

:	tya
	clc
	adc #F_BOUNDSTART
	tay
	LOADB_Y other
	clc
	adc other
	sta src
	lda other+1
	adc #>F_BOUND
	sta src+1
	ldy #$00
	LOADB_Y src
	cmp #$ff
	bne @copy
	iny
	LOADB_Y src		; the argument's error
	sec
	rts

@copy:	lda #$01
	sta srcbank
	lda #$00
	sta dstbank
	sta outlen
	lda #$ff
	sta outlimit
	ldxy #argrecord
	stxy dst
	jsr copyrecord
	bcs @ret
	lda argrecord
	sta @rpnend
	clc
	adc @i
	cmp #MAX_RPN_LEN	; room for the tokens and the terminator?
	bcs @full
	inc @rpnend
	ldy #$01
	ldx @i

@token: cpy @rpnend
	beq @floats
	lda argrecord,y
	sta __expr_rpnlist,x
	sta @toktype
	inx
	iny
	cmp #TOK_PC
	beq @token
	lda @toktype
	cmp #TOK_FLOAT		; .C set only for a float literal
	lda argrecord,y		; operator, or value LSB
	bcc :+
	clc
	adc @fltbase
:	sta __expr_rpnlist,x
	inx
	iny
	lda @toktype
	cmp #TOK_BINARY_OP
	beq @token
	cmp #TOK_UNARY_OP
	beq @token
	lda argrecord,y
	sta __expr_rpnlist,x
	inx
	iny
	bne @token

@floats:
	stx @i
	lda argrecord,y		; float literal bytes
	sta argrecord
	beq @done
	ldx #$00
:	iny
	lda argrecord,y
	sta argrecord+1,x
	inx
	cpx argrecord
	bne :-
@done:	clc
@ret:	rts
@full:	RETURN_ERR ERR_EXPRESSION_TOO_COMPLEX
.endproc

;*******************************************************************************
; STRING ARG
; Copies the text (without quotes) of the string argument at zp::line.
; IN:
;  - zp::line: optional whitespace followed by an argument token
;  - .XY:      destination
;  - .A:       size of the destination (not counting the 0 terminator)
; OUT:
;  - .A:       length of the string
;  - zp::line: after the argument token (unchanged on error)
;  - .C:       set if there is no string argument at zp::line
.export __mac_string_arg
.proc __mac_string_arg
@index=r0
	stxy param
	sta savedscan
	lda zp::line
	pha
	lda zp::line+1
	pha
	CALL LEX_BANK, lex::eatws
	bcs @no
	cmp #LEX_ARG
	beq :+
	cmp #LEX_IMMARG
	bne @no

:	lda zp::line
	sec
	sbc #<mem::asmbuffer
	tax
	CALL LEX_BANK, lex::value_at
	bcs @no
	jsr arg_frame
	bcs @no
	sty parentarg
	tya
	clc
	adc #F_FLAGS
	tay
	LOADB_Y other
	and #ARG_STRING
	beq @no
	lda parentarg
	clc
	adc #F_RAWSTART
	tay
	LOADB_Y other
	clc
	adc #F_RAW+1		; skip the opening quote
	adc other
	sta src
	lda other+1
	adc #$00
	sta src+1
	ldx #$00

@char:	ldy #$00
	LOADB_Y src
	beq @done
	cmp #'"'
	beq @done
	cpx savedscan
	bcs @no
	stx @index
	ldy @index
	sta (param),y
	incw src
	inx
	bne @char

@done:	txa
	tay
	lda #$00
	sta (param),y
	pla
	pla
	incw zp::line
	txa
	clc
	rts
@no:	pla
	sta zp::line+1
	pla
	sta zp::line
	lda #ERR_SYNTAX_ERROR
	sec
	rts
.endproc

;*******************************************************************************
; VALUED AT
; Reads the INTEGER, ARG, or IMMARG token (if any) at the given index.
; IN:
;  - .X:         index into input
;  - input_base: offset of input in mem::asmbuffer
; OUT:
;  - .A:        kind of the token
;  - cached_lo: token's value (LSB)
;  - cached_hi: token's value (MSB)
;  - .C:        set if there is no such token
.proc valued_at
	lda lex::cached
	bpl @no
	txa
	clc
	adc input_base
	tax
	CALL LEX_BANK, lex::value_at
	bcs @ret
	stx cached_lo
	sty cached_hi
@ret:	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; CACHED VALUE
; If there is an INTEGER token (.REP iterator value) at the given index,
; writes its value to the output as "$hhhh". If there is an ARG or IMMARG
; token, writes the spelling of that argument.
; IN:
;  - .X:  index into input
;  - dst: output buffer (see PUT)
; OUT:
;  - .X: index after the token (if written)
;  - .A: 0 if there is no such token, otherwise the error code (if any)
;  - .C: clear if the value was written
.proc cached_value
	stx cached_index
	jsr valued_at
	bcs @absent
	cmp #LEX_INTEGER
	beq @integer

	; an argument of an enclosing invocation: copy its spelling
	ldx cached_lo
	ldy cached_hi
	jsr arg_frame
	bcs @bad
	tya
	;clc
	adc #F_RAWSTART
	tay
	LOADB_Y other
	clc
	adc #F_RAW
	adc other
	sta src
	lda other+1
	adc #$00
	sta src+1
	lda #$01
	sta srcbank
	jsr copystring
	bcs @ret
	bcc @written

@integer:
	lda #'$'
	jsr put
	bcs @ret
	lda cached_hi
	jsr hexbyte
	bcs @ret
	lda cached_lo
	jsr hexbyte
	bcs @ret
@written:
	ldx cached_index
	inx
	clc
	rts

@absent:
	ldx cached_index
	lda #$00
	;sec
@ret:	rts
@bad:	lda #ERR_INVALID_MACRO_ARGS
	sec
	rts
.endproc

;*******************************************************************************
; HEXBYTE
; Writes a byte to the output as two hex digits
; IN:
;  - .A: byte to write
; OUT:
;  - dst: output buffer (see PUT)
;  - .A:  error code (if .C set)
;  - .C:  set if the output is full
.proc hexbyte
@savex=r0
	pha
	lsr
	lsr
	lsr
	lsr
	jsr @digit
	bcs @pop
	pla
	and #$0f

@digit: stx @savex
	tax
	lda digits,x
	ldx @savex
	jmp put
@pop:	tax			; save the error code
	pla
	txa
	rts
.endproc

;*******************************************************************************
; BIND PC
; Defines a label ("pSSSS.pc") at the address of the invocation so that '*'
; in an argument has the same value wherever the argument is used.
; IN:
;  - frame:         the current frame (for its serial number)
;  - zp::virtualpc: the current PC
; OUT:
;  - .XY: the id of the label
;  - .C:  set on error
.export __mac_bind_pc
.proc __mac_bind_pc
	lda #'p'
	sta token
	ldy #F_SERIAL+1
	LOADB_Y frame
	ldx #$01
	jsr @hex

	ldy #F_SERIAL
	LOADB_Y frame
	ldx #$03
	jsr @hex

	lda #'.'
	sta token+5
	lda #'p'
	sta token+6
	lda #'c'
	sta token+7
	lda #$00
	sta token+8
	ldxy zp::virtualpc
	stxy zp::label_value
	lda asm::segment
	sta zp::label_segmentid
	lda #$01
	sta zp::label_mode
	ldxy #token
	CALLMAIN lbl::set
	rts

@hex:	pha
	lsr
	lsr
	lsr
	lsr
	jsr @digit
	pla
	and #$0f

@digit: tay
	lda digits,y
	sta token,x
	inx
	rts
.endproc

;*******************************************************************************
; READWORD
; Copies a word from input to token (case touppered).
; IN:
;  - .X:      index of the word in input
;  - namelen: length of the word
; OUT:
;  - token: word
;  - .X:    index after the word
.proc readword
	ldy #$00

@next:	lda input,x
	jsr toupper
	sta token,y
	inx
	iny
	cpy namelen
	bne @next
	lda #$00
	sta token,y	; terminate token
	rts
.endproc

;*******************************************************************************
; READTOKEN
; Copies an identifier from input to token (case touppered).
; IN:
;  - scan: the index of the identifier in input
; OUT:
;  - token:   identifier
;  - namelen: length of the identifier
;  - scan:    index after the identifier
.proc readtoken
	ldx scan
	ldy #$00

@next:	lda input,x
	jsr toupper
	sta token,y
	iny
	inx
	lda input,x
	jsr wordchar	; is character separator?
	bcc @next	; repeat til it is

	stx scan
	sty namelen
	lda #$00
	sta token,y	; terminate token
	rts
.endproc

;*******************************************************************************
; FINDPARAM
; Looks up token in the current macro's parameter names.
; IN:
;  - token: name to look for
;  - frame: current frame
; OUT:
;  - paramindex: the number of the parameter
;  - .C:         clear if found
.proc findparam
	ldy #F_PARAMS
	LOADB_Y frame
	sta param
	iny
	LOADB_Y frame
	sta param+1
	lda #$00
	sta paramindex

@next:	ldy #F_COUNT
	LOADB_Y frame
	cmp paramindex
	beq @no
	ldy #$00

@l0:	LOADB_Y param
	cmp token,y
	bne @skip
	cmp #$00
	beq @yes
	iny
	bne @l0

@skip:	ldy #$00
@skipname:
	LOADB_Y param
	incw param
	cmp #$00
	bne @skipname
	inc paramindex
	bne @next
@yes:	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; EMITPARAM
; Writes a parameter (or a property of it) to the output.
; Identifier spellings are converted to the assembler's uppercase form.
; IN:
;  - paramindex: the number of the parameter
;  - function:   FUNC_PLAIN, FUNC_ISIMM, FUNC_PRESENT, FUNC_VALUE,
;                FUNC_IDENT, or FUNC_TEXT
; OUT:
;  - .A: the error code (if .C set)
;  - .C: set if the output is full
.proc emitparam
@saved=r0
	lda paramindex
	clc
	adc #F_FLAGS
	tay
	LOADB_Y frame
	tax
	lda function
	beq @operand
	cmp #FUNC_VALUE
	beq @value
	bcs @raw
	cmp #FUNC_ISIMM
	bne @present
	txa
	and #ARG_IMMEDIATE
	jmp @bool
@present:
	txa
	and #ARG_PRESENT

@bool:	beq :+
	lda #$01
:	clc
	adc #'0'
	sta @saved
	lda #LEX_NUMBER
	jsr put
	jcs @ret
	lda #$01
	jsr put
	jcs @ret
	lda @saved
	jmp put

@operand:
	txa
	and #ARG_IMMEDIATE
	beq @value
	lda #LEX_IMMARG
	skw
@value: lda #LEX_ARG
	jsr put
	jcs @ret
	lda __mac_depth
	jsr put
	jcs @ret
	lda paramindex
	jmp put

@raw:	lda paramindex
	clc
	adc #F_RAWSTART
	tay
	LOADB_Y frame
	clc
	adc #F_RAW
	adc frame
	sta src
	lda frame+1
	adc #$00
	sta src+1
	ldy #$00
:	LOADB_Y src
	beq :+
	iny
	bne :-
:	sty namelen

	; .TEXT adds quotes unless the spelling is already a string
	ldx #$00
	lda function
	cmp #FUNC_TEXT
	bne @size
	ldy #$00
	LOADB_Y src
	cmp #'"'
	beq @size
	ldx #$02

@size:	stx savedscan
	txa
	clc
	adc namelen
	beq @none
	sta @saved
	lda #LEX_RAW
	jsr put
	bcs @ret
	lda @saved
	jsr put
	bcs @ret
	lda savedscan
	beq :+
	lda #'"'
	jsr put
	bcs @ret
:	lda #$01
	sta srcbank
	lda function
	cmp #FUNC_IDENT
	beq @identifier
	jsr copystring
	bcs @ret
	lda savedscan
	beq @none
	lda #'"'
	jmp put

@identifier:
	ldy #$00
	LOADB_Y src
	beq @none
	jsr toupper
	jsr put
	bcs @ret
	incw src
	jmp @identifier

@none:	clc
@ret:	rts
.endproc

;*******************************************************************************
; COPYSTRING
; Appends a 0-terminated string to the output.
; IN:
;  - src:     string to copy
;  - srcbank: nonzero if src is banked
;  - dst:     the output buffer (see PUT)
; OUT:
;  - .A: the error code (if .C set)
;  - .C: set if the output is full
.proc copystring
@next:
	ldy #$00
	lda srcbank
	beq @native
	LOADB_Y src
	jmp @loaded
@native:
	lda (src),y
@loaded:
	beq @done
	jsr put
	bcs @ret
	incw src
	jmp @next
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; LOCALNAME
; Writes token as a WORD that is unique to this invocation ("mSSSS.name").
; IN:
;  - token: the name
;  - frame: the current frame (for its serial number)
; OUT:
;  - .A: the error code (if .C set)
;  - .C: set if the output is full
.proc localname
	lda #LEX_WORD
	jsr put
	bcs @ret

	ldx #$00
:	lda token,x
	beq :+
	inx
	bne :-

:	txa
	clc
	adc #$06		; 'm', four serial digits and '.'
	jsr put
	bcs @ret
	lda #'m'
	jsr put
	bcs @ret
	ldy #F_SERIAL+1
	LOADB_Y frame
	jsr hexbyte
	bcs @ret
	ldy #F_SERIAL
	LOADB_Y frame
	jsr hexbyte
	bcs @ret
	lda #'.'
	jsr put
	bcs @ret
	lda #$00
	sta srcbank
	ldxy #token
	stxy src
	jmp copystring
@ret:	rts
.endproc

;*******************************************************************************
; PUTSPELLED
; Writes a token's kind, length, and spelling (from input) to the output.
; IN:
;  - .A:        kind of the token
;  - savedscan: index of the token in input
;  - namelen:   length of the token
; OUT:
;  - .X: the index after the token
;  - .A: the error code (if .C set)
;  - .C: set if the output is full
.proc putspelled
	jsr put
	bcs @ret
	lda namelen
	jsr put
	bcs @ret
	ldx savedscan

@next:	lda input,x
	jsr put
	bcs @ret
	inx
	dec namelen
	bne @next
@ret:	rts
.endproc

;*******************************************************************************
; FINDLOCAL
; Looks up token in the names declared with .LOCAL in the current macro
; invocation
; IN:
;  - token: name to look for
;  - frame: current frame
; OUT:
;  - .C: clear if found
.proc findlocal
	lda frame
	clc
	adc #F_NAMES
	sta param
	lda frame+1
	adc #$00
	sta param+1
	ldy #F_LOCALS
	LOADB_Y frame
	tax
	beq @no

@next:	ldy #$00
@compare:
	LOADB_Y param
	cmp token,y
	bne @skip		; not a match, check next local
	cmp #$00
	beq @yes
	iny
	bne @compare

@skip:	; param += $10
	lda param
	clc
	adc #$10
	sta param
	bcc :+
	inc param+1
:	dex
	bne @next
@no:	sec
	rts
@yes:	clc
	rts
.endproc

;*******************************************************************************
; EXPAND
; Writes a body line to CTX_TOKEN_BUFFER with each parameter replaced by an
; argument token and each local name made unique. Comments are dropped.
; IN:
;  - input: decoded body line
;  - frame: current frame
; OUT:
;  - CTX_TOKEN_BUFFER: expanded record (its line number is kept)
;  - .C:               set on error
.proc expand
	ldxy #CTX_TOKEN_BUFFER
	stxy dst
	lda #CTX_TOKEN_LIMIT
	sta outlimit
	lda #$03
	sta outlen
	lda #$00
	sta scan
	sta localdef
	sta dstbank
	sta token

	jsr eat_ws
	jcs @ret
	ldy scan
	sty begin

	cmp #LEX_WORD		; are we at a WORD?
	bne :+
	stx namelen
	ldx scan
	jsr readword		; read the word
	stx scan
:	jsr @islocal

	sta localdef
	; .IFDEF PARAM becomes .IF .PRESENT(PARAM)
	ldx #$00

@ifdefword:
	; check if token is ".IFDEF"
	lda token,x
	cmp @ifdef,x
	bne @notifdef
	inx
	cmp #$00
	bne @ifdefword

	; token is .IFDEF, read the next token
	jsr eat_ws
	jcs @ret
	cmp #LEX_WORD
	bne @notifdef		; if next token is not a WORD -> invalid .IFDEF
	stx namelen
	ldx scan
	jsr readword		; copy word into token buffer
	jsr findparam		; is the argument a parameter?
	bcs @notifdef		; if not, proceed as normal

	; replace the .IFDEF with .IF
	ldx #$00
@writeif:
	lda @iftext,x
	jsr put
	jcs @ret
	inx
	cpx #@iftextlen
	bne @writeif
	lda #FUNC_PRESENT
	sta function
	jsr emitparam		; write the .IF .PRESENT
	jcs @ret
	jmp @done
@notifdef:
	lda begin
	sta scan

@next:	ldx scan
	stx savedscan
	jsr valued_at
	bcs @token
	jsr put
	jcs @ret
	lda cached_lo
	jsr put
	jcs @ret
	lda cached_hi
	jsr put
	jcs @ret
	inc scan
	jmp @next

@token: ldx scan
	jsr lexarg		; read/parse the token
	jcs @ret
	stx namelen
	cmp #LEX_END
	jeq @done
	cmp #';'
	jeq @done

	cmp #LEX_WORD
	beq @word
	cmp #LEX_SPACE
	beq @single
	cmp #LEX_NUMBER
	bcc @single		; punctuation
	cmp #LEX_EQ
	bcc @spelled		; NUMBER, STRING, CHAR
	cmp #LEX_GE+1
	bcc @single		; comparisons

@spelled:
	jsr putspelled
	jcs @ret
	stx scan
	jmp @next

@single:
	jsr put
	jcs @ret
	lda scan
	clc
	adc namelen
	sta scan
	jmp @next

@word:	ldx scan
	jsr readword
	stx scan
	lda #FUNC_PLAIN
	sta function
	lda input,x
	cmp #'('
	bne @ordinary
	jsr @builtin
	sta function
	beq @ordinary
	inc scan
	jsr eat_ws
	jcs @ret
	cmp #LEX_WORD
	jne @bad
	stx namelen
	ldx scan
	jsr readword
	stx scan
	jsr eat_ws
	jcs @ret
	cmp #')'
	jne @bad			; imbalanced parens -> error
	inc scan
	jsr findparam
	jcs @bad
	jsr emitparam
	jcs @ret
	jmp @next

@ordinary:
	jsr findparam
	bcs @local
	jsr emitparam
	jcs @ret
	jmp @next

@local: lda localdef
	bne @literal
	lda token
	cmp #'@'
	beq @private
	jsr findlocal
	bcs @literal

@private:
	jsr localname
	jcs @ret
	jmp @next

@literal:
	lda #LEX_WORD
	jsr putspelled
	jcs @ret
	jmp @next

@done:	lda #LEX_END
	jsr put
	bcs @ret
	lda outlen
	sta CTX_TOKEN_BUFFER
	clc
	rts
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts

;-------------------------------------------------------------------------------
; check if the given property is is a LOCAL (".local") declaration
; OUT:
;   - .Z: set if LOCAL
@islocal:
	ldx #$00
:	lda token,x
	cmp @localword,x
	bne @notlocal
	inx
	cmp #$00
	bne :-
	lda #$01
	rts
@notlocal:
	lda #$00
	rts

;-------------------------------------------------------------------------------
; check if the given property is is a builtin property (e.g. ".ISIMM")
; OUT:
;   - .Z: set if builtin
@builtin:
	ldx #$00
	lda #FUNC_ISIMM
	sta function

@bn:	ldy #$00
@bc:	lda property_names,x
	cmp token,y
	bne @bs
	inx
	iny
	cmp #$00
	bne @bc
	lda function
	rts

@bs:	lda property_names,x
	inx
	cmp #$00
	bne @bs
	inc function
	lda property_names,x
	bne @bn
	;lda #$00
	rts

;-------------------------------------------------------------------------------
@localword: .byte ".local",0
@ifdef:     .byte ".ifdef",0
@iftext:    .byte LEX_WORD,3,".if",LEX_SPACE
@iftextlen = *-@iftext
.endproc

;*******************************************************************************
; LOCAL
; Handles the .LOCAL directive: records names that are unique to each
; invocation of the macro.
; IN:
;  - zp::line: the list of names
; OUT:
;  - .A: ASM_DIRECTIVE or an error code
;  - .C: set on error
.export __mac_local
.proc __mac_local
	lda zp::verify
	jne @done
	lda __mac_depth
	jeq @bad
	jsr select_frame

	; copy directive operand to the input buffer
	ldy #$00
@copy:	lda (zp::line),y
	sta input,y
	beq @start
	iny
	cpy #MAX_LINE_LEN+1
	bcc @copy
	RETURN_ERR ERR_LINE_TOO_LONG

@start: lda #$00
	sta scan

;-------------------------------------------------------------------------------
; parse one LOCAL definition
@next:	jsr skipspace
	beq @bad

	jsr readtoken		; read the symbol name into "token"
	lda namelen
	cmp #$10
	bcs @bad

	ldxy #token
	CALLMAIN lbl::isvalid	; make sure given name is valid
	bcs @ret

	jsr findparam
	bcc @bad		; make sure name does not shadow macro param
	jsr findlocal
	bcc @sep		; already declared (e.g. in .REP), nothing to do

	; get address to store the symbols's name to
	ldy #F_LOCALS
	LOADB_Y frame		; read # of locals
	cmp #$08
	bcs @bad
	asl
	asl
	asl
	asl			; * 16 (offset to local)
	;clc
	adc #F_NAMES		; + offset to names
	adc frame		; + frame base
	sta param
	lda frame+1
	adc #$00
	sta param+1

	; copy the symbol name to the macro frame
	ldy #$00
@store: lda token,y
	STOREB_Y param
	iny
	cmp #$00
	bne @store
	ldy #F_LOCALS
	LOADB_Y frame
	clc
	adc #$01
	STOREB_Y frame

@sep:	jsr skipspace
	beq @done
	cmp #';'
	beq @done
	cmp #','
	bne @bad
	inc scan
	jmp @next		; check for another declaration

@done:	lda #ASM_DIRECTIVE
	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts
.endproc

;*******************************************************************************
; FILENAME
; Builds a filename from quoted strings and string arguments (joined with '+').
; This is used to parse macro parameters and for the .INC/.INCBIN directives
; in the assembler.
; IN:
;  - zp::line: filename expression
; OUT:
;  - $100: filename
;  - .XY:  address of the final closing quote
;  - .C:   set on error
.export __mac_filename
.proc __mac_filename
	lda #$00
	sta dstbank
	ldxy #$100
	stxy dst
	ldxy zp::line
	stxy src
	lda #$00
	sta outlen
	lda #MAX_LINE_LEN
	sta outlimit

;-------------------------------------------------------------------------------
@component:
	jsr @ws
	cmp #'"'
	beq @quoted
	ldxy src
	stxy zp::line
	lda outlen
	clc
	adc dst
	tax
	lda dst+1
	adc #$00
	tay

	lda outlimit
	sec
	sbc outlen
	jsr __mac_string_arg
	bcs @notarg

	adc outlen
	sta outlen
	ldxy zp::line
	stxy src
	decw src		; the argument token replaces the closing quote
	jmp @close

;-------------------------------------------------------------------------------
@notarg:
	lda zp::verify
	jeq @bad
	ldxy src
	stxy zp::line
	jsr __mac_verify_property
	jcs @bad
	cmp #FUNC_TEXT
	jne @bad
	jsr __mac_verify_name
	jcs @ret
	ldxy zp::line
	stxy src
	decw src
	jmp @close

;-------------------------------------------------------------------------------
@quoted:
	incw src

@chars: ldy #$00
	lda (src),y
	jeq @bad
	cmp #'"'
	beq @close		; close the quote
	jsr put
	bcs @ret
	incw src
	jmp @chars

@close: ldxy src
	stxy savedid
	incw src
	jsr @ws
	cmp #'+'
	bne @done
	incw src
	jmp @component

;-------------------------------------------------------------------------------
@done:	cmp #$00
	beq @ok
	cmp #';'
	beq @ok
	cmp #','
	jne @bad
@ok:	jsr terminate
	ldxy savedid
	RETURN_OK
@bad:	RETURN_ERR ERR_SYNTAX_ERROR
@ret:	rts

;-------------------------------------------------------------------------------
@ws:	ldy #$00
	lda (src),y
	cmp #' '
	beq :+
	cmp #$09
	bne @ret
:	incw src
	jmp @ws
.endproc

;*******************************************************************************
; SET
; Handles the .SET directive: defines or updates an absolute constant.
; IN:
;  - zp::line: name followed by an expression
; OUT:
;  - .A: ASM_DIRECTIVE or an error code
;  - .C: set on error
.export __mac_set
.proc __mac_set
	lda zp::verify
	beq @run
	lda #ASM_DIRECTIVE
	RETURN_OK

@run:	lda zp::line
	pha
	lda zp::line+1
	pha

	CALLMAIN line::process_word
	CALLMAIN line::process_ws
	CALLMAIN expr::eval
	jcs @fail

	lda expr::kind
	cmp #VAL_ABS
	beq :+
	lda #ERR_CANNOT_REDUCE		; if not constant -> fail
	sec
	jcs @fail

:	; define ABS symbol for the variable
	stxy zp::label_value
	lda #SEG_ABS
	sta zp::label_segmentid
	lda #$01
	sta zp::label_mode

	; restore line pointer to the variable name
	pla
	tay
	pla
	tax
	stxy other
	CALLMAIN lbl::find		; check if symbol already defined
	bcs @create			; if not -> create it

	stxy savedid
	jsr findmutable			; ensure mutable defined for this var
	bcc @update			; if found -> update it

	; non-mutable already using this label
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED

@update:
	ldxy savedid
	CALLMAIN lbl::setaddr		; overwrite value
	jmp @result

@create:
	lda mutable_count
	cmp #$40
	bcc :+
	RETURN_ERR ERR_TOO_MANY_LABELS

:	ldxy other
	CALLMAIN lbl::set
	jcs @ret
	stxy savedid

	ldxy #mutable_ids
	stxy src
	lda mutable_count
	asl
	tay
	lda savedid
	STOREB_Y src
	iny
	lda savedid+1
	STOREB_Y src
	inc mutable_count

@result:
	jcs @ret
	lda #ASM_DIRECTIVE
	RETURN_OK

@fail:	tax
	pla
	pla
	txa
	sec
@ret:	rts
.endproc

;*******************************************************************************
; FINDMUTABLE
; Checks if a symbol was defined with .SET.
; IN:
;  - savedid: id of the symbol
; OUT:
;  - .C: clear if symbol is defined (.SET)
.proc findmutable
	ldxy #mutable_ids
	stxy src
	ldy #$00
	ldx mutable_count

@next:	txa
	beq @no
	LOADB_Y src
	cmp savedid
	bne @skip
	iny
	LOADB_Y src
	cmp savedid+1
	beq @yes
	dey

@skip:	iny
	iny
	dex
	jmp @next

@yes:	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; CLEAR MUTABLES
; Clears the state of all .SET symbols (called at the start of a new assembly)
; IN:
;  - None
; OUT:
;  - None
.export __mac_clear_mutables
.proc __mac_clear_mutables
	lda #$00
	sta mutable_count
	rts
.endproc

;*******************************************************************************
; RESET MUTABLES
; Makes all .SET symbols undefined before pass 2 so that each one takes its
; value from each .SET next go-around.
; OUT:
;  - .C: clear
.export __mac_reset_mutables
.proc __mac_reset_mutables
	lda #$00
	sta paramindex

@next:	lda paramindex
	cmp mutable_count
	beq @done

	; look up id of the .SET variable
	asl
	tay
	ldx #<mutable_ids
	stx src
	ldx #>mutable_ids
	stx src+1
	LOADB_Y src
	sta savedid
	iny
	LOADB_Y src
	sta savedid+1

	; set label to UNDEFINED
	lda #SEG_UNDEF
	sta zp::label_segmentid
	lda #$01
	sta zp::label_mode
	lda #$00
	sta zp::label_value
	sta zp::label_value+1
	ldxy savedid
	CALLMAIN lbl::setaddr

	inc paramindex
	jmp @next		; repeat for all mutables

@done:	RETURN_OK
.endproc

;*******************************************************************************
; VERIFY PROPERTY
; Checks for a macro property (.ISIMM, .PRESENT, etc.) when verifying syntax
; IN:
;  - zp::line: a possible property name
; OUT:
;  - .A:       the property's selector (FUNC_ISIMM through FUNC_TEXT)
;  - zp::line: at the '(' (unchanged if not found)
;  - .C:       clear if found
.export __mac_verify_property
.proc __mac_verify_property
	ldx #$00
	lda #FUNC_ISIMM
	sta function

@next:	ldy #$00
@match: lda property_names,x
	beq @endname
	cmp (zp::line),y
	bne @skip
	inx
	iny
	bne @match
@endname:
	lda (zp::line),y
	cmp #'('
	beq @found
@skip:	lda property_names,x
	inx
	cmp #$00
	bne @skip
	inc function
	lda property_names,x
	bne @next
	sec		; not found
	rts

@found: tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	lda function
	clc		; .ISIMM, .PRESENT, etc. found
	rts
.endproc

;*******************************************************************************
; VERIFY NAME
; Checks that the "(NAME)" after a macro property is valid when verifying syntax
; IN:
;  - zp::line: at the '('
; OUT:
;  - zp::line: after the ')'
;  - .C:       set on error
.export __mac_verify_name
.proc __mac_verify_name
	incw zp::line
	CALLMAIN line::process_ws

	ldy #$00
@copy:	lda (zp::line),y
	jsr wordchar
	bcs @endname
	sta token,y
	iny
	cpy #MAX_LINE_LEN
	bcc @copy
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS

@endname:
	cpy #$00
	beq @bad
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	lda #$00
	sta token,y
	ldxy #token
	CALLMAIN lbl::isvalid
	bcs @ret
	CALLMAIN line::process_ws
	cmp #')'
	bne @bad

	incw zp::line
	RETURN_OK
@ret:	rts
.endproc

;*******************************************************************************
; PROPERTY NAMES
; 0-terminated list of macro property names in FUNC_ISIMM through FUNC_TEXT order
property_names:
	.byte ".isimm",0,".present",0,".value",0,".ident",0,".text",0,0

;*******************************************************************************
; DIGITS
; Array of hex digits
digits: .byte "0123456789abcdef"
