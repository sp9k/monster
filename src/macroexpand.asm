;*******************************************************************************
; Operand-aware macro invocation frames and bounded source expansion.
.include "asm.inc"
.include "config.inc"
.include "codes.inc"
.include "ctx.inc"
.include "context_tokens.inc"
.include "lexer.inc"
.include "errors.inc"
.include "expr.inc"
.include "labels.inc"
.include "line.inc"
.include "limits.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "target.inc"
.include "zeropage.inc"
.macpack longbranch
.import macro_addresses, macro_enabled, __mac_get
.import __asm_ifstack, __asm_ifdepth
.import __label_namespace_depth, __label_namespace_unwind

MAX_DEPTH = 8
; Each $200-byte frame owns its argument spelling, caller-bound expressions,
; local names, body cursor and saved control state. The first page contains the
; header, 80 bytes of spelling, and eight 16-byte local-name slots; the second
; page holds the bound expressions. Frames use LOADB/STOREB on both targets.
F_BODY = 0
F_PARAMS = 2
F_COUNT = 4
F_SERIAL = 5
F_RAWSTART = 8
F_BOUNDSTART = 12
F_FLAGS = 16
F_LOCALS = 20
F_SCOPE = 21
F_IF = 22
F_IFSTATE = 23
F_NAMESPACE = 31
F_RAW = 32
F_NAMES = 128
F_BOUND = 256
ARG_PRESENT = 1
ARG_IMMEDIATE = 2

.segment "MACROBSS"
frames: .res MAX_DEPTH*$200
mutable_ids: .res $80

.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "BSS_NOINIT"
.endif
.export __mac_source, __mac_depth
__mac_source = mem::spare+$80
__mac_depth: .byte 0

.segment "MACRO_VARS"
expand_vars:
serial: .word 0
input = __mac_source
; Shared staging buffers are consumed before calling the assembler again.
; Nested calls retain all persistent data in their own frames.
output = mem::spare+$d1
token = lbl::namebuffer
scan: .byte 0
begin: .byte 0
finish: .byte 0
quote: .byte 0
parens: .byte 0
arg: .byte 0
rawtop: .byte 0
boundtop: .byte 0
outlen: .byte 0
outlimit: .byte 0
function: .byte 0
forward: .byte 0
localdef: .byte 0
paramindex: .byte 0
namelen: .byte 0
savedscan: .byte 0
savedid: .word 0
hexsave: .byte 0
dstbank: .byte 0
srcbank: .byte 0
parentarg: .byte 0
mutable_count: .byte 0
mayoperand: .byte 0
input_base: .byte 0
cached_index: .byte 0
cached_lo: .byte 0
cached_hi: .byte 0
.res $20-(*-expand_vars)

frame = zp::macros
src = zp::macros+2
dst = zp::macros+4
param = zp::macros+6
other = zp::macros+8

BANKED_SEG "MACROCODE", FINAL_BANK_MACROS

;*******************************************************************************
; RESET EXPANSION
; Clears invocation state at the start of an assembly pass.
; IN: None
; OUT: .C clear
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
; Selects the current invocation's banked frame.
; IN: __mac_depth, in the range 1 through MAX_DEPTH
; OUT: frame points to the current invocation
.proc select_frame
	lda #<frames
	sta frame
	lda __mac_depth
	sec
	sbc #$01
	asl
	clc
	adc #>frames
	sta frame+1
	rts
.endproc

;*******************************************************************************
; ASM
; Collects operands, expands a macro, and restores the caller on every exit.
; IN: .A macro ID; zp::line points after the invocation name
; OUT: .C set and .A error code on failure
.export __mac_asm
.proc __mac_asm
	pha
	lda __mac_depth
	cmp #MAX_DEPTH
	bcc :+
	pla
	RETURN_ERR ERR_STACK_OVERFLOW
:	inc __mac_depth
	jsr select_frame
	lda #$00
	ldy #$1f
@clear:
	STOREB_Y frame
	dey
	bpl @clear
	ldy #F_IF
	lda __asm_ifdepth
	STOREB_Y frame
	ldx #$00
@saveifs:
	lda __asm_ifstack,x
	iny
	STOREB_Y frame
	inx
	cpx #$08
	bcc @saveifs
	lda __label_namespace_depth
	ldy #F_NAMESPACE
	STOREB_Y frame
	pla
	asl
	tax
	lda macro_addresses,x
	sta src
	lda macro_addresses+1,x
	sta src+1
	ldy #$00
@name:
	LOADB_Y src
	incw src
	cmp #$00
	bne @name
	LOADB_Y src
	ldy #F_COUNT
	STOREB_Y frame
	incw src
	ldy #F_PARAMS
	lda src
	STOREB_Y frame
	iny
	lda src+1
	STOREB_Y frame
	incw serial
	lda serial
	ora serial+1
	bne :+
	lda #ERR_TOO_MANY_MACROS
	jmp @fail
:	ldy #F_SERIAL
	lda serial
	STOREB_Y frame
	iny
	lda serial+1
	STOREB_Y frame

	; Copy original spelling before a nested invocation reuses the source buffer.
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
@collect:
	jsr collect
	jcs @fail
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
	; Locate the first body line after the parameter-name list.
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
@body:
	ldy #F_BODY
	lda src
	STOREB_Y frame
	iny
	lda src+1
	STOREB_Y frame
@next:
	jsr select_frame
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
@copy:
	LOADBLK8 src, CTX_TOKEN_BUFFER, CTX_TOKEN_BUFFER
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
	CALL LEX_BANK, lex::decode
	jcs @fail
	lda #$00
	sta input_base
	CALL FINAL_BANK_ASM, macro_enabled
	beq @disabled
	jsr expand
	bcs @fail
	jmp @assemble
@disabled:
	ldy #$00
:	lda input,y
	sta output,y
	beq @assemble
	iny
	bne :-
@assemble:
	; Unchanged rows can reuse the decoded view and its token spans.
	ldy #$00
@compare:
	lda output,y
	cmp mem::asmbuffer,y
	bne @changed
	cmp #$00
	beq @unchanged
	iny
	bne @compare
@unchanged:
	CALLMAIN asm::assemble_view
	bcs @fail
	jmp @next
@changed:
	ldy #$00
@stage:
	lda output,y
	sta mem::asmbuffer,y
	beq @prepared
	iny
	bne @stage
@prepared:
	CALLMAIN asm::assemble_line
	bcs @fail
	jmp @next
@ok:
	ldy #F_IF
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
@fail:
	php
	pha
	jsr select_frame
	ldy #F_IF
	LOADB_Y frame
	sta __asm_ifdepth
	ldx #$00
@restoreifs:
	iny
	LOADB_Y frame
	sta __asm_ifstack,x
	inx
	cpx #$08
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
; IN: __mac_depth is nonzero
; OUT: .A namespace depth
.export __mac_namespace_floor
.proc __mac_namespace_floor
	jsr select_frame
	ldy #F_NAMESPACE
	LOADB_Y frame
	rts
.endproc

;*******************************************************************************
; FOLD
; Normalizes an ASCII lower-case letter to the assembler's PETSCII spelling.
; IN: .A character
; OUT: .A normalized character
.proc fold
	cmp #$61
	bcc :+
	cmp #$7b
	bcs :+
	eor #$20
:	rts
.endproc

;*******************************************************************************
; WORD CHARACTER
; Recognizes the characters allowed within an identifier.
; IN: .A character
; OUT: .C clear for a word character, set otherwise
.proc wordchar
	jsr fold
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
; SKIP SPACE
; Advances scan past spaces and tabs in input.
; IN: input, scan
; OUT: .A first non-space character; scan updated
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
; Appends a byte to the selected banked or shared output buffer.
; IN: .A byte; dst buffer; outlen cursor; outlimit exclusive data limit
; OUT: .C set on overflow; outlen advanced on success
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
; Terminates an output buffer without advancing its cursor.
; IN: dst buffer; outlen length
; OUT: terminating zero stored
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
; Splits comma-separated arguments, retaining empty slots and original text.
; IN: input invocation operands; frame parameter count
; OUT: frame argument records; .C set on malformed or oversized input
.proc collect
	lda #$00
	sta scan
	sta arg
	sta rawtop
	sta boundtop
	jsr skipspace
	jeq @done
	cmp #';'
	jeq @done
@argument:
	ldy #F_COUNT
	LOADB_Y frame
	cmp arg
	jeq @bad
	jcc @bad
	jsr skipspace
	lda scan
	sta begin
	lda #$00
	sta quote
	sta parens
@scan:
	ldx scan
	lda input,x
	beq @end
	ldx quote
	beq @unquoted
	cmp quote
	bne @step
	lda #$00
	sta quote
	beq @step
@unquoted:
	cmp #'"'
	beq @quote
	cmp #$27
	beq @character
	cmp #'('
	bne :+
	inc parens
	bne @step
:	cmp #')'
	bne :+
	lda parens
	jeq @bad
	dec parens
	jmp @step
:	cmp #';'
	beq @end
	cmp #','
	bne @step
	lda parens
	beq @end
	bne @step
@character:
	ldx scan
	lda input+1,x
	jeq @bad
	lda input+2,x
	cmp #$27
	jne @bad
	inc scan
	inc scan
	jmp @step
@quote:
	sta quote
@step:
	inc scan
	bne @scan
@end:
	lda quote
	ora parens
	jne @bad
	lda scan
	sta finish
	; Trim whitespace before recording the slot.
@trim:
	lda finish
	cmp begin
	beq @store
	tax
	lda input-1,x
	cmp #' '
	beq :+
	cmp #$09
	bne @store
:	dec finish
	bne @trim
@store:
	jsr store_argument
	bcs @ret
	inc arg
	ldx scan
	lda input,x
	cmp #','
	bne @done
	inc scan
	jmp @argument
@done:	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts
.endproc

;*******************************************************************************
; STORE ARGUMENT
; Copies a trimmed source argument and binds its expression identifiers.
; IN: begin/finish indices in input; arg index
; OUT: raw and bound frame strings; .C set on overflow
.proc store_argument
	lda #$01
	sta dstbank
	lda arg
	clc
	adc #F_RAWSTART
	tay
	lda rawtop
	STOREB_Y frame
	lda arg
	clc
	adc #F_BOUNDSTART
	tay
	lda boundtop
	STOREB_Y frame
	ldx begin
	lda input,x
	cmp #'!'
	jne @normal
	lda finish
	sec
	sbc begin
	cmp #$02
	jne @normal
	lda __mac_depth
	cmp #$02
	jcc @badforward
	lda input+1,x
	sec
	sbc #'0'
	cmp #$04
	jcs @badforward
	sta parentarg
	jmp clone_argument
@badforward:
	RETURN_ERR ERR_INVALID_MACRO_ARGS
@normal:
	lda finish
	cmp begin
	beq @empty
	lda #ARG_PRESENT
	ldx begin
	ldy input,x
	cpy #'#'
	bne :+
	ora #ARG_IMMEDIATE
:	pha
	lda arg
	clc
	adc #F_FLAGS
	tay
	pla
	STOREB_Y frame
@empty:
	lda frame
	clc
	adc #F_RAW
	sta dst
	lda frame+1
	adc #$00
	sta dst+1
	lda rawtop
	sta outlen
	lda #MAX_LINE_LEN
	sta outlimit
	ldx begin
@raw:
	cpx finish
	beq @rawdone
	lda lex::cached
	bpl @source_byte
	jsr cached_integer
	bcc @raw
	cmp #$00
	bne @ret
@source_byte:
	lda input,x
	jsr put
	bcs @ret
	inx
	bne @raw
@rawdone:
	lda #$00
	jsr put
	bcs @ret
	lda outlen
	sta rawtop
	lda frame
	sta dst
	lda frame+1
	clc
	adc #$01
	sta dst+1
	lda boundtop
	sta outlen
	lda #$ff
	sta outlimit
	jsr bind
	bcs @ret
	lda #$00
	jsr put
	bcs @ret
	lda outlen
	sta boundtop
	clc
@ret:	rts
.endproc

;*******************************************************************************
; CLONE ARGUMENT
; Forwards an enclosing argument without losing its spelling or presence.
; IN: parentarg index; child frame with initialized start offsets
; OUT: child argument copied; .C set on overflow
.proc clone_argument
	lda frame
	sta other
	lda frame+1
	sec
	sbc #$02
	sta other+1
	lda parentarg
	clc
	adc #F_FLAGS
	tay
	LOADB_Y other
	pha
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
	clc
	adc #F_RAW
	sta dst
	lda frame+1
	adc #$00
	sta dst+1
	lda rawtop
	sta outlen
	lda #MAX_LINE_LEN
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
	adc #$01
	sta src+1
	lda frame
	sta dst
	lda frame+1
	clc
	adc #$01
	sta dst+1
	lda boundtop
	sta outlen
	lda #$ff
	sta outlimit
	jsr copystring
	bcs @ret
	lda #$00
	jsr put
	bcs @ret
	lda outlen
	sta boundtop
	clc
@ret:	rts
.endproc

;*******************************************************************************
; CACHED INTEGER
; Appends a captured iterator value as a literal to an argument or expansion.
; IN: .X input index, input_base source-view offset, current output destination
; OUT: .C clear and .X advanced if emitted; .C set/.A zero if absent, error otherwise
.proc cached_integer
	stx cached_index
	txa
	clc
	adc input_base
	tax
	CALL LEX_BANK, lex::value_at
	bcs @absent
	stx cached_lo
	sty cached_hi
	lda #'$'
	jsr put
	bcs @ret
	lda cached_hi
	jsr hexbyte
	bcs @ret
	lda cached_lo
	jsr hexbyte
	bcs @ret
	ldx cached_index
	inx
	clc
	rts
@absent:
	ldx cached_index
	lda #$00
	sec
@ret:	rts
.endproc

;*******************************************************************************
; HEX BYTE
; Writes two hexadecimal digits.
; IN: .A byte; selected output buffer
; OUT: .C set on overflow
.proc hexbyte
	pha
	lsr
	lsr
	lsr
	lsr
	jsr @digit
	bcs @pop
	pla
	and #$0f
@digit:
	stx hexsave
	tax
	lda @digits,x
	ldx hexsave
	jmp put
@pop:
	pla
	sec
	rts
@digits: .byte "0123456789abcdef"
.endproc

;*******************************************************************************
; BIND
; Replaces expression identifiers with stable symbol-ID atoms in caller scope.
; IN: input[begin:finish], frame, dst, outlen
; OUT: bound source; .C set on invalid names or overflow
.proc bind
	ldx begin
	lda #$00
	sta quote
	lda #$01
	sta mayoperand
@next:
	cpx finish
	jeq @ok
	lda lex::cached
	bpl @source_byte
	jsr cached_integer
	bcs :+
	lda #$00
	sta mayoperand
	jmp @next
:
	cmp #$00
	jne @ret
@source_byte:
	lda input,x
	ldy quote
	beq @plain
	cmp quote
	bne @copy
	lda #$00
	sta quote
	sta mayoperand
	lda input,x
	jmp @copy
@plain:
	cmp #'*'
	bne :+
	lda mayoperand
	beq @copy_original
	inx
	stx savedscan
	jsr bind_pc
	jcs @ret
	jmp @found
:	cmp #'"'
	beq @quoted
	cmp #$27
	beq @character
	; Numbers and already-bound atoms contain letters that are not names.
	cmp #'$'
	beq @number
	cmp #'0'
	bcc @start
	cmp #'9'+1
	bcc @number
@start:
	jsr fold
	cmp #'@'
	beq @name
	cmp #'.'
	beq @name
	cmp #'a'
	bcc @copy_original
	cmp #'Z'+1
	bcc @name
@copy_original:
	lda input,x
	cmp #' '
	beq @copy
	cmp #$09
	beq @copy
	ldy #$01
	cmp #')'
	bne :+
	dey
:	sty mayoperand

@copy:
	jsr put
	jcs @ret
	inx
	jmp @next
@character:
	jsr copychar
	jcs @ret
	lda #$00
	sta mayoperand
	jmp @next
@quoted:
	sta quote
	jmp @copy
@number:
	lda #$00
	sta mayoperand
	lda input,x
	jsr put
	jcs @ret
	inx
	cpx finish
	jeq @ok
	lda input,x
	jsr wordchar
	bcc @number
	jmp @next
@name:
	ldy #$00
@word:
	lda input,x
	jsr fold
	sta token,y
	iny
	inx
	cpx finish
	beq @worddone
	lda input,x
	jsr wordchar
	bcc @word
@worddone:
	lda #$00
	sta token,y
	stx savedscan
	; Numeric functions retain their spelling and are parsed by expr.
	lda input,x
	cmp #'('
	beq @function
	ldxy #token
	CALLMAIN lbl::find
	bcc @found
	cmp #ERR_LABEL_UNDEFINED
	bne @ret
	ldxy #SYM_UNRESOLVED
@found:
	lda #$00
	sta mayoperand
	stxy savedid
	lda #'?'
	jsr put
	bcs @ret
	lda #'$'
	jsr put
	bcs @ret
	lda savedid+1
	jsr hexbyte
	bcs @ret
	lda savedid
	jsr hexbyte
	bcs @ret
	ldx savedscan
	jmp @next
@function:
	ldx #$00
@fncopy:
	lda token,x
	beq @fnend
	jsr put
	bcs @ret
	inx
	bne @fncopy
@fnend:
	ldx savedscan
	jmp @next
@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; BIND PC
; Gives the invocation's current address a stable relocatable symbol.
; IN: frame serial and the caller's current PC/segment
; OUT: .XY symbol ID; .C set on failure
.proc bind_pc
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
@hex:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr @digit
	pla
	and #$0f
@digit:
	tay
	lda @digits,y
	sta token,x
	inx
	rts
@digits: .byte "0123456789abcdef"
.endproc

;*******************************************************************************
; READ TOKEN
; Copies an input identifier to the normalized token buffer.
; IN: scan at the identifier
; OUT: token, namelen; scan after the identifier
.proc readtoken
	ldx scan
	ldy #$00
@next:
	lda input,x
	jsr fold
	sta token,y
	iny
	inx
	lda input,x
	jsr wordchar
	bcc @next
	stx scan
	sty namelen
	lda #$00
	sta token,y
	rts
.endproc

;*******************************************************************************
; FIND PARAMETER
; Looks up a normalized token in the current macro's parameter list.
; IN: token, frame
; OUT: paramindex and .C clear if found; .C set otherwise
.proc findparam
	ldy #F_PARAMS
	LOADB_Y frame
	sta param
	iny
	LOADB_Y frame
	sta param+1
	lda #$00
	sta paramindex
@next:
	ldy #F_COUNT
	LOADB_Y frame
	cmp paramindex
	beq @no
	ldy #$00
@compare:
	LOADB_Y param
	cmp token,y
	bne @skip
	cmp #$00
	beq @yes
	iny
	bne @compare
@skip:
	ldy #$00
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
; EMIT PARAMETER
; Expands a parameter or one of its explicit properties.
; IN: paramindex; function: 0 operand, 1 isimm, 2 present, 3 value,
;     4 ident, 5 text
; OUT: output appended; .C set on malformed use or overflow
.proc emitparam
	lda #$01
	sta srcbank
	lda paramindex
	clc
	adc #F_FLAGS
	tay
	LOADB_Y frame
	tax
	lda function
	cmp #$01
	beq @imm
	cmp #$02
	beq @present
	txa
	and #ARG_PRESENT
	jne @value
	RETURN_ERR ERR_INVALID_MACRO_ARGS
@imm:
	txa
	and #ARG_IMMEDIATE
	beq @false
@true:
	lda #'1'
	jmp put
@present:
	txa
	and #ARG_PRESENT
	bne @true
@false:
	lda #'0'
	jmp put
@value:
	lda function
	cmp #$04
	bcs @raw
	lda paramindex
	clc
	adc #F_BOUNDSTART
	tay
	LOADB_Y frame
	clc
	adc frame
	sta src
	lda frame+1
	adc #$01
	sta src+1
	ldy #$00
	LOADB_Y src
	cmp #'#'
	bne @open
	incw src
	lda function
	bne @open
	lda #'#'
	jsr put
	jcs @ret
@open:
	ldy #$00
	LOADB_Y src
	cmp #'"'
	beq @copy
	lda #'+'
	jsr put
	jcs @ret
	lda #'('
	jsr put
	jcs @ret
	jsr copystring
	jcs @ret
	lda #')'
	jmp put
@raw:
	lda paramindex
	clc
	adc #F_RAWSTART
	tay
	LOADB_Y frame
	clc
	adc frame
	sta src
	lda frame+1
	adc #$00
	sta src+1
	lda src
	clc
	adc #F_RAW
	sta src
	bcc :+
	inc src+1
:	lda function
	cmp #$05
	bne @copy
	ldy #$00
	LOADB_Y src
	cmp #'"'
	beq @copy
	lda #'"'
	jsr put
	bcs @ret
	jsr copystring
	bcs @ret
	lda #'"'
	jmp put
@copy:
	jmp copystring
@ret:	rts
.endproc

;*******************************************************************************
; COPY STRING
; Appends a banked zero-terminated string to the output.
; IN: src string, dst output
; OUT: .C set on overflow
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
; LOCAL NAME
; Emits the expansion's unique qualified local name.
; IN: token; frame serial
; OUT: output appended; .C set on overflow
.proc localname
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
; FIND LOCAL
; Looks up a token in the expansion's explicit .LOCAL declarations.
; IN: token, frame
; OUT: .C clear if declared local
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
@next:
	cpx #$00
	beq @no
	ldy #$00
@compare:
	LOADB_Y param
	cmp token,y
	bne @skip
	cmp #$00
	beq @yes
	iny
	bne @compare
@skip:
	lda param
	clc
	adc #$10
	sta param
	bcc :+
	inc param+1
:	dex
	jmp @next
@yes:	clc
	rts
@no:	sec
	rts
.endproc

;*******************************************************************************
; EXPAND
; Substitutes operands and local names outside strings and comments.
; IN: input macro body line; current frame
; OUT: output expanded line; .C set on malformed use or overflow
.proc expand
	ldxy #output
	stxy dst
	lda #MAX_LINE_LEN
	sta outlimit
	lda #$00
	sta scan
	sta outlen
	sta quote
	sta localdef
	sta dstbank
	jsr skipspace
	lda scan
	pha
	jsr readtoken
	lda scan
	sta finish
	ldxy #token
	jsr @islocal
	sta localdef
	ldxy #token
	jsr __mac_get
	lda #$00
	rol
	eor #$01
	sta forward
	; Preserve the documented .IFDEF test for omitted formal parameters.
	ldx #$00
@ifdefword:
	lda token,x
	cmp @ifdef,x
	bne @notifdef
	inx
	cmp #$00
	bne @ifdefword
	jsr skipspace
	jsr readtoken
	jsr findparam
	bcs @notifdef
	pla
	ldx #$00
@writeif:
	lda @iftext,x
	beq @presence
	jsr put
	jcs @ret
	inx
	bne @writeif
@presence:
	lda #$02
	sta function
	jsr emitparam
	jcs @ret
	jmp @done
@notifdef:
	pla
	sta scan
@next:
	ldx scan
	lda lex::cached
	bpl @source_byte
	jsr cached_integer
	bcs :+
	stx scan
	jmp @next
:	cmp #$00
	jne @ret
@source_byte:
	lda input,x
	jeq @done
	ldx quote
	beq @plain
	cmp quote
	bne @copy
	lda #$00
	sta quote
	ldx scan
	lda input,x
	jmp @copy
@plain:
	cmp #';'
	jeq @done
	cmp #'$'
	beq @numeric
	cmp #'0'
	bcc :+
	cmp #'9'+1
	bcc @numeric
:
	cmp #'"'
	beq @quoted
	cmp #$27
	beq @character
	jsr fold
	cmp #'@'
	beq @word
	cmp #'.'
	beq @word
	cmp #'a'
	bcc @copyoriginal
	cmp #'Z'+1
	bcc @word
@copyoriginal:
	ldx scan
	lda input,x
@copy:
	jsr put
	jcs @ret
	inc scan
	jmp @next
@numeric:
	jsr put
	jcs @ret
	inc scan
	ldx scan
	lda input,x
	jsr wordchar
	bcc @numeric
	jmp @next
@character:
	ldx scan
	jsr copychar
	jcs @ret
	stx scan
	jmp @next
@quoted:
	sta quote
	jmp @copy
@word:
	lda scan
	sta begin
	jsr readtoken
	lda #$00
	sta function
	ldx scan
	lda input,x
	cmp #'('
	bne @ordinary
	jsr @builtin
	sta function
	beq @ordinary
	sta function
	inc scan
	jsr skipspace
	jsr readtoken
	jsr skipspace
	cmp #')'
	jne @bad
	inc scan
	jsr findparam
	jcs @bad
	jsr emitparam
	jcs @ret
	jmp @next
@ordinary:
	jsr findparam
	bcs @local
	lda forward
	beq @emit
	ldx begin
@before:
	cpx finish
	beq @after
	dex
	lda input,x
	cmp #' '
	beq @before
	cmp #$09
	beq @before
	cmp #','
	bne @emit
@after:
	ldx scan
@afterspace:
	lda input,x
	cmp #' '
	beq @space
	cmp #$09
	bne @aftervalue
@space:	inx
	bne @afterspace
@aftervalue:
	lda input,x
	cmp #$00
	beq @forward
	cmp #','
	beq @forward
	cmp #';'
	bne @emit
@forward:
	lda #'!'
	jsr put
	jcs @ret
	lda paramindex
	clc
	adc #'0'
	jsr put
	jcs @ret
	jmp @next
@emit:
	jsr emitparam
	jcs @ret
	jmp @next
@local:
	lda localdef
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
	lda #$00
	sta srcbank
	ldxy #token
	stxy src
	jsr copystring
	jcs @ret
	jmp @next
@done:
	jsr terminate
	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts
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
@localword: .byte ".local",0
@ifdef: .byte ".ifdef",0
@iftext: .byte ".if ",0
@builtin:
	ldx #$00
	lda #$01
	sta function
@bn:
	ldy #$00
@bc:
	lda @builtins,x
	cmp token,y
	bne @bs
	inx
	iny
	cmp #$00
	bne @bc
	lda function
	rts
@bs:
	lda @builtins,x
	inx
	cmp #$00
	bne @bs
	inc function
	lda @builtins,x
	bne @bn
	lda #$00
	rts
@builtins:
	.byte ".isimm",0,".present",0,".value",0,".ident",0,".text",0,0
.endproc

;*******************************************************************************
; LOCAL
; Records a list of ordinary expansion-local names.
; IN: zp::line declaration operands
; OUT: .C set on invalid names, excessive declarations, or use outside a macro
.export __mac_local
.proc __mac_local
	lda zp::verify
	jne @done
	lda __mac_depth
	jeq @bad
	jsr select_frame
	ldy #$00
@copy:
	lda (zp::line),y
	sta input,y
	beq @start
	iny
	cpy #MAX_LINE_LEN+1
	bcc @copy
	RETURN_ERR ERR_LINE_TOO_LONG
@start:
	lda #$00
	sta scan
@next:
	jsr skipspace
	beq @bad
	jsr readtoken
	lda namelen
	cmp #$10
	bcs @bad
	ldxy #token
	CALLMAIN lbl::isvalid
	bcs @ret
	jsr findparam
	bcc @bad
	jsr findlocal
	bcc @bad
	ldy #F_LOCALS
	LOADB_Y frame
	cmp #$08
	bcs @bad
	asl
	asl
	asl
	asl
	clc
	adc #F_NAMES
	adc frame
	sta param
	lda frame+1
	adc #$00
	sta param+1
	ldy #$00
@store:
	lda token,y
	STOREB_Y param
	iny
	cmp #$00
	bne @store
	ldy #F_LOCALS
	LOADB_Y frame
	clc
	adc #$01
	STOREB_Y frame
	jsr skipspace
	beq @done
	cmp #';'
	beq @done
	cmp #','
	bne @bad
	inc scan
	jmp @next
@done:
	lda #ASM_DIRECTIVE
	RETURN_OK
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
@ret:	rts
.endproc

;*******************************************************************************
; FILENAME
; Concatenates quoted filename components separated by plus signs.
; IN: zp::line string expression
; OUT: $100 filename; .XY at final closing quote; .C set on invalid input
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
@component:
	jsr @ws
	cmp #'"'
	beq @quoted
	lda zp::verify
	jeq @bad
	ldxy src
	stxy zp::line
	jsr __mac_verify_property
	jcs @bad
	cmp #$05
	jne @bad
	jsr __mac_verify_name
	jcs @ret
	ldxy zp::line
	stxy src
	decw src
	jmp @close
@quoted:
	incw src
@chars:
	ldy #$00
	lda (src),y
	jeq @bad
	cmp #'"'
	beq @close
	jsr put
	bcs @ret
	incw src
	jmp @chars
@close:
	ldxy src
	stxy savedid
	incw src
	jsr @ws
	cmp #'+'
	bne @done
	incw src
	jmp @component
@done:
	cmp #$00
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
@ws:
	ldy #$00
	lda (src),y
	cmp #' '
	beq @skip
	cmp #$09
	bne @ret
@skip:	incw src
	jmp @ws
.endproc
;*******************************************************************************
; SET
; Assigns a resolved integer constant on each assembly pass.
; IN: zp::line name followed by an expression
; OUT: .C set and .A error code on invalid input
.export __mac_set
.proc __mac_set
	lda zp::verify
	beq @run
	lda #ASM_DIRECTIVE
	RETURN_OK
@run:
	lda zp::line
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
	lda #ERR_CANNOT_REDUCE
	sec
	jcs @fail
:	clc
	stxy zp::label_value
	lda #SEG_ABS
	sta zp::label_segmentid
	lda #$01
	sta zp::label_mode
	pla
	tay
	pla
	tax
	stxy other
	CALLMAIN lbl::find
	bcs @create
	stxy savedid
	jsr findmutable
	bcc @update
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED
@update:
	ldxy savedid
	CALLMAIN lbl::setaddr
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
; FIND MUTABLE
; Checks whether a symbol was declared with .SET in this assembly.
; IN: savedid symbol ID
; OUT: .C clear if present, set otherwise
.proc findmutable
	ldxy #mutable_ids
	stxy src
	ldy #$00
	ldx mutable_count
@next:
	cpx #$00
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
; Discards mutable-symbol ownership when a new assembly begins.
; IN: None
; OUT: mutable_count is zero
.export __mac_clear_mutables
.proc __mac_clear_mutables
	lda #$00
	sta mutable_count
	rts
.endproc

;*******************************************************************************
; RESET MUTABLES
; Invalidates .SET values before replaying assignments in pass two.
; IN: mutable_ids from pass one
; OUT: mutable symbols have undefined values until their assignments execute
.export __mac_reset_mutables
.proc __mac_reset_mutables
	lda #$00
	sta paramindex
@next:
	lda paramindex
	cmp mutable_count
	beq @done
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
	jmp @next
@done:	RETURN_OK
.endproc
;*******************************************************************************
; COPY CHARACTER
; Copies one three-byte character literal, including an apostrophe literal.
; IN: .X offset of opening quote in input; selected output buffer
; OUT: .X after closing quote; .C set on invalid input or overflow
.proc copychar
	lda input+1,x
	beq @bad
	lda input+2,x
	cmp #$27
	bne @bad
	lda input,x
	jsr put
	bcs @ret
	inx
	lda input,x
	jsr put
	bcs @ret
	inx
	lda input,x
	jsr put
	bcs @ret
	inx
	clc
@ret:	rts
@bad:	RETURN_ERR ERR_INVALID_MACRO_ARGS
.endproc
;*******************************************************************************
; VERIFY PROPERTY
; Recognizes a macro property while checking an editor line's syntax.
; IN: zp::line at a possible property name
; OUT: .A property index (1..5), .C clear, line at '(' on success;
;      .C set and line unchanged when not recognized
.export __mac_verify_property
.proc __mac_verify_property
	ldx #$00
	lda #$01
	sta function
@next:
	ldy #$00
@match:
	lda @names,x
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
@skip:
	lda @names,x
	inx
	cmp #$00
	bne @skip
	inc function
	lda @names,x
	bne @next
	sec
	rts
@found:
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	lda function
	clc
	rts
@names: .byte ".isimm",0,".present",0,".value",0,".ident",0,".text",0,0
.endproc

;*******************************************************************************
; VERIFY NAME
; Checks a property's parenthesized formal-parameter name without expansion.
; IN: zp::line at '('; syntax verification active
; OUT: zp::line after ')'; .C set on invalid syntax
.export __mac_verify_name
.proc __mac_verify_name
	incw zp::line
	CALLMAIN line::process_ws
	ldy #$00
@copy:
	lda (zp::line),y
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
