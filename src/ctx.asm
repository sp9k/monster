;*******************************************************************************
; CTX.ASM
; This file contains the code for interacting with the assembly "context"
; The "context" is a special buffer used by the .MAC and .REP directives to
; store lines of data, which is required to complete the assembly of these
; directives when their corresponding .ENDMAC or .ENDREP directive is found.
; Bodies contain length-prefixed lexical records with source line numbers.
; See context_tokens.inc for the internal encoding.
;*******************************************************************************

.include "asm.inc"
.include "config.inc"
.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "util.inc"
.include "target.inc"
.include "zeropage.inc"
.include "lexer.inc"
.include "context_tokens.inc"
.import __ctx_encode
.macpack longbranch

;*******************************************************************************
.exportzp __ctx_numlines
.export __ctx_push
.export __ctx_rewind
.export __ctx_pop
.export __ctx_getline
.export __ctx_getrecord
.export __ctx_getparams
.export __ctx_write_parent
.export __ctx_write
.export __ctx_end
.export __ctx_addparam

.export __ctx_active
.export __ctx_open

;*******************************************************************************
; CONSTANTS
CONTEXT_SIZE      = $1000	; size of buffer per context
PARAM_LENGTH      = 16		; size of param (stored after the context data)
MAX_PARAMS        = 4		; max params for a context
MAX_CONTEXTS      = 4		; max nesting depth for contexts
SIZEOF_CTX_HEADER = 13

;*******************************************************************************
; CONTEXTS
; Contexts are stored in spare mem, which is unused by the assembler during the
; assembly of a program.
; The number of contexts is limited by the size of a context (defined as
; CONTEXT_SIZE).
.segment "CTX_BSS"
.export contexts
contexts: .res MAX_CONTEXTS*CONTEXT_SIZE
contexts_top:

;*******************************************************************************
; BSS
.segment "SHAREBSS"

__ctx_active: .byte 0	; # of contexts on stack - !0: a context is active
__ctx_open:   .byte 0	; !0: current context is "closed" (ctx::end was called)

;*******************************************************************************
; CTX META
ctx       = zp::ctx+0	; address of context
meta      = zp::ctx+2	; context metadata base
iter      = meta+0	; (REP) iterator's current value (set externally)
iterend   = meta+2	; (REP) iterator's end value (set externally)
cur       = meta+4	; cursor to current ctx data
params    = meta+6	; address of params (grows down from CONTEXT+$200-PARAM_LENGTH)
numparams = meta+8	; the number of parameters for the context
type      = meta+9	; the type of the context
numlines  = meta+10	; number of lines in the context
parent    = meta+11	; address of parent context's line buffer

__ctx_numlines  = numlines
__ctx_numparams = numparams

.CODE
;*******************************************************************************
; INIT
; initializes the context state by clearing the stack
.export  __ctx_init
.proc __ctx_init
	; init ctx pointer to base of contexts - CONTEXT_SIZE
	lda #<(contexts-CONTEXT_SIZE+2)
	sta ctx
	clc
	adc #SIZEOF_CTX_HEADER
	sta cur

	lda #>(contexts-CONTEXT_SIZE+2)
	sta ctx+1
	adc #$00
	sta cur+1

	lda #$00
	sta __ctx_active	; set activectx id to base (0)
	sta __ctx_open		; no context open

	rts
.endproc

;*******************************************************************************
; Banked memory mappings
__ctx_push:         JUMP FINAL_BANK_CTX, push
__ctx_rewind:       JUMP FINAL_BANK_CTX, rewind
__ctx_pop:          JUMP FINAL_BANK_CTX, pop
__ctx_getline:      JUMP FINAL_BANK_CTX, getline
__ctx_getrecord:    JUMP FINAL_BANK_CTX, getrecord
__ctx_getparams:    JUMP FINAL_BANK_CTX, getparams
__ctx_write_parent: JUMP FINAL_BANK_CTX, write_parent
__ctx_write:	    JUMP FINAL_BANK_CTX, write
__ctx_end:	    JUMP FINAL_BANK_CTX, end
__ctx_addparam:     JUMP FINAL_BANK_CTX, addparam

BANKED_SEG "CTX", FINAL_BANK_CTX

;*******************************************************************************
; IS WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.proc is_whitespace
	.include "inline/is_ws.asm"
.endproc

;*******************************************************************************
; PUSH
; Saves the current context and beings a new one
; OUT:
; - .C: set if there is no room to create a new context
.proc push
	pha			; save type for the new context

	lda __ctx_active
	beq @init		; no active context -> continue
	cmp #MAX_CONTEXTS
	bcc @save

@err:	pla			; clean stack
	lda #ERR_STACK_OVERFLOW	; too many contexts
	sec
	rts

@save:	lda cur
	pha
	lda cur+1
	pha

	; save the active context's state
	STOREBLK meta, ctx, SIZEOF_CTX_HEADER

	; set current context's cursor as new one's parent
	pla
	sta parent+1
	pla
	sta parent

@init:	pla			; restore type
	sta type		; set the type of the new context
	inc __ctx_active
	inc __ctx_open		; flag that a context is now open

	; move ctx pointer to next context space
	lda ctx
	clc
	adc #<CONTEXT_SIZE
	lda ctx+1
	adc #>CONTEXT_SIZE
	sta ctx+1

	; initialize metadata (numparams, line count, buffer)
	lda #$00
	sta numparams
	sta numlines
	sta mem::ctxbuffer

	jsr rewind
	; initialize an empty body (zero record size)
	lda #$00
	tay
	STOREB_Y cur
	RETURN_OK
.endproc

;*******************************************************************************
; REWIND
; Rewinds the context so that the cursor points to the beginning of its line
; data
.proc rewind
	jsr get_data_addr	; get base address of context lines
	stxy cur		; reset cursor to it

	; init param buffer to end of ctx buffer (grows downward)
	txa
	clc
	adc #<(CONTEXT_SIZE-SIZEOF_CTX_HEADER-PARAM_LENGTH)
	sta params
	tya
	adc #>(CONTEXT_SIZE-SIZEOF_CTX_HEADER-PARAM_LENGTH)
	sta params+1

	rts
.endproc

;*******************************************************************************
; POP
; Restores the last PUSH'ed context
; OUT:
;  -.C: set if there are no contexts to pop
.proc pop
	lda __ctx_active
	bne :+
	RETURN_ERR ERR_STACK_UNDERFLOW

:	lda ctx
	sec
	sbc #<CONTEXT_SIZE
	sta ctx
	lda ctx+1
	sbc #>CONTEXT_SIZE
	sta ctx+1

	lda parent
	pha
	lda parent+1
	pha

	; restore the ctx metadata (iter, iterend, cur, param, etc.)
	ldy #SIZEOF_CTX_HEADER-1
@l0:	LOADB_Y ctx
	sta meta,y
	dey
	bpl @l0

	; if we modified this context (the previous one's parent), update the
	; cursor with the modified value
	pla
	sta cur+1
	pla
	sta cur

	lda #$01
	sta __ctx_open	; mark context as open (again)

	dec __ctx_active
@done:  lda __ctx_active
	RETURN_OK
.endproc

;*******************************************************************************
; GETLINE
; Returns a line from the active context.
; OUT:
;  - .XY:            the address of the line returned
;  - .A:             the # of bytes read (0 if EOF)
;  - .C:             set on error
;  - mem::asmbuffer: the line read from the context
;  - asm::linenum:   line number that the line corresponds to
.proc getline
	jsr getrecord
	bcs @ret
	cmp #$00
	beq @ret
	JUMP LEX_BANK, lex::decode
@ret:	rts
.endproc

;*******************************************************************************
; GET RECORD
; Copies the next framed binary record to the shared token buffer.
; IN: active context cursor
; OUT: .A record size (zero at EOF), .XY token buffer; .C set on invalid bounds
.proc getrecord
@size=r0
@next=r2
	ldy #$00
	LOADB_Y cur
	sta CTX_TOKEN_BUFFER
	beq @done
	cmp #$04
	bcc @bad
	sta @size
	clc
	adc cur
	sta @next
	lda cur+1
	adc #$00
	sta @next+1
	cmp params+1
	bcc @copy
	bne @bad
	lda @next
	cmp params
	bcs @bad
@copy:
	LOADBLK8 cur, CTX_TOKEN_BUFFER, @size
	ldxy @next
	stxy cur
	lda CTX_TOKEN_BUFFER+1
	sta asm::linenum
	lda CTX_TOKEN_BUFFER+2
	sta asm::linenum+1
@done:
	lda CTX_TOKEN_BUFFER
	ldxy #CTX_TOKEN_BUFFER
	clc
	rts
@bad:	RETURN_ERR ERR_CTX_FULL
.endproc

;*******************************************************************************
; GETPARAMS
; returns a list of the parameters for the active context
; IN:
;  - .XY: address of buffer to store params in
; OUT:
;  - .A:    number of parameters
;  - (.XY): the updated buffer filled with 0-separated params
.proc getparams
@buff=r0
@cnt=r2
@params=r3
	stxy @buff
	ldx numparams
	beq @done
	stx @cnt

	lda params
	sta @params
	lda params+1
	sta @params+1

@l0:	ldy #$00
@l1:	LOADB_Y @params
	sta (@buff),y
	beq @next
	iny
	cpy #PARAM_LENGTH
	bcc @l1
	RETURN_ERR ERR_PARAM_NAME_TOO_LONG

@next:	; @buff += .Y+1
	tya
	sec		; +1
	adc @buff
	sta @buff
	bcc :+
	inc @buff+1

:	; @params -= PARAM_LENGTH
	lda @params
	sec
	sbc #PARAM_LENGTH
	sta @params
	bcs :+
	dec @params+1
:	dec @cnt
	bne @l0

@done:	lda numparams
	RETURN_OK
.endproc

;*******************************************************************************
; GETDATAADDR
; returns the address of the data for the active context.
; OUT:
;  - .XY: the address of the data for the current context
.proc get_data_addr
	lda ctx
	clc
	adc #SIZEOF_CTX_HEADER
	tax
	lda ctx+1
	adc #$00
	tay
	rts
.endproc

;*******************************************************************************
; WRITE PARENT
; Captures a reduced binary line in the parent context.
; IN: mem::asmbuffer source view; current iterator and source line
; OUT: .C set on encoding failure or full parent; parent cursor advanced on success
.proc write_parent
@dst=r0
@limit=r2
	ldxy #CTX_ITER_NAME
	jsr getparams
	bcs @ret
	ldxy #mem::asmbuffer
	lda numparams
	jsr __ctx_encode
	bcs @ret
	; The parent's saved parameter pointer is its exclusive allocation limit.
	lda ctx
	sta @dst
	lda ctx+1
	sec
	sbc #>CONTEXT_SIZE
	sta @dst+1
	ldy #$06
	LOADB_Y @dst
	sta @limit
	iny
	LOADB_Y @dst
	sta @limit+1
	ldxy parent
	stxy @dst
	jsr append_record
	bcs @ret
	ldxy @dst
	stxy parent
@ret:	rts
.endproc

;*******************************************************************************
; WRITE
; Captures a lexical record in the active context.
; IN: .XY source; asm::linenum source location
; OUT: .C set on encoding failure or full context; cursor advanced on success
.proc write
@dst=r0
@limit=r2
	lda #$00
	jsr __ctx_encode
	bcs @ret
	ldxy cur
	stxy @dst
	ldxy params
	stxy @limit
	jsr append_record
	bcs @ret
	ldxy @dst
	stxy cur
@ret:	rts
.endproc

;*******************************************************************************
; APPEND RECORD
; Checks space before writing any bytes, including the end-of-body marker.
; IN: r0 destination cursor, r2 exclusive limit; CTX_TOKEN_BUFFER record
; OUT: r0 advanced, .C clear; .C set without writes when full
.proc append_record
@dest=r0
@limit=r2
@next=r4
	lda @dest
	clc
	adc CTX_TOKEN_BUFFER
	sta @next
	lda @dest+1
	adc #$00
	sta @next+1
	cmp @limit+1
	bcc @copy
	bne @full
	lda @next
	cmp @limit
	bcs @full
@copy:
	STOREBLK8 CTX_TOKEN_BUFFER, @dest, CTX_TOKEN_BUFFER
	lda #$00
	STOREB_Y @dest
	ldxy @next
	stxy @dest
	inc numlines
	RETURN_OK
@full:	RETURN_ERR ERR_CTX_FULL
.endproc

;*******************************************************************************
; END
; Closes the active context by writing a terminating 0 to its line data
; and decrementing the __ctx_active value.
; Calling this tells the assembler to, for example, begin emitting assembly
; instead of storing lines to the context.
; This is called before the corresponding ctx::pop, which will completely
; deactivate the context.
; IN:
;   - .A: the type of context we're closing
; OUT:
;   - .C: set on error
.proc end
	; make sure a context is open
	ldx __ctx_active
	bne :+
	RETURN_ERR ERR_NO_MATCHING_SCOPE

:	; make sure the open context type matches the type we're closing
	cmp type
	beq :+
	RETURN_ERR ERR_NO_MATCHING_SCOPE ; if scope types mismatch, return err

:	; write a terminating 0 to the context's buffer
	ldy #$00
	tya
	STOREB_Y cur
	sta __ctx_open	; mark context as closed
	RETURN_OK
.endproc

;*******************************************************************************
; ADDPARAM
; Adds the given parameter to the active context
; IN:
;  - .XY: the 0, whitepace, or ',' terminated parameter to add to the active
;  context
; OUT:
;  - .XY: the rest of the string after the parameter that was extracted
.proc addparam
@param=r0
	lda numparams
	cmp #MAX_PARAMS+1	; +1 (macro NAME is stored as param 0)
	bcc :+
	RETURN_ERR ERR_INVALID_MACRO_ARGS

:	stxy @param

	ldy #$00
@copy:  lda (@param),y
	STOREB_Y params
	beq @done
	cmp #','
	beq @done
	jsr is_whitespace
	beq @done
	iny
	cpy #PARAM_LENGTH
	bne @copy
	RETURN_ERR ERR_LINE_TOO_LONG

@done:	inc numparams
	lda #$00
	STOREB_Y params	; 0-terminate

	; move pointer to next open param
	; params -= PARAM_LENGTH
	lda params
	sec
	sbc #PARAM_LENGTH
	sta params
	bcs :+
	dec params+1

:	; get addr of rest of string for caller
	tya
	clc
	adc @param
	tax
	lda @param+1
	adc #$00
	tay
	RETURN_OK
.endproc
