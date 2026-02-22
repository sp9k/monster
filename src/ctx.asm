;*******************************************************************************
; CTX.ASM
; This file contains the code for interacting with the assembly "context"
; The "context" is a special buffer used by the .MAC and .REP directives to
; store lines of data, which is required to complete the assembly of these
; directives when their corresponding .ENDMAC or .ENDREP directive is found.
;*******************************************************************************

.include "config.inc"
.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "util.inc"
.include "target.inc"
.include "zeropage.inc"

;*******************************************************************************
.exportzp __ctx_numlines
.export __ctx_push
.export __ctx_rewind
.export __ctx_pop
.export __ctx_getline
.export __ctx_getparams
.export __ctx_getdata
.export __ctx_write_parent
.export __ctx_write
.export __ctx_end
.export __ctx_addparam

.export __ctx_active
.export __ctx_open

;*******************************************************************************
; CONSTANTS
CONTEXT_SIZE = $200	; size of buffer per context
PARAM_LENGTH = 16	; size of param (stored after the context data)
MAX_PARAMS   = 4	; max params for a context
.ifdef ultimem
MAX_CONTEXTS = 8	; max nesting depth for contexts
.else
MAX_CONTEXTS = 3	; max nesting depth for contexts
.endif
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
; Flat memory procedure mappings (FE3 only)
.if .defined(vic20) && .defined(fe3)
__ctx_push         = push
__ctx_rewind       = rewind
__ctx_pop          = pop
__ctx_getline      = getline
__ctx_getparams    = getparams
__ctx_getdata      = getdata
__ctx_write_parent = write_parent
__ctx_write        = write
__ctx_end          = end
__ctx_addparam     = addparam

is_whitespace = util::is_whitespace

;*******************************************************************************
; Banked memory mappings
.else
__ctx_push:         JUMP FINAL_BANK_CTX, push
__ctx_rewind:       JUMP FINAL_BANK_CTX, rewind
__ctx_pop:          JUMP FINAL_BANK_CTX, pop
__ctx_getline:      JUMP FINAL_BANK_CTX, getline
__ctx_getparams:    JUMP FINAL_BANK_CTX, getparams
__ctx_getdata:      JUMP FINAL_BANK_CTX, getdata
__ctx_write_parent: JUMP FINAL_BANK_CTX, write_parent
__ctx_write:	    JUMP FINAL_BANK_CTX, write
__ctx_end:	    JUMP FINAL_BANK_CTX, end
__ctx_addparam:     JUMP FINAL_BANK_CTX, addparam

.segment "CTX"
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
.endif

;*******************************************************************************
; PUSH
; Saves the current context and beings a new one
; OUT:
; - .C: set if there is no room to create a new context
.proc push
	sta type

	lda __ctx_active
	beq @init		; no active context -> continue
	cmp #MAX_CONTEXTS+1
	bcc @save

@err:	;sec
	pla			; clean stack
	lda #ERR_STACK_OVERFLOW	; too many contexts
	rts

@save:	lda cur
	pha
	lda cur+1
	pha

	; save the active context's state
	ldy #SIZEOF_CTX_HEADER-1
@l0:	lda meta,y
	STOREB_Y ctx
	dey
	bpl @l0

	; set current context's cursor as new one's parent
	pla
	sta parent+1
	pla
	sta parent

@init:	inc __ctx_active
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

	; fall through to __ctx_rewind to initialize cur pointer
.endproc

;*******************************************************************************
; REWIND
; Rewinds the context so that the cursor points to the beginning of its line
; data
.proc rewind
	jsr getdataaddr	; get base address of context lines
	stxy cur	; reset cursor to it

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
@l0:	lda (ctx),y
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
;  - .XY: the address of the line returned
;  - .A: the # of bytes read (0 if EOF)
;  - .C: set on error
;  - mem::ctxbuffer: the line read from the context
.proc getline
@out=mem::ctxbuffer
	; read until a newline or EOF
	ldy #$00
	LOADB_Y cur
	beq @ok		; if line is empty -> we're done

@read:	LOADB_Y cur
	sta @out,y
	beq @done
	iny
	cpy #LINESIZE
	bcc @read
	RETURN_ERR ERR_LINE_TOO_LONG

@done:	iny
	tya
	clc
	adc cur
	sta cur
	bcc :+
	inc cur+1
:	ldxy #@out
	tya		; restore # of bytes read
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; GETPARAMS
; returns a list of the parameters for the active context
; IN:
;  - .XY: address of buffer to store params in
; OUT:
;  - .A: the number of parameters
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
; GETDATA
; Copies the data for the current context to mem::spare
; OUT:
;  - mem::spare: the contents of the current context
;  - .XY:        address of data (mem::spare)
.proc getdata
@src=r0
@dst=r2
@prev=r4
	jsr getdataaddr
	stxy @src
	ldxy #mem::spare
	stxy @dst

	ldy #$00
	sty @prev
@l0:	lda (@src),y
	sta (@dst),y
	tax
	ora @prev	; check if we encountered 2 consecutive 0's
	beq @done	; if yes, we're at the end
	stx @prev	; save character we just read as previous
	incw @src
	incw @dst
	bne @l0		; branch always

@done:	ldxy #mem::spare
	rts
.endproc

;*******************************************************************************
; GETDATAADDR
; returns the address of the data for the active context.
; OUT:
;  - .XY: the address of the data for the current context
.proc getdataaddr
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
; Writes the given line to parent of the current context's line buffer
; Comments are ignored to save space in the context buffer.
; IN:
;  - .XY: the line to write to the active context
; OUT:
;  - .XY: the address of the active context.
;  - .C:  set on error
.proc write_parent
@line=r0
	stxy @line

	ldy #$00
	lda (@line),y
	beq @ok		; don't store empty lines

@write: lda (@line),y
	beq @done
	cmp #$0d
	beq @done
	cmp #';'
	beq @done
	STOREB_Y parent

	incw @line
	incw parent

	; did the parent catch up to the child's context?
	ldxy parent
	cmpw ctx
	bne @write

	; parent is now overwriting this context -> return error
	;sec
	lda #ERR_CTX_FULL
	rts

@done:	lda #$00
	STOREB_Y parent	; terminate this line in the buffer
	incw parent

@ok:	inc numlines
	RETURN_OK
.endproc

;*******************************************************************************
; WRITE
; Writes the given line to the context at its current position
; Comments are ignored to save space in the context buffer.
; IN:
;  - .XY: the line to write to the active context
; OUT:
;  - .XY: the address of the active context.
;  - .C:  set on error
.proc write
@line=r0
	stxy @line
	ldy #$00
	lda (@line),y
	beq @ok		; don't store empty lines

@write: lda (@line),y
	beq @done
	cmp #$0d
	beq @done
	cmp #';'
	beq @done
	STOREB_Y cur

	incw @line

	; increment context pointer and make sure the context isn't full
	incw cur
	lda cur+1
	cmp #>contexts_top
	bcc @write
	lda cur
	cmp #<contexts_top
	bne @write

	;sec
	lda #ERR_CTX_FULL
	rts

@err:	; sec
	lda #ERR_LINE_TOO_LONG
	rts

@done:	lda #$00
	STOREB_Y cur	; terminate this line in the buffer
	incw cur

@ok:	inc numlines
	RETURN_OK
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
	; make sure the open context type matches the type we're closing
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
	stxy @param

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
