;*******************************************************************************
; LABELS.ASM
; This file defines procedures for creating and retrieving labels.
;
; ------------------------------------------------------------------------------
; LABELS OVERVIEW
; Labels map a text string to an address in memory.  They can be looked up
; by address or name.  They are stored in a sorted list to enable efficient
; alphabetic retrieval and are also indexed by address (value) to allow for
; efficient retrieval by address.
;
; The buckets, scopes, and anonymous labels are stored in the SYMBOLS bank.
; Records, hash-chain nodes, indexes and names are stored in a separate
; contiguous pool of memory (see symbolpages.asm).
;
; ------------------------------------------------------------------------------
; ANONYMOUS LABELS OVERVIEW
; Anonymous labels are simpler than named ones.  They are stored as a
; sorted list of addresses.  Because they are so compact, it is preferable
; to use them when possible.
; The list is sorted on insertion, meaning it is a good idea to assemble from
; the lowest address.
;*******************************************************************************

;*******************************************************************************
; LABEL STRUCT
; The label structure uses the following layout
; 0 FLAGS
;   bitfield of metadata about symbol (1 byte)
;   bits:
;     0:   mode (0=zeropage, 1=absolute)
;     1-7: segment-id
; 1 HASH
;   precomputed hash value            (2 bytes)
; 3 ADDR
;   symbol address                    (2 bytes)
; 5 ID
;   symbol id                         (2 bytes)
; 7 NAME
;   word offset into name pool        (2 bytes)
; 9 FILE
;   id of file containing label       (1 byte)
; 10 LINE
;   line number of label definition   (2 bytes)
;*******************************************************************************

;*******************************************************************************
; field offsets for LABEL
LABEL_FLAGS = 0
LABEL_HASH  = 1
LABEL_ADDR  = 3
LABEL_ID    = 5
LABEL_NAME  = 7
LABEL_FILE  = 9
LABEL_LINE  = 10

;*******************************************************************************
; LIST NODE STRUCT
; Labels are stored in linked-lists that each bucket in the hash map points to.
; The structure of this list's nodes is:
; 0 ADDR
;   address to this symbol's definition
; 2 NEXT
;   pointer to next symbol (or $0000 if end of list)
;*******************************************************************************

;*******************************************************************************
; field offsets for LIST
LIST_LABEL  = 0
LIST_NEXT   = 2

;*******************************************************************************
.include "asm.inc"
.import __asm_do_label
.include "codes.inc"
.include "debuginfo.inc"
.include "config.inc"
.include "errors.inc"
.include "keycodes.inc"
.include "expr.inc"
.include "fp.inc"
.include "kernal.inc"
.include "limits.inc"
.include "object.inc"
.include "ram.inc"
.include "macros.inc"
.include "macro.inc"
.include "target.inc"
.include "string.inc"
.include "symbolpages.inc"
.include "zeropage.inc"

.macpack longbranch

;*******************************************************************************
; ZEROPAGE
label   = zp::labels		; pointer to base label struct
; the following fields mirror the on-record layout (see LABEL_* offsets below):
; each is at label+2+LABEL_x, so a whole record loads in one block transfer
flags   = zp::labels+2		; FLAGS field for active symbol    (LABEL_FLAGS)
hash    = zp::labels+3		; precomputed hash value (2 bytes) (LABEL_HASH)
addr    = zp::labels+5		; ADDR field for active label      (LABEL_ADDR)
id      = zp::labels+7		; ID field for active label        (LABEL_ID)
name    = zp::labels+9		; pointer to NAME field for symbol (LABEL_NAME)
file_id = zp::labels+$b		; FILE field for active symbol     (LABEL_FILE)
lineno  = zp::labels+$c		; LINE field for active symbol     (LABEL_LINE)
list    = zp::labels+$e		; address to list of nodes for current bucket
listtop = zp::labels+$10	; address of free memory (next available list
bucket  = zp::labels+$12
temp    = zp::labels+$14	; temporary scratchpad

;*******************************************************************************
; CONSTANTS
SCOPE_LEN  = 17		; legacy @-label/macro anchor (name + terminator)

; NOTE: BE CAREFUL CHANGING THIS
; BUCKETING LOGIC RELIES ON AN EXACT SIZE (BITS 8-11)
; INITIALIZATION ALSO RELIES ON PAGE ALIGNED SIZE (e.g. $1000)
NUM_BUCKETS = 2048	; number of buckets for the symbol hash map

MAX_SCOPES         = 4

SIZEOF_LABEL           = 12
SIZEOF_LABEL_LIST_NODE = 4

SEG_ABS = $ff
SEG_FLOAT = $7e
SEG_FLOAT_PACKED = $7d

;*******************************************************************************
.export __label_clr
.export __label_add
.export __label_find
.export __label_by_addr
.export __label_by_id
.export __label_dump
.export __label_isvalid
.export __label_get_addr
.export __label_get_name
.export __label_get_line
.export __label_load
.export __label_is_local
.export __label_set
.export __label_address
.export __label_address_by_id
.export __label_setscope
.export __label_popscope
.export __label_addanon
.export __label_get_fanon
.export __label_get_banon
.export __label_index
.export __label_id_by_addr_index
.export __label_addrmode
.export __label_get_segment
.export __label_set_addr
.export __label_id_by_alpha_index

;*******************************************************************************
; Label JUMP table
.macro LBLJUMP proc
	JUMP FINAL_BANK_SYMBOLS, proc
.endmacro

.RODATA

__label_clr:               LBLJUMP clr
__label_add:               LBLJUMP add
__label_find:              LBLJUMP find
__label_by_addr:           LBLJUMP by_addr
__label_by_id:             LBLJUMP by_id
__label_isvalid:           LBLJUMP is_valid
__label_get_name:          LBLJUMP get_name
__label_get_addr:          LBLJUMP getaddr
__label_is_local:          LBLJUMP is_local
__label_set:               LBLJUMP set
__label_address:           LBLJUMP address
__label_address_by_id:     LBLJUMP address_by_id
__label_setscope:          LBLJUMP set_scope
__label_popscope:          LBLJUMP pop_scope
__label_addanon:           LBLJUMP add_anon
__label_get_fanon:         LBLJUMP get_fanon
__label_get_banon:         LBLJUMP get_banon
__label_index:             LBLJUMP index
__label_id_by_addr_index:  LBLJUMP id_by_addr_index
__label_addrmode:          LBLJUMP addrmode
__label_get_segment:       LBLJUMP get_segment
__label_set_addr:          LBLJUMP setaddr
__label_dump:              LBLJUMP dump
__label_load:              LBLJUMP load
__label_id_by_alpha_index: LBLJUMP id_by_alpha_index
__label_get_line:          LBLJUMP get_file_and_line
.export __label_remap_files
__label_remap_files:       LBLJUMP remap_files

;*******************************************************************************
; paged address spaces
labels      = $0010
label_nodes = $0004
.assert MAX_LABELS = 4096, error, "paged indexes require 4096 symbols"

label_addresses_sorted     = $2000
label_addresses_sorted_ids = label_addresses_sorted + MAX_LABELS*2
label_names_sorted         = label_addresses_sorted_ids + MAX_LABELS*2
label_names_sorted_ids     = label_names_sorted + MAX_LABELS*2

.export labels, label_nodes, label_names_sorted

.segment "LABEL_BSS"
.export label_buckets
label_buckets: .res NUM_BUCKETS*2
scopes: .res SCOPE_LEN*MAX_SCOPES
MAX_NAMESPACES = $10
namespace_path:    .res $100
namespace_ends:    .res MAX_NAMESPACES
namespace_kinds:   .res MAX_NAMESPACES
namespace_anchors: .res MAX_NAMESPACES
.export anon_addrs
anon_addrs: .res MAX_ANON*2
.assert * <= $4000, error, "symbol metadata must fit below the paging window"

;*******************************************************************************
; VARS (shared RAM)
.segment "SHAREBSS"
labelvars:
.export __label_num
__label_num: .word 0		; total number of labels

.export __label_numanon
__label_numanon:
numanon: .word 0		; total number of anonymous labels
.export __label_anon_cursor
__label_anon_cursor: .word 0	; next source-order anonymous label in object pass 2

scopesp:   .byte 0		; offset of next free scope (0 = no scope)
name_full: .byte 0		; all 128 KiB of name storage allocated
name_top:  .word 0		; word offset of the next free name-pool entry
labelvars_size=*-labelvars

; Lexical scope state and lookup scratch are visible from every code bank.
.ifdef vic20
.segment "SHAREBSS2"
.else
.segment "BSS_NOINIT"
.endif
namespace_vars:
.export __label_namespace_depth
__label_namespace_depth:
namespace_depth:  .byte $00
namespace_len:    .byte $00
namespace_anchor: .byte $00
namespace_kind:   .byte $00
namespace_newlen: .byte $00
name_chars:       .byte $00
name_qualified:   .byte $00
name_len:         .byte $00
search_len:       .byte $00
search_depth:     .byte $00
search_parents:   .byte $00
namespace_vars_size = *-namespace_vars

BANKED_SEG "LABELS", FINAL_BANK_SYMBOLS

;*******************************************************************************
; CANONICAL NAME
; Reads a symbol token into "name_chars" and identifies absolute dotted names.
; IN:
;   - .XY: source name
; OUT:
;   - .XY: canonical token in sympage::scope_buffer
;   - .C:  set on error
;   - .A:  error code  (if error occurred)
.proc canonical_name
@src = temp
	stxy @src
	ldy #$00
	sty name_qualified
	ldx #$00

@scan:	lda (@src),y
	cmp #':'
	beq @colon
	cmp #';'
	beq @end
	jsr isseparator
	beq @end
	cmp #'.'
	bne @copy
	sta name_qualified

@copy:	sta sympage::scope_buffer,x
	inx
	beq @long
	iny
	bne @scan
@long:	RETURN_ERR ERR_LABEL_TOO_LONG

@colon: iny
	beq @long
	lda (@src),y
	cmp #':'
	beq @bad
	dey
@end:	cpx #$00
	beq @bad
	sty name_chars
	stx name_len
	lda #$00
	sta sympage::scope_buffer,x
	ldxy #sympage::scope_buffer
	RETURN_OK
@bad:	RETURN_ERR ERR_ILLEGAL_LABEL
.endproc

;*******************************************************************************
; PREFIX NAME
; Prepends a lexical path to the canonical token, checking the full name length.
; IN:
;   - .A: number of namespace_path bytes to prepend
;   - name_len: canonical token length
; OUT:
;   - .XY: qualified token in sympage::scope_buffer
;   - .C:  set on error
;   - .A:  ERR_LABEL_TOO_LONG on error
.proc prefix_name
@path = temp
	sta search_len
	clc
	adc name_len
	bcs @long
	tax
	ldy name_len
@shift:
	lda sympage::scope_buffer,y
	sta sympage::scope_buffer,x
	dex
	dey
	cpy #$ff
	bne @shift
	ldxy #namespace_path
	stxy @path
	ldy #$00
@copy:	cpy search_len
	beq @done
	LOADB_Y @path
	sta sympage::scope_buffer,y
	iny
	bne @copy
@done:	ldxy #sympage::scope_buffer
	RETURN_OK
@long:	RETURN_ERR ERR_LABEL_TOO_LONG
.endproc

;*******************************************************************************
; DEFINITION NAME
; Resolves dotted names absolutely and bare names in their lexical or local
; scope.
; IN:
;   - .XY: source name
; OUT:
;   - .XY: fully qualified name
;   - search_parents: nonzero for ordinary lexical references
;   - .C: set and .A = error code on failure
.proc definition_name
	jsr is_local
	beq @ordinary
	lda namespace_depth
	beq @legacy
	lda scopesp
	cmp namespace_anchor
	bne @legacy
	lda #$00
	beq @prepare
@ordinary:
	lda #$01
@prepare:
	sta search_parents
	lda #$00
	sta search_len
	jsr canonical_name
	bcs @ret
	lda name_qualified
	beq @relative
	lda #$00
	sta search_parents
	beq @prefix
@relative:
	lda namespace_len
@prefix:
	jmp prefix_name
@legacy:
	lda #$00
	sta search_parents
	sta search_len
	jmp prepend_scope
@ret:	rts
.endproc

;*******************************************************************************
; FIND
; Looks up dotted names at root and bare names through the enclosing namespaces.
; IN:
;   - .XY: source name, optionally qualified with dots
; OUT:
;   - .XY: symbol ID on success
;   - .C:  set and .A = error code if no matching symbol exists
.proc find
@path = temp
	jsr definition_name
	bcc @ready
	cmp #ERR_LABEL_TOO_LONG
	bne @error
	ldx search_parents
	beq @error
	ldx search_len
	beq @error

	; oversized candidate cannot exist, but an enclosing name still may
	lda namespace_depth
	sta search_depth
	jmp @parent

@ready: lda namespace_depth
	sta search_depth
@try:	jsr find_exact
	bcc @ret
	lda search_parents
	beq @missing
	lda search_len
	beq @missing

	; remove this attempt's path before prepending the enclosing path
	tay
	ldx #$00
@strip: lda sympage::scope_buffer,y
	sta sympage::scope_buffer,x
	beq @parent
	inx
	iny
	bne @strip

@parent:
	dec search_depth
	ldxy #namespace_ends
	stxy @path
	ldy search_depth
	LOADB_Y @path
	jsr prefix_name
	bcc @try
	bcs @parent
@ret:	rts
@missing:
	RETURN_ERR ERR_LABEL_UNDEFINED
@error:	sec
	rts
.endproc

;*******************************************************************************
; SOURCE SCOPE
; Updates the legacy @-label anchor outside explicit lexical scopes.
; IN:
;   - .XY: source label name
; OUT:
;   - .C: set and .A = error code on failure
.export __label_source_scope
.proc __label_source_scope
	lda namespace_depth
	bne @done
	jsr is_local
	bne @done

	jsr pop_scope
	ldxy zp::line
	jmp set_scope

@done:	RETURN_OK
.endproc

;*******************************************************************************
; NAMESPACE RESET
; Returns to the root namespace at the start of an assembly pass.
; IN:
;   - None
; OUT:
;   - .C: clear
.export __label_namespace_reset
.proc __label_namespace_reset
	lda #$00
	ldx #namespace_vars_size
@clear:
	sta namespace_vars-1,x
	dex
	bne @clear
	sta scopesp
	RETURN_OK
.endproc

;*******************************************************************************
; NAMESPACE CHECK
; Checks that all lexical scope directives have been closed.
; IN:
;   - None
; OUT:
;   - .C: set and .A = ERR_NO_MATCHING_SCOPE for an unclosed scope
.export __label_namespace_check
.proc __label_namespace_check
	lda namespace_depth
	beq @done
	RETURN_ERR ERR_NO_MATCHING_SCOPE
@done:	RETURN_OK
.endproc

;*******************************************************************************
; NAMESPACE OPEN
; Opens a named scope or defines a procedure label and opens its scope.
; IN:
;   - .A: $00 for .SCOPE, $01 for .PROC
;   - zp::line: unqualified scope name and optional comment
; OUT:
;   - .A: ASM_DIRECTIVE on success, error code on failure
;   - .C: set on invalid input, stack overflow, or label-definition failure
.export __label_namespace_open
.proc __label_namespace_open
@path = temp
	sta namespace_kind
	; when checking a macro body line, accept .ident() as the name
	lda zp::verify
	beq @literal

	ldy #$00
	lda (zp::line),y
	cmp #'.'
	bne @literal
	CALL FINAL_BANK_MACROS, mac::verify_property
	jcs @bad

	cmp #$04		; .ident?
	jne @bad
	CALL FINAL_BANK_MACROS, mac::verify_name
	jcs @ret
	ldy #$00
	jmp @tail

@literal:
	ldxy zp::line
	jsr is_valid
	jcs @ret
	lda name_qualified
	bne @bad
	ldx #$00

@simple:
	lda sympage::scope_buffer,x
	beq @syntax
	cmp #'.'
	beq @bad
	cmp #'@'
	beq @bad
	inx
	bne @simple

@syntax:
	ldy name_chars
@tail:	lda (zp::line),y
	beq @valid
	cmp #';'
	beq @valid
	jsr iswhitespace
	bne @bad
	iny
	bne @tail
@bad:	RETURN_ERR ERR_UNEXPECTED_CHAR

@valid: lda zp::verify
	jne @done
	lda namespace_depth
	cmp #MAX_NAMESPACES
	jcs @full
	lda namespace_len
	clc
	adc name_len
	jcs @long
	adc #$01
	jcs @long
	sta namespace_newlen

	lda namespace_kind
	beq @openpath
	CALL FINAL_BANK_ASM, __asm_do_label
	jcs @ret
	; Label lookup uses the shared name buffer; reconstruct the scope token.
	ldxy zp::line
	jsr canonical_name
	jcs @ret

@openpath:
	lda namespace_len
	jsr prefix_name
	jcs @ret

	ldxy #namespace_ends
	stxy @path
	ldy namespace_depth
	lda namespace_len
	STOREB_Y @path

	ldxy #namespace_kinds
	stxy @path
	ldy namespace_depth
	lda namespace_kind
	STOREB_Y @path

	ldxy #namespace_anchors
	stxy @path
	ldy namespace_depth
	lda namespace_anchor
	STOREB_Y @path

	lda scopesp
	sta namespace_anchor
	ldxy #namespace_path
	stxy @path
	ldy #$00

@copy:	lda sympage::scope_buffer,y
	beq @dot
	STOREB_Y @path
	iny
	bne @copy
@dot:	lda #'.'
	STOREB_Y @path
	lda namespace_newlen
	sta namespace_len
	inc namespace_depth

@done:	lda #ASM_DIRECTIVE
	RETURN_OK
@long:	RETURN_ERR ERR_LABEL_TOO_LONG
@full:	RETURN_ERR ERR_STACK_OVERFLOW
@ret:	rts
.endproc

;*******************************************************************************
; NAMESPACE CLOSE
; Closes a scope of the matching kind and restores the enclosing namespace.
; IN:
;   - .A: $00 for .ENDSCOPE, $01 for .ENDPROC
;   - zp::line: optional comment, no operands
; OUT:
;   - .A: ASM_DIRECTIVE on success, error code on failure
;   - .C: set for malformed, unmatched, or mismatched closing directives
.export __label_namespace_close
.proc __label_namespace_close
@path = temp
	sta namespace_kind
	ldy #$00
	lda (zp::line),y
	beq @valid
	cmp #';'
	bne @syntax

@valid: lda zp::verify
	bne @done
	lda mac::depth
	beq :+
	; a macro can't close a scope that was opened outside of it
	CALL FINAL_BANK_MACROS, mac::namespace_floor
	cmp namespace_depth
	bne :+
	tax
	beq @empty		; no scope is open at all
	bne @mismatch		; the open scope is outside of the macro

:	lda namespace_depth
	beq @empty
	ldxy #namespace_kinds
	stxy @path

	ldy namespace_depth
	dey
	LOADB_Y @path
	cmp namespace_kind
	bne @mismatch
	ldxy #namespace_ends
	stxy @path
	ldy namespace_depth
	dey
	LOADB_Y @path
	sta namespace_len
	ldxy #namespace_anchors
	stxy @path
	ldy namespace_depth
	dey
	LOADB_Y @path
	sta namespace_anchor
	dec namespace_depth

@done:	lda #ASM_DIRECTIVE
	RETURN_OK
@empty:
	RETURN_ERR ERR_NO_OPEN_SCOPE
@mismatch:
	RETURN_ERR ERR_NO_MATCHING_SCOPE
@syntax:
	RETURN_ERR ERR_UNEXPECTED_CHAR
.endproc

;*******************************************************************************
; NAMESPACE UNWIND
; Restores the parent namespace after a macro expansion.
; IN:
;   - .A: namespace depth at macro entry (<= current depth)
; OUT:
;   - namespace depth and active path restored
.export __label_namespace_unwind
.proc __label_namespace_unwind
@path = temp
	cmp namespace_depth
	beq @done
	sta namespace_depth
	ldxy #namespace_ends
	stxy @path

	ldy namespace_depth
	LOADB_Y @path
	sta namespace_len
	ldxy #namespace_anchors
	stxy @path

	ldy namespace_depth
	LOADB_Y @path
	sta namespace_anchor
@done:	rts
.endproc

;*******************************************************************************
; POP SCOPE
; Pops the current scope, returning to the next scope on the stack. If no other
; scope is open, returns to the "root" scope
.proc pop_scope
	lda scopesp
	beq @done	; nothing to POP -> exit
	sec
	sbc #SCOPE_LEN
	sta scopesp
@done:	rts
.endproc

;*******************************************************************************
; SET SCOPE
; Sets the current scope to the given scope.
; This affects local labels, which will be namespaced by prepending the scope.
; IN:
;  - .XY: the address of the scope string to set as the current scope
; OUT:
;   - .C: set on error
.proc set_scope
@scope  = temp
@scopes = temp+2
	; make sure there is room to push another scope
	lda scopesp
	cmp #MAX_SCOPES*SCOPE_LEN
	bcc :+
	;sec
	lda #ERR_STACK_OVERFLOW
	rts

:	; get @scope-scopesp so that @scope+scopesp points to the start of the
	; input string
	txa
	sec
	sbc scopesp
	sta @scope
	tya
	sbc #$00
	sta @scope+1

	ldxy #scopes
	stxy @scopes

	ldx #SCOPE_LEN-1
	ldy scopesp
:	lda (@scope),y		; (@scope-scopesp)+scopesp + Y
	cmp #'.'		; retain filename-stem scope behavior
	beq @done
	jsr isseparator
	beq @done
	STOREB_Y @scopes	; scopes+scopesp + Y
	iny
	dex
	bne :-
	; maxed out scope len; truncate (fall through to terminate)

@done:  lda #$00
	STOREB_Y @scopes	; terminate

	; scopesp += SCOPE_LEN
	lda scopesp
	clc
	adc #SCOPE_LEN
	sta scopesp
	RETURN_OK
.endproc

;*******************************************************************************
; PREPEND SCOPE
; Prepends the current scope to the label in .XY and returns a buffer containing
; the namespaced label.
; IN:
;  - .XY: the label to add the scope to
; OUT:
;  - .XY: pointer to the buffer containing the scope namespaced label
;  - .C: set if there is no open scope
.proc prepend_scope
@lbl = temp
@scopes = temp+2
@len = r0
@prefix = r1
	stxy @lbl
	lda scopesp
	beq @noscope
	ldxy #scopes
	stxy @scopes
	lda scopesp
	sec
	sbc #SCOPE_LEN
	tay
	ldx #0
@scope:	LOADB_Y @scopes
	beq @length
	sta sympage::scope_buffer,x
	inx
	iny
	bne @scope
@length:
	stx @prefix
	ldy #0
@scan:	lda (@lbl),y
	jsr isseparator
	beq @copy
	iny
	bne @scan
	RETURN_ERR ERR_LABEL_TOO_LONG
@copy:	sty @len
	tya
	clc
	adc @prefix
	bcs @long
	ldy #0
@chars:	cpy @len
	beq @done
	lda (@lbl),y
	sta sympage::scope_buffer,x
	inx
	iny
	bne @chars
@done:	lda #0
	sta sympage::scope_buffer,x
	ldxy #sympage::scope_buffer
	RETURN_OK
@long:	RETURN_ERR ERR_LABEL_TOO_LONG
@noscope:
	RETURN_ERR ERR_NO_OPEN_SCOPE
.endproc

;*******************************************************************************
; CLR
; Removes all labels effectively resetting the label state
.proc clr
@map=r0
	jsr __label_namespace_reset
	CALL FINAL_BANK_EXPR, expr::fconst_clr
	; clear the hash map (linked lists of LABEL nodes)
	ldxy #label_buckets
	stxy @map

	ldx #>(NUM_BUCKETS*2)	; number of pages to clear
	lda #$00
	tay
:	STOREB_Y @map
	dey
	bne :-
	inc @map+1
	dex
	bne :-

	ldx #labelvars_size
:	sta labelvars-1,x
	dex
	bne :-

	; default to unavailable until assembly/object loading sets a location
	sta zp::label_fileid
	sta zp::label_lineno
	sta zp::label_lineno+1

	; init list free pointer to base of the node data array
	ldxy #label_nodes
	stxy listtop

	rts
.endproc

;*******************************************************************************
; FIND
; Returns the label ID corresponding to the given label name and returns it.
; IN:
;  - .XY: the name of the label to look for
; OUT:
;  - .C:  set if label is not found
;  - .XY: the id of the label (if found)
.proc find_exact
@label = temp
	stxy @label		; save name to look for

	lda __label_num
	bne @find
	lda __label_num+1
	bne @find
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

@find:	; look for the symbol in the hash map
	ldxy @label
	jsr hash_name		; hash the symbol name
	jsr getlist		; and get the address of the list for the symbol
	ldxy @label		; load name of label to look for in list
	jsr find_in_list	; find our string (if it exists)
	bcs @done		; not found -> we're done

	; get the ID of the symbol
	ldy #LABEL_ID
	PAGE_LOAD label, SYM_RECORDS
	tax			; LSB in X
	iny
	PAGE_LOAD label, SYM_RECORDS
	tay			; MSB in Y

	;clc
@done:	rts
.endproc

;*******************************************************************************
; ADDRMODE
; Returns the "address mode" for the label of the given ID
; IN:
;   - .XY: the ID of the label to get the address mode for
; OUT:
;   - .A: the address mode (0=ZP, 1=ABS)
.proc addrmode
	jsr loadlabel
	lda flags
	and #$01		; mask bit 0 (mode)
	rts
.endproc

;*******************************************************************************
; SETADDR
; Overwrites the address, segment-id, and mode for the given label with the
; provided values
; IN:
;   - .XY:                 ID of the symbol to update the address of
;   - zp::label_value:     value to set the symbol's address to
;   - zp::label_segmentid: new value for label's SEGMENT ID
;   - zp::label_mode:      new value for label's MODE
.proc setaddr
	jsr loadlabel

	; fall through to set_addr
.endproc

;*******************************************************************************
; SET ADDR
; Updates address related fields (FLAGS, ADDR) and indices with the new value
; for the label that is already loaded (via "loadlabel")
; IN:
;   - zp::label_value:     value to set the symbol's address to
;   - zp::label_segmentid: new value for label's SEGMENT ID
;   - zp::label_mode:      new value for label's MODE
.proc set_addr
@addr_index = temp
	; overwrite the current MODE and SEGMENT
	; FLAGS = (SEG << 1) | MODE
	ldy #LABEL_FLAGS
	lda zp::label_segmentid
	asl
	ora zp::label_mode
	PAGE_STORE label, SYM_RECORDS

	; overwrite the current value for the label with zp::value
	ldy #LABEL_ADDR
	lda zp::label_value
	PAGE_STORE label, SYM_RECORDS
	iny
	lda zp::label_value+1
	PAGE_STORE label, SYM_RECORDS

	; get location of label address in the sorted index
	lda id
	asl
	sta @addr_index
	lda id+1
	rol
	sta @addr_index+1
	lda @addr_index
	;clc
	adc #<label_addresses_sorted
	sta @addr_index
	lda @addr_index+1
	adc #>label_addresses_sorted
	sta @addr_index+1

	; overwrite the index's value for the label
	ldy #$00
	lda zp::label_value
	PAGE_STORE @addr_index, SYM_INDEXES
	iny
	lda zp::label_value+1
	PAGE_STORE @addr_index, SYM_INDEXES

	; get location of label id in sorted index
	lda id
	asl
	sta @addr_index
	lda id+1
	rol
	sta @addr_index+1
	lda @addr_index
	;clc
	adc #<label_addresses_sorted_ids
	sta @addr_index
	lda @addr_index+1
	adc #>label_addresses_sorted_ids
	sta @addr_index+1

	; write ID to label_addresses_sorted_ids
	lda id
	ldy #$00
	PAGE_STORE @addr_index, SYM_INDEXES
	lda id+1
	iny
	PAGE_STORE @addr_index, SYM_INDEXES

	RETURN_OK
.endproc

;*******************************************************************************
; SET
; Set adds the label, but doesn't produce an error if the label already exists
; IN:
;  - .XY: the name of the label to add
;  - zp::label_value: the value to assign to the given label name
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc set
	lda #$01
	skw

	; fallthrough to ADD
.endproc

;*******************************************************************************
; ADD
; Adds a label to the internal label state.
; IN:
;  - .XY:             the name of the label to add
;  - zp::label_value: the value to assign to the given label name
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc add
	lda #$00

	; fallthrough to ADDLABEL
.endproc

;*******************************************************************************
; ADDLABEL
; Adds a label to the internal label state.
; IN:
;  - .XY:             the name of the label to add
;  - zp::label_value: the value to assign to the given label name
;  - zp::label_mode:  the "mode" of the label to add (0=ZP, 1=ABS)
; OUT:
;  - .XY: the ID of the label added
;  - .C:  set on error or clear if the label was successfully added
.proc addlabel
@name   = r6
@allow_overwrite=r9
	sta @allow_overwrite	; set overwrite flag (SET) or clear (ADD)

	; make sure the name is valid
	stxy @name
	jsr is_valid
	bcs @ret		; return err

	; Definitions are checked in their own scope, without parent lookup.
	ldxy @name
	jsr definition_name
	bcs @ret
	stxy @name
	jsr find_exact
	stxy id
	bcs @insert		; label doesn't exist -> continue to add it

	; label exists, if we are in SET mode overwrite, else return with error
	lda @allow_overwrite
	bne @overwrite
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED

;------------------------------------------------------------------------------
@overwrite:
	; label exists, overwrite its old value
	jsr setaddr		; set the new value for the label
	jsr set_location		; an export replaces its import placeholder
	ldxy id
	clc			; ok
@ret:	rts

;------------------------------------------------------------------------------
@insert:
	; check if there's room for another label
	ldxy __label_num
	cmpw #MAX_LABELS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_LABELS
	rts

:
;------------------------------------------------------------------------------
@cont:	; load the pointers for the label we are creating
	ldxy __label_num
	jsr loadlabel

	; write the symbol's data to the label structure
	; 1. write the hash value for the symbol
	ldxy @name
	jsr hash_name
	ldy #LABEL_HASH
	lda hash
	PAGE_STORE label, SYM_RECORDS		; store LSB of hash
	iny
	lda hash+1
	PAGE_STORE label, SYM_RECORDS		; store MSB of hash

	; 2. write the ID for the label (current number of labels)
	ldy #LABEL_ID
	lda __label_num
	sta id
	PAGE_STORE label, SYM_RECORDS		; store LSB of label's id
	iny
	lda __label_num+1
	sta id+1
	PAGE_STORE label, SYM_RECORDS		; store MSB of label's id

	; 3. set NAME for the label (string) and NAME field (pointer to it)
	ldxy @name
	stxy r0			; r0  = label name to set
	jsr set_name		; set name pointer + string
	bcs @ret		; packed name pool exhausted

	; 4. write all ADDR related fields (FLAGS, ADDR)
	jsr set_addr

	; 5. write the FILE and LINE number for the label
	jsr set_location

	; 6. append pointer to the node we just built to its bucket's list
	ldxy hash
	jsr getlist		; load relevant list from the label's hash
	jsr listappend		; append node to that list

	incw __label_num	; success, increment symbol count

;------------------------------------------------------------------------------
@done:	ldxy id
	RETURN_OK
.endproc

;*******************************************************************************
; ADD ANON
; Adds an anonymous label at the given address
; IN:
;  - .XY: the address to add an anonymous label at
; OUT:
;  - .C: set if there are too many anonymous labels to add another
.proc add_anon
@dst=r2
@addr=r6
@src=r8
	stxy @addr
	lda numanon+1
	cmp #>MAX_ANON
	bcc :+
	lda numanon
	cmp #<MAX_ANON
	bcc :+
	lda #ERR_TOO_MANY_LABELS
	;sec
	rts			; return err

:	lda numanon+1
	sta @src+1

	lda numanon
	asl			 ; *2
	rol @src+1
	adc #<anon_addrs
	sta @src
	lda #>anon_addrs
	adc @src+1
	sta @src+1

	lda asm::mode
	beq :+
	ldxy @src
	stxy @dst
	jmp @finish		; object labels retain source order across fragments
:
	jsr seek_anon
	stxy @dst
	cmpw @src
	beq @finish		; skip shift if this is the highest address

	; shift the labels at/above the insertion point up one entry
@shift:	; src -= 2 (next entry to move up)
	lda @src
	sec
	sbc #$02
	sta @src
	bcs :+
	dec @src+1

:	; src[2] = src[0]
	; src[3] = src[1]
	ldy #$00
	LOADB_Y @src	; LSB
	ldy #$02	; move up 2 bytes
	STOREB_Y @src
	dey
	LOADB_Y @src	; MSB
	ldy #$03	; move up 2 bytes
	STOREB_Y @src

	; loop until the entry at the insertion point has been shifted
	lda @src
	cmp @dst
	bne @shift
	lda @src+1
	cmp @dst+1
	bne @shift

@finish:
	; insert the address of the anonymous label we're adding
	lda @addr
	ldy #$00
	STOREB_Y @src
	lda @addr+1
	iny
	STOREB_Y @src

	incw numanon
	RETURN_OK
.endproc

;*******************************************************************************
; SEEK ANON
; Finds the address of the first anonymous label that has a greater address than
; or equal to the given address.
; If there is no anonymous label greater or equal to the address given,
; returns the address of the end of the anonymous labels
; (anon_addrs+(2*numanons))
; This procedure doesn't return the address represented by the anonymous label
; but rather where that label is actually stored.
; IN:
;  - .XY: the address to search for
; OUT:
;  - .XY: the address where the 1st anon label with a bigger address than the
;         one given is stored in the anon_addrs table
;  - .C: set if the given address is greater than all in the table
;        (if .XY represents an address outside the range of the table)
.proc seek_anon
@cnt=r0
@seek=r2
@addr=r4
	stxy @addr
	ldxy #anon_addrs

	lda numanon+1
	ora numanon
	beq @ret	; no anonymous labels defined -> return the base address

	stxy @seek
	lda #$00
	sta @cnt
	sta @cnt+1

	tay		; .Y = 0
@l0:	LOADB_Y @seek	; get LSB
	tax		; .X = LSB
	incw @seek
	LOADB_Y @seek	; get MSB
	incw @seek

	cmp @addr+1
	bcc @next	; if MSB is < our address, check next
	bne @found	; if > we're done
	cpx @addr	; MSB is =, check LSB
	bcs @found	; if LSB is >, we're done

@next:	incw @cnt
	lda @cnt+1
	cmp numanon+1
	bne @l0
	lda @cnt
	cmp numanon
	bne @l0		; loop til we've checked all anonymous labels

	; none found, get last address and return
	jsr @found
	sec		; given address is > all in table
	rts

@found:
	lda @cnt+1
	sta @seek+1
	lda @cnt
	asl
	rol @seek+1
	adc #<anon_addrs
	tax
	lda @seek+1
	adc #>anon_addrs
	sta @seek+1
	tay
	;clc
@ret:	rts
.endproc

;*******************************************************************************
; GET FANON
; Returns the address of the nth forward anonymous label relative to the given
; address. That is the nth anonymous label whose address is greater than
; the given address.
; IN:
;  - .XY: the address relative to the anonymous label to get
;  - .A:  how many anonymous labels forward to look
; OUT:
;  - .A:  the size of the address (or error code if none)
;  - .XY: the nth anonymous label whose address is > than the given address
;  - .C:  set if there is not an nth forward anonymous label
.proc get_fanon
@cnt=r0
@fcnt=r2
@addr=r4
@seek=r6
	sta @fcnt
	lda asm::mode
	beq @direct

	; assembling to object code
	lda @fcnt
	sec
	sbc #$01
	clc
	adc __label_anon_cursor
	tax
	lda __label_anon_cursor+1
	adc #$00
	tay
	jmp object_anon_value

@direct:
	stxy @addr

	ldxy #anon_addrs
	stxy @seek

	ldy numanon+1
	ldx numanon
	bne :+
	dey
	bmi @err		; no anonymous labels defined
:	dex
	stxy @cnt

@l0:	ldy #$01		; MSB
	LOADB_Y @seek
	cmp @addr+1
	beq @chklsb		; if =, check the LSB
	bcc @next		; MSB is < what we're looking for, try next
	bcs @f
@chklsb:
	dey		; .Y = 0
	LOADB_Y @seek	; check if our address is less than the seek one
	cmp @addr
	beq @next
	bcc @next

	; MSB is >= base and LSB is >= base address
@f:	dec @fcnt		; is this the nth label yet?
	jeq get_anon_retval	; if our count is 0, yes, end
	bne @next		; if count is not 0, continue

@next:	lda @seek
	clc
	adc #$02
	sta @seek
	bcc :+
	inc @seek+1

:	; loop until we run out of anonymous labels to search
	lda @cnt
	bne :+
	dec @cnt+1
	bmi @err	; count exhausted
	dec @cnt	; wrap LSB to $ff and continue
	jmp @l0
@err:	RETURN_ERR ERR_LABEL_UNDEFINED

:	dec @cnt
	jmp @l0
.endproc

;*******************************************************************************
; GET BANON
; Returns the address of the nth backward anonymous address relative to the
; given  address. That is the nth anonymous label whose address is less than
; the given address.
; IN:
;  - .XY: the address relative to the anonymous label to get
;  - .A:  how many anonymous labels backwards to look
; OUT:
;  - .XY: the nth anonymous label whose address is < than the given address
;  - .C: set if there is no backwards label matching the given address
.proc get_banon
@bcnt=r8
@addr=r4
@seek=r6
	sta @bcnt
	lda asm::mode
	beq @direct

	lda __label_anon_cursor
	sec
	sbc @bcnt
	tax
	lda __label_anon_cursor+1
	sbc #$00
	tay
	jmp object_anon_value

@direct:
	stxy @addr

	; get address to start looking backwards from
	jsr seek_anon
	bcc :+			; if found, skip ahead

	; if we ended after the end of the anonymous label list, move
	; to a valid location in it (to the last item)
	txa
	;sec
	sbc #$02
	tax
	tya
	sbc #$00
	tay

:	stxy @seek
	iszero numanon
	beq @err		; no anonymous labels defined

@l0:	ldy #$01		; MSB
	LOADB_Y @seek
	cmp @addr+1
	beq @chklsb		; if =, check the LSB
	bcs @next		; MSB is > what we're looking for, try next

	; MSB is >= base and LSB is >= base address
@b:	dec @bcnt		; is this the nth label yet?
	jeq get_anon_retval	; if our count is 0, yes, end
	bne @next		; if count is not 0, continue

@chklsb:
	dey			; .Y=0
	LOADB_Y @seek
	cmp @addr		; check if our address is less than the seek one
	beq @b			; if our address is <= this is a backward anon
	bcc @b

@next:	lda @seek
	sec
	sbc #$02
	sta @seek
	tax
	bcs :+
	dec @seek+1

:	; loop until we run out of anonymous labels to search
	ldy @seek+1
	cmpw #anon_addrs-2
	bne @l0

@err:	RETURN_ERR ERR_LABEL_UNDEFINED
.endproc

;*******************************************************************************
; ANON VALUE
; Resolve an anonymous label by source position, not address (for object code
; generation)
; IN:
;   - .XY: index of the anonymous label to find the address of
; OUT:
;   - .XY: the resolved address for the requested anonymous label
.proc object_anon_value
@index = r0
@entry = r6
	cmpw numanon
	bcs @missing
	stxy @index

	txa
	asl
	sta @entry
	tya
	rol
	sta @entry+1

	lda @entry
	clc
	adc #<anon_addrs
	sta @entry
	lda @entry+1
	adc #>anon_addrs
	sta @entry+1

	ldy #$00
	LOADB_Y @entry
	sta expr::value

	iny
	LOADB_Y @entry
	sta expr::value+1

	; get the fragment of the anonymous label
	ldxy @index
	CALL FINAL_BANK_LINKER, obj::anon_fragment
	sta expr::segment

	cmp #SEG_ABS
	beq @absolute
	lda #VAL_REL
	skw
@absolute:
	lda #VAL_ABS
	sta expr::kind

	lda #$00
	sta expr::postproc
	ldxy expr::value
	lda #$02
	clc
	rts

@missing:
	RETURN_ERR ERR_LABEL_UNDEFINED
.endproc

.proc get_anon_retval
@seek=r6
	ldy #$00
	LOADB_Y @seek		; get LSB of anonymous label address
	tax
	iny
	LOADB_Y @seek		; get the MSB of our anonymous label
	tay
	lda #$02		; always use 2 bytes for anon address size
	RETURN_OK
.endproc

;*******************************************************************************
; GET SEGMENT
; Returns the SEGMENT ID for the given label ID
; IN:
;  - .XY: the label ID to get the segment for
; OUT:
;  - .A: the segment ID for the label
.proc get_segment
	; load the symbol and mask the segment-id bits (1-7)
	jsr loadlabel
	lda flags
	lsr
	cmp #$7f		; are all bits set?
	bne :+
	lda #SEG_ABS		; if segment id is $7f, pad to SEG_ABS ($ff)
:	rts
.endproc

;*******************************************************************************
; COUNT FLOATS
; Count float symbols in the current symbol table. This is a direct entry in
; FINAL_BANK_SYMBOLS.
; OUT:
;   - .XY = count
;   - .Z = set if zero
.export __label_count_floats
.proc __label_count_floats
@i=temp+2		; loadlabel uses temp+0..+1
@count=temp+4
	lda #$00
	sta @i
	sta @i+1
	sta @count
	sta @count+1
@loop:	ldxy @i
	cmpw __label_num
	beq @done
	jsr get_segment
	cmp #SEG_FLOAT
	bne :+
	incw @count
:	incw @i
	jmp @loop
@done:	ldxy @count
	txa
	ora @count+1
	clc
	rts
.endproc

;*******************************************************************************
; ADDRESS
; Returns the address of the label in (.YX)
; The address mode of the label is returned as well.
; IN:
;  - .XY: the address of the label name to get the address of
; OUT:
;  - .XY: the address of the label
;  - .C:  is set if no label was found, clear if it was
;  - .A:  the size (address mode) of the label
.proc address
	jsr find		; get the id in .XY
	bcc address_by_id	; if found, get the address for the id
	rts			; not found; return with .C set
.endproc

;*******************************************************************************
; ADDRESS BY ID
; Returns the address of the label of the given ID
; Also returns the address mode.
; IN:
;  - .XY: the ID of the label to find the address/mode of
; OUT:
;  - .XY: the address of the label
;  - .A:  the size (address mode) of the label (0=ZP, 1=ABS)
.proc address_by_id
	jsr getaddr		; get address
	lda flags
	and #$01		; mask MODE bit
	rts
.endproc

;*******************************************************************************
; SET NAME
; Allocates a packed name and stores its word offset in the label record and
; name index. Initializes the parallel name-index ID entry as well.
; IN:
;   - label:    byte offset of the new record in SYM_RECORDS
;   - id:       ID of the new label
;   - r0:       CPU address of the name string
;   - name_top: word offset of the next free name-pool entry
; OUT:
;   - name_top: advanced past the terminated name, rounded up to a word
;   - .C: set if the input is unterminated or the name pool is full
.proc set_name
@name = r0
@dest = r2
@idx = temp+2
	; check the complete name before writing any bytes to the pool
	ldy #$00
@length:
	lda (@name),y
	jsr isseparator
	beq @reserve
	iny
	bne @length
	RETURN_ERR ERR_LABEL_TOO_LONG

@reserve:
	jsr check_name_space
	bcc :+
	rts

:	ldxy name_top
	stxy @dest
	ldy #LABEL_NAME
	txa
	PAGE_STORE label, SYM_RECORDS
	iny
	lda @dest+1
	PAGE_STORE label, SYM_RECORDS

	lda id
	asl
	sta @idx
	lda id+1
	rol
	sta @idx+1
	lda @idx
	clc
	adc #<label_names_sorted
	sta @idx
	lda @idx+1
	adc #>label_names_sorted
	sta @idx+1
	ldy #$00
	lda @dest
	PAGE_STORE @idx, SYM_INDEXES
	iny
	lda @dest+1
	PAGE_STORE @idx, SYM_INDEXES
	lda @idx+1
	clc
	adc #>(MAX_LABELS*2)
	sta @idx+1
	ldy #$00
	lda id
	PAGE_STORE @idx, SYM_INDEXES
	iny
	lda id+1
	PAGE_STORE @idx, SYM_INDEXES

	ldy #$00
@copy:
	lda (@name),y
	jsr isseparator
	bne :+
	lda #$00
:	PAGE_STORE @dest, SYM_NAMES
	beq @done
	iny
	bne @copy
@done:
	; fall through to advance_name
.endproc

;*******************************************************************************
; ADVANCE NAME
; Advances the name-pool word offset past a terminated name, including padding
; to the next word boundary: name_top += floor(Y / 2) + 1.
; IN:
;   - .Y: byte offset of the terminator (number of name bytes before the NUL)
; OUT:
;   - name_top: word offset of the next free name-pool entry
.proc advance_name
	tya
	lsr
	sec
	adc name_top
	sta name_top
	bcc @done
	inc name_top+1
	bne @done
	inc name_full
@done:	clc
	rts
.endproc

;*******************************************************************************
; CHECK NAME SPACE
; Check if the given name fits in memory
; OUT:
;   - .C: set if we are out of memory in the name memory pool
.proc check_name_space
	lda name_full
	bne @full
	tya
	lsr
	sec
	adc name_top
	tax
	lda name_top+1
	adc #0
	bcc @ok
	bne @full
	cpx #0
	beq @ok
@full:	RETURN_ERR ERR_OOM
@ok:	clc
	rts
.endproc

;*******************************************************************************
; IS LOCAL
; Returns with .Z clear if the given label name represents a
; scoped label (begins with '@', or '.' for an object-local symbol)
; IN:
;  - .XY: the label to test
; OUT:
;  - .A: nonzero if the label is local
;  - .Z: clear if label is local, set if not
.proc is_local
@l=temp+2
	stxy @l
	ldy #$00
	lda (@l),y
	ldy @l+1
	cmp #'@'
	beq @local
	cmp #'.'
	bne :+
@local:
	lda #$01	; flag that label IS local
	rts
:	lda #$00	; flag that label is NOT local
	rts
.endproc

;*******************************************************************************
; LABEL BY ID
; Returns the address for the label with the given ID
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY:   address of the given label id
;  - label: points to the label struct that was returned
.proc by_id
	jsr loadlabel
	ldxy addr
	rts
.endproc

;*******************************************************************************
; ID BY ALPHA INDEX
; Looks up the ID of the label at the provided alphabetical index.
; Labels must be indexed (lbl::index) for this to return the correct ID.
; IN:
;  - .XY: the alphabetical index of the label to get the ID of
; OUT:
;  - .XY: the ID of the label at the given alphabetical index
.proc id_by_alpha_index
@arr = r0
	txa
	asl
	sta @arr
	tya
	rol
	sta @arr+1
	;clc
	lda @arr
	adc #<label_names_sorted_ids
	sta @arr
	lda @arr+1
	adc #>label_names_sorted_ids
	sta @arr+1

	ldy #$00
	PAGE_LOAD @arr, SYM_INDEXES
	tax
	iny
	PAGE_LOAD @arr, SYM_INDEXES
	tay
	rts
.endproc

;*******************************************************************************
; BY ADDR
; Returns the label for a given address by performing a binary search on the
; cache of sorted label addresses
; NOTE: Labels must be indexed (lbl::index) in order for this function to return
; the correct ID. If you've added a label since the last index, it is necessary
; to re-index.
; IN:
;  - .XY: the label address to get the name of
; OUT:
;  - .XY: the ID of the label (exact match or closest one at address less than
;         the one provided.
;  - .C: set if no EXACT match for the label is found
.proc by_addr
@arr        = r0
@comparator = r2
@target     = r6
@cursor     = zp::tmp10
	lda __label_num
	ora __label_num+1
	beq @none
	lda #<label_addresses_sorted
	sta @arr
	lda #>label_addresses_sorted
	sta @arr+1

	lda #<addr_comparator
	sta @comparator
	lda #>addr_comparator
	sta @comparator+1

	jsr find_sorted
@candidate:
	jsr loadlabel
	lda flags
	and #$fe
	cmp #(SEG_FLOAT << 1)
	beq @previous		; pool handles are not addresses
	ldxy addr
	cmpw @target
	beq @exact
	bcs @previous
	ldxy id
	sec
	rts
@exact:
	ldxy id
	clc
	rts
@previous:
	ldxy @cursor
	cmpw #label_addresses_sorted_ids
	beq @none
	bcc @none
	decw @cursor
	decw @cursor
	ldy #$00
	PAGE_LOAD @cursor, SYM_INDEXES
	tax
	iny
	PAGE_LOAD @cursor, SYM_INDEXES
	tay
	jmp @candidate

@none:	ldxy #$ffff
	RETURN_ERR ERR_LABEL_UNDEFINED
.endproc

;*******************************************************************************
; ID BY ADDR INDEX
; Returns the ID of the nth label sorted by address.
; IN:
;   - .XY: the index of the label to get from the sorted addresses
; OUT:
;   - .XY: the id of the nth label (in sorted order)
.proc id_by_addr_index
@tmp = rc
	txa
	asl
	sta @tmp
	tya
	rol
	sta @tmp+1
	lda @tmp
	adc #<label_addresses_sorted_ids
	sta @tmp
	lda @tmp+1
	adc #>label_addresses_sorted_ids
	sta @tmp+1
	ldy #$00
	PAGE_LOAD @tmp, SYM_INDEXES
	tax
	iny
	PAGE_LOAD @tmp, SYM_INDEXES
	tay
	rts
.endproc

;*******************************************************************************
; ISVALID
; checks if the label name given is a valid label name
; IN:
;  - .XY: the address of the label
; OUT:
;  - .C: set if the label is NOT valid
.proc is_valid
	jsr canonical_name
	bcs @ret
	jmp is_valid_name
@ret:	rts
.endproc

;*******************************************************************************
; IS VALID NAME
; Validates a canonical symbol token.
; IN:
;   - .XY: zero-terminated name
; OUT:
;   - .C: set and .A = error code if invalid
.proc is_valid_name
@name = r4
	stxy @name
	ldy #$00

; first character must be a letter, '@', or an object-local '.'
@l0:	lda (@name),y
	iny
	jsr iswhitespace
	beq @l0

	; check first non whitespace char
	cmp #'@'
	beq @cont
	cmp #'.'
	beq @cont
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

	; make sure string is not an opcode (opcodes are not valid labels).
	lda zp::line
	pha
	lda zp::line+1
	pha
	lda @name
	sta zp::line
	lda @name+1
	sta zp::line+1
	tya
	pha			; save name offset (isopcode clobbers .Y)
	CALLMAIN asm::isopcode
	pla
	tay			; restore name offset
	pla
	sta zp::line+1
	pla
	sta zp::line
	bcc @err

	; following characters must be between '0' and 'Z'
@cont:
@l1:	lda (@name),y
	jsr isseparator
	beq @done
	cmp #'.'		; object qualifier within a symbol name
	beq @nextchar
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	bcs @err
@nextchar:
	iny
	bne @l1
	beq @toolong		; unterminated byte-indexed input
@err:	RETURN_ERR ERR_ILLEGAL_LABEL

@toolong:
	RETURN_ERR ERR_LABEL_TOO_LONG
@done:	RETURN_OK
.endproc

;*******************************************************************************
; GET NAME
; Copies the name of the label ID given to the provided buffer
; IN:
;  - .XY: the ID of the label to get the name of
;  - r0:  destination with room for a page (normally lbl::namebuffer)
; OUT:
;  - (r0): the label name
;  - .Y:   the length of the copied label
.proc get_name
@dst = r0
	jsr loadlabel
	ldy #0
@copy:	PAGE_LOAD name, SYM_NAMES
	sta (@dst),y
	beq @done
	iny
	bne @copy
@done:	rts
.endproc

;*******************************************************************************
; GET ADDR
; Returns the address of the given label ID.
; IN:
;  - .XY: the ID of the label to get the address of
; OUT:
;  - label: label data is loaded (via loadlabel)
;  - .XY:   address of the label
.proc getaddr
	jsr loadlabel
	ldxy addr
	rts
.endproc

;*******************************************************************************
; GET FILE AND LINE
; Returns the file ID and the line number for the requested label ID
; IN:
;   - .XY: ID of label to get file/line # of
; OUT:
;   - .A:  file ID for label
;   - .XY: line number within file (zero if no source definition is available)
.proc get_file_and_line
	jsr loadlabel
	ldxy lineno
	lda file_id
	rts
.endproc

;*******************************************************************************
; SET LOCATION
; Stores definition metadata for the already loaded symbol.
; IN:
;   - label_fileid: file ID of the symbol
;   - label_lineno: line number for the symbol
.proc set_location
	ldy #LABEL_FILE
	lda zp::label_fileid
	PAGE_STORE label, SYM_RECORDS
	iny
	lda zp::label_lineno
	PAGE_STORE label, SYM_RECORDS
	iny
	lda zp::label_lineno+1
	PAGE_STORE label, SYM_RECORDS
	rts
.endproc

;*******************************************************************************
; REMAP FILES
; Translate the file IDs of the loaded .D symbol table after dbgi::load.
.proc remap_files
@id=r8
	ldxy #$0000
	stxy @id
@loop:	ldxy @id
	cmpw __label_num
	beq @done
	jsr get_file_and_line
	cpx #$00
	bne @map
	cpy #$00
	beq @next		; no source definition -> skip

@map:	CALL FINAL_BANK_DEBUG, dbgi::globalfile
	bcs @ret
	ldy #LABEL_FILE
	PAGE_STORE label, SYM_RECORDS
@next:	incw @id
	jmp @loop
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; IS DEFINITION SEPARATOR
; IN:
;  - .A: character to test
; OUT:
;  - .Z: set if the char is whitespace or a ':'
.proc is_definition_separator
	cmp #':'
	beq :+		; -> rts
	; fall through to iswhitespace
.endproc

;*******************************************************************************
; ISWHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.proc iswhitespace
	cmp #$0d	; newline
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #' '
:	rts
.endproc

;*******************************************************************************
; ISSEPARATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is any separator
.proc isseparator
@xsave=zp::util
	jsr iswhitespace
	beq @done

	stx @xsave
	ldx #@numops-1
:	cmp @ops,x
	beq @end
	dex
	bpl :-
@end:	php
	ldx @xsave
	plp
@done:	rts
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', K_PIPE, ',', ':',0
	.byte '<', '>', '=', '!'
@numops = *-@ops
.endproc

;*******************************************************************************
; MACROS
; These macros are used by sort_by_addr

;*******************************************************************************
; SETPTRS ADDR
; update @idi and @idj based on the values of @i and @j
; these pointers are offset by a fixed amount from @i and @j
.proc setptrs_addr
@i   = r0
@j   = r2
@idi = zp::tmp10
@idj = zp::tmp12
	lda @i
	clc
	adc #<(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idi
	lda @i+1
	adc #>(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idi+1

	lda @j
	;clc
	adc #<(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idj
	lda @j+1
	adc #>(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idj+1
	rts
.endproc

;*******************************************************************************
; SETPTRS NAME
; update @idi and @idj based on the values of @i and @j
; these pointers are offset by a fixed amount from @i and @j
.proc setptrs_name
@i   = r0
@j   = r2
@idi = zp::tmp10
@idj = zp::tmp12
	lda @i
	clc
	adc #<(label_names_sorted_ids-label_names_sorted)
	sta @idi
	lda @i+1
	adc #>(label_names_sorted_ids-label_names_sorted)
	sta @idi+1

	lda @j
	;clc
	adc #<(label_names_sorted_ids-label_names_sorted)
	sta @idj
	lda @j+1
	adc #>(label_names_sorted_ids-label_names_sorted)
	sta @idj+1
	rts
.endproc

;*******************************************************************************
; FIND SORTED
; Finds the last entry at or below the target using the given comparator
; IN:
;   - .XY: value to find
;   - r0:  array to seek within
;   - r2:  comparator to use for binary search
.proc find_sorted
@arr        = r0
@comparator = r2
@a          = r4
@b          = r6
@lb         = rc
@ub         = re
@m          = zp::tmp10
@top        = zp::tmp12
	stxy @b

	; if no labels exist, there is nothing to find
	lda __label_num
	ora __label_num+1
	bne :+
	RETURN_ERR ERR_LABEL_UNDEFINED

:	lda __label_num
	asl
	sta @ub
	lda __label_num+1
	rol
	sta @ub+1

	; @lb = @arr
	; @ub = @arr + (__label_num*2)
	lda @arr
	sta @lb
	adc @ub
	sta @ub
	sta @top
	lda @arr+1
	sta @lb+1
	adc @ub+1
	sta @ub+1
	sta @top+1

;-------------------------------------------------------------------------------
@loop:	lda @ub
	sec
	sbc @lb
	tax
	lda @ub+1
	sbc @lb+1
	bcc @done	; if low > high, not found
	lsr		; calculate (high-low) / 2
	tay
	txa
	ror		; carry cleared because multiple of 2
	and #$fe	; align to element size
	adc @lb		; mid = low + ((high - low) / 2)
	sta @m
	tya
	adc @lb+1
	sta @m+1

	; if mid is one past the last element, the target is > all entries
	ldx @m
	ldy @m+1
	cmpw @top
	bne :+
	jmp @past_end

:	ldy #$00

	; load A[mid] and compare our target against it
	PAGE_LOAD @m, SYM_INDEXES
	sta @a
	iny
	PAGE_LOAD @m, SYM_INDEXES
	sta @a+1
	jsr @compare_func
	beq @modlow	; search right through all equal entries
	bcs @modhigh	; A[mid] > value

@modlow:
	; A[mid] <= value
	lda @m		; low = mid + element size
	clc		; equality also reaches here, with carry set
	adc #$02
	sta @lb
	lda @m+1
	adc #$00
	sta @lb+1
	jmp @loop

@modhigh:		; A[mid] > value
	lda @m		; high = mid - element size
	;sec
	sbc #$02
	sta @ub
	lda @m+1
	sbc #$00
	sta @ub+1
	jmp @loop

@done:	bne @err	; if not exact match, jump to error handling

;-------------------------------------------------------------------------------
@ok:	; look up the ID for the address
	lda @m
	clc
	adc #<(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m

	lda @m+1
	adc #>(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m+1

	ldy #$00
	PAGE_LOAD @m, SYM_INDEXES
	tax
	iny
	PAGE_LOAD @m, SYM_INDEXES
	tay
	RETURN_OK

@err:	ldxy @ub	; get the lower bound of where our search ended
	stxy @m		; and set our result variable to it (ub < lb here)
	jsr @ok		; get the closest label
	cmpw __label_num	; was the result a valid label?
	bcc :+		; if so, continue to return

@past_end:
	; if label wasn't valid, get the highest label by address
	lda @top
	sec
	sbc #$02
	sta @m
	lda @top+1
	sbc #$00
	sta @m+1
	jsr @ok

:	sec
	rts

;-------------------------------------------------------------------------------
@compare_func:
	jmp (@comparator)
.endproc

;*******************************************************************************
; INDEX
; Updates the by-address and by-name sorting of the labels. This allows labels
; to be looked up by their address (see lbl::by_addr), or their
; name (see lbl::by_name)
;
; Code adapted from code by Vladimir Lidovski aka litwr (with help of BigEd)
; via codebase64.org
.proc index
@i          = r0
@j          = r2
@a          = r4
@b          = r6
@ub         = r8
@lb         = ra
@tmp        = rc
@num        = re
@idi        = zp::tmp10
@idj        = zp::tmp12
@sp         = zp::tmp14
@comparator = zp::tmp15
@setptrs_fn = zp::util
@arr        = zp::util+2
	lda __label_num+1
	bne @index_by_addr	; > 255 labels -> sort
	lda __label_num
	cmp #$02
	bcs @index_by_addr
	clc
	rts			; 0 or 1 labels -> nothing to sort

;-------------------------------------------------------------------------------
; index by address
@index_by_addr:
.ifdef vic20
	lda #SYMBOL_INDEXES_BANK
	sta $9ffa
	lda #SYMBOL_INDEXES_BANK+1
	sta $9ffc
	ldxy #$4000
.else
	ldxy #label_addresses_sorted
.endif
	stxy @arr
	ldxy #setptrs_addr
	stxy @setptrs_fn
	ldxy #addr_comparator
	jsr @index

;-------------------------------------------------------------------------------
; index by name
@index_by_name:
.ifdef vic20
	lda #SYMBOL_INDEXES_BANK+2
	sta $9ffa
	lda #SYMBOL_INDEXES_BANK+3
	sta $9ffc
	ldxy #$4000
.else
	ldxy #label_names_sorted
.endif
	stxy @arr
	ldxy #setptrs_name
	stxy @setptrs_fn
	ldxy #name_comparator

	jsr @index
	SELECT_BANK "SYMBOLS"
	clc
	rts

	; fall through to @index

;-------------------------------------------------------------------------------
; MAIN INDEX SUBPROC
@index:	stxy @comparator

	; @num = 2*(__label_num-1)
	lda __label_num
	sec
	sbc #$01
	sta @num
	lda __label_num+1
	sbc #$00
	sta @num+1
	asl @num
	rol @num+1
	jmp @quicksort	; enter the sort routine

@quicksort0:
	tsx
	cpx #16		; stack limit
	bcs @qsok

	ldx @sp
	txs

@quicksort:
	; initialize upper bound pointer to end of array to sort
	lda @arr
	sta @lb
	clc
	adc @num
	sta @ub
	lda @arr+1
	sta @lb+1
	adc @num+1
	sta @ub+1

	tsx
	stx @sp

@qsok:	; @i = @lb
	lda @lb
	sta @i
	lda @lb+1
	sta @i+1

	; @j = @ub
	ldy @ub+1
	sty @j+1
	lda @ub
	sta @j

	; @tmp = (@j + @i) / 2
	clc		; this code works only for the evenly aligned arrays
	adc @i
	and #$fc
	sta @tmp
	tya
	adc @i+1
	ror
	sta @tmp+1
	ror @tmp

	; @a = array[(@j+@i) / 2]
	ldy #$00
.ifdef vic20
	LOADB_Y @tmp
.else
	PAGE_LOAD @tmp, SYM_INDEXES
.endif
	sta @a
	iny
.ifdef vic20
	LOADB_Y @tmp
.else
	PAGE_LOAD @tmp, SYM_INDEXES
.endif
	sta @a+1

@qsloop1:
	; @b = array[i]
	; while (@b  > @a) { inc @i }
	ldy #$00		; compare array[i] and x
.ifdef vic20
	LOADB_Y @i
.else
	PAGE_LOAD @i, SYM_INDEXES
.endif
	sta @b
	iny
.ifdef vic20
	LOADB_Y @i
.else
	PAGE_LOAD @i, SYM_INDEXES
.endif
	sta @b+1

	jsr @compare_func	; is @a < @b?
	bcc @qs_l1
	beq @qs_l1

	lda #$02		; move @i to next element
	clc
	adc @i
	sta @i
	bcc @qsloop1
	inc @i+1
	bne @qsloop1		; branch always

@qs_l1:	ldy #$00		; compare array[j] and x
.ifdef vic20
	LOADB_Y @j
.else
	PAGE_LOAD @j, SYM_INDEXES
.endif
	sta @b
	iny
.ifdef vic20
	LOADB_Y @j
.else
	PAGE_LOAD @j, SYM_INDEXES
.endif
	sta @b+1
	jsr @compare_func	; is @a < @b?
	bcs @qs_l3		; if so, break

	lda @j
	sec
	sbc #$02		; move @j to prev element
	sta @j
	bcs @qs_l1
	dec @j+1
	bne @qs_l1		; branch always

@qs_l3:
	lda @j			; compare iterators i and j
	cmp @i
	lda @j+1
	sbc @i+1
	bcc @qs_l8

@qs_l6:	jsr @setptrs
.ifdef vic20
	SWAPB_Y @i, @j
.else
	INDEX_SWAP @i, @j
.endif		; swap array[@i] and array[@j]
.ifdef vic20
	SWAPB_Y @idi, @idj
.else
	INDEX_SWAP @idi, @idj
.endif	; swap ids[@i] and ids[@j]

	dey
	bpl @qs_l6

	clc
	lda #$02
	adc @i
	sta @i
	bcc :+
	inc @i+1
:	sec
	lda @j
	sbc #$02
	sta @j
	bcs :+
	dec @j+1
	;lda @j
:	cmp @i
	lda @j+1
	sbc @i+1
	;bcc *+5
	jmp @qsloop1

@qs_l8:	lda @lb
	cmp @j
	lda @lb+1
	sbc @j+1
	bcs @qs_l5

	lda @i+1
	pha
	lda @i
	pha
	lda @ub+1
	pha
	lda @ub
	pha
	lda @j+1
	sta @ub+1
	lda @j
	sta @ub
	jsr @quicksort0

	pla
	sta @ub
	pla
	sta @ub+1
	pla
	sta @i
	pla
	sta @i+1

@qs_l5:	lda @i
	cmp @ub
	lda @i+1
	sbc @ub+1
	bcs @done

	lda @i+1
	sta @lb+1
	lda @i
	sta @lb
	jmp @qsok
@done:  rts

;-------------------------------------------------------------------------------
@compare_func:
	jmp (@comparator)

;-------------------------------------------------------------------------------
@setptrs:
	jmp (@setptrs_fn)
.endproc

;*******************************************************************************
; ADDR COMPARATOR
; Comparator for the quicksort procedure for sort-by ADDRESS.
; IN:
;   - @a: address of first address to compare (LHS of comparison)
;   - @b: address of second address to compare (RHS of comparison)
; OUT:
;   - .C: set if the address @a >= @b
;   - .Z: set if the values are equal
.proc addr_comparator
@a = r4
@b = r6
	lda @a+1
	cmp @b+1
	bne :+
	lda @a
	cmp @b
:	rts
.endproc

;*******************************************************************************
; NAME COMPARATOR
; Comparator for the quicksort procedure for sort-by NAME.
; IN:
;   - r4: word offset of the pivot name (LHS of comparison)
;   - r6: word offset of the candidate name (RHS of comparison)
; OUT:
;   - .C: set if the pivot name sorts after or equals the candidate name
;   - .Z: set if the names are equal
; Compare NUL-terminated names directly in the pool; no fixed-size cache.
.proc name_comparator
@str_a = r4
@str_b = r6
@b = rc
@savey = rd
	sty @savey
	ldy #0
@loop:	PAGE_LOAD @str_b, SYM_NAMES
	sta @b
	PAGE_LOAD @str_a, SYM_NAMES
	cmp @b
	bne @done
	cmp #0
	beq @equal
	iny
	bne @loop
@equal:	sec
@done:	php
	ldy @savey
	plp
	rts
.endproc

;*******************************************************************************
; LOAD LABEL
; Loads the label pointers for the label of the given ID
; IN:
;   - .XY: id of the label to get the data for
; OUT
;   - label, flags, hash, addr, id, name: values for the requested label
.proc loadlabel
@tmp=temp
	; get address of the label data to (*SIZEOF_LABEL)
	stxy label

	txa
	asl			; *2
	sta @tmp
	rol label+1
	ldx label+1
	stx @tmp+1
	asl			; *4
	rol label+1
	adc @tmp		; *6
	sta label
	lda label+1
	adc @tmp+1
	sta label+1
	asl label		; *12
	rol label+1

	; add offset to labels data
	lda label
	clc
	adc #<labels
	sta label
	lda label+1
	adc #>labels
	sta label+1

	; load the complete record into its matching zero-page fields
	ldxy label
	jmp sympage::copy_record
.endproc

;*******************************************************************************
; HASH NAME
; Returns a hash key for the given label
; IN:
;   - .XY: address of the label to return hash key for
; OUT:
;   - hash: hashed value for the symbol
.proc hash_name
@name=r0
	stxy @name
	ldy #$00
	sty hash
	sty hash+1
@l0:
	lda (@name),y
	jsr isseparator
	beq @done
	pha
	ldx #$05
@rotate:
	asl hash
	rol hash+1
	bcc :+
	inc hash
:	dex
	bne @rotate
	pla
	eor hash
	sta hash
	iny
	bne @l0

@done:	ldxy hash
	rts
.endproc

;*******************************************************************************
; GET LIST
; Returns the address of the linked list of symbols for the given hash value
; The list returned is based on the lower 11 bits of the hash
; IN:
;  - .XY: the symbol's hash
; OUT:
;  - list: the address of the list
.proc getlist
	; get offset of bucket for the given hash
	tya
	and #$07		; only use low 3 bits of MSB
	sta bucket+1

	; *2 to get word alignment
	txa
	asl
	rol bucket+1
	adc #<label_buckets
	sta bucket
	lda bucket+1
	adc #>label_buckets
	sta bucket+1

	; initialize list pointer to first element
	ldy #$00
	LOADB_Y bucket
	sta list
	iny
	LOADB_Y bucket
	sta list+1
	rts
.endproc

;*******************************************************************************
; LIST NEXT
; Advances the given symbol linked list to the next node
; IN:
;   - r0: the list to advance
; OUT:
;   - r0: now points to next node in list
;   - .C: set if the list is already at the end
.proc listnext
@tmp=ra
	ldy #LIST_NEXT		; offset to NEXT pointer

	; check if we're already at end of the list, and return .C set if so
	PAGE_LOAD list, SYM_NODES		; get LSB of NEXT pointer
	sta @tmp
	tax
	iny
	PAGE_LOAD list, SYM_NODES		; get MSB
	ora @tmp		; is NEXT pointer value $0000?
	beq @end		; if so, we're at the end of the list

	; update list pointer to next node
	PAGE_LOAD list, SYM_NODES
	stx list
	sta list+1
	RETURN_OK

@end:	sec
	rts
.endproc

;*******************************************************************************
; LIST END
; Follows the list pointer until it is at the tail of the list (last node)
; LIST END
.proc listend
:	jsr listnext
	bcc :-
	rts
.endproc

;*******************************************************************************
; LIST APPEND
; Appends the given pointer to the current list
; IN:
;   - list:  list to advance
;   - label: pointer to symbol data to append as node to the list
.proc listappend
	; check if the list already exists
	iszero list
	bne @append_list

	; empty list, initialize it by creating the HEAD node
	ldy #$00
	lda listtop
	STOREB_Y bucket
	iny
	lda listtop+1
	STOREB_Y bucket
	jmp @set_node	; continue to write the LABEL and NEXT pointers for node

@append_list:
	; go to end of existing list and point TAIL to node we WILL add
	jsr listend

	ldy #LIST_NEXT
	lda listtop
	PAGE_STORE list, SYM_NODES
	iny
	lda listtop+1
	PAGE_STORE list, SYM_NODES

@set_node:
	; write the data for this new node (label pointer)
	ldy #LIST_LABEL
	lda label
	PAGE_STORE listtop, SYM_NODES
	iny
	lda label+1
	PAGE_STORE listtop, SYM_NODES

	; set NEXT pointer for new node to 0 (TAIL)
	ldy #LIST_NEXT
	lda #$00
	PAGE_STORE listtop, SYM_NODES
	iny
	PAGE_STORE listtop, SYM_NODES

	; move listtop to next available node
	lda listtop
	clc
	adc #SIZEOF_LABEL_LIST_NODE
	sta listtop
	bcc @done
	inc listtop+1

@done:	rts
.endproc

;*******************************************************************************
; FIND IN LIST
; Seeks for the given symbol name in the given list
; IN:
;   - .XY:  name of the symbol to look for
;   - list: linked list of bucket containing symbol
; OUT:
;   - .C:    set if the label is not found
;   - .A:    ERR_LABEL_UNDEFINED (if .C is set)
;   - label: if found, pointer to the label data for the matching symbol
.proc find_in_list
@sym   = r0
@len   = r2
@name  = temp
@other = temp+2
	; check if the list exists
	iszero list
	beq @notfound

	stxy @name

	; get the length to compare
	ldy #0
:	lda (@name),y
	jsr isseparator
	beq @cont
	iny
	bne :-
	beq @notfound
@cont:	sty @len

@l0:	; get address of the label data for this node
	ldy #LIST_LABEL
	PAGE_LOAD list, SYM_NODES
	sta @sym
	iny
	PAGE_LOAD list, SYM_NODES
	sta @sym+1
	ora @sym
	beq @notfound	; if LABEL address is $0000, label doesn't exist in list

	; first: does the HASH match? if not, don't bother comparing the NAME
	ldy #LABEL_HASH
	PAGE_LOAD @sym, SYM_RECORDS	; LSB of symbol's hash
	cmp hash
	bne @next
	iny
	PAGE_LOAD @sym, SYM_RECORDS	; MSB of symbol's hash
	cmp hash+1
	bne @next

	; hash matches, does the NAME match?
	ldy #LABEL_NAME
	PAGE_LOAD @sym, SYM_RECORDS
	sta @other
	iny
	PAGE_LOAD @sym, SYM_RECORDS
	sta @other+1
	lda @len
	jsr cmp_name	; do labels match?
	beq @found	; if so, we're done

@next:	jsr listnext	; move list to next node (if there is one)
	bcc @l0

@notfound:
	RETURN_ERR ERR_LABEL_UNDEFINED

@found:	; set the label pointer to the matching label's data
	ldy #LIST_LABEL
	PAGE_LOAD list, SYM_NODES
	sta label
	iny
	PAGE_LOAD list, SYM_NODES
	sta label+1
	RETURN_OK
.endproc

;*******************************************************************************
; DUMP
; Dumps the symbol table to the open file.
; A 2-byte header (number of symbols) is stored first
; Named symbol records include definition lines and file IDs local to dbgi::dump's
; filename table. Anonymous symbols are not dumped.
.proc dump
@symname = r0
@symdata = r2
@cnt     = r4
	CALL FINAL_BANK_DEBUG, dbgi::preparefiles
	; write the number of symbols
	lda __label_num
	sta @cnt
	jsr krn::chrout
	lda __label_num+1
	sta @cnt+1
	jsr krn::chrout
	ora @cnt
	beq @done			; no symbols

	jsr setup_for_load_or_dump

@l0:	; write symbol data for this label
	ldy #$00
	SELECT_BANK "SYMBOLS"
@record:
	PAGE_LOAD @symdata, SYM_RECORDS
	cpy #LABEL_FILE
	bne :+
	CALL FINAL_BANK_DEBUG, dbgi::localfile
:
	cpy #LABEL_FLAGS
	bne @writebyte
	cmp #(SEG_FLOAT << 1)
	bne @writebyte
	lda #(SEG_FLOAT_PACKED << 1)
@writebyte:
	jsr krn::chrout
	iny
	cpy #SIZEOF_LABEL
	bcc @record

	ldy #LABEL_FLAGS
	PAGE_LOAD @symdata, SYM_RECORDS
	cmp #(SEG_FLOAT << 1)
	bne @name
	ldy #LABEL_ADDR
	PAGE_LOAD @symdata, SYM_RECORDS
	tax
	iny
	PAGE_LOAD @symdata, SYM_RECORDS
	tay
	CALL FINAL_BANK_EXPR, expr::fconst_write
	bcs @ret
@name:
	ldy #LABEL_NAME
	PAGE_LOAD @symdata, SYM_RECORDS
	sta @symname
	iny
	PAGE_LOAD @symdata, SYM_RECORDS
	sta @symname+1
	; write the symbol name
	ldy #$00
	; name references are decoded by the paging backend
:	PAGE_LOAD @symname, SYM_NAMES
	jsr krn::chrout
	iny
	cmp #$00
	bne :-

	jsr next_sym

	; decrement count and repeat until all labels are dumped
	lda @cnt
	bne :+
	dec @cnt+1
:	dec @cnt
	bne @l0
	lda @cnt+1
	bne @l0

@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; LOAD
; Loads the symbol table from the open file and rebuilds the hash map and
; sorted indexes for all loaded symbols.
; OUT:
;   - .C: set on error (corrupt or truncated symbol table)
.proc load
@symname = r0
@symdata = r2
@cnt     = r4
@idx     = r6
	jsr clr

	; load the number of symbols
	jsr @getb
	sta __label_num
	sta @cnt
	jsr @getb
	sta __label_num+1
	sta @cnt+1

	; validate the symbol count
	ldxy __label_num
	cmpw #MAX_LABELS
	bcc :+
	beq :+
	lda #ERR_TOO_MANY_LABELS
	jmp @error

:	iszero __label_num
	bne :+
	RETURN_OK		; no symbols

:	jsr setup_for_load_or_dump

@l0:	; load each symbol
	ldy #$00
	SELECT_BANK "SYMBOLS"
:	jsr @getb
	PAGE_STORE @symdata, SYM_RECORDS
	iny
	cpy #SIZEOF_LABEL
	bcc :-

	ldy #LABEL_FLAGS
	PAGE_LOAD @symdata, SYM_RECORDS
	and #$fe
	cmp #(SEG_FLOAT_PACKED << 1)
	bne @nameptr
	CALL FINAL_BANK_EXPR, expr::fconst_read
	jcs @error

	tya
	ldy #LABEL_ADDR+1
	PAGE_STORE @symdata, SYM_RECORDS
	dey
	txa
	PAGE_STORE @symdata, SYM_RECORDS
	ldy #LABEL_FLAGS
	lda #(SEG_FLOAT << 1)
	PAGE_STORE @symdata, SYM_RECORDS
@nameptr:
	ldxy name_top
	stxy @symname
	ldy #LABEL_NAME
	lda @symname
	PAGE_STORE @symdata, SYM_RECORDS
	iny
	lda @symname+1
	PAGE_STORE @symdata, SYM_RECORDS

	; Read a terminated name into shared scratch before reserving pool space.
	ldy #0
@readname:
	jsr @getb
	sta sympage::name_buffer,y
	beq @checkspace
	iny
	bne @readname
	lda #ERR_IO_ERROR	; unterminated byte-indexed name
	jmp @error
@checkspace:
	jsr check_name_space
	jcs @error
	ldy #0
@storename:
	lda sympage::name_buffer,y
	PAGE_STORE @symname, SYM_NAMES
	beq @nameend
	iny
	bne @storename
@nameend:
	jsr advance_name
	jsr next_sym

	lda @cnt
	bne :+
	dec @cnt+1
:	dec @cnt
	jne @l0
	lda @cnt+1
	jne @l0

	; rebuild the hash map and index arrays from the loaded symbols
	SELECT_BANK "SYMBOLS"
	lda #$00
	sta @cnt
	sta @cnt+1
@rebuild:
	ldxy @cnt
	cmpw __label_num
	beq @indexit		; all symbols rebuilt -> sort the indexes
	jsr loadlabel		; load label, hash, addr, and name pointers

	; @idx = label_addresses_sorted + id*2
	lda @cnt
	asl
	sta @idx
	lda @cnt+1
	rol
	sta @idx+1
	lda @idx
	adc #<label_addresses_sorted
	sta @idx
	lda @idx+1
	adc #>label_addresses_sorted
	sta @idx+1

	ldxy addr
	jsr @putidx		; label_addresses_sorted[id] = ADDR
	ldxy @cnt
	jsr @putidx		; label_addresses_sorted_ids[id] = id
	ldxy name
	jsr @putidx		; label_names_sorted[id] = NAME pointer
	ldxy @cnt
	jsr @putidx		; label_names_sorted_ids[id] = id

	; use the stored hash to append the label to its bucket's list
	ldxy hash
	jsr getlist
	jsr listappend

	incw @cnt
	jmp @rebuild

@indexit:
	jsr index		; sort the rebuilt indexes
	RETURN_OK

;-------------------------------------------------------------------------------
; read a byte from the file, checking for read errors/EOF
@getb:	jsr krn::readst
	bne @trunc
	jmp krn::chrin

@trunc:	pla			; unwind @getb's return address
	pla
	lda #ERR_IO_ERROR

@error:	pha
	SELECT_BANK "SYMBOLS"
	jsr clr			; don't leave a partially loaded symbol table
	pla
	sec
	rts

;-------------------------------------------------------------------------------
; write .XY to (@idx) then advance @idx to point to the next parallel array
@putidx:
	tya
	ldy #$01
	PAGE_STORE @idx, SYM_INDEXES
	txa
	dey
	PAGE_STORE @idx, SYM_INDEXES
	lda @idx
	clc
	adc #<(MAX_LABELS*2)
	sta @idx
	lda @idx+1
	adc #>(MAX_LABELS*2)
	sta @idx+1
	rts
.endproc

;*******************************************************************************
; NEXT SYM
; Advances r2 to the next fixed-size record
.proc next_sym
@symdata = r2
	lda @symdata
	clc
	adc #SIZEOF_LABEL
	sta @symdata
	bcc :+
	inc @symdata+1
:	rts
.endproc

;*******************************************************************************
; SETUP FOR LOAD OR DUMP
.proc setup_for_load_or_dump
@symname = r0
@symdata = r2
	ldxy #0
	stxy @symname
	ldxy #labels
	stxy @symdata
	rts
.endproc

;*******************************************************************************
; CMP NAME
; Compares the string in (str0) to the label name in (str2)
; Compare the complete stored name directly in the paged pool.
; IN:
;  temp:   one of the strings to compare
;  temp+2: packed word-offset reference of the other string
;  .A:       the max length to compare
; OUT:
;  -A: 0 if strings are equal
;  .Z: set if the strings are equal
.export cmp_name
.proc cmp_name
@name = temp
@other = temp+2
	tax
	beq @nomatch
	tay
	PAGE_LOAD @other, SYM_NAMES
	bne @nomatch		; stored name must end at the requested length
@loop:	dey
	PAGE_LOAD @other, SYM_NAMES
	cmp (@name),y
	bne @nomatch
	dex
	bne @loop
	lda #0
	rts
@nomatch:
	lda #$ff
	rts
.endproc
