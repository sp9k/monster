;*******************************************************************************
; LABELS.ASM
; This file defines procedures for creating and retrieving labels.
;
; -----------------------------------------------------------------------------
; LABELS OVERVIEW
; Labels map a text string to an address in memory.  They can be looked up
; by address or name.  They are stored in a sorted list to enable efficient
; alphabetic retrieval and are also indexed by address (value) to allow for
; efficient retrieval by address.
;
; Due to the size of labels and the data structures used to manage them,
; they have a more segmented memory map than most modules.
; Two logical banks are used:
; SYMBOLS:      anonymous labels, hash map for labels, label metadata, index of
;               labels by address/name, and general BSS
; SYMBOL NAMES: just the names of the symbols
;
; -----------------------------------------------------------------------------
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
;   address of symbol's name          (2 bytes)
;*******************************************************************************

;*******************************************************************************
; field offsets for LABEL
LABEL_FLAGS = 0
LABEL_HASH  = 1
LABEL_ADDR  = 3
LABEL_ID    = 5
LABEL_NAME  = 7

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
.include "config.inc"
.include "errors.inc"
.include "kernal.inc"
.include "ram.inc"
.include "macros.inc"
.include "target.inc"
.include "string.inc"
.include "zeropage.inc"

;*******************************************************************************
; ZEROPAGE
label   = zp::labels	; pointer to base label struct
name    = zp::labels+2	; pointer to NAME field for symbol
hash    = zp::labels+4	; precomputed hash value (2 bytes)
addr    = zp::labels+6	; ADDR field for active label
id      = zp::labels+8	; ID field for active label
flags   = zp::labels+$a	; FLAGS field for active symbol
list    = zp::labels+$b	; address to linked list of nodes for currenet bucket
listtop = zp::labels+$d	; address of free memory (next available list
bucket  = zp::labels+$f

temp    = zp::labels+$11	; temporary scratchpad

;*******************************************************************************
; CONSTANTS
MAX_ANON   = 1776	; max number of anonymous labels
MAX_LABELS = 728	; max number of named labels
SCOPE_LEN  = 8		; max len of namespace (scope)

; NOTE: BE CAREFUL CHANGING THIS
; BUCKETING LOGIC RELIES ON AN EXACT SIZE (BITS 8-11)
; INITIALIZATION ALSO RELIES ON PAGE ALIGNED SIZE (e.g. $1000)
NUM_BUCKETS = 2048	; number of buckets for the symbol hash map

MAX_LABEL_NAME_LEN = 32
MAX_SCOPES         = 4

SIZEOF_LABEL           = 9
SIZEOF_LABEL_LIST_NODE = 4

SEG_ABS = $ff

;*******************************************************************************
.export __label_clr
.export __label_add
.export __label_find
.export __label_by_addr
.export __label_by_id
.export __label_dump
.export __label_name_by_id
.export __label_isvalid
.export __label_get_name
.export __label_get_addr
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

;*******************************************************************************
; Label JUMP table
.macro LBLJUMP proc
	JUMP FINAL_BANK_SYMBOLS, proc
.endmacro

.RODATA

__label_clr:              LBLJUMP clr
__label_add:              LBLJUMP add
__label_find:             LBLJUMP find
__label_by_addr:          LBLJUMP by_addr
__label_by_id:            LBLJUMP by_id
__label_name_by_id:       LBLJUMP name_by_id
__label_isvalid:          LBLJUMP is_valid
__label_get_name:         LBLJUMP get_name
__label_get_addr:         LBLJUMP getaddr
__label_is_local:         LBLJUMP is_local
__label_set:              LBLJUMP set
__label_address:          LBLJUMP address
__label_address_by_id:    LBLJUMP address_by_id
__label_setscope:         LBLJUMP set_scope
__label_popscope:         LBLJUMP pop_scope
__label_addanon:          LBLJUMP add_anon
__label_get_fanon:        LBLJUMP get_fanon
__label_get_banon:        LBLJUMP get_banon
__label_index:            LBLJUMP index
__label_id_by_addr_index: LBLJUMP id_by_addr_index
__label_addrmode:         LBLJUMP addrmode
__label_get_segment:      LBLJUMP get_segment
__label_set_addr:         LBLJUMP setaddr
__label_dump:             LBLJUMP dump
__label_load:             LBLJUMP load

;*******************************************************************************
; LABEL NAMES
; Table of label names
.segment "LABELNAMES"
labelnames: .res MAX_LABELS*MAX_LABEL_NAME_LEN

.segment "LABEL_BSS"

;*******************************************************************************
; LABEL BUCKETS
; List of the linked-lists containing the symbol definitions
; The values in this list are the "head" nodes of the linked list for the bucket
.export label_buckets
label_buckets: .res NUM_BUCKETS*2

;*******************************************************************************
; LABEL NODES
; Nodes for the linked lists of each bucket.
; Each node contains a pointer to a LABEL (see labels) and a NEXT pointer to
; the next node in the list
.export label_nodes
label_nodes: .res MAX_LABELS*SIZEOF_LABEL_LIST_NODE

;*******************************************************************************
; LABELS
; List of all the label definitions (see LABEL STRUCT)
.export labels
labels: .res MAX_LABELS*SIZEOF_LABEL

;*******************************************************************************
; SCOPES
; Stack of "scopes". These are defined by a global (non-local) label definition
; And they end at the definition of another non-local symbol
scopes: .res 8*MAX_SCOPES		; scope stack buffer

;*******************************************************************************
; LABEL ADDRESSES INDEX
; Index tables to find labels by address
; The address of a given label id is label_addresses + (id * 2)
; Labels are also stored sorted by address in label_addresses_sorted.
; A corresponding array maps the sorted addresses to their ID.
;
; e.g. for the following labels:
;    | label |  id   |  address |
;    |-------|-------|----------|
;    |   A   |   1   |  $1003   |
;    |   B   |   2   |  $1009   |
;    |   C   |   3   |  $1000   |
;
; the sorted addresses will look like this:
;    | address_sorted| sorted_id |
;    |---------------|-----------|
;    |    $1000      |    3      |
;    |    $1003      |    1      |
;    |    $1009      |    2      |

.assert * & $01 = $00, error, "label_addresses_sorted must be word aligned"

;*******************************************************************************
; LABEL ADDRESSES SORTED
; Sorted array of addresses.
; Each element in this array corresponds to the element in the parallel
; "label_addresses" array (the id for the symbol with this address)
label_addresses_sorted:     .res MAX_LABELS*2
label_addresses_sorted_ids: .res MAX_LABELS*2

;*******************************************************************************
; LABEL NAMES INDEX
; Index table for names of each label to support by-name listing of labels
; TODO: unimplemented
.assert * & $01 = $00, error, "label_names_sorted must be word aligned"

.export label_names_sorted
label_names_sorted:     .res MAX_LABELS*2
label_names_sorted_ids: .res MAX_LABELS*2

;*******************************************************************************
; ANON ADDRS
; address table for each anonymous label
.export anon_addrs
anon_addrs: .res MAX_ANON*2

;*******************************************************************************
; VARS (shared RAM)
.segment "SHAREBSS"
labelvars:
.export __label_num
__label_num:
numlabels: .word 0		; total number of labels

.export __label_numanon
__label_numanon:
numanon: .word 0		; total number of anonymous labels

scopesp: .byte 0		; index of curent active scope's name
labelvars_size=*-labelvars

.segment "LABELS"

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
.proc set_scope
@scope  = temp
@scopes = temp+2
	; scopesp += SCOPE_LEN
	lda scopesp
	clc
	adc #SCOPE_LEN
	sta scopesp

	; get @scope-scopesp so that @scope+scopesp points to the start of the
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
	jsr isseparator
	beq @done
	STOREB_Y @scopes	; scopes+scopesp + Y
	iny
	dex
	bne :-
	rts			; maxed out scope len; don't terminate

@done:  lda #$00
	STOREB_Y @scopes	; terminate
	rts
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
@buff   = $100
@lbl    = temp
@scopes = temp+2
	stxy @lbl
	ldxy #scopes
	stxy @scopes

	ldx #$00
	ldy scopesp			; check if there is a scope defined
	bne @l0				; if so, continue
	RETURN_ERR ERR_NO_OPEN_SCOPE

@l0:	; write the scope to the buffer
	LOADB_Y @scopes
	beq :+
	sta @buff,x
	iny
	inx
	cpx #SCOPE_LEN
	bne @l0

:	; copy the label after the scope we just added to the buffer
	ldy #$00
@l1:	lda (@lbl),y
	jsr isseparator
	beq @done
	sta @buff,x
	iny
	inx
	cpx #MAX_LABEL_LEN
	bne @l1
	RETURN_OK			; maxed out scope buffer, return

@done:	lda #$00
	sta @buff,x
	ldxy #@buff
	RETURN_OK
.endproc

;*******************************************************************************
; CLR
; Removes all labels effectively resetting the label state
.proc clr
@map=r0
	; clear the hash map (linked lists of LABEL nodes)
	ldxy #label_buckets
	stxy @map

	ldy #$00
	sty numlabels
	sty numlabels+1
	sty scopesp
	tya

	ldx #>(NUM_BUCKETS*2)	; number of pages to clear
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
.proc find
@label = temp
	stxy @label		; save name to look for

	; check (and flag) if the label is local. if it is, we will start
	; searching at the end of the label table, where locals are stored
	jsr is_local
	beq @cont

	; if local, prepend the scope as the namespace
	ldxy @label
	jsr prepend_scope
	bcs @done		; return err

	stxy @label

@cont:	ldx numlabels
	bne @find
	ldy numlabels+1
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
	LOADB_Y label
	tax			; LSB in X
	iny
	LOADB_Y label
	tay			; MSB in Y

	;clc
@done:	rts
.endproc

;******************************************************************************
; ADDRMODE
; Returns the "address mode" for the label of the given ID
; IN:
;   - .XY: the ID of the label to get the address mode for
; OUT:
;   - .A: the address mode (0=ZP, 1=ABS)
.proc addrmode
	jsr loadlabel
	ldy #$00
	LOADB_Y label		; read the FLAGS byte
	and #$01		; mask bit 0 (mode)
	rts
.endproc

;******************************************************************************
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

;******************************************************************************
; SET ADDR
; Updates address related fields (FLAGS, ADDR) and indices with the new value
; for the label that is already loaded (via "loadlabel")
; IN:
;   - zp::label_value:     value to set the symbol's address to
;   - zp::label_segmentid: new value for label's SEGMENT ID
;   - zp::label_mode:      new value for label's MODE
.proc set_addr
@index = temp
	; overwrite the current MODE and SEGMENT
	; FLAGS = (SEG << 1) | MODE
	ldy #LABEL_FLAGS
	lda zp::label_segmentid
	asl
	ora zp::label_mode
	STOREB_Y label

	; overwrite the current value for the label with zp::value
	ldy #LABEL_ADDR
	lda zp::label_value
	STOREB_Y label
	iny
	lda zp::label_value+1
	STOREB_Y label

	; get location of label address in the sorted index
	lda id
	asl
	sta @index
	lda id+1
	rol
	sta @index+1
	lda @index
	adc #<label_addresses_sorted
	sta @index
	lda @index+1
	adc #>label_addresses_sorted
	sta @index+1

	; overwrite the index's value for the label
	ldy #$00
	lda zp::label_value
	STOREB_Y @index
	iny
	lda zp::label_value+1
	STOREB_Y @index

	; get location of label id in sorted index
	lda id
	asl
	sta @index
	lda id+1
	rol
	sta @index+1
	lda @index
	adc #<label_addresses_sorted_ids
	sta @index
	lda @index+1
	adc #>label_addresses_sorted_ids
	sta @index+1

	; write ID to label_addresses_sorted_ids
	lda id
	ldy #$00
	STOREB_Y @index
	lda id+1
	iny
	STOREB_Y @index

	RETURN_OK
.endproc

;******************************************************************************
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

;******************************************************************************
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

;******************************************************************************
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
@addr   = temp
@id     = r4
@name   = r6
@exists = r8
@allow_overwrite=r9
	sta @allow_overwrite	; set overwrite flag (SET) or clear (ADD)

	; make sure the name is valid
	stxy @name
	jsr is_valid
	bcs @ret		; return err

	; check if the label already exists
	lda #$00
	sty @exists
	ldxy @name
	jsr find
	stxy @id
	bcs @insert		; label doesn't exist -> continue to add it

	; label exists, if we are in SET mode overwrite, else return with error
	inc @exists
	lda @allow_overwrite
	bne @overwrite
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED

;------------------------------------------------------------------------------
@overwrite:
	; label exists, overwrite its old value
	jsr setaddr		; set the new value for the label
	clc			; ok
@ret:	rts

;------------------------------------------------------------------------------
@insert:
	; check if there's room for another label
	ldxy numlabels
	cmpw #MAX_LABELS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_LABELS
	rts

:	; check if label is local or not
	ldxy @name
	jsr is_local
	beq @cont

@local:	; if local, prepend the scope
	jsr prepend_scope
	bcs @ret		; return err
	stxy @name		; save the symbol name

;------------------------------------------------------------------------------
@cont:	; load the pointers for the label we are creating
	ldxy numlabels
	jsr loadlabel

	; write the symbol's data to the label structure
	; 1. write the hash value for the symbol
	ldxy @name
	jsr hash_name
	ldy #LABEL_HASH
	lda hash
	STOREB_Y label		; store LSB of hash
	iny
	lda hash+1
	STOREB_Y label		; store MSB of hash

	; 2. write the ID for the label (current number of labels)
	ldy #LABEL_ID
	lda numlabels
	sta @id
	STOREB_Y label		; store LSB of label's id
	iny
	lda numlabels+1
	sta @id+1
	STOREB_Y label		; store MSB of label's id

	; 3. set NAME for the label (string) and NAME field (pointer to it)
	ldxy @name
	stxy r0			; r0  = label name to set
	ldxy @id		; .XY = ID of label to (re)name
	jsr set_name		; set name pointer + string

	; 4. write all ADDR related fields (FLAGS, ADDR)
	jsr set_addr

	; 5. append pointer to the node we just built to its bucket's list
	ldxy hash
	jsr getlist		; load relevant list from the label's hash
	jsr listappend		; append node to that list

	incw numlabels		; success, increment symbol count

;------------------------------------------------------------------------------
@done:	RETURN_OK
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

:	lda #$00
	sta @src+1

	lda numanon
	asl			 ; *2
	rol @src+1
	adc #<anon_addrs
	sta @src
	lda #>anon_addrs
	adc @src+1
	sta @src+1

	jsr seek_anon
	stxy @dst
	cmpw @src
	beq @finish		; skip shift if this is the highest address

	; shift all the existing labels
@shift:	; src[i+2] = src[i]
	; src[i+3] = src[i+1]
	ldy #$00
	LOADB_Y @src	; LSB
	ldy #$02	; move up 2 bytes
	STOREB_Y @src
	dey
	LOADB_Y @src	; MSB
	ldy #$03	; move up 2 bytes
	STOREB_Y @src

	; src -= 2
	lda @src
	sec
	sbc #$02
	sta @src
	lda @src+1
	sbc #$00
	sta @src+1

	; check if src == dst
	cmp @dst+1
	bne @shift
	lda @src
	cmp @dst
	bne @shift	; loop til we have shifted all labels

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
	lda #$00
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
	stxy @addr
	sta @fcnt

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
	beq get_anon_retval	; if our count is 0, yes, end
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
	bpl @l0
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
	stxy @addr
	sta @bcnt

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
	beq get_anon_retval	; if our count is 0, yes, end
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
; GET ANON RETVAL
; Space saving helper to get the return address from r6
; IN:
;   - r6: address of value to return
; OUT:
;  - .XY: the nth anonymous label whose address is < than the given address
;  - .C:  clear to indicate success
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
@sec = temp
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
; ADDRESS
; Returns the address of the label in (.YX)
; The address mode of the label is returned as well.
; IN:
;  - .XY: the address of the label name to get the address of
; OUT:
;  - .XY: the address of the label
;  - .C:  is set if no label was found, clear if it was
;  - .A:  the size (address mode) of the label
;  - r2:  the ID of the label
.proc address
	jsr find		; get the id in YX
	bcs :-			; -> rts

	; fall through to address_by_id
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
;  - r2:  the ID of the label
.proc address_by_id
@tmp=zp::labels
	jsr getaddr		; get address
	sty @tmp
	ldy #LABEL_FLAGS
	LOADB_Y label		; and address mode
	and #$01		; mask MODE bit
	ldy @tmp
	rts
.endproc

;*******************************************************************************
; SET NAME
; Writes the NAME for the label
; IN:
;   - .XY: id of the label to (over)write the NAME of
;   - r0:  address to the string to write
.proc set_name
@name=r0
@addr=r2
	; get address of the label (id * MAX_LABEL_NAME_LEN)
	stxy @addr

	txa
	asl		; *2
	rol @addr+1
	asl		; *4
	rol @addr+1
	asl		; *8
	rol @addr+1
	asl		; *16
	rol @addr+1
	asl		; *32
	rol @addr+1
	adc #<labelnames
	sta @addr
	lda @addr+1
	adc #>labelnames
	sta @addr+1

	; set the NAME pointer to the address we're storing the NAME to
	ldy #LABEL_NAME
	lda @addr
	STOREB_Y label	; write pointer LSB
	iny
	lda @addr+1
	bne :+
:	STOREB_Y label	; write pointer MSB

	; switch to SYMBOL NAMES bank
	SELECT_BANK "SYMBOL_NAMES"

	; store the string (symbol name) data
	ldy #$00
@l0:	lda (@name),y
	jsr is_definition_separator
	bne :+
	lda #$00
:	STOREB_Y @addr
	cmp #$00
	beq @done
	iny
	cpy #MAX_LABEL_NAME_LEN
	bcc @l0

@done:	; switch back to main SYMBOLS bank
	SELECT_BANK "SYMBOLS"
	rts
.endproc

;*******************************************************************************
; IS LOCAL
; Returns with .Z set if the given label name represents a
; "local" label (begins with '@')
; IN:
;  - .XY: the label to test
; OUT:
;  - .A: nonzero if the label is local
;  - .Z: clear if label is local, set if not
.proc is_local
@l=zp::labels+7
	stxy @l
	ldy #$00
	lda (@l),y
	ldy @l+1
	cmp #'@'
	bne :+
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

	ldy #LABEL_ADDR		; offset to ADDR
	LOADB_Y label
	tax
	iny
	LOADB_Y label
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
@addr = ra
@lb   = rc
@ub   = re
@m    = zp::tmp10
@top  = zp::tmp12
	stxy @addr

	lda numlabels
	asl
	sta @ub
	lda numlabels+1
	rol
	sta @ub+1

	; @lb = label_addresses_sorted
	; @ub = label_addresses_sorted + (numlabels*2)
	lda #<label_addresses_sorted
	sta @lb
	adc @ub
	sta @ub
	sta @top
	lda #>label_addresses_sorted
	sta @lb+1
	adc @ub+1
	sta @ub+1
	sta @top+1

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
	and #$02	; align to element size
	adc @lb		; mid = low + ((high - low) / 2)
	sta @m
	tya
	adc @lb+1
	sta @m+1
	ldy #$01		; load index to MSB
	LOADB_Y @m
	cmp @addr+1
	beq @chklsb
	bcs @modhigh		; A[mid] > value

@modlow:
	; A[mid] < value
	lda @m		; low = mid + element size
	adc #2-1	; carry always set
	sta @lb
	lda @m+1
	adc #$00
	sta @lb+1
	jmp @loop

@chklsb:
	dey		; set index to LSB (0)
	LOADB_Y @m	; compare LSB
	cmp @addr	; load target value LSB
	beq @done
	bcc @modlow	; A[mid] < value

@modhigh:		; A[mid] > value
	lda @m		; high = mid - element size
	;clc
	sbc #2-1	; carry always clear
	sta @ub
	lda @m+1
	sbc #$00
	sta @ub+1
	jmp @loop

@done:	bcc @err

@ok:	; look up the ID for the address
	lda @m
	clc
	adc #<(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m

	lda @m+1
	adc #>(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m+1

	ldy #$00
	LOADB_Y @m
	tax
	iny
	LOADB_Y @m
	tay
	RETURN_OK

@err:	ldxy @ub	; get the lower bound of where our search ended
	stxy @m		; and set our result variable to it (ub < lb here)
	jsr @ok		; get the closest label
	cmpw numlabels	; was the result a valid label?
	bcc :+		; if so, continue to return

	; if label wasn't valid, get the highest label by address
	lda @top
	;sec
	sbc #$02
	sta @m
	lda @top+1
	sbc #$00
	sta @m+1
	jsr @ok

:	sec
	rts
.endproc

;******************************************************************************
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
	LOADB_Y @tmp
	tax
	iny
	LOADB_Y @tmp
	tay
	rts
.endproc

;*******************************************************************************
; NAME BY ID
; Returns the address name of the label ID requested
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XA: the address of the name for the given label id
.proc name_by_id
@addr = temp
	sty @addr
	txa
	asl		; *2
	rol @addr
	asl		; *4
	rol @addr
	asl		; *8
	rol @addr
	asl		; *16
	rol @addr
	asl		; *32
	rol @addr

	adc #<labelnames
	tax
	lda @addr
	;clc
	adc #>labelnames
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
@name = r4
	stxy @name
	ldy #$00

; first character must be a letter or '@'
@l0:	lda (@name),y
	iny
	jsr iswhitespace
	beq @l0

	; check first non whitespace char
	cmp #'@'
	beq @cont
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

	;jsr getopcode	; opcodes are not valid labels
	;bcs @cont
	;sec
	;rts

	; following characters must be between '0' and 'Z'
@cont:	ldx #$00
@l1:	inx
	cpx #(MAX_LABEL_LEN/2)+1
	bcs @toolong
	lda (@name),y
	jsr isseparator
	beq @done
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l1
@err:	RETURN_ERR ERR_ILLEGAL_LABEL
@toolong:
	lda #ERR_LABEL_TOO_LONG
	;sec
	rts
@done:	RETURN_OK
.endproc

;*******************************************************************************
; GET NAME
; Copies the name of the label ID given to the provided buffer
; IN:
;  - .XY: the ID of the label to get the name of
;  - r0:  the address to copy to
; OUT:
;  - (r0): the label name
;  - .Y:   the length of the copied label
.proc get_name
@dst = r0
@src = temp
	jsr loadlabel

	; switch to SYMBOL NAMES bank
	SELECT_BANK "SYMBOL_NAMES"

	; write the symbol name to its destination
	ldy #$00
@l0:	LOADB_Y name
	sta (@dst),y
	tax			; 0?
	beq @done		; if so, we're done
	iny
	cpy #MAX_LABEL_LEN
	bcc @l0

	; terminate the (max length) buffer
	lda #$00
	sta (@dst),y

	; switch back to main SYMBOLS bank
	SELECT_BANK "SYMBOLS"

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

	ldy #LABEL_ADDR
	LOADB_Y label
	tax
	iny
	LOADB_Y label
	tay
	rts
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
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.', ',', ':',0
@numops = *-@ops
.endproc

;*******************************************************************************
; MACROS
; These macros are used by sort_by_addr

;*******************************************************************************
; SETPTRS
; update @idi and @idj based on the values of @i and @j
; these pointers are offset by a fixed amount from @i and @j
.macro setptrs
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
.endmacro

;*******************************************************************************
; INDEX
; Updates the by-address sorting of the labels. This allows labels to be looked
; up by their address (see lbl::by_addr).
;
; Code adapted from code by Vladimir Lidovski aka litwr (with help of BigEd)
; via codebase64.org
.proc index
@i   = r0
@j   = r2
@x   = r4
@ub  = r6
@lb  = r8
@tmp = ra
@num = rc
@idi = zp::tmp10
@idj = zp::tmp12
@sp  = zp::tmp14
	lda numlabels
	ora numlabels+1
	bne @setup
	rts			; nothing to index

@setup:	; @num = 2*(numlabels-1)
	lda numlabels
	sec
	sbc #$01
	sta @num
	lda numlabels+1
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
	lda #<label_addresses_sorted
	clc
	adc @num
	sta @ub
	lda #>label_addresses_sorted
	adc @num+1
	sta @ub+1

	lda #>label_addresses_sorted
	sta @lb+1
	lda #<label_addresses_sorted
	sta @lb

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

	; @x = array[(@j+@i) / 2]
	ldy #$00
	LOADB_Y @tmp
	sta @x
	iny
	LOADB_Y @tmp
	sta @x+1

@qsloop1:
	; while (array[i] > @x) { inc @i }
	ldy #$00		; compare array[i] and x
	LOADB_Y @i
	cmp @x
	iny
	LOADB_Y @i
	sbc @x+1
	bcs @qs_l1
	lda #$02	; move @i to next element
	adc @i
	sta @i
	bcc @qsloop1
	inc @i+1
	bne @qsloop1	; branch always

@qs_l1:	ldy #$00	; compare array[j] and x
	LOADB_Y @j
	sta @tmp
	iny
	LOADB_Y @j
	sta @tmp+1
	lda @x
	cmp @tmp
	lda @x+1
	sbc @tmp+1
	bcs @qs_l3

	lda @j
	sec
	sbc #$02	; move @j to prev element
	sta @j
	bcs @qs_l1
	dec @j+1
	bne @qs_l1	; branch always

@qs_l3:
	lda @j		; compare i and j
	cmp @i
	lda @j+1
	sbc @i+1
	bcc @qs_l8

@qs_l6:	setptrs
	SWAPB_Y @i, @j		; swap array[@i] and array[@j]
	SWAPB_Y @idi, @idj	; swap ids[@i] and ids[@j]

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
	sty label+1
	stx @tmp
	txa
	asl			; *2
	rol label+1
	asl			; *4
	rol label+1
	asl			; *8
	rol label+1
	adc @tmp		; *9
	bcc :+
	inc label+1
	clc

:	; add offset to labels data
	adc #<labels
	sta label
	lda label+1
	adc #>labels
	sta label+1

	; load the FLAGS, HASH, ID, and ADDR
	; load FLAGS
	ldy #$00
	LOADB_Y label
	sta flags

	; load HASH
	iny
	LOADB_Y label
	sta hash
	iny
	LOADB_Y label
	sta hash+1

	; load ADDR
	iny
	LOADB_Y label
	sta addr
	iny
	LOADB_Y label
	sta addr+1

	; load ID
	; TODO: should already have this as we passed it to this proc.
	iny
	LOADB_Y label
	sta id
	iny
	LOADB_Y label
	sta id+1

	; load NAME pointer
	iny
	LOADB_Y label
	sta name
	iny
	LOADB_Y label
	sta name+1

	rts
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

	clc
@l0:	rol hash
	rol hash+1
	lda (@name),y
	jsr isseparator
	beq @done
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
	LOADB_Y list		; get LSB of NEXT pointer
	sta @tmp
	tax
	iny
	LOADB_Y list		; get MSB
	ora @tmp		; is NEXT pointer value $0000?
	beq @end		; if so, we're at the end of the list

	; update list pointer to next node
	LOADB_Y list
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
@nodes = r2
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
	STOREB_Y list
	iny
	lda listtop+1
	STOREB_Y list

@set_node:
	; write the data for this new node (label pointer)
	ldy #LIST_LABEL
	lda label
	STOREB_Y listtop
	iny
	lda label+1
	STOREB_Y listtop

	; set NEXT pointer for new node to 0 (TAIL)
	ldy #LIST_NEXT
	lda #$00
	STOREB_Y listtop
	iny
	STOREB_Y listtop

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
@name  = zp::str0
@other = zp::str2
	; check if the list exists
	iszero list
	beq @notfound

	stxy @name

	; get the length to compare
	ldy #$ff		; -1
:	iny
	bmi @notfound
	lda (@name),y
	jsr isseparator
	beq @cont
	bne :-
@cont:	sty @len

@l0:	; get address of the label data for this node
	ldy #LIST_LABEL
	LOADB_Y list
	sta @sym
	iny
	LOADB_Y list
	sta @sym+1
	ora @sym
	beq @notfound	; if LABEL address is $0000, label doesn't exist in list

	; first: does the HASH match? if not, don't bother comparing the NAME
	ldy #LABEL_HASH
	LOADB_Y @sym	; LSB of symbol's hash
	cmp hash
	bne @next
	iny
	LOADB_Y @sym	; MSB of symbol's hash
	cmp hash+1
	bne @next

	; hash matches, does the NAME match?
	ldy #LABEL_NAME
	LOADB_Y @sym
	sta @other
	iny
	LOADB_Y @sym
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
	LOADB_Y list
	sta label
	iny
	LOADB_Y list
	sta label+1
	RETURN_OK
.endproc

;*******************************************************************************
; DUMP
; Dumps the symbol table to the open file.
; A 2-byte header (number of symbols) is stored first
; NOTE: anonymous symbols and symbol metadata (mode, etc.) is not dumped
.proc dump
@sym  = r0
@cnt  = r2
@addr = r4
	; write the number of symbols
	lda numlabels
	sta @cnt
	jsr krn::chrout
	lda numlabels+1
	sta @cnt+1
	jsr krn::chrout
	ora @cnt
	beq @done			; no symbols

	jsr setup_for_load_or_dump

	; write each symbol
@l0:	ldy #$00

	; write the symbol name
@l1:	LOADB_Y @sym
	jsr krn::chrout
	iny
	cmp #$00
	bne @l1

	; write the address
	ldy #$00
	LOADB_Y @addr
	jsr krn::chrout
	incw @addr
	LOADB_Y @addr
	jsr krn::chrout
	incw @addr

	jsr next_sym

	lda @cnt
	bne :+
	dec @cnt+1
:	dec @cnt
	bne @l0
	lda @cnt+1
	bne @l0

@done:	rts
.endproc

;*******************************************************************************
; LOAD
; Loads the symbol table from the open file.
.proc load
@sym  = r0
@cnt  = r2
@addr = r4
	; load the number of symbols
	jsr krn::chrin
	sta numlabels
	sta @cnt
	jsr krn::chrin
	sta numlabels+1
	sta @cnt+1

	ora @cnt
	beq @done			; no symbols

	jsr setup_for_load_or_dump

@l0:	; load each symbol
	ldy #$00

@l1:	; load the name
	jsr krn::chrin
	STOREB_Y @sym
	iny
	cmp #$00
	bne @l1

	; load the address
	jsr krn::chrin
	tax
	jsr krn::chrin
	tay
	STOREW @addr
	incw @addr
	incw @addr

	jsr next_sym

	lda @cnt
	bne :+
	dec @cnt+1
:	dec @cnt
	bne @l0
	lda @cnt+1
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; NEXT SYM
; Advances the "@sym" pointer to the next symbol (label)
; OUT:
;   r0: increased by MAX_LABEL_NAME_LEN
.proc next_sym
@sym = r0
	lda @sym
	clc
	adc #MAX_LABEL_NAME_LEN
	sta @sym
	bcc :+
	inc @sym+1
:	rts
.endproc

;*******************************************************************************
; SETUP FOR LOAD OR DUMP
.proc setup_for_load_or_dump
@sym  = r0
@addr = r4
	ldxy #labels
	stxy @sym
	ldxy #label_addresses_sorted
	stxy @addr
	rts
.endproc

;*******************************************************************************
; CMP NAME
; Compares the string in (str0) to the label name in (str2)
; The label name is assumed to be in the SYMBOL NAMES logical bank
; IN:
;  zp::str0: one of the strings to compare
;  zp::str2: the other string to compare (in SYMBOL NAMES)
;  .A:       the max length to compare
; OUT:
;  -A: 0 if strings are equal
;  .Z: set if the strings are equal
.export cmp_name
.proc cmp_name
	tay		; is length to compare 0?
	beq @match	; if 0-length comparison, it's a match by default

	; activate the SYMBOL NAMES bank for comparison
	SELECT_BANK "SYMBOL_NAMES"

@l0:	dey
	bmi @match
	LOADB_Y zp::str2	; get byte of the label to compare
	cmp (zp::str0),y	; compare with our name
	beq @l0

@nomatch:
	lda #$ff
	skw
@match:	lda #$00

@ret:	pha
	; restore SYMBOLS bank
	SELECT_BANK "SYMBOLS"
	pla
	rts
.endproc
