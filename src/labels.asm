;*******************************************************************************
; LABELS.ASM
; This file defines procedures for creating and retrieving labels.
; Labels map a text string to an address in memory.  They can be looked up
; by address or name.  They are stored in a sorted list to enable efficient
; alphabetic retrieval and are also indexed by address (value) to allow for
; efficient retrieval by address.
;*******************************************************************************

.include "config.inc"
.include "errors.inc"
.include "kernal.inc"
.include "ram.inc"
.include "macros.inc"
.include "target.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
MAX_ANON   = 650	; max number of anonymous labels
SCOPE_LEN  = 8		; max len of namespace (scope)
MAX_LABELS = 732

MAX_LABEL_NAME_LEN = 32
MAX_SCOPES = 4

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

.ifndef vic20

;*******************************************************************************
; Flat memory procedure mappings
__label_clr              = clr
__label_add		 = add
__label_find             = find
__label_by_addr          = by_addr
__label_by_id            = by_id
__label_dump             = dump
__label_name_by_id       = name_by_id
__label_isvalid          = is_valid
__label_get_name         = get_name
__label_load             = load
__label_get_addr         = getaddr
__label_get_segment      = get_segment
__label_is_local         = is_local
__label_set              = set
__label_address          = address
__label_address_by_id    = address_by_id
__label_setscope         = set_scope
__label_popscope         = pop_scope
__label_addanon          = add_anon
__label_get_fanon        = get_fanon
__label_get_banon        = get_banon
__label_index            = index
__label_id_by_addr_index = id_by_addr_index
__label_addrmode         = addrmode
__label_set_addr         = setaddr

.else
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

.endif

;*******************************************************************************
; LABELS
; Table of label names. Each entry corresponds to an entry in label_addresses,
; which contains the value (address) for the label name.
.segment "LABELNAMES"
.export labels
.ifdef ultimem
; The Ultimem can actually hold more labels than this, but we only bank in
; $2000 bytes at a time
labels: .res $2000
.segment "LABEL_BSS"
.else
labels: .res MAX_LABELS*MAX_LABEL_NAME_LEN
.endif

;*******************************************************************************
; SEGMENT IDS
; These bytes correspond to each label and tell us which segment it is defined
; within
; $ff means that the label is absolute (not relative to any segment)
.export segment_ids
segment_ids: .res MAX_LABELS

.segment "SHAREBSS"

;*******************************************************************************
labelvars:
.export __label_num
__label_num:
numlabels: .word 0		; total number of labels

.export __label_numanon
__label_numanon:
numanon: .word 0		; total number of anonymous labels

scopesp: .byte 0		; index of curent active scope's name
labelvars_size=*-labelvars

;*******************************************************************************
; LABEL MODES
; This bit array contains the size of each label
; Bit 7 of the first byte corresponds to label ID 0, bit 6 to the label ID 1,
; and so on
.segment "LABELMODES"
.export label_modes
label_modes: .res MAX_LABELS / 8	; modes (0=absolute, 1=zeropage)

scopes: .res 8*MAX_SCOPES		; scope stack buffer

.segment "LABEL_BSS"

;*******************************************************************************
; LABEL ADDRESSES
; Table of addresses for each label
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
.export label_addresses
label_addresses: .res MAX_LABELS*2

.assert * & $01 = $00, error, "label_addresses_sorted must be word aligned"

.export label_addresses_sorted
label_addresses_sorted:     .res MAX_LABELS*2
label_addresses_sorted_ids: .res MAX_LABELS*2

; address table for each anonymous label
.export anon_addrs
anon_addrs: .res MAX_ANON*2

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
@scope  = zp::labels
@scopes = zp::labels+2
	SELECT_BANK "SYMBOLS"

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
@buff=$100
@lbl=zp::labels
@scopes=zp::labels+2
	SELECT_BANK "SYMBOLS"

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
	lda #$00
	sta scopesp
	ldx #labelvars_size
:	sta labelvars-1,x
	dex
	bne :-
	rts
.endproc

;*******************************************************************************
; FIND
; Looks for the ID corresponding to the given label and returns it.
; in:
;  - .XY: the name of the label to look for
; out:
;  - .C: set if label is not found
;  - .A: contains the length of the label (why not) or error code
;  - .XY: the id of the label or the id where the label WOULD be if not found
.proc find
@cnt=zp::labels+1
@search=zp::labels+3
@label=zp::labels+5
@tmp=r0
	SELECT_BANK "SYMBOLS"
	stxy @label

	; check (and flag) if the label is local. if it is, we will start
	; searching at the end of the label table, where locals are stored
	jsr is_local
	beq @cont

	; if local, prepend the scope as the namespace
	ldxy @label
	jsr prepend_scope
	bcc :+
	rts		; return err
:	stxy @label

@cont:	ldx numlabels
	bne :+
	ldy numlabels+1
	bne :+
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

:	lda #$00
	sta @cnt
	sta @cnt+1
	ldxy #labels
	stxy @search

@seek:	ldy #$00
@l0:	LOADB_Y @search
	sta @tmp
	lda (@label),y
	jsr isseparator
	beq @chkend

	cmp @tmp
	beq @chmatch

	; labels are alphabetical, if our label is not alphabetically greater,
	; we're done
	bcc @notfound

	; if our label IS greater alphabetically, try the next label
	bne @next

@chmatch:
	iny
	cpy #MAX_LABEL_LEN-1
	bcs @found
	bcc @l0
@chkend:
	; make sure string matched is 0-terminated
	lda @tmp
	beq @found

@next:
	lda @search
	clc
	adc #MAX_LABEL_LEN
	sta @search
	bcc :+
	inc @search+1
:
.ifdef ultimem
	lda @search+1
	bpl :+
	; move to the next bank
	incw $9ffc
:
.endif
	incw @cnt
	ldxy @cnt
	cmpw numlabels
	bne @seek

@notfound:
	ldxy @cnt
	RETURN_ERR ERR_LABEL_UNDEFINED

@found:	tya
	ldxy @cnt
	RETURN_OK
.endproc

;******************************************************************************
; ADDRMODE
; Returns the "address mode" for the label of the given ID
; IN:
;   - .XY: the ID of the label to get the address mode for
; OUT:
;   - .A: the address mode (0=ZP, 1=ABS)
.proc addrmode
@tmp=zp::labels
	SELECT_BANK "SYMBOLS"
	sty @tmp		; save MSB
	txa			; .A=LSB
	pha
	ldx @tmp		; .X=MSB

	jsr div8		; .X = byte offset
	pla			; restore LSB
	and #$07
	tay			; .Y = bit offset
	lda $8268,y		; \
	ldy #$00
	and label_modes,x
	beq :+
	iny
:	tya
	rts
.endproc

;******************************************************************************
; SETADDR
; Overwrites the address of the label with the given ID.
; IN:
;   - .XY:       the ID of the symbol to update the address of
;   - zp::value: the value to set the symbol's address to
.proc setaddr
@addr=zp::labels
	SELECT_BANK "SYMBOLS"
	jsr by_id 		; get the address of the label

	; overwrite the current value for the label with zp::value
	ldy #$00
	lda zp::label_value
	STOREB_Y @addr
	iny
	lda zp::label_value+1
	STOREB_Y @addr
	rts
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
@addr=zp::labels
@id=r0
@label=r2
@name=r4
@src=r6
@dst=r8
@cnt=ra
@mode=r8
@tmp=ra
@segid=re
@allow_overwrite=rf
	SELECT_BANK "SYMBOLS"

	sta @allow_overwrite	; set overwrite flag (SET) or clear (ADD)
	stxy @name
	jsr is_valid
	bcs @ret		; return err

@seek:	; get the label length
	ldy #$00
:	lda (@name),y
	jsr isseparator
	beq @lenfound
	iny
	bne :-

@lenfound:
	ldxy @name
	jsr find
	bcs @insert

	lda @allow_overwrite
	bne :+
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED

:	; label exists, overwrite its old value
	jsr by_id 		; get the address of the label
	ldxy zp::label_value
	STOREW @addr
	clc			; ok
@ret:	rts

@insert:
	; @id is the index where the new label will live
	stxy @id

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
	beq @shift

	; if local, prepend the scope
	jsr prepend_scope
	bcs @ret		; return err
	stxy @name

;------------------
; open a space for the new label by shifting everything left
@shift:
	; get top of segment ids (segment_ids+numlabels-1)
	lda #<segment_ids
	clc
	adc numlabels
	sta @segid
	lda #>segment_ids
	adc numlabels+1
	sta @segid+1
	decw @segid		; -1

	; get address where last label WILL go (numlabels * MAX_LABEL_LEN)
	ldxy numlabels
	jsr name_by_id
	stx @src
	sta @src+1
	stx @dst
	sta @dst+1

	; addr = label_addresses+(numlabels-1)*2
	lda numlabels+1
	sta @addr+1
	lda numlabels
	asl
	rol @addr+1
	adc #<label_addresses
	sta @addr
	lda @addr+1
	adc #>label_addresses
	sta @addr+1

	iszero numlabels
	bne :+
	jmp @insert_mode

:	; src -= MAX_LABEL_LEN
	lda @src
	sec
	sbc #MAX_LABEL_LEN
	sta @src
	bcs :+
	dec @src+1

:	; addr -= 2
	lda @addr
	sec
	sbc #$02
	sta @addr
	bcs :+
	dec @addr+1

:	; cnt = numlabels-id
	lda numlabels
	sec
	sbc @id
	sta @cnt
	lda numlabels+1
	sbc @id+1
	sta @cnt+1
	ora @cnt
	bne @sh0	; if (numlabels-id) > 0, skip ahead to shift

	; (numlabels-id) == 0, no shift needed
	ldxy @dst
	stxy @src
	lda @addr
	clc
	adc #$02
	sta @addr
	bcc :+
	inc @addr+1
:	jmp @insert_mode

@sh0:	; copy the label (MAX_LABEL_LEN bytes) to the SYMBOL bank
	ldy #MAX_LABEL_LEN-1
	COPY_Y @src, @dst

; shift segment id
	ldy #$00
	LOADB_Y @segid
	iny			; .Y=1
	STOREB_Y @segid
	dey			; .Y=0

; shift address
	LOADB_Y @addr
	ldy #$02
	STOREB_Y @addr
	dey
	LOADB_Y @addr
	ldy #$03
	STOREB_Y @addr

	; segid--
	decw @segid

	decw @cnt
	iszero @cnt
	beq @insert_mode

; update all pointers

	; @addr -= 2
	lda @addr
	sec
	sbc #$02
	sta @addr
	bcs :+
	dec @addr+1

:	; @src-= MAX_LABEL_LEN
	lda @src
	sec
	sbc #MAX_LABEL_LEN
	sta @src
	bcs :+
	dec @src+1

:	; @dst -= MAX_LABEL_LEN
	lda @dst
	sec
	sbc #MAX_LABEL_LEN
	sta @dst
	bcs @sh0
	dec @dst+1
	bne @sh0		; branch always

@insert_mode:
	; (id / 8) is the byte containing the mode we inserted
	lda @id
	ldx @id+1
	jsr div8	; .X=mode byte (assuming < 256*8 labels)

	lda @id
	and #$07	; get bit (from left) to insert
	tay

	; get mask of bits to shift
	; $82f8 %11111111
	; $82f9 %01111111
	; $82fa %00111111
	; $82fb %00011111
	; ...
	lda label_modes,x
	pha			; save current modes for the byte
	and $82f8,y		; mask bits we need to shift
	lsr			; shift the bits we need to shift
	sta @tmp		; and save as temp result

	; get the mask of bits to leave alone
	; $86f8 %00000000
	; $82f9 %10000000
	; $82fa %11000000
	; $82fb %11100000
	; ...
	lda zp::label_mode
	lsr			; set .C if mode is ABS
	pla			; restore mode byte to modify
	pha			; save again
	and $86f8,y		; mask bits that were not shifted
	ora @tmp		; OR with bits we shifted

	bcc :+			; zeropage - leave bit as 0

	; $8268 %10000000
	; $8269 %01000000
	; $826a %00100000
	; $826b %00010000
	; ...
	ora $8268,y		; set ABS bit for this label

:	sta label_modes,x	; save result
	jsr numlabels_div8	; get stopping point (numlabels / 8)
	sta @tmp
	pla			; restore original byte
	cpx @tmp		; if index is at last byte
	beq @storelabel		; no need to shift

:	; now shift the rest of the mode bytes right
	lsr			; set .C from .A
	inx
	ror label_modes,x
	rol			; bring .C into .A
	cpx @tmp
	bne :-

;------------------
; insert the label into the new opening
@storelabel:
	ldy #$00
	; write the label
:	lda (@name),y
	beq @storeaddr
	jsr is_definition_separator
	beq @storeaddr

	; copy a byte to the label name
	STOREB_Y @src

	iny
	cpy #MAX_LABEL_LEN
	bcc :-

@storeaddr:
	; 0-terminate the label name and write the label value
	lda #$00
	STOREB_Y @src

	tay			; .Y=0
	lda zp::label_value
	STOREB_Y @addr		; store LSB of value
	lda zp::label_value+1
	iny			; .Y=1
	STOREB_Y @addr		; store MSB of value

; store the segment ID for the label ($ff if absolute)
@store_segid:
	; ldy #$00
	lda zp::label_segmentid
	STOREB_Y @segid

	incw numlabels
	ldxy @id
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
	SELECT_BANK "SYMBOLS"
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
	SELECT_BANK "SYMBOLS"
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
	SELECT_BANK "SYMBOLS"
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
	SELECT_BANK "SYMBOLS"
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
	SELECT_BANK "SYMBOLS"
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
; Returns the segment ID for the given label ID
; IN:
;  - .XY: the label ID to get the segment for
; OUT:
;  - .A: the segment ID for the label
.proc get_segment
@sec=zp::labels
	SELECT_BANK "SYMBOLS"
	txa
	clc
	adc #<segment_ids
	sta @sec
	tya
	adc #>segment_ids
	sta @sec+1
	LOADB @sec
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
@table=zp::labels+2
@id=zp::labels+4
@addr=zp::labels+6
@mode=zp::labels+8
	SELECT_BANK "SYMBOLS"
	stxy @id
	jsr by_id	; get address of label
	stxy @table

	ldy #$00
	LOADB_Y @table
	pha		; save address LSB
	iny
	LOADB_Y @table
	pha		; save address MSB

	; get the size of the label from its address mode
	ldxy @id
	jsr addrmode
	sta @mode

	; restore address
	pla
	tay
	pla
	tax
	lda @mode
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
; Returns the address of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY:        the address of the given label id
;  - zp::labels: the address of the label (same as .XY)
.proc by_id
@addr=zp::labels
	txa
	asl
	sta @addr
	tya
	rol
	sta @addr+1
	lda @addr
	adc #<label_addresses
	sta @addr
	tax
	lda @addr+1
	adc #>label_addresses
	sta @addr+1
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
@addr=ra
@lb=rc
@ub=re
@m=zp::tmp10
@top=zp::tmp12
	SELECT_BANK "SYMBOLS"
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

;*******************************************************************************
; NUMLABELS DIV8
; Returns the number of labels divided by 8 (assumes result is 8 bit)
; OUT:
;   - .A: the number of labels / 8
.proc numlabels_div8
@tmp=r9
	lda numlabels+1
	sta @tmp
	lda numlabels
	lsr @tmp	; /2
	ror
	lsr @tmp	; /4
	ror
	lsr @tmp	; /8
	ror
	rts
.endproc

;*******************************************************************************
; DIV8
; Returns the given number divided by 8. Assumes an 8 bit quotient
; IN:
;   - .AX: the number to divide by 8
; OUT:
;   - .A: the result
;   - .X: the result
.proc div8
@tmp=r9
	stx @tmp
	lsr @tmp	; /2
	ror
	lsr @tmp	; /4
	ror
	lsr @tmp	; /8
	ror
	tax
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
@tmp=rc
	SELECT_BANK "SYMBOLS"
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
@addr=zp::labels
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

.ifdef ultimem
	; get the bank of the symbol (%$2000)
	pha

	; % $2000
	lda @addr+1
:	cmp #$80	; > end of of BLK3?
	bcc :+
	sbc #$20
	lda #$60	; start of BLK3
	sta @addr+1
	inc $9ffc	; next bank
	bne :-

:	pla
.endif

	adc #<labels
	tax
	lda @addr
	;clc
	adc #>labels
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
@name=r4
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

	;jsr getopcode	; make sure string is not an opcode
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
@dst=r0
@src=zp::labels
	SELECT_BANK "SYMBOLS"
	jsr name_by_id
	stx @src
	sta @src+1

	ldy #$00
@l0:	LOADB_Y @src
	sta (@dst),y
	beq @done
	iny
	cpy #MAX_LABEL_LEN
	bcc @l0

	lda #$00
	STOREB_Y @dst

@done:	rts
.endproc

;*******************************************************************************
; GET ADDR
; Returns the address of the given label ID.
; IN:
;  - .XY: the ID of the label to get the address of
; OUT:
;  - .XY: the address of the label
.proc getaddr
@src=zp::labels
	SELECT_BANK "SYMBOLS"
	jsr by_id
	stxy @src

	LOADW @src
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
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.', ',', ':'
	.byte $00
@numops = *-@ops
.endproc

;*******************************************************************************
; MACROS
; These macros are used by sort_by_addr

;*******************************************************************************
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
; copies the unsorted addresses to the sorted addresses array and initializes
; the unsorted ids array
.macro setup
@cnt=r0
@src=r2
@dst=r4
@id=r0
	; @cnt = numlabels
	lda numlabels
	sta @cnt
	lda numlabels+1
	sta @cnt+1

	ldxy #label_addresses
	stxy @src
	ldxy #label_addresses_sorted
	stxy @dst

	; copy the addresses
	ldy #$00
@l0:	LOADB_Y @src
	STOREB_Y @dst
	iny
	LOADB_Y @src
	STOREB_Y @dst
	iny
	bne :+
	inc @src+1	; next page
	inc @dst+1

:	decw @cnt
	bne @l0
	lda @cnt+1
	bne @l0

@cont:	; init the unsorted ids array to the pattern 0, 1, 2, 3, ...
	ldxy #label_addresses_sorted_ids
	stxy @dst

	ldy #$00
	sty @id
	sty @id+1

@idloop:
	lda @id
	STOREB_Y @dst
	iny
	lda @id+1
	STOREB_Y @dst
	iny
	bne :+
	inc @dst+1	; next page

:	incw @id
	lda @id
	cmp numlabels
	bne @idloop
	lda @id+1
	cmp numlabels+1
	bne @idloop
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
	SELECT_BANK "SYMBOLS"

	lda numlabels
	ora numlabels+1
	bne @setup
	rts			; nothing to index

@setup:	setup

	; @num = 2*(numlabels-1)
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
; DUMP
; Dumps the symbol table to the open file.
; A 2-byte header (number of symbols) is stored first
; NOTE: anonymous symbols and symbol metadata (mode, etc.) is not dumped
.proc dump
@sym=r0
@cnt=r2
@addr=r4
	SELECT_BANK "SYMBOLS"
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
@sym=r0
@cnt=r2
@addr=r4
	SELECT_BANK "SYMBOLS"
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
@sym=r0
	lda @sym
	clc
	adc #MAX_LABEL_NAME_LEN
	sta @sym
	bcc :+
	inc @sym+1
.ifdef ultimem
	bpl :+
	inc $9ffc	; next bank
	lda #$60	; start of BLK3
	sta @sym+1
.endif
:	rts
.endproc

;*******************************************************************************
; SETUP FOR LOAD OR DUMP
.proc setup_for_load_or_dump
@sym=r0
@addr=r4
	ldxy #labels
	stxy @sym
	ldxy #label_addresses
	stxy @addr
	rts
.endproc
