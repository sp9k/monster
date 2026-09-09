;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;
; OBJECT FILE FORMAT:
; FILENAMES[]                ; zero-terminated names, empty name ends the list
; NUM FRAGMENTS   [1 byte]   ; number of object-local fragments
; NUM_EXPORTS     [1 byte]   ; number of symbols exported in object file
; NUM_IMPORTS     [2 bytes]  ; number of symbols imported by object file
; NUM_LOCALS      [2 bytes]  ; number of LOCAL symbols in object file
; FRAGMENT HEADERS           ; 16 bytes per fragment, in layout order
;  NAME   [$0:$7]            ; named SEGMENT, or zeroes for absolute .ORG code
;  ORIGIN [$8:$9]            ; 0 for REL code, literal address for ABS code
;  TYPE   [$a]               ; TYPE_SEGZP/SEG/BSS/BSSZP/ABS (see below)
;  SIZE   [$b:$c]            ; raw bytes in fragment; excludes alignment padding
;  ALIGN  [$d:$e]            ; boundary preceding this fragment; 0=no constraint
;  FILL   [$f]               ; fill byte for this boundary
; IMPORTS[]
;   NAME[...]
;   MODE[1]
; EXPORTS[]
;   NAME[...]
;   FRAGMENT ID[1]
;   RELATIVE ADDR[2]
;   FILE ID[1], LINE[2]    ; index in FILENAMES, one-based source line (0=unknown)
; LOCALS[]
;   NAME[...]
;   FRAGMENT ID[1]
;   RELATIVE ADDR[2]
;   FILE ID[1], LINE[2]    ; floats use SEG_FLOAT_PACKED + five bytes before these
; FRAGMENT TABLES:
;   TYPE[1]
;   CODE SIZE[2]
;   RELOCATION SIZE[2] (non-BSS only)
;   OBJCODE[]          (non-BSS only)
;   RELOCATIONS[]      (non BSS only)
;   Foreach relocation:
;     FLAGS[1]
;     SITE OFFSET[2]
;     TARGET[2]
;     FLAGS:
;       bit 0:    word
;       bit 1:    fragment (else import)
;       bits 2-3: byte selection
;       bit 4:    PC-relative branch
;       bit 5:    fragment difference
;   If FLAGS & $3c: ADDEND HIGH[1]; if FLAGS & $20: NEGATIVE FRAGMENT[1].
;   The low addend is in OBJCODE; word operands also carry its high byte.
;   Branches subtract the final RUN address immediately after the operand.
;   A fragment target of SEG_ABS means a zero base (literal branch target).
; DEBUGINFO
;   FILENAMES               ; its own local-ID table for the debug block headers
;   HEADERS
;   PROGRAM
; All two-byte fields are little-endian. There is no version/legacy header.
;*******************************************************************************

.include "asm.inc"
.include "debuginfo.inc"
.include "errors.inc"
.include "expr.inc"
.include "fp.inc"
.include "file.inc"
.include "kernal.inc"
.include "labels.inc"
.include "log.inc"
.include "limits.inc"
.include "linker.inc"
.include "macros.inc"
.include "math.inc"
.include "ram.inc"
.include "target.inc"
.include "text.inc"
.include "util.inc"
.include "vmem.inc"
.include "zeropage.inc"

.macpack longbranch

;*******************************************************************************
; CONSTANTS (see limits.inc for others)
; max number of memory sections per OBJ file.
; Must be >= MAX_SEGMENTS (limits.inc): every SEGMENT has at least one
; SECTION and the per-object segment tables are sized by MAX_SECTIONS
MAX_SECTIONS         = MAX_FRAGMENTS
MAX_SEGMENT_NAME_LEN = 8	; max length of a single segment name

MAX_SYMBOL_INDEXES = $200	; max number of symbols that may be referenced

MAX_SYMBOL_NAME_LEN = 32

SYM_IMPORT_BYTE     = 1
SYM_IMPORT_WORD     = 2
SYM_REL_EXPORT_BYTE = 3
SYM_REL_EXPORT_WORD = 4
SYM_ABS_EXPORT_BYTE = 5
SYM_ABS_EXPORT_WORD = 6

;*******************************************************************************
; ZEROPAGE
reloc = zp::link	; when linking, pointer to current relocation

;*******************************************************************************
; BSS
.segment "OBJBSS"

.export __obj_sections_sizelo
.export __obj_sections_sizehi
.export __obj_segments
.export __obj_segments_sizelo
.export __obj_segments_sizehi

;*******************************************************************************
; Type flags for SEGMENT
TYPE_UNDEF = 0		; undefined (initial state during linking only)
TYPE_SEGZP = 1		; zeropage (code/data)
TYPE_SEG   = 2		; absolute (code/data)
TYPE_BSS   = 3		; absolute (uninitialized)
TYPE_BSSZP = 4		; zeropage (uninitialized)
TYPE_ABS   = $ff	; constants, etc.

;*******************************************************************************
; RELOC TABLES
; This buffer contains the relocation tables for the object file
; sections_relocstartlo/hi contain the start address for each SECTION's
; relocation table, and each table is sections_relocsizelo/hi bytes long
; Calling obj::addreloc appends a relocation to this table
reloc_tables:
.ifdef vic20
	.res $3000
.else
	.res $8000	; REU-virtual (bank FINAL_BANK_LINKER); costs no real RAM
.endif
reloc_tables_end=*

;*******************************************************************************
.segment "SHAREBSS"

.export __obj_num_exports
__obj_num_exports:
numexports: .byte 0

.export __obj_num_imports
__obj_num_imports:
numimports: .word 0

.export __obj_num_locals
__obj_num_locals:
numlocals: .word 0

.export __obj_numsegments
__obj_numsegments:
numsegments: .byte 0	; number of SEGMENTs in obj file being written/read

;*******************************************************************************
; VARIABLES
.segment "OBJVARS"
reloctop: .word 0	; pointer to top of relocation table being built

; SEGMENT index/counter used while loading an object file.
seg_idx: .byte 0
seg_cnt: .byte 0

.export __obj_numsections
__obj_numsections:
numsections: .byte 0	; number of sections in obj file being written/read

.export __obj_filename
__obj_filename: .word 0	; pointer to name of object file being loaded

import_label_idshi:   .res MAX_IMPORTS	; MSBs of label IDs for imports
import_label_idslo:   .res MAX_IMPORTS	; LSBs of label IDs for imports

num_reloctables_mapped: .byte 0

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS

; segment-relative base of each SECTION (0 for the first SECTION of a
; SEGMENT; the SEGMENT's accumulated size for SECTIONS that re-open it).
; sections_start* always holds the PHYSICAL address of the section's code.
sections_baselo:   .res MAX_SECTIONS
sections_basehi:   .res MAX_SECTIONS
sections_anonlo:   .res MAX_SECTIONS
sections_anonhi:   .res MAX_SECTIONS
__obj_sections_sizelo:
sections_sizelo:   .res MAX_SECTIONS
__obj_sections_sizehi:
sections_sizehi:   .res MAX_SECTIONS
__obj_segments_sizelo:
segments_sizelo:   .res MAX_SECTIONS
__obj_segments_sizehi:
segments_sizehi:   .res MAX_SECTIONS
.export segments_type
segments_type:     .res MAX_SECTIONS

; Each object-local SEGMENT is a FRAGMENT of a named linker SEGMENT.
; .ALIGN starts a new fragment; its boundary/fill are resolved by the linker.
.export __obj_segments_alignlo
__obj_segments_alignlo:
segments_alignlo:  .res MAX_SECTIONS
.export __obj_segments_alignhi
__obj_segments_alignhi:
segments_alignhi:  .res MAX_SECTIONS

__obj_segments_fill: .res MAX_SECTIONS
.export __obj_segments_fill, __obj_fragment_ids
__obj_fragment_ids: .res MAX_SECTIONS

force_fragment: .byte 0

__obj_segments:
segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG

; link-time LOAD start addresses of each SEGMENT (where its bytes are placed)
.export __obj_segments_startlo
__obj_segments_startlo:
segments_startlo:      .res MAX_SECTIONS
.export __obj_segments_starthi
__obj_segments_starthi:
segments_starthi:      .res MAX_SECTIONS

; link-time RUN start addresses of each SEGMENT (what its code is relocated
; for)
segments_runlo:        .res MAX_SECTIONS
segments_runhi:        .res MAX_SECTIONS

; SEGMENT id for each SECTION
.export __obj_segment_ids
__obj_segment_ids: .res MAX_SECTIONS

; relocation table offsets/sizes for each section
sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS
segments_relocsizelo:  .res MAX_FRAGMENTS
segments_relocsizehi:  .res MAX_FRAGMENTS

;*******************************************************************************
; EXPORTS
; We store the id's for each export defined so that we can find its name when we
; dump the object file
export_label_idslo: .res MAX_EXPORTS	; LSB of label ID for exports
export_label_idshi: .res MAX_EXPORTS	; MSB of label ID for exports

;*******************************************************************************
.RODATA

.export __obj_init
.export __obj_add_reloc
.export __obj_close_section
.export __obj_split_fragment

; the cart builds run this code banked, so entry must go through a far-call
.if .defined(vic20) .or .defined(CART)
;*******************************************************************************
__obj_init:
	JUMP FINAL_BANK_LINKER, init

;*******************************************************************************
__obj_add_reloc:
	JUMP FINAL_BANK_LINKER, add_reloc

;*******************************************************************************
__obj_close_section:
	JUMP FINAL_BANK_LINKER, close_section
.else
__obj_init          = init
__obj_add_reloc     = add_reloc
__obj_close_section = close_section
.endif

BANKED_SEG "OBJCODE", FINAL_BANK_LINKER

;*******************************************************************************
; INIT
; Clears the object state in preparation for a new object file to be assembled
.proc init
	lda #$00
	sta numsections
	sta numsegments
	sta numimports
	sta numimports+1
	sta numexports
	sta numlocals
	sta numlocals+1
	sta num_reloctables_mapped
	sta force_fragment

	; clear arrays
	ldx #MAX_FRAGMENTS
@clrsizes:
	sta segments_sizelo-1,x
	sta segments_sizehi-1,x
	sta sections_sizelo-1,x
	sta sections_sizehi-1,x
	sta segments_alignlo-1,x
	sta segments_alignhi-1,x
	sta __obj_segments_fill-1,x
	dex
	bne @clrsizes

	; reset relocation tables "top" pointer
	ldxy #reloc_tables
	stxy reloctop

	rts
.endproc

;*******************************************************************************
; ADD SEGMENT
; Adds a new SEGMENT with the given name
; IN:
;   - .XY: address of the SEGMENT name to add
; OUT:
;   - .A: the ID of the segment added
;   - .C: set on error
.proc add_segment
@name=r0
@dst=r2
	stxy @name

	; make sure there is room for another SEGMENT
	lda numsegments
	cmp #MAX_FRAGMENTS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	; get the address for the new name (16-bit: the offset exceeds
	; 8 bits for segment ids >= 32)
	lda #$00
	sta @dst+1
	lda numsegments
	asl
	rol @dst+1
	asl
	rol @dst+1
	asl			; * MAX_SEGMENT_NAME_LEN
	rol @dst+1
	adc #<segments
	sta @dst
	lda @dst+1
	adc #>segments
	sta @dst+1

	ldy #$00
@l0:	lda (@name),y
	sta (@dst),y
	beq @pad
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l0
	bcs @ok

@pad:	; pad remainder of buffer with 0's
	lda #$00
@l1:	sta (@dst),y
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l1

@ok:	inc numsegments
	lda numsegments		; get 1-based section ID
	clc			; ok

	RETURN_OK
.endproc

;*******************************************************************************
; SPLIT FRAGMENT
; Close the current section and create another FRAGMENT with the same name/type
; IN:
;  - .XY: constant alignment
;  - .A = fill byte
__obj_split_fragment:
.proc split_fragment
@name = r0
@align = r6
@fill  = r8
	stxy @align
	sta @fill
	lda asm::segment
	jsr __obj_get_segment_name_by_id
	stxy @name

	ldy #$00
:	lda (@name),y
	sta $100,y
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bne :-

	; close current local SEGMENT/FRAGMENT and start a new one
	jsr close_section
	lda #$01
	sta force_fragment
	lda asm::segtype
	jsr __obj_add_section

	; turn force_fragment back off
	ldx #$00
	stx force_fragment
	bcs @ret

	; set the alignment for the new FRAGMENT
	tax
	lda @align
	sta segments_alignlo-1,x
	lda @align+1
	sta segments_alignhi-1,x
	lda @fill
	sta __obj_segments_fill-1,x
	txa			; X still holds the new fragment ID

	ldxy #$0000
	clc
@ret:	rts
.endproc

;*******************************************************************************
; ADD SECTION
; Adds a new section to the current object file in construction at the given
; address. This address is the where the section is stored while
; building the object file. The actual address of the code within the section
; will be determined by the linker when the program is linked.
; The base address of the SEGMENT is also returned, which will be 0 if this is
; a never before seen SEGMENT or where the last section that referenced this
; SEGMENT left off if not.
; IN:
;   - .A:             TYPE: 0=ZP relocate, 1=ABS relocate, 2=BSS, $FF=ABS
;   - zp::asmresult:  the physical address to begin the section at
;   - $100:           the name of the SEGMENT for the SECTION (if relative)
; OUT:
;   - .A:  the ID of the SEGMENT the section corresponds to
;   - .XY: the base address for the section
;   - .C:  set if the section could not be added
.export __obj_add_section
.proc __obj_add_section
@name=$100
@segaddr=r0
@info=r4
	ldy zp::pass
	cpy #$01
	beq @pass1

	; in pass 2 we just need to store relocation table start address
	ldx num_reloctables_mapped
	lda reloctop
	sta sections_relocstartlo,x	; set reloc start LSB
	lda reloctop+1
	sta sections_relocstarthi,x	; set reloc start MSB

	; get SEGMENT id and segment-relative base for this section
	; (must match what pass 1 returned so labels validate)
	ldx num_reloctables_mapped
	lda __obj_segment_ids,x		; segment id
	pha
	ldy sections_basehi,x		; segment-relative base MSB
	lda sections_baselo,x		; segment-relative base LSB
	tax
	pla
	inc num_reloctables_mapped

	RETURN_OK

@pass1:	ldx numsections
	cpx #MAX_SECTIONS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	sta @info
	lda lbl::numanon
	sta sections_anonlo,x
	lda lbl::numanon+1
	sta sections_anonhi,x
	lda zp::asmresult
	sta sections_startlo,x	; set obj section start LSB
	lda zp::asmresult+1
	sta sections_starthi,x	; set obj section start MSB

	lda @info
	cmp #TYPE_ABS
	beq @abs		; if ABS, continue to create a new segment

	lda force_fragment
	bne @add		; .ALIGN always opens a new fragment

	; is there already a SEGMENT by this name?
	ldxy #@name
	jsr get_segment_by_name
	bcs @add		; new name, add a new SEGMENT

; existing SEGMENT, start section where the SEGMENT left off
@get:	pha
	ldy numsections
	sta __obj_segment_ids,y

	tax

	; the SECTION resumes at its SEGMENT's current size; record that as
	; the section's segment-relative base (sections_start keeps the
	; physical address where the object code is stored)
	lda segments_sizehi-1,x
	sta sections_basehi,y
	pha			; MSB of SEGMENT's current top
	lda segments_sizelo-1,x
	sta sections_baselo,y
	tax
	pla
	tay			; MSB of SEGMENT's current top
	pla			; restore SEGMENT id

	inc numsections
	RETURN_OK

@abs:	; set name to 0 (empty) for absolute segments
	lda #$00
	sta @name

@add:	ldxy #@name
	jsr add_segment		; add new SEGMENT
	pha
	tax
	lda @info
	cmp #TYPE_ABS
	bne @addrel

	; if ABS (.org), set START address to the literal PC value
	lda zp::asmresult
	sta segments_startlo-1,x
	lda zp::asmresult+1
	sta segments_starthi-1,x

@addrel:
	; init SEGMENT SIZE and START (unless ABS) to 0
	lda #$00
	sta segments_sizelo-1,x
	sta segments_sizehi-1,x

	; store TYPE byte (ZP/BSS/etc) for the SEGMENT
	lda @info
	sta segments_type-1,x
	cmp #TYPE_ABS
	beq :+

	; init REL segments start address to 0
	lda #$00
	sta segments_startlo-1,x
	sta segments_starthi-1,x

:	ldx numsections
	pla			; restore SEGMENT id
	sta __obj_segment_ids,x

	pha			; save SEGMENT id (returned in .A)
	lda #$00
	sta sections_baselo,x	; new SEGMENT: section begins at base 0
	sta sections_basehi,x
	pla

	ldxy #$0000		; return 0 for address for new segment
	inc numsections
	RETURN_OK
.endproc

;*******************************************************************************
; CLOSE SECTION
; Closes the open section (if there is one)
.proc close_section
	ldx numsections
	beq @done		; no section to close
	ldy zp::pass
	cpy #$01
	beq @pass1

@pass2:	; in pass 2 we just need to calculate relocation table size
	; calculate size for the previous section's relocation table
	lda reloctop
	sec
	ldx num_reloctables_mapped
	sbc sections_relocstartlo-1,x
	sta sections_relocsizelo-1,x
	lda reloctop+1
	sbc sections_relocstarthi-1,x
	sta sections_relocsizehi-1,x
	RETURN_OK

@pass1:	; calculate/set the size for the previous section
	lda zp::asmresult
	sec
	sbc sections_startlo-1,x
	sta sections_sizelo-1,x
	lda zp::asmresult+1
	sbc sections_starthi-1,x
	sta sections_sizehi-1,x

	; update segment size (running sum)
	ldy __obj_segment_ids-1,x
	lda segments_sizelo-1,y
	clc
	adc sections_sizelo-1,x
	sta segments_sizelo-1,y
	lda segments_sizehi-1,y
	adc sections_sizehi-1,x
	sta segments_sizehi-1,y

@done:	RETURN_OK
.endproc

;*******************************************************************************
; ADD EXPORT
; Defines an EXPORT for the given label name
; IN:
;   - .XY: address of the symbol name to define an EXPORT for
; OUT:
;   - .C: set on error
.export __obj_add_export
.proc __obj_add_export
	; make sure there is room for another EXPORT
	lda numexports
	cmp #MAX_EXPORTS
	bcs @toomany

	CALLMAIN lbl::find		; look up the label by name
	bcs @ret			; not found -> err

	txa
	ldx numexports
	sta export_label_idslo,x	; get LSB of index for symbol
	tya
	sta export_label_idshi,x	; get MSB of index for symbol
	inc numexports
	clc				; ok
@ret:	rts

@toomany:
	RETURN_ERR ERR_TOO_MANY_LABELS
.endproc

;*******************************************************************************
; ADD IMPORT
; Defines an IMPORT for the given label name
; IN:
;   - .XY: address of the symbol name to define an IMPORT for
; OUT:
;   - .C: set on error
.export __obj_add_import
.proc __obj_add_import
	; make sure there is room for another IMPORT
	lda numimports+1
	bne @toomany
	lda numimports
	cmp #MAX_IMPORTS
	bcs @toomany

	; define a label for the import so that references to it succeed
	lda #SEG_UNDEF			; UNDEF (external)
	sta zp::label_segmentid
	lda #$00			; dummy value
	sta zp::label_value
	sta zp::label_value+1
	sta zp::label_lineno		; an import is not a source definition
	sta zp::label_lineno+1
	lda #$ff			; and so has no file
	sta zp::label_fileid

	CALLMAIN lbl::add
	bcs @ret			; not found -> err

	; store the ID of the label that was added
	txa
	ldx numimports
	sta import_label_idslo,x	; get LSB of index for symbol
	tya
	sta import_label_idshi,x	; get MSB of index for symbol
	incw numimports
	clc				; ok
@ret:	rts

@toomany:
	RETURN_ERR ERR_TOO_MANY_LABELS
.endproc

;*******************************************************************************
; ADD RELOC
; Adds a new relocation entry to the current object file in construction
; NOTE: the addend is written by the assembler
; IN:
;   - .A:      size of value to relocate (0=ZP, 1=ABS)
;   - .Y:      offset to apply relocation at
;   - expr::*: various values containing result of expression eval
; OUT:
;   - .C: set on error
.proc add_reloc
@sz=r0
@rel=r1
@offset=r3
@tmp=r4
	sta @sz
	sty @offset

	lda expr::kind
	cmp #VAL_REL
	beq :+
	cmp #VAL_DIFF
	jne @ok		; expression doesn't require relocation
	lda @sz
	ora #$20
	sta @sz

:	ldxy reloctop
	cmpw #(reloc_tables_end-7)	; full addend and negative fragment, if present
	bcc :+				; below max -> ok
	beq :+				; at max -> still fits
	RETURN_ERR ERR_OOM

:	stxy @rel

.ifdef c64
	lda #FINAL_BANK_LINKER
	sta reu::reuaddr+2
.endif

;-------------------------------------------------------------------------------
; encode the "info" byte for the relocation based on the result of the
; expression evaluation and the size of the relocation
;  field   bit(s)   description
; size       0   size of target value to modify 0=1 byte, 1=2 bytes
; mode       1   type of relocation: 1=section-relative, 0=symbol-relative
; postproc  2-3  post-processing (0=NONE, 1=LSB, 2=MSB)
; pcrel      4   subtract the RUN address after the branch operand
; difference 5   subtract another fragment's RUN base
@encode_size:
	lda expr::postproc
	asl
	asl
	ora @sz

	; is symbol in expression unresolved (section_id == SEG_UNDEF)?
	; yes -> use SYMBOL-based relocation
	; no  -> use SEGMENT-based relocation
	ldx expr::segment
	cpx #SEG_UNDEF
	beq :+
	ora #1<<1		; flag section based relocation

:	ldy #$00
	pha			; save info byte
	STOREB_Y @rel		; write info byte

	; write the offset of the target within its SEGMENT:
	; (asmresult+offset) - section physical start + section base
	lda zp::asmresult
	clc
	adc @offset
	sta @tmp
	lda zp::asmresult+1
	adc #$00
	sta @tmp+1

	ldx num_reloctables_mapped	; current section (1-based in pass 2)
	lda @tmp
	sec
	sbc sections_startlo-1,x
	sta @tmp
	lda @tmp+1
	sbc sections_starthi-1,x
	sta @tmp+1

	lda @tmp
	clc
	adc sections_baselo-1,x
	sta @tmp
	lda @tmp+1
	adc sections_basehi-1,x
	sta @tmp+1

	ldy #$01
	lda @tmp
	STOREB_Y @rel		; write offset LSB
	iny			; .Y=2
	lda @tmp+1
	STOREB_Y @rel		; write offset MSB
	iny			; .Y=3

	pla			; restore info byte
	and #$02		; mask "type" bit
	beq @sym_based		; if 0, write symbol index

@sec_based:
	lda expr::segment
	STOREB_Y @rel		; write symbol-id LSB
	lda #$00		; MSB of section is always 0
	iny			; .Y=4
	STOREB_Y @rel		; write symbol-id MSB
	bne @done		; branch always

@sym_based:
	ldxy expr::symbol
	jsr get_import_id	; look up object-local ID for symbol
	bcs @err		; not an IMPORT -> return error
	ldy #$03
	txa
	STOREB_Y @rel		; write local symbol-id LSB
	iny			; .Y=4
	lda #$00		; MSB (always 0 for now)
	STOREB_Y @rel		; write local symbol-id MSB

@done:	ldy #$05
	; byte postprocessing and branches retain the addend's high byte.
	lda @sz
	and #$30
	ora expr::postproc
	beq @difference
	lda expr::value+1	; MSB of the evaluated expression
	STOREB_Y @rel
	iny
@difference:
	lda @sz
	and #$20
	beq @advance
	lda expr::symbol	; negative fragment of a deferred difference
	STOREB_Y @rel
	iny

@advance:
	; update reloctop
	tya
	clc
	adc reloctop
	sta reloctop
	bcc @ok
	inc reloctop+1
@ok:	RETURN_OK

@err:	rts			; return error from get_import_id
.endproc

;*******************************************************************************
; GET IMPORT ID
; Translates the given label ID (from the assembly symbol table) to a "local"
; one for the active object state.
; IN:
;   - .XY: symobl ID to translate
; OUT:
;   - .A: local ID (index into import_label_idslo/hi)
;   - .C: set if the ID is not found (wasn't marked as an IMPORT)
.proc get_import_id
@id=r4
	stxy @id

	ldx #$00
	cpx numimports		; any imports defined?
	bcs @notfound		; if not, don't probe the (stale) table
@l0:	; look for the matching symbol ID in the table of mapped IMPORTs
	lda @id
	cmp import_label_idslo,x
	bne @next
	lda @id+1
	cmp import_label_idshi,x
	beq @found

@next:	inx
	cpx numimports
	bcc @l0
@notfound:
	RETURN_ERR ERR_IMPORT_UNDEFINED

@found:	txa
	RETURN_OK
.endproc

;*******************************************************************************
; DUMP IMPORTS
; Stores the names of the imported symbols along with their mapped symbol
; indices.
; Imports must be declared with the .IMPORT directive to map them to the object
; file.
; They are indentified by a SEG_UNDEF section index in the relocation tables at
; link time
.proc dump_imports
@i=zp::tmp10
@idx=zp::tmp12
@buff=$100
	lda #$00
	sta @i
	sta @i+1
	iszero numimports
	beq @done			; if no imports -> done

@l0:	; get the symbol name
	ldxy #@buff
	stxy r0
	ldx @i
	ldy import_label_idshi,x	; get MSB of index for symbol
	sty @idx+1
	lda import_label_idslo,x	; get LSB of index for symbol
	sta @idx
	tax
	CALLMAIN lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr krn::chrout
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; write the address mode for the IMPORT (ZP or ABS)
	ldxy @idx			; restore the label ID
	CALLMAIN lbl::addrmode
	jsr krn::chrout

	; next symbol
	incw @i
	ldxy @i
	cmpw numimports
	bne @l0

@done:	rts
.endproc

;*******************************************************************************
; DUMP EXPORTS
; Stores the names of the exported symbols along with their section indices
; and section offsets.
; Exports may or may not be referenced within the object code
; They must be explicitly mapped to the object code by a ".EXPORT" directive
.proc dump_exports
@i=zp::tmp10
@id=zp::tmp12
@buff=$100
	lda #$00
	sta @i
	cmp numexports
	beq @done			; if no exports -> done

@l0:	; get the symbol name by looking up its label ID
	ldxy #@buff
	stxy r0
	ldx @i
	ldy export_label_idshi,x	; get LSB of index for symbol
	lda export_label_idslo,x	; get MSB of index for symbol
	tax
	stxy @id

	CALLMAIN lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr krn::chrout
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; write the SEGMENT id
	ldxy @id
	jsr dump_symbol_value
	bcs @ret

	inc @i
	lda @i
	cmp numexports
	bcc @l0

@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; DUMP LOCALS
; Dumps the local (not-export, not-import) symbols to the open object file
.proc dump_locals
@id=zp::tmp12
@segid=zp::tmp14
@buff=$100
	lda #$00
	sta @id
	sta @id+1

	iszero lbl::num
	beq @done			; if no exports -> done

@l0:	; check if the label is already dumped as an export and skip it if so
	jsr @isexport
	beq @next

	; check if label was dumped as IMPORT (segment is UNDEF)
	ldxy @id
	CALLMAIN lbl::getsegment	; get SEGMENT id
	sta @segid
	cmp #SEG_UNDEF
	beq @next

	; get the symbol name by looking up its label ID
	ldxy #@buff
	stxy r0
	ldxy @id
	CALLMAIN lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr krn::chrout
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; write the SEGMENT id
	ldxy @id
	jsr dump_symbol_value
	bcs @ret

@next:	incw @id
	ldxy @id
	cmpw lbl::num
	bne @l0

@done:	clc
@ret:	rts

;-------------------------------------------------------------------------------
; check if the given label ID is an EXPORT, in which case we don't need to dump
; it
@isexport:
	ldx numexports
	beq @notexport

@l1:	lda @id+1
	cmp export_label_idshi-1,x	; get LSB of index for symbol
	bne :+
	lda @id
	cmp export_label_idslo-1,x	; get MSB of index for symbol
	beq @isexport_done
:	dex
	bne @l1
@notexport:
	lda #$ff			; flag NOT export
@isexport_done:
	rts
.endproc

;*******************************************************************************
; DUMP SYMBOL VALUE
; Integer values use tag+word; floats use a disk-only tag and all five bytes.
; Both are followed by a local file ID and a two-byte source line.
.proc dump_symbol_value
@id=zp::tmp16
	stxy @id
	CALLMAIN lbl::getsegment
	cmp #SEG_FLOAT
	beq @float
	jsr krn::chrout
	ldxy @id
	CALLMAIN lbl::getaddr
	txa
	jsr krn::chrout
	tya
	jsr krn::chrout
	jmp @location
@float:
	lda #SEG_FLOAT_PACKED
	jsr krn::chrout
	ldxy @id
	CALLMAIN lbl::getaddr
	CALL FINAL_BANK_EXPR, expr::fconst_write
	bcs @ret
@location:
	ldxy @id
	CALLMAIN lbl::get_line
	stxy zp::label_lineno
	CALL FINAL_BANK_DEBUG, dbgi::localfile
	jsr krn::chrout
	lda zp::label_lineno
	jsr krn::chrout
	lda zp::label_lineno+1
	jsr krn::chrout
	clc
@ret:	rts
.endproc

;*******************************************************************************
; DUMP SEGMENTS
; Dumps the SEGMENTS used in the object file and their sizes
; Also computes the sizes of the object and relocation tables, which are
; written in front of their corresponding data tables.
.proc dump_segments
@name=r0
@sec_idx=r2
@seg_idx=r4
@i=rc
	lda numsegments
	bne :+
	RETURN_OK			; no SEGMENTS to dump

:	; init segment sizes to 0
	ldx numsegments
	lda #$00
	sta @seg_idx
:	sta segments_relocsizelo-1,x
	sta segments_relocsizehi-1,x
	dex
	bne :-

;-------------------------------------------------------------------------------
; compute the size of each SEGMENT's relocation table (the sum of the
; relocation tables of all SECTIONS that use it)
@l0:	lda #$00
	sta @sec_idx
	inc @seg_idx			; next SEGMENT id (1-based)

; iterate over all SECTIONS and check if they're in this SEGMENT
@l1:	lda @seg_idx			; get current SEGMENT
	ldx @sec_idx
	cmp __obj_segment_ids,x		; is this SECTION in this SEGMENT?
	bne :+				; if not, continue

	tay				; .Y=segment_id for section
	lda sections_relocsizelo,x	; get reloc table size for section
	clc
	adc segments_relocsizelo-1,y	; add with current SEGMENT size
	sta segments_relocsizelo-1,y
	lda sections_relocsizehi,x
	adc segments_relocsizehi-1,y
	sta segments_relocsizehi-1,y

:	inc @sec_idx
	lda @sec_idx
	cmp numsections
	bne @l1

	lda @seg_idx
	cmp numsegments
	bne @l0

	lda #$00
	sta @i
@dump_headers:
	; get offset to name for this section name (*8)
	; (16-bit: the offset exceeds 8 bits for segment ids >= 32)
	lda #$00
	sta @name+1
	lda @i
	asl
	rol @name+1
	asl
	rol @name+1
	asl
	rol @name+1
	adc #<segments
	sta @name
	lda @name+1
	adc #>segments
	sta @name+1

	; write the name of the SEGMENT
	ldy #$00
:	lda (@name),y
	jsr krn::chrout
	iny
	cpy #$08
	bne :-

	; write SEGMENT offset (always $0000 for relative) or
	; literal start address of SEGMENT (for ABSOLUTE segments)
	ldx @i
	lda segments_startlo,x
	jsr krn::chrout
	lda segments_starthi,x
	jsr krn::chrout

	; write TYPE byte (SEGZP, SEG, BSS, etc.)
	lda segments_type,x
	jsr krn::chrout

	; write the number of bytes used for this SEGMENT (2 bytes)
	lda __obj_segments_sizelo,x
	jsr krn::chrout
	lda __obj_segments_sizehi,x
	jsr krn::chrout

	; write the alignment the SEGMENT's code requires (2 bytes)
	lda segments_alignlo,x
	jsr krn::chrout
	lda segments_alignhi,x
	jsr krn::chrout

	lda __obj_segments_fill,x
	jsr krn::chrout

	; next SEGMENT
	inc @i
	lda @i
	cmp numsegments
	bne @dump_headers

@done:	RETURN_OK
.endproc

;*******************************************************************************
; DUMP SEGMENT TABLES
; Concatenates all SECTIONS that share a SEGMENT and dumps them to the object
; file under construction.
.proc dump_segment_tables
@sec=r0
@sz=r2
@sec_idx=r4
@seg_idx=r5
	lda #$00
	sta @seg_idx
	cmp numsections

	bne @l0
	RETURN_OK		; no sections

;-------------------------------------------------------------------------------
; iterate over all SEGMENTs and dump them
@l0:	ldx @seg_idx

	; write the INFO byte
	lda segments_type,x
	pha
	jsr krn::chrout

	; write the size of the SEGMENT
	lda __obj_segments_sizelo,x
	jsr krn::chrout
	lda __obj_segments_sizehi,x
	jsr krn::chrout

	; check if this is a BSS segment. We don't emit object/relocation code
	; for segments of this TYPE
	pla
	jsr is_bss
	jeq @nextseg			; if BSS -> skip

	; write the size of the relocation table
	lda segments_relocsizelo,x
	jsr krn::chrout
	lda segments_relocsizehi,x
	jsr krn::chrout

;-------------------------------------------------------------------------------
; OBJECT CODE
; iterate over all SECTIONS and dump them if they're part of the SEGMENT we're
; working on
	lda #$00
	sta @sec_idx		; reset section counter
@objloop:
	ldx @sec_idx

	; check if this SECTION is part of the SEGMENT we're building
	lda @seg_idx
	clc
	adc #$01			; +1 because id's are 1-based
	cmp __obj_segment_ids,x		; is our SECTION part of the SEGMENT?
	bne @obj_next			; not our SEGMENT, try next SECTION

	ldx @sec_idx
	lda __obj_sections_sizelo,x
	sta @sz
	lda __obj_sections_sizehi,x
	sta @sz+1
	ora @sz
	beq @obj_next			; if no OBJ code, done with this SECTION

	; get start address of SECTION to dump
	lda sections_startlo,x
	sta @sec
	lda sections_starthi,x
	sta @sec+1

:	; dump the object code for the section
	ldxy @sec		; address to load
	jsr vmem_load		; load a byte of object code
	jsr krn::chrout		; and dump it
	incw @sec
	decw @sz
	iszero @sz
	bne :-			; repeat til done

@obj_next:
	inc @sec_idx
	lda @sec_idx
	cmp numsections
	jne @objloop

;-------------------------------------------------------------------------------
; RELOCATION TABLE
; iterate over all SECTIONS and dump them if they're part of the SEGMENT we're
; working on
	lda #$00
	sta @sec_idx		; reset SECTION index
@dump_rel:
	ldx @sec_idx

	; check if this SECTION is part of the SEGMENT we're building
	lda @seg_idx
	clc
	adc #$01			; +1 because id's are 1-based
	cmp __obj_segment_ids,x		; is our SECTION part of the SEGMENT?
	bne @reloc_next			; not our SEGMENT, try next SECTION

@reloc:	; dump the relocation table
	ldx @sec_idx
	lda sections_relocstartlo,x
	sta @sec
	lda sections_relocstarthi,x
	sta @sec+1
	lda sections_relocsizelo,x
	sta @sz
	lda sections_relocsizehi,x
	sta @sz+1
	ora @sz
	beq @reloc_next			; if no relocation table, skip

.ifdef c64
	lda #FINAL_BANK_LINKER
	sta reu::reuaddr+2
.endif

	ldy #$00
@relocloop:
	LOADB_Y @sec
	jsr krn::chrout
	incw @sec
	decw @sz
	iszero @sz
	bne @relocloop

@reloc_next:
	inc @sec_idx
	lda @sec_idx
	cmp numsections
	jne @dump_rel

;-------------------------------------------------------------------------------
; move to next SEGMENT index and repeat til all are dumped
@nextseg:
	inc @seg_idx
	lda @seg_idx
	cmp numsegments
	jne @l0

@done:	RETURN_OK
.endproc

;*******************************************************************************
; DUMP
; Writes the complete object file to the given filename using the state built
; from the most recent successful assembly.
; The file to dump to should be open and set as the output file before calling
; this procedure.
; OUT:
;   - .C: set on error
.export __obj_dump
.proc __obj_dump
@tmp=r0
@src=r0
@cnt=r2
	CALL FINAL_BANK_DEBUG, dbgi::preparefiles
	CALL FINAL_BANK_DEBUG, dbgi::dumpfiles
	; write the main OBJ header
	lda numsegments			; # of segments
	jsr krn::chrout
	lda numexports			; # of EXPORTS
	jsr krn::chrout
	lda numimports			; # of IMPORTS (LSB)
	jsr krn::chrout
	lda numimports+1		; # of IMPORTS (MSB)
	jsr krn::chrout

	; locals = total labels - (numexports + numimports)
	lda numexports
	clc
	adc numimports
	sta @tmp
	lda numimports+1
	adc #$00
	sta @tmp+1

	lda lbl::num
	sec
	sbc @tmp
	php
	jsr krn::chrout			; write # of LOCALS (LSB)
	plp
	lda lbl::num+1
	sbc @tmp+1
	jsr krn::chrout			; write # of LOCALS (MSB)

	; write the SEGMENTS used (names and sizes)
	jsr dump_segments

	; write the SYMBOL TABLE (in order: IMPORTS, EXPORTS)
	jsr dump_imports
	jsr dump_exports
	bcs @ret
	jsr dump_locals
	bcs @ret

	; write each SEGMENT (object code, relocation data)
	jsr dump_segment_tables

	; lastly, write the debug info for the object file
	CALL FINAL_BANK_DEBUG, dbgi::dump

	RETURN_OK
@ret:	rts
.endproc

;*******************************************************************************
; APPLY RELOCATION
; Produces the final binary for the object table for the given SEGMENT
; IN:
;   - .A: id of SEGMENT to apply relocation for
; OUT:
;   - .C: set if there is no remaining relocation to apply for the section
.proc apply_relocation
@negative_base = r0
; The loader has finished with its per-table scratch before calling here.
; KERNAL input, checked_run_base, lbl::getaddr and vmem preserve r2-rf and
; tmp10/tmp11; r0/r1 remain available to callees and the subtraction below.
@record    = r2             ; r2-r6: flags, offset, target
@remaining = r7             ; r7-r8
@length    = r9
@local     = ra
@siteaddr  = rb             ; rb-rc
@runsite   = rd             ; rd-re
@addendhi  = rf
@value     = zp::tmp10      ; tmp10-tmp11
	sta @local
	tax
	lda segments_relocsizelo,x
	sta @remaining
	lda segments_relocsizehi,x
	sta @remaining+1
@next:	lda @remaining
	ora @remaining+1
	jeq @done
	lda @remaining+1
	bne @read
	lda @remaining
	cmp #$05
	jcc @bad
@read:	ldy #$00
:	jsr krn::chrin
	sta @record,y
	iny
	cpy #$05
	bne :-
	sty @length
	lda #$00
	sta @addendhi
	lda @record
	and #$3c
	beq @base
	jsr krn::chrin
	sta @addendhi
	inc @length
@base:	lda @record
	and #$02
	beq @symbol
	lda @record+3
	jsr checked_run_base
	jcs @bad
	jmp @resolved
@symbol:
	ldx @record+3
	cpx numimports
	jcs @bad
	ldy import_label_idshi,x
	lda import_label_idslo,x
	tax
	CALLMAIN lbl::getaddr
@resolved:
	stxy @value
	lda @record
	and #$20
	beq @site
	jsr krn::chrin
	inc @length
	jsr checked_run_base
	jcs @bad
	stxy @negative_base
	lda @value
	sec
	sbc @negative_base
	sta @value
	lda @value+1
	sbc @negative_base+1
	sta @value+1
@site:	ldx @local
	lda segments_startlo,x
	clc
	adc @record+1
	sta @siteaddr
	lda segments_starthi,x
	adc @record+2
	sta @siteaddr+1
	jcs @bad
	lda segments_runlo,x
	clc
	adc @record+1
	sta @runsite
	lda segments_runhi,x
	adc @record+2
	sta @runsite+1
	jcs @bad
	; Fetch both addend bytes before adding, so no call can disturb carry.
	lda @record
	and #$01
	beq @add
	ldxy @siteaddr
	inx
	bne :+
	iny
:	jsr vmem_load
	sta @addendhi
@add:	ldxy @siteaddr
	jsr vmem_load
	clc
	adc @value
	sta @value
	lda @addendhi
	adc @value+1
	sta @value+1

	lda @record
	and #$10
	beq @store

	; PC after the branch is one byte after its operand's RUN address
	incw @runsite
	lda @value
	sec
	sbc @runsite
	sta @value
	lda @value+1
	sbc @runsite+1
	beq @forward
	cmp #$ff
	bne @range
	lda @value
	bpl @range
	jmp @storebyte

@forward:
	lda @value
	bmi @range
	jmp @storebyte

@store:	lda @record
	and #$0c
	cmp #POSTPROC_MSB<<2
	beq @msb
	cmp #POSTPROC_LSB<<2
	beq @selected
	lda @record
	and #$01
	bne @word
	lda @value+1
	bne @byte_range
	beq @storebyte

@word:	ldxy @siteaddr
	inx
	bne :+
	iny
:	lda @value+1
	jsr vmem_store

@storebyte:
	lda @value
	jmp @write
@msb:	lda @value+1
	sta @value

@selected:
	lda #$00		; selected byte is 0-extended in a word operand
	sta @value+1
	lda @record
	lsr
	bcs @word
	bcc @storebyte

@write:	ldxy @siteaddr
	jsr vmem_store
	lda @remaining
	sec
	sbc @length
	sta @remaining
	lda @remaining+1
	sbc #$00
	sta @remaining+1
	jcc @bad
	jmp @next
@done:	RETURN_OK
@range:	RETURN_ERR ERR_BRANCH_OUT_OF_RANGE

@byte_range:
	RETURN_ERR ERR_OVERSIZED_OPERAND
@bad:	RETURN_ERR ERR_UNKNOWN_SEGMENT
.endproc

;*******************************************************************************
; GET SEGMENT RUN BASE
; IN:
;  - .A: 1-based object local FRAGMENT ID
; OUT:
;  - .XY: final RUN address for base of FRAGMENT
.proc get_segment_run_base
.export __obj_get_fragment_run
__obj_get_fragment_run:
	tax
	ldy segments_runhi-1,x
	lda segments_runlo-1,x
	tax
	rts
.endproc

;*******************************************************************************
; CHECKED RUN BASE
; Translates the provided object-local FRAGMENT ID to its final RUN address
; IN:
;   - .A: fragment ID
; OUT:
;   - .XY: base of the SEGMENT
;   - .C:  set on invalid fragment ID
.proc checked_run_base
	cmp #SEG_ABS
	bne :+
	ldxy #$0000		; absolute branch target (no extra offset)
	clc
	rts

:	cmp #$01
	bcc @bad
	cmp numsegments
	beq @ok
	bcs @bad

@ok:	jsr get_segment_run_base	; get FRAGMENT base address
	clc
	rts

@bad:	sec
	rts
.endproc

;*******************************************************************************
; ANON FRAGMENT
; IN:
;  - .XY: "index" of the anonymous label
; OUT:
;  -  .A: FRAGMENT ID of the anonymous label or SEG_ABS if not part of one
.export __obj_anon_fragment
.proc __obj_anon_fragment
@index = r0
	stxy @index
	ldx numsections
@find:	dex
	lda @index
	cmp sections_anonlo,x
	lda @index+1
	sbc sections_anonhi,x
	bcc @find
	lda __obj_segment_ids,x
	tax
	lda segments_type-1,x
	cmp #TYPE_ABS
	beq @absolute
	txa
	rts

@absolute:
	lda #SEG_ABS
	rts
.endproc

;*******************************************************************************
; LOAD INFO
; Loads the first part of the object file and extracts basic info from it
; (e.g. number of symbols)
; OUT:
;   - .C: set on error
.proc load_info
@i=r4
@name=r6
@symoff=r8
@namebuff=$100
	CALL FINAL_BANK_DEBUG, dbgi::loadfiles
	jcs @ret
	lda #<segments
	sta @name
	lda #>segments
	sta @name+1

	; read number of SEGMENTs used
	jsr krn::chrin
	sta numsegments

	; read number of EXPORTS (1 byte)
	jsr krn::chrin
	sta numexports

	; read number of IMPORTS (2 bytes)
	jsr krn::chrin
	sta numimports
	jsr krn::chrin
	sta numimports+1

	; read number of LOCALS (2 bytes)
	jsr krn::chrin
	sta numlocals
	jsr krn::chrin
	sta numlocals+1

	; validate the symbol counts
	lda numimports+1
	bne @toomany
	lda numimports
	cmp #MAX_IMPORTS+1
	bcs @toomany
	lda numexports
	cmp #MAX_EXPORTS+1
	bcc @counts_ok
@toomany:
	RETURN_ERR ERR_TOO_MANY_LABELS

@counts_ok:
	; validate the SEGMENT count
	lda numsegments
	jeq @segments_done	; no SEGMENTS -> done
	cmp #MAX_FRAGMENTS+1
	bcc @segments_ok
	RETURN_ERR ERR_TOO_MANY_SEGMENTS

@segments_ok:
;-------------------------------------------------------------------------------
; read the SEGMENTS used in the object file (names and sizes)
	lda #$00
	sta @i

@load_segments:
	ldy #$00
@segname:
	; read the SEGMENT name
	jsr krn::chrin
	sta (@name),y
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bne @segname

	; read OFFSET (or literal start address if ABS) for SEGMENT
	ldy @i
	jsr krn::chrin
	sta segments_startlo,y
	jsr krn::chrin
	sta segments_starthi,y

	; read TYPE byte
	jsr krn::chrin
	sta segments_type,y

	; get the number of bytes used in the SEGMENT
	jsr krn::chrin
	sta __obj_segments_sizelo,y
	jsr krn::chrin
	sta __obj_segments_sizehi,y

	; get the alignment the SEGMENT's code requires
	jsr krn::chrin
	sta segments_alignlo,y
	jsr krn::chrin
	sta segments_alignhi,y
	jsr krn::chrin
	sta __obj_segments_fill,y

	; if ABS segment, directly set the SEGMENT start address
	ldy #$00
	lda (@name),y			; is name empty?
	beq @abs			; if so (ABS), already know start addr

@rel:	; for REL segments, get the base address of this SEGMENT in the linker
	; NOTE: this will be garbage in pass 1
	; look up the linker's id for this SEGMENT and map it
	ldxy @name
	jsr link::segid_by_name
	jcs @ret
	ldy @i
	sta __obj_segment_ids,y		; store GLOBAL id for this SEGMENT

	; set the global TYPE for the segment (if not already set)
	tax				; .X = global segment id
	lda segments_type,y		; .A = TYPE from this object file
	jsr link::set_segtype		; set type for the segment
	bcs @ret			; if conflicts with existing seg -> rts

	jmp @fragment

@abs:	ldy @i
	lda #SEG_ABS
	sta __obj_segment_ids,y

@fragment:
	ldy @i
	lda __obj_segment_ids,y
	ldx @i
	jsr link::fragment		; register (pass 1) / locate (pass 2)
	bcs @ret
	ldy @i
	sta __obj_fragment_ids,y
	tax
	lda link::fragment_loadlo-1,x
	sta segments_startlo,y
	lda link::fragment_loadhi-1,x
	sta segments_starthi,y
	lda link::fragment_runlo-1,x
	sta segments_runlo,y
	lda link::fragment_runhi-1,x
	sta segments_runhi,y

@next:	; move name pointer to next location
	lda @name
	clc
	adc #MAX_SEGMENT_NAME_LEN
	sta @name
	bcc :+
	inc @name+1

:	; increment counter and loop til we've done all SEGMENTS
	inc @i
	ldy @i
	cpy numsegments
	jne @load_segments

@segments_done:
	clc				; ok
@ret:	rts
.endproc

;*******************************************************************************
; LOAD HEADERS
; Extracts the SEGMENT usage info and global symbols for the given object file.
; This is called by the linker for each object file to build the global link
; state.
.export __obj_load_headers
.proc __obj_load_headers
@name=r2
@i=zp::tmp10
@namebuff=$100
	jsr load_info
	bcs @ret

; add the IMPORTS to the global symbol table (as placeholders if the
; symbols are not yet defined)
@imports:
	lda #$00
	sta @i
	cmp numimports
	beq @exports

@import_loop:
	jsr load_import
	bcs @ret

	inc @i
	lda @i
	cmp numimports
	bne @import_loop

;-------------------------------------------------------------------------------
; add EXPORTS to the global symbol table.  Their values are segment-relative
; until resolve_symbols finalizes them (after all objects have been processed
; and the segment origins are known)
@exports:
	lda #$00
	sta @i
	cmp numexports
	beq @locals

@export_loop:
	jsr load_export
	bcs @ret

	inc @i
	lda @i
	cmp numexports
	bne @export_loop

;-------------------------------------------------------------------------------
; add the LOCAL symbols to the global symbol table (also segment-relative),
; scoped to the filename of this object file
@locals:
	; copy the filename to shared RAM and set it as the scope
	ldxy __obj_filename
	stxy @name
	ldy #$00
:	lda (@name),y
	sta @namebuff,y
	beq :+
	iny
	bne :-

:	ldxy #@namebuff
	CALLMAIN lbl::setscope

	ldxy numlocals
	stxy @i			; 16-bit counter for LOCALs
@local_loop:
	iszero @i
	beq @locals_done
	jsr load_local
	jcs @ret
	decw @i
	jmp @local_loop

@locals_done:
	CALLMAIN lbl::popscope

@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; LOAD IMPORT
; Adds the next IMPORT in the open OBJECT file to the symbol table unless
; it is already defined.
; Returns an error if the symbol already exists and conflicts
; OUT:
;   - .C: set on error
.proc load_import
@namebuff=$120
	; get the name of a symbol
	ldy #$00
	sty zp::label_value	; dummy value (0)
	sty zp::label_value+1
	sty zp::label_lineno	; imports have no definition location
	sty zp::label_lineno+1
	lda #$ff		; and so have no file
	sta zp::label_fileid
:	jsr krn::chrin
	sta @namebuff,y
	beq @cont
	iny
	cpy #MAX_LABEL_NAME_LEN
	bcc :-
	RETURN_ERR ERR_LABEL_TOO_LONG		; corrupt object file

@cont:	jsr krn::chrin				; get info byte (address mode)
	sta zp::label_mode

	ldxy #@namebuff
	CALLMAIN lbl::find			; was label already added?
	bcs @add				; if no -> add it

	; validate: does address mode match existing symbol?
	CALLMAIN lbl::addrmode
	cmp zp::label_mode
	beq @ok					; matches -> ok

	; error: address mode doesn't match
	RETURN_ERR ERR_ADDRMODE_MISMATCH	; conflicting import/exports

@add:	lda #SEG_UNDEF
	sta zp::label_segmentid
	ldxy #@namebuff
	JUMPMAIN lbl::add

@ok:	RETURN_OK
.endproc

;*******************************************************************************
; LOAD EXPORT
; Adds the next EXPORT in the open OBJECT file to the global symbol table.
; This is called during pass 1: the value stored is relative to the symbol's
; SEGMENT (this object's offset within the SEGMENT plus the symbol's offset
; within the object).  The final value is produced by link's resolve_symbols
; once all objects have been processed and the SEGMENT origins are known.
; OUT:
;   - .C: set on error
.proc load_export
@namebuff=$120
	; get the name of a symbol
	ldy #$00
:	jsr krn::chrin
	sta @namebuff,y
	beq @addexport
	iny
	cpy #MAX_LABEL_NAME_LEN
	bcc :-
	RETURN_ERR ERR_LABEL_TOO_LONG		; corrupt object file

@addexport:
	jsr load_symbol_value
	bcs @ret

	ldxy #@namebuff
	CALLMAIN lbl::find			; was label already added?
	bcs @add				; no -> add it

	; validate: is segment SEG_UNDEF (previously added as an IMPORT)?
	; if not, error (only 1 EXPORT is allowed per symbol)
	CALLMAIN lbl::getsegment
	cmp #SEG_UNDEF
	beq @set

	lda #ERR_ALREADY_EXPORTED		; multiple exports
	sec
@ret:	rts

@set:	; overwrite symbol with the new (corrected) segment id
	; TODO: this is pretty heavy. make a label util to overwrite info
	ldxy #@namebuff
	JUMPMAIN lbl::set

@add:	ldxy #@namebuff
	JUMPMAIN lbl::add
.endproc

;*******************************************************************************
; LOAD LOCAL
; Adds the next LOCAL symbol in the open OBJECT file to the global symbol
; table.
; This is called during pass 1: the value stored is relative to the symbol's
; SEGMENT (see load_export).  The final value is produced by link's
; resolve_symbols once the SEGMENT origins are known.
; OUT:
;   - .C: set on error
.proc load_local
@namebuff=$120
	; read symbol name
	ldy #$00
	lda #'@'
	sta @namebuff
:	jsr krn::chrin
	sta @namebuff+1,y
	cmp #$00
	beq @cont
	iny
	cpy #MAX_LABEL_NAME_LEN-1	; -1 for the '@' prefix
	bcc :-
	RETURN_ERR ERR_LABEL_TOO_LONG	; corrupt object file

@cont:	jsr load_symbol_value
	bcs @ret

	ldxy #@namebuff
	JUMPMAIN lbl::add
@ret:	rts
.endproc

;*******************************************************************************
; LOAD SYMBOL VALUE
; Reads the SEGMENT id and offset for a symbol from the open OBJECT file and
; stores the corresponding address mode, global SEGMENT id, and
; segment-relative value to zp::label_mode/zp::label_segmentid/zp::label_value
; (as consumed by lbl::add/lbl::set)
.proc load_symbol_value
@offset=r0
	jsr krn::chrin				; get SEGMENT id
	cmp #SEG_FLOAT_PACKED
	beq @float
	cmp #SEG_ABS				; is ID $FF (ABS)?
	bne @rel				; if not, resolve segment base

	; ABS symbols have no segment base; their value is absolute
	sta zp::label_segmentid
	lda #$01				; absolute address mode
	sta zp::label_mode
	lda #$00
	sta @offset
	sta @offset+1
	beq @value				; branch always

@rel:	; find the global segment id from the object-local one
	; In particular, SEG_FLOAT is a transient pool handle, not an index.
	cmp #$01
	bcc @badsegment
	cmp #MAX_FRAGMENTS+1
	bcs @badsegment
	cmp numsegments
	bcc :+
	beq :+
@badsegment:
	RETURN_ERR ERR_UNKNOWN_SEGMENT
:
	tax
	lda segments_type-1,x
	jsr type_to_mode
	sta zp::label_mode			; set address mode for label
	lda __obj_fragment_ids-1,x		; global fragment til
						; layout resolves it
	sta zp::label_segmentid
	lda #$00
	sta @offset
	sta @offset+1

@value:	; store the segment-relative value (obj offset in seg + offset)
	jsr krn::chrin				; get LSB of symbol offset
	clc
	adc @offset
	sta zp::label_value
	php
	jsr krn::chrin				; get MSB of symbol offset
	plp
	adc @offset+1
	sta zp::label_value+1
	jmp load_symbol_location

@float:
	CALL FINAL_BANK_EXPR, expr::fconst_read
	bcs @ret
	stxy zp::label_value
	lda #SEG_FLOAT
	sta zp::label_segmentid
	lda #$00
	sta zp::label_mode
	jmp load_symbol_location
@ret:	rts
.endproc

;*******************************************************************************
; LOAD SYMBOL LOCATION
; Load and translate the source location after an integer or float value.
.proc load_symbol_location
	jsr @getb
	sta zp::label_fileid
	jsr @getb
	sta zp::label_lineno
	jsr @getb
	sta zp::label_lineno+1
	ora zp::label_lineno
	bne @map
	lda #$ff		; no definition location: no file either
	sta zp::label_fileid
	bne @done		; branch always
@map:	lda zp::label_fileid
	CALL FINAL_BANK_DEBUG, dbgi::globalfile
	bcs @ret
	sta zp::label_fileid
@done:	clc
@ret:	rts
@getb:	jsr krn::readst
	bne @truncated
	jmp krn::chrin
@truncated:
	pla
	pla
	RETURN_ERR ERR_IO_ERROR
.endproc

;*******************************************************************************
; GET SEGMENT BASE
; Returns the base address for the given SEGMENT at link time
; NOTE: all indexing in this procedure is relative to table-1 because segment
; id's are 1-based.
; IN:
;   - .A: the ID of the SEGMENT to get the current base address of
; OUT:
;   - .XY: the base address of the section
.proc get_segment_base
@tmp=r0
	; segments_start[segment_id]
	tax
	ldy segments_starthi-1,x
	lda segments_startlo-1,x
	tax
	rts
.endproc

;*******************************************************************************
; LOAD
; Effectively links the object file.
; The global symbol table is expected to be built (labels created for any
; IMPORTs used in the program being linked) as well as the global segment
; layout.
.export __obj_load
.proc __obj_load
@name=r6
@addr=r6
@symcnt=r8
@sz=r8
@symaddr=ra
@symoff=rc
@seg=re
@namebuff=$100
@i=zp::tmp10
@symid=zp::tmp12
	jsr load_info
	bcc :+
@ret:	rts

:	iszero numimports	; are there any IMPORTS?
	beq @exports		; if not, skip ahead

;-------------------------------------------------------------------------------
; read IMPORTs and map them to their object-local ids
	lda #$00
	sta @i
	sta @i+1
@load_imports:
	; get the name of a symbol
	ldy #$00
:	jsr krn::chrin
	sta @namebuff,y
	cmp #$00
	beq @mapimport
	iny
	bne :-

@mapimport:
	; look up the import's fully resolved address by its name
	ldxy #@namebuff
	CALLMAIN lbl::find	; find label ID by name
	bcs @ret
	stxy @symid		; save the label id

	; make sure the symbol was defined (EXPORTed by some object file)
	CALLMAIN lbl::getsegment
	cmp #SEG_UNDEF		; still undefined?
	jeq @undef		; if so -> error
	cmp #SEG_FLOAT
	bne :+
	RETURN_ERR ERR_INVALID_EXPRESSION ; addresses cannot relocate to a float
:

	; store the resolved (GLOBAL) id for this symbol's index (LOCAL id)
	ldy @i
	lda @symid+1
	sta import_label_idshi,y
	lda @symid
	sta import_label_idslo,y

	jsr krn::chrin		; eat info byte

	incw @i
	ldxy @i
	cmpw numimports
	bne @load_imports	; repeat for all IMPORTS

;-------------------------------------------------------------------------------
; skip over the EXPORTS and LOCALS (these were loaded in pass 1 by
; load_headers and finalized by the linker's resolve_symbols)
@exports:
	lda #$00
	sta @i
	cmp numexports
	beq @locals

@skip_export:
	jsr @eat_symbol		; skip name, segment id, and offset
	inc @i
	lda @i
	cmp numexports
	bne @skip_export

@locals:
	ldxy numlocals
	stxy @i			; 16-bit counter for LOCALs
@skip_local:
	iszero @i
	beq @load_segments
	jsr @eat_symbol		; skip name, segment id, and offset
	decw @i
	jmp @skip_local

;-------------------------------------------------------------------------------
; done with symbols, now load all the SEGMENT information to get the sizes
; of each table we will need to walk
@load_segments:
	lda #$00
	sta seg_idx
	lda numsegments
	sta seg_cnt
	jeq @dbginfo		; no segments; continue to the debug info

@load_segment:
	ldx seg_idx
	lda __obj_fragment_ids,x
	jsr link::pad_fragment
	jsr krn::chrin			; eat "info" byte for SEGMENT
	pha

	; read the table sizes for this SEGMENT
	ldy seg_idx
	jsr krn::chrin			; get code size LSB
	sta segments_sizelo,y
	sta @sz
	jsr krn::chrin			; get code size MSB
	sta segments_sizehi,y
	sta @sz+1

	; log the SEGMENT name and table sizes for it
	ldx seg_idx
	inx				; +1 (SEGMENTs are 1-based)
	txa
	jsr __obj_get_segment_name_by_id
	jsr log_msg

	pla
	pha
	jsr is_bss
	bne :+
	ldxy #@reloc_na
	jsr log_msg
	jmp @obj

:	jsr krn::chrin
	pha
	ldy seg_idx
	sta segments_relocsizelo,y	; get relocation table size LSB
	jsr krn::chrin
	pha
	sta segments_relocsizehi,y	; get relocation table size MSB

	ldxy #@reloc_log
	jsr log_msg

@obj:	; get the address to write the object code to
	ldx seg_idx
	inx			; get in base 1
	txa
	jsr get_segment_base
	stxy @seg

	; get the stop address for logging
	txa
	clc
	adc @sz
	pha
	tya
	adc @sz+1
	pha

	; push the start address for logging
	lda @seg
	pha
	lda @seg+1
	pha
	ldxy #@obj_log
	jsr log_msg

	; check segment TYPE, if it is BSS, no obj/relocation code to load
	pla			; restore TYPE byte
	jsr is_bss		; BSS or BSSZP?
	beq @next_seg		; if so, skip to the next SEGMENT

	; if the segment is empty, there is no object code to load
	iszero @sz
	beq @reltab

@objcode:
	; finally, load the object code for the segment to vmem
	jsr krn::chrin
	ldxy @seg		; address to store to
	jsr vmem_store		; store a byte of object code
	incw @seg

	lda @sz
	bne :+
	dec @sz+1
:	dec @sz
	bne @objcode
	lda @sz+1
	bne @objcode

@reltab:
	lda seg_idx
	jsr apply_relocation		; load/apply relocation table
	jcs @ret			; propagate relocation errors

@next_seg:
	inc seg_idx
	dec seg_cnt			; decrement segment counter
	jne @load_segment		; repeat for all segments

;-------------------------------------------------------------------------------
; finally, link the debug information for the object file
@dbginfo:
	lda #$01				; flag to apply relocation
	CALL FINAL_BANK_DEBUG, dbgi::load	; load debug info
	bcs @dbgierr				; propagate debug info errors

;-------------------------------------------------------------------------------
@done:	RETURN_OK

@undef:	RETURN_ERR ERR_IMPORT_UNDEFINED
@dbgierr:
	sec
	rts

;-------------------------------------------------------------------------------
; Reads past a symbol record (name, typed value, file ID, source line).
@eat_symbol:
:	jsr krn::chrin		; read past the name
	cmp #$00
	bne :-
	jsr krn::chrin		; skip the segment id
	cmp #SEG_FLOAT_PACKED
	bne :+
	jsr krn::chrin
	jsr krn::chrin
	jsr krn::chrin
:
	jsr krn::chrin		; skip the offset LSB
	jsr krn::chrin		; skip the offset MSB
	jsr krn::chrin		; skip file ID
	jsr krn::chrin		; skip line LSB
	jmp krn::chrin		; skip line MSB (and return)

;-------------------------------------------------------------------------------
; object code: $xxxx-$xxxx
@obj_log: .byte "  object code: $", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0

; relocation: $xxxx bytes"
@reloc_log: .byte "  relocation:  $", ESCAPE_VALUE, " bytes",0

; n/a (used for relocation for BSS segments)
@reloc_na: .byte "  relocation:  n/a",0
.endproc

;*******************************************************************************
; GET SEGMENT NAME BY ID
; Returns the (object-local) name of the segment from its id
; IN:
;  - .A: the id of the SEGMENT to get the name of
; OUT:
;  - .XY: the name of the SEGMENT (object-local)
.export __obj_get_segment_name_by_id
.proc __obj_get_segment_name_by_id
@seg=r0
	ldx #$00
	stx @seg
	asl
	rol @seg
	asl
	rol @seg
	asl					; *8 (MAX_SEGMENT_NAME_LEN)
	rol @seg
	adc #<(segments-(1*MAX_SEGMENT_NAME_LEN))
	tax
	lda @seg
	adc #>(segments-(1*MAX_SEGMENT_NAME_LEN))
	tay
	;clc
	rts
.endproc

;*******************************************************************************
; GET SEGMENT BY NAME
; Returns the ID of the segment from its name
; IN:
;  - .XY: the name of the segment
; OUT:
;  - .A:  the ID of the segment
;  - .XY: if not found, address of the next available SEGMENT name
;  - .C:  set if no segment exists by the given name
.proc get_segment_by_name
@name=zp::str0
@other=zp::str2
@cnt=r0
@latest=r1
	stxy @name
	ldxy #segments
	stxy @other
	lda #$00
	sta @cnt
	sta @latest
	cmp numsegments
	beq @end
@loop:	jsr strcmp
	bne @next
	lda @cnt
	clc
	adc #$01
	sta @latest
@next:	lda @other
	clc
	adc #MAX_SEGMENT_NAME_LEN
	sta @other
	bcc :+
	inc @other+1
:	inc @cnt
	lda @cnt
	cmp numsegments
	bcc @loop
@end:	ldxy @other
	lda @latest
	beq @missing
	clc
	rts
@missing:
	sec
	rts
.endproc

;*******************************************************************************
; STRCMP
; Compares the strings in (zp::str0) and (zp::str2) up to a length of .A
; IN:
;  zp::str0: one of the strings to compare
;  zp::str1: the other string to compare
; OUT:
;  .Z: set if the strings are equal
.proc strcmp
	ldy #$00
@l0:	lda (zp::str0),y
	beq :+
	jsr is_ws
	beq :+
	cmp (zp::str2),y
	bne @ret
	iny
	bne @l0

:	lda (zp::str2),y	; make sure strings terminate at same index
@ret:	rts
.endproc

;*******************************************************************************
; VMEM LOAD
; Calls vmem::load
.proc vmem_load
	JUMPMAIN vmem::load
.endproc

;*******************************************************************************
; VMEM STORE
; Calls vmem::store
.proc vmem_store
	JUMPMAIN vmem::store
.endproc

;*******************************************************************************
; INLINE HELPERS
inline_proc is_ws, util::is_whitespace

;*******************************************************************************
; IS BSS
; Checks if the given TYPE represents a BSS segment (BSS or BSSZP)
; IN:
;   - .A: type byte to check
; OUT:
;   - .Z: set if the given segment is a BSS one
.proc is_bss
	cmp #TYPE_BSS
	beq @done
	cmp #TYPE_BSSZP
@done:	rts
.endproc

;*******************************************************************************
; TYPE TO MODE
; Returns the label address mode that corresponds to the given TYPE
; IN:
;   - .A: the segment TYPE to get the address mode for (e.g. TYPE_BSS)
; OUT:
;   - .A: the corresponding MODE (*ZP=0, others=1)
.proc type_to_mode
	cmp #TYPE_SEGZP
	beq @zp
	cmp #TYPE_BSSZP
	beq @zp
@abs:	lda #$01
	rts
@zp:	lda #$00
	rts
.endproc

;*******************************************************************************
; LOG STATE
; Emits the active state of the assembled object metadata to the log.
; This is used to inform the user about the number/size of their segments after
; assembly, etc.
.export __obj_log_state
.proc __obj_log_state
	jsr log_segments
	jsr log_symbols

	rts
.endproc

;*******************************************************************************
; LOG SEGMENTS
; Logs the relative and absolute segments in the current object state
.proc log_segments
@i    = zp::link
@buff = r0
@name = $110
@numrel = zp::tmp10
@numabs = zp::tmp12
	; count number of ABS and number of REL segments
	ldx numsegments
	bne @cont
	rts

@cont:	jsr log_banner
	ldxy #@segments
	jsr log_msg
	jsr log_banner

	lda #$00
	sta @numrel
	sta @numabs

@count:	lda segments_type-1,x
	cmp #TYPE_ABS		; is this an ABS segment?
	bne :+			; if not, skip it
	inc @numabs
	bne :++
:	inc @numrel
:	dex
	bne @count

	lda @numabs
	beq @rel

	; output all absolute segments
	ldxy #@abs_title
	jsr log_msg

	lda #$00
	sta @i
@abs:	ldx @i
	lda segments_type,x
	cmp #TYPE_ABS		; is this an ABS segment?
	bne :+			; if not, skip it

	; push stop address (start + size) then start address
	lda segments_startlo,x
	clc
	adc segments_sizelo,x
	pha
	lda segments_starthi,x
	adc segments_sizehi,x
	pha

	lda segments_startlo,x
	pha
	lda segments_starthi,x
	pha
	ldxy #@abs_seg
	jsr log_msg		; write segment range to log

:	inc @i
	lda @i
	cmp numsegments
	bne @abs

	lda @numrel
	beq @done

	; output all REL segments
	ldxy #@rel_title
	jsr log_msg

	lda #$00
	sta @i
@rel:	ldx @i
	lda segments_type,x
	cmp #TYPE_ABS		; is this an ABS segment?
	beq @next		; if so, skip it

	; copy name to buffer
	lda @i
	clc
	adc #$01		; get 1-based ID
	jsr __obj_get_segment_name_by_id
	stxy @buff

	ldy #MAX_SEGMENT_NAME_LEN-1
:	lda (@buff),y
	sta @name,y
	dey
	bpl :-

	; push the size of this SEGMENT
	ldx @i
	lda segments_sizelo,x
	pha
	lda segments_sizehi,x
	pha

	; push address of name
	lda #>@name
	pha
	lda #<@name
	pha

	ldxy #@rel_seg
	jsr log_msg		; write section range to log

@next:	inc @i
	lda @i
	cmp numsegments
	bne @rel
@done:	rts

;-------------------------------------------------------------------------------
@segments:
.ifdef hard8x8
.byte ESCAPE_SPACING,8
.else
.byte ESCAPE_SPACING,15
.endif
.byte "segments",0

@abs_title: .byte "absolute segments:",0
@abs_seg:   .byte "$", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0
@rel_title: .byte "relative segments:",0
@rel_seg:   .byte ESCAPE_STRING, ": ", ESCAPE_VALUE, 0
.endproc

;*******************************************************************************
; LOG SYMBOLS
; Logs the number of LOCAL, IMPORT, and EXPORT symbols in the active object
; state.
.proc log_symbols
	jsr log_banner
	ldxy #@symbols
	jsr log_msg
	jsr log_banner

	lda numlocals
	pha
	lda numlocals+1
	pha
	ldxy #@locals
	jsr log_msg

	lda numimports
	pha
	lda numimports+1
	pha
	ldxy #@imports
	jsr log_msg

	lda numexports
	pha
	lda #$00
	pha
	ldxy #@exports
	jsr log_msg
	jmp log_banner

;-------------------------------------------------------------------------------
@symbols:
.ifdef hard8x8
.byte ESCAPE_SPACING,8
.else
.byte ESCAPE_SPACING,15
.endif
.byte "symbols",0
@locals:  .byte "locals:  ", ESCAPE_VALUE_DEC,0
@imports: .byte "imports: ", ESCAPE_VALUE_DEC,0
@exports: .byte "exports: ", ESCAPE_VALUE_DEC,0
.endproc

;*******************************************************************************
; LOG BANNER
; Logs a '*' banner
.proc log_banner
	JUMPMAIN log::banner
.endproc

;*******************************************************************************
; LOG MSG
; Copies the provided string to shared RAM and logs it
; IN:
;   - .XY: address of string to log
.proc log_msg
@ret=r4
@str=r4
@buff=$100
	stxy @str

	; copy the string to RAM
	ldy #$ff
:	iny
	lda (@str),y
	sta @buff,y
	cmp #$00
	bne :-

	pla
	sta @ret
	pla
	sta @ret+1

	ldxy #@buff
	RENDER_STR			; render the string
	CALLMAIN log::out		; and log it

	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc
