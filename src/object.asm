;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;
; OBJECT FILE FORMAT:
; NUM SEGMENTS    [0]      ; number of segments in object file
; NUM_EXPORTS     [1]      ; number of symbols exported in object file
; NUM_IMPORTS     [2:3]    ; number of symbols imported by object file
; NUM_LOCALS      [4:5]	   ; number of LOCAL symbols in object file
; SEGMENT HEADERS [6:...]  ; headers for each SEGMENT
;  NAME  [$0:$7] ; segment name or $0000 for ABSOLUTE (.org derived segments)
;  ALIGN [$8:$9] ; offset of data (RELATIVE) or aboslute position (ABSOLUTE)
;  SIZE  [$a:$b] ; bytes used in segment
; IMPORTS[]
;   NAME
;   INDEX                  ; object-local id for the import
; EXPORTS[]
;   NAME
;   SEGMENT ID
;   RELATIVE ADDR
; LOCALS[]
;   NAME[...]
;   SEGMENT ID[1]
;   RELATIVE ADDR[2]
; SEGMENT TABLES:
;   OBJCODE
; DEBUGINFO
;   HEADERS
;   PROGRAM
;*******************************************************************************

.include "asm.inc"
.include "debuginfo.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "kernal.inc"
.include "labels.inc"
.include "log.inc"
.include "limits.inc"
.include "linker.inc"
.include "macros.inc"
.include "ram.inc"
.include "target.inc"
.include "text.inc"
.include "vmem.inc"
.include "zeropage.inc"

.macpack longbranch

;*******************************************************************************
; CONSTANTS (see limits.inc for others)
MAX_SECTIONS         = 8	; max number of memory sections per OBJ file
MAX_OBJS             = 16	; max number of object files that may be used
MAX_SECTION_NAME_LEN = 8	; max length of a single section name
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
; SYMBOL INDEX MAP
; Each entry in this array contains the index that we will map the corresponding
; label to (see labels.asm)
; This lets us emit a more compact list of only symbols that are used
; If the symbol is unused, we store $ff
; The indexes in this array represent the id of the symbol at assembly time
symbol_index_map: .res MAX_LABELS*2

;*******************************************************************************
; SYMBOL INFO
; This table contains the fully resolved addresses for each symbol index used
; in the object file.
symbol_addresses: .res MAX_IMPORTS+MAX_EXPORTS

;*******************************************************************************
; SEG IDX, SEG CNT
; Variables used during object file loading
seg_idx: .byte 0
seg_cnt: .byte 0

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

.export __obj_numsections
__obj_numsections:
numsections: .byte 0	; number of sections in obj file being written/read

.export __obj_filename
__obj_filename: .word 0	; pointer to name of object file being loaded

import_indexeshi:   .res MAX_EXPORTS	; MSBs for index (in symbol_index_map)
import_indexeslo:   .res MAX_EXPORTS	; LSBs for index (in symbol_index_map)

;*******************************************************************************
; NUM SYMBOLS MAPPED
; The number of symbols to store to the symbol table.
; Also the index that the next mapped symbol will be stored at
num_symbols_mapped: .word 0

num_reloctables_mapped: .byte 0

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS
__obj_sections_sizelo:
sections_sizelo:   .res MAX_SECTIONS
__obj_sections_sizehi:
sections_sizehi:   .res MAX_SECTIONS
__obj_segments_sizelo:
segments_sizelo:   .res MAX_SECTIONS
__obj_segments_sizehi:
segments_sizehi:   .res MAX_SECTIONS
segments_info:     .res MAX_SECTIONS

__obj_segments:
segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG

; link-time start addresses of each SEGMENT
segments_startlo:      .res MAX_SECTIONS
segments_starthi:      .res MAX_SECTIONS

; SEGMENT id for each SECTION
segment_ids: .res MAX_SECTIONS

; relocation table offsets/sizes for each section
sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS
segments_relocsizelo:  .res MAX_SEGMENTS
segments_relocsizehi:  .res MAX_SEGMENTS

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

.ifdef vic20
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

.segment "OBJCODE"

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
	sta num_symbols_mapped
	sta num_symbols_mapped+1
	sta num_reloctables_mapped

	; clear arrays
	ldx #MAX_SEGMENTS
@clrsizes:
	sta segments_sizelo-1,x
	sta segments_sizehi-1,x
	sta sections_sizelo-1,x
	sta sections_sizehi-1,x
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

	lda numsegments
	asl
	asl
	asl			; * MAX_SEGMENT_NAME_LEN
	adc #<segments
	sta @dst
	lda #>segments
	adc #$00
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

@ok:	lda numsegments
	cmp #MAX_SEGMENTS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	inc numsegments
	lda numsegments		; get 1-based section ID
	clc			; ok

	RETURN_OK
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
;   - .A:             address mode: 0=ZP relocate, 1=ABS relocate, $FF=ABS
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

	; get SEGMENT id and address for this section
	ldx num_reloctables_mapped
	lda segment_ids,x		; segment id
	pha
	ldy sections_starthi,x		; section start LSB
	lda sections_startlo,x		; section start MSB
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
	lda zp::asmresult
	sta sections_startlo,x	; set obj section start LSB
	lda zp::asmresult+1
	sta sections_starthi,x	; set obj section start MSB

	lda @info
	cmp #SEG_ABS
	beq @abs	; if ABS, continue to create a new segment

	; is there already a SEGMENT by this name?
	ldxy #@name
	jsr get_segment_by_name
	bcs @add		; new name, add a new SEGMENT

; existing SEGMENT, start section where the SEGMENT left off
@get:	pha
	ldy numsections
	sta segment_ids,y

	tax

	; set the start address for this SECTION to the current size of
	; its SEGMENT
	lda segments_sizehi-1,x
	sta sections_starthi,y
	pha			; LSB of SEGMENTS current top
	lda segments_sizelo-1,x
	sta sections_startlo,y
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
	cmp #SEG_ABS
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
	sta sections_sizehi-1,x

	; store INFO byte (address mode) for the SEGMENT
	lda @info
	sta segments_info-1,x
	cmp #SEG_ABS
	beq :+

	; init REL segments start address to 0
	lda #$00
	sta segments_startlo-1,x
	sta segments_starthi-1,x

:	ldx numsections
	pla			; restore SEGMENT id
	sta segment_ids,x

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
	ldy segment_ids-1,x
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
	CALLMAIN lbl::find	; look up the label by name
	bcs @ret			; not found -> err
	txa
	ldx numexports
	sta export_label_idslo,x	; get LSB of index for symbol
	tya
	sta export_label_idshi,x	; get MSB of index for symbol
	inc numexports
	clc				; ok
@ret:	rts
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
	sta @sz
	sty @offset

	lda expr::kind
	cmp #VAL_REL
	bne @ok		; expression doesn't require relocation

	ldxy reloctop
	cmpw #(reloc_tables_end-4)
	bcc :+
	RETURN_ERR ERR_OOM

:	stxy @rel

; encode the "info" byte for the relocation based on the result of the
; expression evaluation and the size of the relocation
;  field   bit(s)   description
; size       0   size of target value to modify 0=1 byte, 1=2 bytes
; mode       1   type of relocation: 1=section-relative, 0=symbol-relative
; postproc  2-3  post-processing (0=NONE, 1=LSB, 2=MSB)
@encode_size:
	lda expr::postproc
	asl
	asl
	ora @sz

	; is symbol in the expression is unresolved (section_id == SEG_UNDEF)?
	; yes -> use symbol-based relocation
	; no  -> use segment-based relocation
	ldx expr::segment
	cpx #SEG_UNDEF
	beq :+
	ora #1<<1		; flag section based relocation

:	ldy #$00
	pha			; save info byte
	STOREB_Y @rel		; write info byte

	; write offset in obj file (current "assembly" address)
	iny			; .Y=1
	lda zp::asmresult	; write offset LSB
	clc
	adc @offset
	STOREB_Y @rel
	iny			; .Y=2
	lda zp::asmresult+1
	adc #$00
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
	lda expr::symbol
	STOREB_Y @rel		; write symbol-id LSB
	lda expr::symbol+1
	iny			; .Y=4
	STOREB_Y @rel		; write symbol-id MSB

@done:  ; update reloctop
	lda expr::postproc
	cmp #$01		; set .C if post-processing is used
	lda #$05
	adc reloctop		; +5 (no post-proc), +6 (post-proc)
	sta reloctop
	bcc @ok
	inc reloctop+1
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; BUILD SYMBOL INDEX MAP
; Constructs the map of symbols to the index to store for them in the symbol
; table.  Also replaces the ID's in the relocation table with the indices with
; the indices in the IMPORTS table.
.proc build_symbol_index_map
@idx=r0
@symtab=r2
@reltab=r4
	; walk the relocation tables to determine which symbols are referenced
	; in relocations. only these will be emitted.
	ldxy #reloc_tables
	stxy @reltab
	cmpw reloctop
	bne :+
	rts		; no relocations

:	; initialize symbol index map to all $ff's (unmapped)
	ldxy #symbol_index_map
	stxy @symtab

	lda #$ff
@init:	ldy #$00
	sty @idx
	sty @idx+1
	STOREB_Y @symtab
	incw @symtab
	ldxy @symtab
	cmpw #symbol_index_map+(MAX_LABELS*2)
	bcc @init

@l0:	ldy #$00
	sty @symtab+1
	LOADB_Y @reltab
	pha			; save info byte

	and #$02		; mask mode bit
	bne @next		; if 1-> not a symbol-based relocation

	; get position of symbol (symbol_index_map + id*2)
	LOADB_Y @reltab		; get symbol ID (LSB)
	asl
	rol @symtab+1
	adc #<symbol_index_map
	sta @symtab
	iny
	LOADB_Y @reltab		; symbol ID (MSB)
	adc #>symbol_index_map
	sta @symtab+1

	; check if symbol is already mapped
	ldy #$00
	LOADB_Y @symtab
	cmp #$ff
	bne @update_rel		; not $ffff (already mapped) -> update table
	iny
	LOADB_Y @symtab
	cmp #$ff
	bne @update_rel		; not $ffff (already mapped) -> update table

	; not mapped, assign this symbol the next available index
	lda @idx
	dey			; .Y=0
	STOREB_Y @symtab
	iny
	lda @idx+1
	STOREB_Y @symtab
	incw @idx
	incw num_symbols_mapped

@update_rel:
	; rewrite the relocation table's stored index with the mapped index
	ldy #$00
	LOADB_Y @symtab
	ldy #$03
	STOREB_Y @reltab
	ldy #$01
	LOADB_Y @symtab
	ldy #$04
	STOREB_Y @reltab

@next:	pla			; restore info byte
	and #$0c		; mask postproc bits (2, 3)
	cmp #$01		; set .C if post-processing is used
	lda @reltab
	adc #$05		; +5 (no post-proc), +6 (post-proc)
	tax
	sta @reltab
	bcc :+
	inc @reltab+1
:	ldy @reltab+1
	cmpw reloctop
	jcc @l0
@done:	rts
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
@i=r0
@idx=r2
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
	ldy import_indexeshi,x		; get LSB of index for symbol
	sty @idx+1
	lda import_indexeslo,y		; get MSB of index for symbol
	sta @idx
	CALLMAIN lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr krn::chrout
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; write the object-local index for the import
	lda @idx	; restore index LSB
	jsr krn::chrout	; and write it
	ldy @idx	; and MSB
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
	CALLMAIN lbl::getsegment	; get SEGMENT id
	jsr krn::chrout			; write SEGMENT id
	ldxy @id

	; look up symbol's (segment-relative) address and write it
	CALLMAIN lbl::getaddr
	txa
	jsr krn::chrout			; write offset LSB
	tya
	jsr krn::chrout			; write offset MSB

	inc @i
	lda @i
	cmp numexports
	bcc @l0

@done:	rts
.endproc

;*******************************************************************************
; DUMP LOCALS
; Dumps the local (not-export, not-import) symbols to the open object file
.proc dump_locals
@id=zp::tmp12
@buff=$100
	lda #$00
	sta @id
	sta @id+1

	iszero lbl::num
	beq @done			; if no exports -> done

@l0:	; check if the label is already dumped as an export and skip it if so
	jsr @isexport
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
	CALLMAIN lbl::getsegment	; get SEGMENT id
	jsr krn::chrout			; write SEGMENT id
	ldxy @id

	; look up symbol's (segment-relative) address and write it
	CALLMAIN lbl::getaddr
	txa
	jsr krn::chrout			; write offset LSB
	tya
	jsr krn::chrout			; write offset MSB

@next:	incw @id
	ldxy @id
	cmpw lbl::num
	bne @l0

@done:	rts

;-------------------------------------------------------------------------------
; Checks if the given label ID is an EXPORT, in which case we don't need to dump it
@isexport:
	ldx numexports
	beq @notexport

@l1:	lda @id
	cmp export_label_idshi-1,x	; get LSB of index for symbol
	bne :+
	lda @id+1
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
; compute the size of each SEGMENT (sum of all SECTIONS that use it) and size
; of all relocation tables
@l0:	lda #$00
	sta @sec_idx

; iterate over all SECTIONS and check if they're in this SEGMENT
@l1:	inc @seg_idx
	lda @seg_idx			; get current SEGMENT
	clc
	ldx @sec_idx
	cmp segment_ids,x		; is this SECTION in this SEGMENT?
	bne :+				; if not, continue

	tay				; .Y=segment_id for section
	lda sections_relocsizelo,x	; get size of code for section
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
	inc @seg_idx
	cmp numsegments
	bne @l0

	lda #$00
	sta @i
@dump_headers:
	; get offset to name for this section name (*8)
	lda @i
	asl
	asl
	asl
	adc #<segments
	sta @name
	lda #>segments
	adc #$00
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

	; write the number of bytes used for this SEGMENT (2 bytes)
	lda __obj_segments_sizelo,x
	jsr krn::chrout
	lda __obj_segments_sizehi,x
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
	lda segments_info,x
	jsr krn::chrout

	; write the size of the SEGMENT
	lda __obj_segments_sizelo,x
	jsr krn::chrout
	lda __obj_segments_sizehi,x
	jsr krn::chrout

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
	cmp segment_ids,x		; is our SECTION part of the SEGMENT?
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
	cmp segment_ids,x		; is our SECTION part of the SEGMENT?
	bne @reloc_next			; not our SEGMENT, try next SECTION

	; get start address of SECTION to dump
	lda sections_startlo,x
	sta @sec
	lda sections_starthi,x
	sta @sec+1

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
	; write the main OBJ header
	jsr build_symbol_index_map

	lda numsegments			; # of segments
	jsr krn::chrout
	lda numexports			; # of EXPORTS
	jsr krn::chrout
	lda numimports			; # of IMPORTS (LSB)
	jsr krn::chrout
	lda numimports+1		; # of IMPORTS (MSB)
	jsr krn::chrout

	; number of locals is total labels - (numexports+numimports)
	lda numexports
	clc
	adc numimports
	sta @tmp
	lda numexports+1
	adc numimports
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
	jsr dump_locals

	; write each SEGMENT (object code, relocation data)
	jsr dump_segment_tables

	; lastly, write the debug info for the object file
	CALL FINAL_BANK_DEBUG, dbgi::dump

	RETURN_OK
.endproc

;*******************************************************************************
; APPLY RELOCATION
; Produces the final binary for the object table for the given SEGMENT
; IN:
;   - .A: id of SEGMENT to apply relocation for
; OUT:
;   - .C: set if there is no remaining relocation to apply for the section
.proc apply_relocation
@symbol_id=r0
@symbol_addr=r0
@tmp=r0
@addrmode=r2
@pc=r4
@rec=r7
@sz=re
@seg_base=zp::tmp10
	tax				; .X=id (index)
	lda segments_relocsizelo,x
	sta @sz
	lda segments_relocsizehi,x
	sta @sz+1
	iszero @sz
	bne :+
	RETURN_OK

:	inx				; get 1-based section
	txa
	jsr get_segment_base
	stxy @seg_base

@relocate:
	; read a record from the RELOCATION table
	ldy #$00
:	jsr krn::chrin	; read a byte
	sta @rec,y
	iny
	cpy #$05	; sizeof(relocation_record)
	bne :-

	; get the address mode (size of the target to update)
	lda @rec	; get the info byte
	and #$01	; mask size bit
	sta @addrmode	; and save address mode for later

	; check if we are using a symbol or another section as the base
	; address for the relocation
	lda @rec	; get the info byte again
	and #$02	; mask type bit
	bne @seg	; 1 = segment, 0 = symbol

@sym:	; apply (global) symbol based relocation
	; symbols are fully resolved by the time we apply relocation, so
	; just look up the address in symbol_addresses
	lda @rec+3		; get symbol LSB
	sta @symbol_id
	lda @rec+4		; get symbol MSB
	sta @symbol_id+1

	; look up the address that we resolved for this symbol id (index)
	lda @symbol_id
	clc
	adc #<symbol_addresses
	sta @symbol_addr
	lda @symbol_id+1
	adc #>symbol_addresses
	sta @symbol_addr+1
	ldy #$00
	LOADB_Y @symbol_addr
	tax
	iny
	LOADB_Y @symbol_addr
	tay
	jmp @add_offset		; continue to calculate target address

@seg:	; apply segment (local symbol) based relocation
	lda @rec+3		; 3 = index to SEGMENT ID

	; get the current address of the SEGMENT
	jsr get_segment_base

@add_offset:
	stxy @tmp		; save resolved symbol/section value

	; get the address of the byte/word to apply relocation to
	lda @seg_base		; current section base address (LSB)
	clc
	adc @rec+1		; add LSB of offset
	sta @pc
	lda @seg_base+1		; current section base address (MSB)
	adc @rec+2		; add MSB of offset
	sta @pc+1

@apply_addend:
	ldy #$00
	lda @addrmode		; get address mode
	beq @zp			; if 0 -> apply zeropage relocation

@abs:	ldxy @pc
	jsr vmem_load		; load LSB of addend
	clc
	adc @tmp
	ldxy @pc
	jsr vmem_store		; store updated value

	incw @pc
	ldxy @pc
	jsr vmem_load		; load MSB of addend
	clc
	adc @tmp+1
	ldxy @pc
	jsr vmem_store		; store MSB of relocated operand
	jmp @nopostproc_done

@zp:	ldxy @pc
	jsr vmem_load		; load addend
	clc
	adc @tmp
	sta @tmp

	lda @rec		; read INFO byte again
	and #$0c		; is any postproc needed (bit 2 or 3 set)?
	bne @postproc		; if so, continue to apply it

@nopostproc:
	; no post-processing, just add 1 byte addend and we're done
	lda @tmp
	jsr vmem_store		; store relocated value
@nopostproc_done:
	; update sz (sz -= 5)
	lda @sz
	sec
	sbc #$05
	sta @sz
	bcs @next
	dec @sz+1
	bcc @next		; branch always

@postproc:
	php			; save carry from LSB addition
	jsr krn::chrin		; read another byte to get the MSB of addend
	plp			; restore .C
	adc @tmp+1		; add with operand MSB
	sta @tmp+1

	; update sz (sz -= 6)
	lda @sz
	sec
	sbc #$06
	sta @sz
	bcs :+
	dec @sz+1

:	lda @rec		; get info again
	and #$0c		; mask postproc bits (2, 3)
	cmp #POSTPROC_LSB<<2	; are we taking LSB?
	beq @postproc_lsb

@postproc_msb:
	lda @tmp+1				; get the MSB (post-proc)
	ldxy @pc
	jsr vmem_store		; store relocated value
	jmp @next

@postproc_lsb:
	lda @tmp				; get the LSB ldxy @pc
	ldxy @pc
	jsr vmem_store		; and store
	jmp @next

@next:	iszero @sz
	jne @relocate

@done:	RETURN_OK
.endproc

;*******************************************************************************
; LOAD INFO
; Loads the first part of the object file and extracts basic info from it
; (e.g. number of symbols)
; OUT:
;   - .C: set on error
.proc load_info
@cnt=r0
@i=r4
@name=r6
@symoff=r8
@namebuff=$100
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
	cpy #MAX_SECTION_NAME_LEN
	bne @segname

	; read OFFSET (or literal start address if ABS) for SEGMENT
	ldy @i
	jsr krn::chrin
	sta segments_startlo,y
	jsr krn::chrin
	sta segments_starthi,y

	; get the number of bytes used in the SEGMENT
	jsr krn::chrin
	sta __obj_segments_sizelo,y
	jsr krn::chrin
	sta __obj_segments_sizehi,y

	; if ABS segment, directly set the SEGMENT start address
	ldy #$00
	lda (@name),y
	beq @next			; if ABS, already know start addr

@rel:	; for REL segments, get the base address of this SEGMENT in the linker
	; NOTE: this will be garbage in pass 1
	ldxy @name
	jsr link::segaddr_for_file_by_name

	; add the offset (always $0000 for now) to the SEGMENT
	tya
	pha
	ldy @i
	txa
	clc
	adc segments_startlo,y		; add offset
	sta segments_startlo,y		; store LSB of SEGMENT base
	pla
	adc segments_starthi,y		; get MSB of SEGMENT base
	sta segments_starthi,y		; store MSB of SEGMENT base

@next:	; move name pointer to next location
	lda @name
	clc
	adc #MAX_SECTION_NAME_LEN
	sta @name
	bcc :+
	inc @name+1

:	; increment counter and loop til we've done all SEGMENTS
	inc @i
	ldy @i
	cpy numsegments
	bne @load_segments

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
@cnt=r0
@name=r6
@i=zp::tmp10
@namebuff=$100
	jsr load_info

; read the IMPORTS and EXPORTS and add them to the global symbol table
@imports:
	lda #SEG_UNDEF
	sta zp::label_segmentid	; imports' section is UNDEFINED
	lda #$00
	sta @i
	cmp numimports
	beq @exports
@import_loop:
	jsr load_import
	inc @i
	lda @i
	cmp numimports
	bne @import_loop

@exports:
	; copy name to shared RAM
	ldxy __obj_filename
	stxy @name
	ldy #$00
:	lda (@name),y
	sta @namebuff,y
	beq :+
	iny
	bne :-

:	; prepend the filename as scope so that we know the file the symbol
	; was defined in when we resolve it
	ldxy #@namebuff
	CALLMAIN lbl::setscope

	lda #$00
	sta @i
	cmp numexports
	beq @ok

@export_loop:
	jsr load_export
	bcs @ret
	inc @i
	lda @i
	cmp numexports
	bne @export_loop

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
@namebuff=$100
	; get the name of a symbol
	ldx #$00
	stx zp::label_value
	stx zp::label_value+1
:	jsr readb
	bcs @ret
	sta @namebuff,x
	beq @cont
	inx
	bne :-

@cont:
	jsr readb				; get info byte (address mode)
	bcs @ret
	sta zp::label_mode

	ldxy #@namebuff
	CALLMAIN lbl::find			; was label already added?
	bcs :+					; if no -> add it

	; validate: does address mode match existing symbol?
	CALLMAIN lbl::addrmode
	cmp zp::label_mode
	beq @ok					; matches -> ok

	; error: address mode doesn't match
	RETURN_ERR ERR_ADDRMODE_MISMATCH	; conflicting import/exports

:	JUMPMAIN lbl::add
@ok:	clc
@ret:
:	rts
.endproc

;*******************************************************************************
; LOAD EXPORT
; Adds the next EXPORT in the open OBJECT file to the global symbol table
; OUT:
;   - .C: set on error
.proc load_export
@namebuff=$120
	; get the name of a symbol
	ldx #$00
	lda #'@'
	sta @namebuff
:	jsr krn::chrin
	sta @namebuff+1,x
	beq @addexport
	inx
	bne :-

@addexport:
	jsr readb				; get SEGMENT id for EXPORT
	bcs @ret

	tax
	lda segments_info-1,x
	sta zp::label_mode			; set address mode for label

	ldxy @namebuff
	jsr __obj_get_segment_name_by_id	; get name of SEGMENT
	jsr link::segid_by_name			; get global id for SEGMENT
	sta zp::label_segmentid			; and store with the symbol
	jsr readb				; get LSB of symbol offset
	bcs @ret
	sta zp::label_value
	jsr readb				; get MSB of symbol offset
	bcs @ret
	sta zp::label_value+1

	ldxy #@namebuff
	CALLMAIN lbl::find			; was label already added?
	bcs @add				; no -> add it

	; validate: is segment SEG_UNDEF?
	; if not, error (only 1 EXPORT is allowed per symbol)
	CALLMAIN lbl::getsegment
	cmp #SEG_UNDEF
	beq :+
	RETURN_ERR ERR_ALREADY_EXPORTED		; multiple exports

:	; validate: does address mode match existing symbol?
	CALLMAIN lbl::addrmode
	cmp zp::label_mode
	bne @addr_mode_mismatch
@ok:	clc
@ret:	rts

@addr_mode_mismatch:
	RETURN_ERR ERR_ADDRMODE_MISMATCH	; conflicting addr modes

@add:	ldxy #@namebuff
	JUMPMAIN lbl::add
.endproc

;*******************************************************************************
; LOAD LOCAL
; Adds the next LOCAL symbol in the open OBJECT file to the global symbol table
; The current file ID is mapped to
; OUT:
;   - .C: set on error
.proc load_local
@namebuff=$120
	; read symbol name
	ldx #$00
	lda #'@'
	sta @namebuff
:	jsr krn::chrin
	sta @namebuff+1,x
	beq @add
	inx
	bne :-

@add:	jsr krn::chrin				; get SEGMENT id
	tax
	lda segments_info-1,x
	sta zp::label_mode			; set address mode for label

	ldxy @namebuff
	jsr __obj_get_segment_name_by_id	; get name of SEGMENT
	jsr link::segid_by_name			; get global id for SEGMENT
	sta zp::label_segmentid			; and store with the symbol
	jsr krn::chrin				; get LSB of symbol offset
	sta zp::label_value
	jsr krn::chrin				; get MSB of symbol offset
	sta zp::label_value+1

	ldxy #@namebuff
	JUMPMAIN lbl::add
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
	jsr load_info
	bcc :+
@ret:	rts

:	iszero numimports	; are there any IMPORTS?
	beq @eat_exports	; if not, skip ahead

;-------------------------------------------------------------------------------
; read IMPORTs and map them to their object-local ids
	lda #$00
	sta @i
@load_imports:
	; get the name of a symbol
	ldy #$00
:	jsr readb
	bcs @ret
	sta @namebuff,y
	beq @mapimport
	iny
	bne :-

@mapimport:
	; look up the import's fully resolved address by its name
	ldxy #@namebuff
	CALLMAIN lbl::find	; find label ID by name
	CALLMAIN lbl::addr	; get the resolved address
	stxy @addr

	jsr readb			; eat info byte
	bcs @ret

	; get pointer to the resolved address for this symbol's index
	lda #$00
	sta @symaddr+1
	lda @i
	asl
	rol @symaddr+1
	adc #<symbol_addresses
	sta @symaddr
	lda #$00
	adc #>symbol_addresses
	sta @symaddr+1

	; store the resolved address for this symbol's index
	ldy #$00
	lda @addr
	STOREB_Y @symaddr
	iny
	lda @addr
	STOREB_Y @symaddr

	incw @i
	ldxy @i
	cmp numimports
	bne @load_imports	; repeat for all IMPORTS

;-------------------------------------------------------------------------------
; EXPORTs are handled in first pass (before this procedure is called), skip em
@eat_exports:
	lda numexports
	sta @symcnt
	beq @load_locals	; no EXPORTS? skip ahead
@eat_export:
@eat_name:
	jsr readb
	bcc :+
@export_error:
	rts			; return error

:	cmp #$00		; did we read terminating 0 for filename?
	bne @eat_name		; loop til we found it
	ldy #3			; sizeof(info+segment_idx+address)
:	jsr readb
	bcs @export_error
	dey
	bne :-
	dec @symcnt
	bne @eat_export

;-------------------------------------------------------------------------------
; load LOCAL symbols
@load_locals:
	ldxy numlocals
	stxy @i
:	iszero @i
	beq @load_segments
	jsr load_local
	decw @i
	jmp :-

;-------------------------------------------------------------------------------
; done with symbols, now load all the SEGMENT information to get the sizes
; of each table we will need to walk
@load_segments:
	lda #$00
	sta seg_idx
	lda numsegments
	sta seg_cnt
	jeq @done

@load_segment:
	jsr krn::chrin			; get "info" byte for SEGMENT

	; TODO: make sure SEGMENT's address mode for SEGMENT matches linker's

	; read the table sizes for this SEGMENT
	ldy seg_idx
	jsr krn::chrin			; get code size LSB
	sta segments_sizelo,y
	sta @sz
	jsr krn::chrin			; get code size MSB
	sta segments_sizehi,y
	sta @sz+1

	jsr krn::chrin
	pha
	sta segments_relocsizelo,y	; get relocation table size LSB
	jsr krn::chrin
	pha
	sta segments_relocsizehi,y	; get relocation table size MSB

	; log the SEGMENT name and table sizes for it
	ldx seg_idx
	inx
	txa
	jsr __obj_get_segment_name_by_id
	jsr log_msg
	ldxy #@reloc_log
	jsr log_msg

	; get the address to write the object code to
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
	jsr apply_relocation			; load/apply relocation table
	lda #$01				; flag for dbgi::load (apply relocation)
	inc seg_idx
	dec seg_cnt				; decrement segment counter
	jne @load_segment			; repeat for all segments

;-------------------------------------------------------------------------------
; finally, link the debug information for the object file
	CALL FINAL_BANK_DEBUG, dbgi::load	; load debug info

;-------------------------------------------------------------------------------
@done:	clc
@eof:	rts

; object code: $xxxx-$xxxx
@obj_log: .byte "object code: $", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0

; relocation: $xxxx bytes"
@reloc_log: .byte "relocation: $", ESCAPE_VALUE, " bytes",0
.endproc

;******************************************************************************
; READB
; Reads a byte and checks for error (READST)
; OUT:
;   - .A: the byte read or error code (0=eof)
;   - .C: set on error/eof
.proc readb
	jsr krn::readst		; call READST (read status byte)
	bne @eof		; either EOF or read error
	jsr krn::chrin		; call CHRIN (get a byte from file)
	RETURN_OK

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	and #$40
	beq @err
	lda #$00	; EOF
	RETURN_OK

@err:	JUMPMAIN file::geterr
.endproc

;******************************************************************************
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

;******************************************************************************
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
	stxy @name
	ldxy #segments
	stxy @other

	lda #$00
	sta @cnt
	cmp numsegments
	beq @notfound

@l0:	lda #MAX_SEGMENT_NAME_LEN
	jsr strcmp
	beq @found
	lda @other
	clc
	adc #MAX_SEGMENT_NAME_LEN
	sta @other
	ldx @cnt
	inx
	stx @cnt
	cpx numsegments
	bcc @l0
@notfound:
	ldxy @other
	;sec
	rts

@found: lda @cnt
	clc
	adc #$01		; get 1-based id
	;clc
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
.ifdef vic20
.proc is_ws
	.include "inline/is_ws.asm"
.endproc
.else
	.include "util.inc"
	is_ws = util::is_whitespace
.endif

;*******************************************************************************
; SECTION STOP
; Gets the stop address for the given SECTION
; IN:
;   - .X: the index of the SECTION to get the address for
; OUT:
;   - .XY: stop address of the SECTION
.proc get_section_stop
	lda sections_startlo,x
	clc
	adc sections_sizelo,x
	pha
	lda sections_starthi,x
	adc sections_sizehi,x
	tay
	pla
	tax
	rts
.endproc

;*******************************************************************************
; LOG STATE
; Emits the active state of the assembled object metadata to the log.
; This is used to inform the user about the number/size of their segments after
; assembly, etc.
.export __obj_log_state
.proc __obj_log_state
@i    = zp::link
@buff = r0
@name = $100
	CALLMAIN log::banner

	; output all absolute segments
	ldxy #@abs_title
	CALLMAIN log::out

	lda #$00
	sta @i

@abs:	ldx @i
	lda segments_info,x
	cmp #SEG_ABS		; is this an ABS segment?
	bne :+			; if not, skip it

	; push stop address then start address
	jsr get_section_stop
	txa
	pha
	tya
	pha

	ldx @i
	lda sections_startlo,x
	pha
	lda sections_starthi,x
	pha
	ldxy #@abs_seg
.ifdef vic20
	CALLMAIN text::render_ind
.endif
	CALLMAIN log::out	; write section range to log

:	inc @i
	lda @i
	cmp numsegments
	bne @abs

	; output all REL segments
	ldxy #@rel_title
	CALLMAIN log::out

	lda #$00
	sta @i
@rel:	ldx @i
	lda segments_info,x
	cmp #SEG_ABS		; is this an ABS segment?
	beq @next		; if so, skip it

	; copy name to buffer
	lda @i
	clc
	adc #$01		; get 1-based ID
	jsr __obj_get_segment_name_by_id
	stxy @buff

	ldy #MAX_SECTION_NAME_LEN-1
:	lda (@buff),y
	sta @name,y
	dey
	bpl :-

	; push the size of this SECTION
	ldx @i
	lda sections_sizelo,x
	pha
	lda sections_sizehi,x
	pha

	; push address of name
	lda #>@name
	pha
	lda #<@name
	pha

	ldxy #@rel_seg
.ifdef vic20
	CALLMAIN text::render_ind
.endif
	CALLMAIN log::out	; write section range to log

@next:	inc @i
	lda @i
	cmp numsegments
	bne @rel

	CALLMAIN log::banner

	rts

.PUSHSEG
.RODATA
@abs_title: .byte "absolute segments",0
@abs_seg:   .byte "$", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0
@rel_title: .byte "relative segments",0
@rel_seg:   .byte ESCAPE_STRING, ": ", ESCAPE_VALUE, 0
.POPSEG
.endproc

;******************************************************************************
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
	CALLMAIN text::render_ind	; render the string
	CALLMAIN log::out		; and log it

	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc
