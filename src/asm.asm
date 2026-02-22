;*******************************************************************************
; ASM.ASM
; This file contains the entrypoint for the primary assembly procedure as well
; as much of the code used to assemble a given line of text to its binary
; representation.
;
; ASSEMBLER OVERVIEW
; The assembler operates in 2 passes.
; Pass 1:
;  - generate symbol table (label names and addresses)
;  - create macro definitions (.MAC)
;
; Pass 2:
;  - validate labels (make sure we correctly inferred their sizes if not
;    forward declared)
;  - write the program binary by assembling:
;   - instructions
;   - macro invocations
;   - .REP blocks
;   - directives like .DB and .DW
;
; There are (sensible) limitations due to the 2 pass nature of the assembler
;  1. Macros must be defined before first use
;  2. The size of all labels must be known (or correctly implied) in the first
;     pass. For example:
;     ```
;      LDA TARGET
;      LOOP:
;     ```
;     To generate the correct addresse for LOOP, the assembler needs to know if
;     TARGET is a zeropage or absolute address.
;     With insufficient data (e.g. forward references), labels are assumed to be
;     absolute (2 byte) addresses, which is usually a safe assumption unless
;     you're writing code in the zeropage.
;  3. Constants (.EQ) must be defined before first use (for similar reasons
;     that labels must be defined before use)
;     For example, consider this erroneous case:
;     ```
;       LDA ADDR
;       .EQ ADDR $10
;     ```
;     In this example, we don't know whether to use zeropage or absolute
;     addressing for the LDA, which could lead to incorrect addresses being
;     generated for the rest of the pass.
;     ```
;       .EQ ADDR $10
;       LDA ADDR
;     ```
;     In this ^ correct example, we know to use zeropage addressing and the
;     first and second passes will generate the same labels hereafter.
;
;     If an address is incorrectly implied in pass 1, there will be an error
;     generated when we validate it in pass 2
;*******************************************************************************

.include "asmflags.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "debuginfo.inc"
.include "errors.inc"
.include "errlog.inc"
.include "expr.inc"
.include "file.inc"
.include "kernal.inc"
.include "layout.inc"
.include "labels.inc"
.include "line.inc"
.include "linker.inc"
.include "macro.inc"
.include "macros.inc"
.include "memory.inc"
.include "object.inc"
.include "string.inc"
.include "text.inc"
.include "source.inc"
.include "util.inc"
.include "strings.inc"
.include "target.inc"
.include "vmem.inc"
.include "zeropage.inc"

.include "ram.inc"

;*******************************************************************************
MAX_IFS      = 4 ; max nesting depth for .if/.endif
MAX_CONTEXTS = 3 ; max nesting depth for contexts (activated by .MAC, .REP, etc)

;*******************************************************************************
; ASM INFORMATION
; These zeropage locations are filled with information after each call to
; asm::tokenize
indirect_hint = zp::asmtmp	; 1=indirect, 0=absolute
indexed       = zp::asmtmp+1	; 1=x-indexed, 2=y-indexed, 0=not indexed
immediate     = zp::asmtmp+2	; 1=immediate, 0=not immediate
operandsz     = zp::asmtmp+3	; size of the operand (in bytes)
				; $ff indicates 1 or 2 bytes
cc            = zp::asmtmp+4	; "cc" bits of opcode under construction
resulttype    = zp::asmtmp+5	; how to format line (ASM_COMMENT, etc.)
opcode        = zp::asmtmp+6	; opcode (if there was one)
operand       = zp::asmtmp+7	; operand (if there was one)

savereg       = zp::text

SEG_CODE = 1	; flag for CODE segment
SEG_BSS  = 2	; flag for BSS segment (all data must be 0, PC not updated)

.segment "BSS_NOINIT"
;*******************************************************************************
.export ifstack
ifstack:   .res MAX_IFS	; contains TRUE/FALSE values for the active IF blocks
ifstacksp: .byte 0	; stack pointer to "if" stack

.export __asm_pcset
__asm_pcset:
pcset: .byte 0

.export __asm_origin
__asm_origin:
origin: .word 0	; the lowest address in the program

.export __asm_top
__asm_top:
top: .word 0	; the highest address in the program

; TOKENIZE uses this as the line number to map the address of the assembled
; instruction to
.export __asm_linenum
__asm_linenum: .word 0

.segment "SHAREBSS"

;*******************************************************************************
; SEGMODE
; When assembling to object code (asm::mode != 0)
; this contains the size of labels defined in that segment
; 0=ZP, 1=ABS (anything but ZP), $FF means no segment is defined
.export __asm_segmode
__asm_segmode: .byte 0

.export __asm_segmentid
__asm_segmentid: .byte 0	; The current SEGMENT id (set by .SEG/.SEGZP)

;*******************************************************************************
; ASM MODE
; object assembly flag
; if !0 (object mode):
;   tokenize will assemble the given line as object code to the current
;   input file (file::open + CHKIN)
; if 0 (direct mode):
;   lines will be assembled directly to memory at the address in
;   zp::asmresult
.export __asm_mode
__asm_mode: .byte 0

;*******************************************************************************
; ASMBUFFER
; Source is copied here so that it can be messed with while assembling
.export asmbuffer
asmbuffer = mem::asmbuffer

.RODATA
;*******************************************************************************
NUM_OPCODES = 58
CC_00       = 0
CC_01       = 8
CC_10       = 16
CC_IMP      = 24
AAA_JMP     = $02
AAA_JMP_IND = $03

opcodes:
; cc = 00
.byte $ff,$ff,$ff ; unused
.byte "bit" ; 001 3
.byte "jmp" ; 010 4
.byte "jmp" ; 011 5
.byte "sty" ; 100 6
.byte "ldy" ; 101 7
.byte "cpy" ; 110 8
.byte "cpx" ; 111 9
;cc = 01
.byte "ora" ; 000 a
.byte "and" ; 001 b
.byte "eor" ; 010 c
.byte "adc" ; 011 d
.byte "sta" ; 100 e
.byte "lda" ; 101 f
.byte "cmp" ; 110 10
.byte "sbc" ; 111 11
;cc = 10
.byte "asl" ; 000 12
.byte "rol" ; 001 13
.byte "lsr" ; 010 14
.byte "ror" ; 011 15
.byte "stx" ; 100 16
.byte "ldx" ; 101 17
.byte "dec" ; 110 18
.byte "inc" ; 111 19
opcode_branches:
; branch $10, $30, $50...
.byte "bpl"
.byte "bmi"
.byte "bvc"
.byte "bvs"
.byte "bcc"
.byte "bcs"
.byte "bne"
.byte "beq"

;implied + jsr
opcode_singles_strings:
.byte "brk"
.byte "jsr"
.byte "rti"
.byte "rts"
.byte "php"
.byte "plp"
.byte "pha"
.byte "pla"
.byte "dey"
.byte "tay"
.byte "iny"
.byte "inx"
.byte "clc"
.byte "sec"
.byte "cli"
.byte "sei"
.byte "tya"
.byte "clv"
.byte "cld"
.byte "sed"
.byte "txa"
.byte "txs"
.byte "tax"
.byte "tsx"
.byte "dex"
.byte "nop"

;*******************************************************************************
; OPCODETAB
; This table is used for instructions (mostly single byte) that don't follow
; the encoding of the other instructions well.
; They are thus considered separately during assembly/disassembly
opcodetab:
.byte $10, $30, $50, $70, $90, $B0, $D0, $F0 	;branches
opcode_singles:
.byte $00, $20, $40, $60			; BRK, JSR, RTI, RTS
.byte $08, $28, $48, $68, $88, $A8, $C8, $E8	; PHP, PLP, PHA, PLA, DEY, TAY, INY, INX
.byte $18, $38, $58, $78, $98, $B8, $D8, $F8	; CLC, SEC, CLI, SEI, TYA, CLV, CLD, SED
.byte $8A, $9A, $AA, $BA, $CA, $EA		; TXA, TXS, TAX, TSX, DEX, NOP
num_opcode_singles=*-opcode_singles

;*******************************************************************************
; DIRECTIVES
DIRECTIVE_ELSE = 9
DIRECTIVE_ENDIF = 10
directives:
	.byte "db",0
	.byte "eq",0
	.byte "dw",0
	.byte "inc",0
	.byte "org",0
	.byte "rorg",0
	.byte "rep",0
	.byte "mac",0
	.byte "if",0
	.byte "else",0
	.byte "endif",0
	.byte "ifdef",0
	.byte "endmac",0
	.byte "endrep",0
	.byte "incbin",0
	.byte "import",0
	.byte "export",0
	.byte "seg",0
	.byte "segzp",0
directives_len=*-directives

;*******************************************************************************
.linecont +
.define directive_vectors definebyte, defineconst, defineword, includefile, \
defineorg, define_psuedo_org, repeat, macro, do_if, do_else, do_endif, \
do_ifdef, create_macro, handle_repeat, incbinfile, import, export, \
directive_seg, directive_segzp
.linecont -

directive_vectorslo: .lobytes directive_vectors
directive_vectorshi: .hibytes directive_vectors

;*******************************************************************************
; see MODE_ constants in asmflags.inc
bbb_modes:
bbb00_modes:
	.byte MODE_IMMEDIATE | MODE_ZP	; 000
	.byte MODE_ZP		        ; 001
	.byte $ff		        ; 010
	.byte MODE_ABS		        ; 011
	.byte $ff		        ; 100
	.byte MODE_ZP | MODE_X_INDEXED  ; 101
	.byte $ff		        ; 110
	.byte MODE_ABS | MODE_X_INDEXED	; 111

bbb01_modes:
	.byte MODE_ZP | MODE_X_INDEXED | MODE_INDIRECT
	.byte MODE_ZP
	.byte MODE_IMMEDIATE | MODE_ZP
	.byte MODE_ABS
	.byte MODE_ZP | MODE_INDIRECT | MODE_Y_INDEXED
	.byte MODE_ZP | MODE_X_INDEXED
	.byte MODE_ABS | MODE_Y_INDEXED
	.byte MODE_ABS | MODE_X_INDEXED

bbb10_modes:
	.byte MODE_IMMEDIATE | MODE_ZP	; 000
	.byte MODE_ZP		        ; 001
	.byte MODE_IMPLIED	        ; 010
	.byte MODE_ABS		        ; 011
	.byte $ff		        ; 100
	.byte MODE_ZP | MODE_X_INDEXED  ; 101 (Y_INDEXED for STX,LDX)
	.byte $ff		        ; 110
	.byte MODE_ABS | MODE_X_INDEXED	; 111 (Y_INDEXED for STX,LDX)

;*******************************************************************************
; ADDRESS MODE TABLES
; The following tables store the bbb values for the encoding for various
; configurations of addressing, e.g. zeropage, x-indexed.
; They are stored consistently such that the same addressing type maps to
; the same location in each table.  This makes it easy to translate from the
; type of addressing we're doing and the bit representation of the bbb encoding
; for the instruction.
; There are 3 tables, each represents the bbb values for a given set of cc
; instructions: cc 01, cc 10, and cc 00.
; A $ff in the table represents an invalid addressing mode for that type of
; instruction.
IMPLIED=0
IMMEDIATE=1
ZEROPAGE=2
ZEROPAGE_X=3
ZEROPAGE_X_INDIRECT=4
ZEROPAGE_Y_INDIRECT=5
ABS=6
ABS_X=7
ABS_Y=8
ABS_IND=9
bbb01:  .byte $ff ; implied/accumulator
	.byte $02 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $00 ; (zp,x)
	.byte $04 ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $06 ; abs,y
	.byte $ff ; (abs)

bbb10:  .byte $02 ; implied/accumulator
	.byte $00 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $ff ; (zp,x)
	.byte $ff ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $ff ; abs,y
	.byte $ff ; (abs)

bbb00:  .byte $ff ; implied/accumulator
	.byte $00 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $ff ; (zp,x)
	.byte $ff ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $ff ; abs,y
	.byte $ff ; (abs)

illegal_opcodes:
.byte %10001001 ; STA #imm

.byte %00000010 ; ASL #imm
.byte %00100010 ; ROL #imm
.byte %01000010 ; LSR #imm
.byte %01100010 ; ROR #imm
.byte %10000010 ; STX #imm
.byte %11000010 ; DEC #imm
.byte %11100010 ; INC #imm
.byte %10001010 ; STX A
.byte %10101010 ; LDX A
.byte %11001010 ; DEC A
.byte %11101010 ; INC A
.byte %10011110 ; STX ABS,X

.byte %00100000 ; BIT #imm
.byte %00110100 ; BIT zp,x
.byte %00111100 ; BIT abs,x
.byte %10000000 ; STY #imm
.byte %10011100	; STY abs,x
.byte %11010100 ; CPY zp,x
.byte %11011100 ; CPY abs,x
.byte %11110100 ; CPX zp,x
.byte %11111100 ; CPX abs,x
num_illegals = *-illegal_opcodes

.CODE

;*******************************************************************************
; RESET
; Resets the internal assembly context (labels and pointer to target)
.export __asm_reset
.proc __asm_reset
	lda #$ff
	sta __asm_segmode		; no .SEG set

	; empty CONTEXT and IF stacks
	lda #$00
	sta ifstacksp

	jsr ctx::init
	CALL FINAL_BANK_MACROS, mac::init
	jsr lbl::clr

	; fall through to RESETPC
.endproc

;*******************************************************************************
; RESETPC
; Resets the PC for, for example, beginning a new pass on the assembler
.export resetpc
.proc resetpc
	lda #$00
	sta pcset
	rts
.endproc

;*******************************************************************************
; START PASS
; Resets assembly context in preparation for the given pass
; IN:
;  - .A: the pass # (1 or 2)
.export __asm_startpass
.proc __asm_startpass
	pha

	; disable VERIFY (assemble)
	lda #$00
	sta zp::verify

	sta top			; set top of program to 0
	sta top+1
	sta origin
	sta origin+1
	sta zp::asmresult	; also set default physical address to 0
	sta zp::asmresult+1
	sta __asm_segmentid

	; ignore whitespace in expressions
	CALL FINAL_BANK_UDGEDIT, expr::end_on_ws

	jsr ctx::init		; init the context
	pla
	sta zp::pass		; set pass #
	cmp #$01
	beq __asm_reset
@pass2: jsr resetpc		; reset PC
	jmp ctx::init		; re-init the context
.endproc

;*******************************************************************************
; TOKENIZE PASS
; Based on the current pass (zp::pass), calls the appropriate routine to
; handle assembly for that pass
; IN:
;  - .XY: the string to tokenize
.export __asm_tokenize_pass
.proc __asm_tokenize_pass
	pha
	lda zp::pass
	cmp #$02
	pla
	bcc __asm_tokenize_pass1

	; fall through
.endproc

;*******************************************************************************
; TOKENIZE PASS2
; Calls tokenize and generated debug info (if enabled)
; IN:
;  - .A:  the bank that the line to assemble resides in
;  - .XY: the line to assemble
;  - .C:  set if an error occurred
.export __asm_tokenize_pass2
.proc __asm_tokenize_pass2
	jsr __asm_tokenize
	bcs @done	; return err

	; store debug info (if enabled)
	ldx zp::gendebuginfo
	beq @retok
	cmp #ASM_ORG
	bne @ok

@org:	; if we assembled a .ORG in pass 2, create a new block at the new address
	ldxy __asm_linenum
	stxy dbgi::srcline
	ldxy zp::virtualpc	; address of new block
	jmp dbgi::newblock	; create a block
	bcs @done
@retok:	lda #$00
@ok:	clc
@done:	rts
.endproc

;*******************************************************************************
; TOKENIZE_PASS1
; Calls tokenize on the given line
; IN:
;  - .A:  the bank that the line to assemble resides in
;  - .XY: the line to assemble
;  - .C:  set if an error occurred
.export __asm_tokenize_pass1
__asm_tokenize_pass1 = __asm_tokenize

;*******************************************************************************
; TOKENIZE
; Assembles the string at (YX) into an instruction in (asm::result)
; if (YX) contains an instruction.  Any labels or comments encountered are
; saved at the address in (pc).
; in:
;  - .XY: the string to assemble
;  - .A:  the bank of the string to assemble
;  - zp::asmresult: pointer to the location to assemble the instruction
; out:
;  - .A: the type of the result e.g. ASM_OPCODE or the error code
;  - .C: set if an error occurred
.export __asm_tokenize
.proc __asm_tokenize
	; copy the line to the main RAM bank and make it uppercase (assembly is
	; case-insensitive)
	stxy zp::bankaddr0
	ldxy #asmbuffer
	stxy zp::bankaddr1
	jsr ram::copyline

	ldxy #asmbuffer
	stxy zp::line
	jsr str::toupper

	ldy #$00
	sty mem::asmbuffer+LINESIZE
	jsr line::process_ws
	beq @noasm			; empty line -> done

;---------------------------------------
; check if we're in an .IF (FALSE) and if we are, return
@checkifs:
	lda ifstacksp
	beq assemble_with_ctx	; no active .IF
	ldx #$00
:	inx
	lda ifstack,x
	beq @if_false
	cpx ifstacksp
	beq assemble_with_ctx
	bne :-

@if_false:
	; asm is off, check for ENDIF or ELSE
	; anything else: return without assembling
	jsr is_directive
	bcs @noasm		; if not directive continue
	jsr getdirective
	bcs @ret		; if error, return it
	ldx #ASM_DIRECTIVE
	stx resulttype
	cmp #DIRECTIVE_ENDIF
	bne :+
	jmp do_endif		; handle .ENDIF
:	cmp #DIRECTIVE_ELSE
	bne @noasm
	jmp do_else		; handle .ELSE
@noasm:	lda #ASM_NONE
	clc
@ret:	rts
.endproc

;*******************************************************************************
; ASSEMBLE WITH CTX
; assembly entrypoint for successive single-line assembly
; if a label is found, for example, we will reenter here after adding the label
; to assemble any opcode, directive, etc. that may still be in the line
.proc assemble_with_ctx
	ldy #$00
	lda (zp::line),y
:	clc			; OK
	beq @ret		; return with .A=0 (ASM_NONE)
	jsr line::process_ws
	beq :-			; empty line, done

; check if the line is a full line comment
@chk_comment:
	;lda (zp::line),y
	cmp #';'
	bne @directive
	; rest of the line is a comment, we're done
	lda #ASM_COMMENT
	sta resulttype
	RETURN_OK

; 1. check if the line contains a directive
@directive:
	jsr is_directive
	bcs @ctx		; if not directive -> continue
	jsr getdirective
	bcs @ret		; return error

@exec_directive:
	lda #ASM_DIRECTIVE
	sta resulttype
	stxy zp::jmpvec
	jmp (zp::jmpvec)

; after directives, handle context if any. this is used for things like an
; active macro definition (the lines between .mac and .endmac)
@ctx:	jsr handle_ctx
	bcs @ret	; err -> exit
	bne assemble	; if context wasn't handled, assemble
@ret:	rts
.endproc

;*******************************************************************************
; ASSEMBLE
; This is the entrypoint for assembly after checking comments, directives, and
; the state of the .IF stack.  That is, this entrypoint will always assemble
; the input line regardless of overall assembly state.
; This is used to assemble lines for open contexts (e.g. .REP)
; asm::tokenize does not complete assembly when a context is open; it simply
; stores the line to the current context.
; IN:
;  - zp::line: string to assemble (assumed to be in shared RAM)
;  - zp::asmresult: pointer to the location to assemble the instruction
; OUT:
;  - .A: the type of the result e.g. ASM_OPCODE or the error code
;  - .C: set if an error occurred
.proc assemble
@tmp=r0
@noctx:	ldy #$00
	sty indirect_hint
	sty indexed
	sty operandsz
	sty immediate

; check if the line contains an instruction
@opcode:
	jsr getopcode
	bcs @macro	; not opcode -> check macro

	; we found an opcode, store it and continue to operand, etc.
	lda #ASM_OPCODE
	sta resulttype
	stx opcode	; save the opcode

	txa
	ldy #$00
	jsr writeb	; store the opcode
	bcs @ret0	; return err
	jmp @getopws	; continue if no error

; check if the line contains a macro
@macro:	ldxy zp::line
	CALL FINAL_BANK_MACROS, mac::get

	bcs @chklabels		; if not macro, skip
	pha			; save macro id
	jsr line::process_word	; read past macro name
	pla			; restore macro id

	jsr assemble_macro
	bcs @ret0		; error
	lda #ASM_MACRO
	sta resulttype
	;clc
@ret0:	rts

; check if the line is a label definition
@chklabels:
	jsr is_anonref		; anonymous label (:)?
	bne @label		; not an anonymous label definition
@anonlabel:
	jsr line::incptr
	lda (zp::line),y
	beq :+
	jsr util::is_whitespace	; anon label must be followed by whitespace
	bne @retlabel		; if not whitespace, go on
:	lda zp::verify
	bne @label_done		; if verifying, don't add a label
	lda zp::pass
	ldxy zp::virtualpc
	cmp #$01		; pass 1?
	bne @validate_anon	; if not, just validate
	jsr lbl::addanon	; add the anonymous label
	bcc @label_done
	rts			; return error (too many anonymous labels?)

@validate_anon:
	; make sure the anonymous label was correctly assigned in pass 1
	lda #$01
	jsr lbl::get_banon
	bcs :+
	cmpw zp::virtualpc
	beq @label_done
	lda #ERR_LABEL_NOT_KNOWN_PASS1
:	rts

@label:	jsr is_label
	bcs @getopws
	jsr do_label
	bcc @label_done
	rts			; return error

@label_done:
	jsr storedebuginfo	; store debug info for label
	jsr line::process_word	; read past the label name
	ldxy zp::line
	jsr assemble_with_ctx	; assemble the rest of the line
	bcs @ret0		; return error
	cmp #ASM_LABEL
	bne @retlabel

	; if we found another label, return error
	RETURN_ERR ERR_UNEXPECTED_CHAR

@retlabel:
	lda #ASM_LABEL		; return as LABEL (don't indent this line)
	RETURN_OK

; from here on we are either reading a comment or an operand
@getopws:
	jsr line::process_ws
	bne @pound
	jmp @done

@pound: cmp #';'		; are we at a comment?
	bne @parse_operand
	jmp @done		; if comment, we're done

@parse_operand:
	cmp #'#'
	bne @lparen		; if not '#' check for a paren (indirect)
	inc immediate		; flag operand as IMMEDIATE
	jsr line::incptr
	lda (zp::line),y	; get the next character

	; if IMMEDIATE, skip parentheses (treat as part of expression)
	jmp @evalexpr

@lparen:
	; not immediate, assume expressions are 2 bytes
	cmp #'('
	bne @evalexpr
	jsr is_indirect
	bne @evalexpr
	inc indirect_hint	; might be dealing with indirect opcode

; all chars not part of expression have been processed, evaluate the expression
@evalexpr:
	; first check if this is an anonymous label reference
	jsr is_anonref
	bne @eval		; not anon ref -> continue
	jsr anonref
	bcc @eval_anon_done

@evalfailed:
	ldx zp::verify
	beq :+

	; did eval fail while verifying due to undefined label?
	; continue if so
	cmp #ERR_LABEL_UNDEFINED
	bne :+

	; treat as valid instruction for verification purposes
	lda #ASM_OPCODE
	RETURN_OK

:	sec
	rts			; return error

@eval:	jsr expr::eval
	bcs @evalfailed		; return error, eval failed
	skw			; skip force address size
@eval_anon_done:
	lda #$02		; force word operand for anon (relative) label

@eval_done:
	stxy operand	; save the operand to store later
	sta operandsz	; save size of the operand

	; in pass 1, force immediate label evaluations to 1 byte
	lda zp::pass
	cmp #$02
	beq @cont
	lda immediate
	beq @cont
	lda #$01
	sta operandsz	; force zeropage addressing for IMMEDIATE operation

; we've evaluated the expression, now look for a right parentheses,
; ',X' or ',Y' to conclude if this is indirect or indexed addressing
@cont:	jsr line::process_ws	; .Y=0
	;ldy #$00
	lda indirect_hint	; is indirect flagged? (we saw a '(' earlier)?
	beq @index		; if not, skip to absolute

; handle indirect hint. May be x pre-indexed, indirect or indirect: ',X)' or ')'
; if no indexing, or opcode is not JMP, not indirect
@rparen:
	; look for indexing or ",X"
	lda (zp::line),y	; get first char
	cmp #','		; is it a ','?
	bne @rparen_noprex	; if not, only valid string is a plain ')'

	jsr line::nextch	; eat any WS and get next char
	cmp #'x'		; is it an .X?
	bne @unexpected_char

	jsr line::nextch	; get next char after ",X"
	inc indexed		; inc once to flag X-indexed
	cmp #')'
	beq @finishline		; if ')', continue

@unexpected_char:
	RETURN_ERR ERR_UNEXPECTED_CHAR

; look for a plain ')' (indirect addressing) or '),y'  (indirect y-indexed)
@rparen_noprex:
	jsr line::incptr
	cmp #')'
	bne @unexpected_char

@index:
	ldy #$00
	lda (zp::line),y
	cmp #','
	bne @getws2
	jsr line::nextch		; get next char (past any whitespace)

@getindexx:
	cmp #'x'
	bne @getindexy		; if not X check Y
	jsr is_ldx_stx		; check the special case of LDX y-indexed
	bcs :+			; if not LDX y-indexed continue
	; ,X is illegal for LDX/STX
	RETURN_ERR ERR_ILLEGAL_ADDRMODE

:	inc indexed	 	; inc once to flag ,X indexed
	bne @finishline		; validate nothing but a comment from here on

@getindexy:
	cmp #'y'
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR

:	inc indexed	 ; inc once for X-indexed
	jsr is_ldx_stx	 ; check LDX y-indexed
	bcc @finishline	 ; treat like ,X for encoding if LDX ,Y or STX ,Y
	inc indexed	 ; if NOT LDX y-indexed, inc twice for Y-indexed

;------------------------------------------------------------------------------
; finish the line by checking for whitespace and/or a comment
@finishline:
	jsr line::incptr		; next char
@getws2:
	jsr line::process_ws

	; check for comment or garbage
	jsr islineterminator
	beq @done
	RETURN_ERR ERR_UNEXPECTED_CHAR

;------------------------------------------------------------------------------
; done, create the assembled result based upon the opcode, operand, and addr mode
@done:	lda resulttype
	cmp #ASM_OPCODE
	beq @chkaddrmode
	; if not an instruction, we're done (nothing to write)
	clc
@ret:	rts

@chkaddrmode:
	jsr getaddrmode
	bcs @ret	; failed to get a valid address mode for instruction

@checkjmp:
	tax
	; JMP (xxxx) has a different opcode than JMP
	lda opcode
	cmp #$40
	bne @getbbb		; if not $40, not a JMP
	lda cc			; if cc is not 00,
	bne @getbbb		; not a JMP
	lda indirect_hint	; get indirect flag
	beq @jmpabs		; if not set, this is an ABS JMP

@jmpind:
	cpx #ABS_IND	; only abs-indirect is supported for JMP (XXXX)
	bne @err
	lda #$6c
	sta opcode		; fix the opcode variable
	jsr writeb
	bcc @noerr
	rts			; return err

@jmpabs:
	cpx #ZEROPAGE
	bne :+
	ldx #ABS	; force ABS for JMP
	inc operandsz
:	cpx #ABS
	bne @err 	; only ABS supported for JMP XXXX
	lda #$4c
	jsr writeb
	bcc @noerr
	rts		; return err

@getbbb:
; get bbb bits based upon the address mode and cc
	lda cc
	cmp #$03
	beq :+
	jmp @validate_cc

:	; check if opcode was a JSR
	lda opcode
	cmp #$20
	bne @chkbra
	cpx #ZEROPAGE
	bne :+
	ldx #ABS	; force ABS for JMP
	inc operandsz
:	cpx #ABS
	bne @err	; only ABS supported for JSR
	beq @noerr

@chkbra:
	; check if opcode was a branch
	and #$1f
	cmp #$10
	bne @verifyimm

@rel_branch:
	cpx #ZEROPAGE
	beq :+
	cpx #ABS	; only ABS/ZP supported for branches
	bne @err

:	; convert operand to relative address (operand - zp::asmresult)
	lda zp::asmresult
	clc
	adc #$02
	sta @tmp
	lda zp::asmresult+1
	adc #$00
	sta @tmp+1

	lda operand		; LSB of operand
	sec
	sbc @tmp
	sta operand		; overwrite operand with new relative address
	tax
	lda operand+1		; MSB of operand
	sbc @tmp+1		; MSB - >PC
	beq @store_offset	; $00xx might be in range
	cmp #$ff		; $ffxx might be in range
	beq @store_offset	; continue

	lda zp::pass
	cmp #$01		; on pass 1, offset might not be correct: allow
	beq @store_offset
	lda #ERR_BRANCH_OUT_OF_RANGE
	rts

@verifyimm:
	; remaining opcodes are single byte- implied/accumulator only
	cpx #IMPLIED
	beq @noerr
@err:	RETURN_ERR ERR_ILLEGAL_ADDRMODE

@store_offset:
	; Relative branches must be in the same section as their target, so
	; write the byte directly (no relocation)
	lda #$01
	sta operandsz	; force operand size to 1 for branches
	txa		; .A = relative offset (operand)
	ldy #$01	; offset to operand
	jsr writeb	; write the offset (no relocation)
	jmp @store_done

@noerr:
; now store the operand byte(s)
@store_value:
	lda operandsz
	beq @dbg		; if no operand, continue
	ldy #$01		; offset to operand
	cmp #$01
	beq @store_byte

@store_word:
	lda operand+1
	ldx operand
	ldy #$01		; offset to operand
	jsr writew_with_reloc
	bcs @opdone		; if unsuccessful, return err
	bcc @dbg

@store_byte:
	lda operand
	jsr writeb_with_reloc	; write the LSB
@store_done:
	bcs @opdone		; if error, return

;------------------
; store debug info if enabled
@dbg:	jsr storedebuginfo

;------------------
; update virtualpc and asmresult by (1 + operand size)
@updatepc:
	ldx operandsz
	inx
	txa
	jsr addpc		; add operand size + 1 to assembly pointers

	lda #ASM_OPCODE
	clc			; ok
@opdone:
	rts

;------------------
; check that the BBB and CC combination we have is valid
@validate_cc:
@optmp=r0
	ldy cc
	bne :+
	lda bbb00,x
:	cpy #$01
	bne :+
	lda bbb01,x
:	cpy #$02
	bne :+
	lda bbb10,x

:	cmp #$ff
	beq @err
	asl
	asl
	ora cc
	ldy #$00
	sta @optmp
	jsr readb
	ora @optmp

	; finally, check for invalid instructions ("gaps" in the ISA)
	ldx #num_illegals-1
:	cmp illegal_opcodes,x
	beq @err
	dex
	bpl :-

	; if instruction is valid, write out its opcode
	sta opcode	; write the fixed opcode
	jsr writeb
	bcc @noerr
	rts		; return err
.endproc

;*******************************************************************************
; IS_LABEL
; IN:
;  - zp::line: string to check if label
; OUT:
;  - .C: set if NOT a label
.proc is_label
	ldxy zp::line
	jmp lbl::isvalid
.endproc

;*******************************************************************************
; DO LABEL
; State machine component
; Extracts the label from the line
; pass 1: adds the label to the symbol table
; pass 2 (if assembling to object): maps symbol to its section
.proc do_label
	lda #ASM_LABEL
	sta resulttype
	ldxy zp::line
	lda zp::virtualpc
	sta zp::label_value
	lda zp::virtualpc+1
	sta zp::label_value+1

	jsr lbl::islocal
	cmp #$00		; check flag
	bne @cont

	; label is global
	jsr lbl::popscope	; end the current scope
	ldxy zp::line
	jsr lbl::setscope	; set the non-local label as the new scope

@cont:	lda zp::verify
	bne @ok			; if verifying, don't add/check label

	lda zp::pass
	ldxy zp::line
	cmp #$01
	bne @validate		; if not pass 1, don't add the label
	lda #$ff		; infer address mode
	jmp add_label

@validate:
	; pass 2: validation and symbol mapping (OBJ)
	; make sure the label's address hasn't moved since pass 1
	jsr lbl::find
	bcs @ret
	jsr lbl::getaddr
	cmpw zp::label_value
	bne @err		; mismatch -> return err
@ok:	RETURN_OK

@err:	lda #ERR_LABEL_NOT_KNOWN_PASS1
	sec
@ret:	rts
.endproc

;*******************************************************************************
; IS ANONREF
; Returns .Z set if zp::line points to an anonymous label reference
; OUT:
;  - .Z: Set if zp::line is on an anonymous label reference
.proc is_anonref
	ldy #$00
	lda (zp::line),y
	cmp #':'
	rts
.endproc

;*******************************************************************************
; ANONREF
; evaluates the anonymous reference in (line) and returns
; the address it corresponds to
; IN:
;  - zp::line points to the anonymous reference e.g. ":++"
; OUT:
;  - .XY:      the address that is referenced
;  - .A:       the size of the label
;  - .C:       set if invalid reference or on error
;  - zp::line: if there was an anonymous reference, updated to point past it
.proc anonref
	lda zp::pass
	cmp #$02
	beq @pass2

@pass1:	; if pass 1, return success with dummy value
	jsr line::process_word
	ldxy zp::virtualpc	; TODO: dummy address
	lda #2
	RETURN_OK

@pass2:	ldy #$01		; past the ':'
	lda (zp::line),y	; forward or backward?
	cmp #'+'
	beq @f			; if +, forward
	cmp #'-'
	bne @err		; if not -, invalid

@b:	; count the '-'s
	iny
	lda (zp::line),y
	beq @bdone
	jsr util::is_whitespace
	beq @bdone
	cmp #'-'
	beq @b
@err:	RETURN_ERR ERR_UNEXPECTED_CHAR

@bdone: tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	dey
	tya
	ldxy zp::virtualpc
	jmp lbl::get_banon

@f:	; count the '+'s
	iny
	lda (zp::line),y
	beq @fdone
	jsr util::is_whitespace
	beq @fdone
	cmp #'+'
	beq @f
	RETURN_ERR ERR_UNEXPECTED_CHAR

@fdone:	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	dey
	tya
	ldxy zp::virtualpc
	jmp lbl::get_fanon
.endproc

;*******************************************************************************
; STOREDEBUGINFO
; Stores the current VPC to the current source line
; If debug info generation is disabled, does nothing
.proc storedebuginfo
	lda zp::pass
	cmp #$01		; are we on pass 1?
	beq @skip		; if so, don't generate debug info

	lda zp::gendebuginfo
	bne @gen
@skip:	rts

@gen:	ldxy zp::virtualpc	; current PC (address)
	stxy r0
	ldxy __asm_linenum	; get line # to map to address
	jmp dbgi::storeline	; map them
.endproc

;*******************************************************************************
; GETADDRMODE
; Returns the address mode according to the global assembly flags:
; immediate, indexed, and indirect.
; This may or may not be legal for the instruction, it is constructed just
; from the syntax of the user's line.
; OUT:
;  - .A: the address mode
;  - .C: clear on success, set on error
.export getaddrmode
.proc getaddrmode
	lda operandsz
	beq @impl
	cmp #$02
	beq @abs
	cmp #$01
	beq @zp
@err:   RETURN_ERR ERR_OVERSIZED_OPERAND

;------------------
@zp:	lda immediate
	bne @imm
	ldx indexed
	lda indirect_hint
	beq :+
	dex
	bpl :+

	; error- indirect zeropage not a valid addressing mode
@illegalmode:
	RETURN_ERR ERR_ILLEGAL_ADDRMODE
:	txa
	clc
	adc indirect_hint
	adc indirect_hint
	adc #ZEROPAGE
@ok:	RETURN_OK

;------------------
@abs:   lda immediate
	bne @oversized	; error- immediate abs illegal (operand too large)
	lda indirect_hint
	beq :+
	lda indexed
	bne @err 	; error- indirect absolute doesn't support indexing
	lda #ABS_IND
	RETURN_OK
:	lda indexed
	clc
	adc #ABS
	RETURN_OK

@imm:	lda indirect_hint
	bne @illegalmode ; error- immediate doesn't support indirection
	lda indexed
	bne @illegalmode ; error- immediate doesn't support indexing
	lda #IMMEDIATE
	RETURN_OK

@impl:	;lda #IMPLIED (0)
@done:	RETURN_OK

;------------------
@oversized:
	lda zp::pass
	cmp #$01
	beq @done
	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc

;*******************************************************************************
; GETTEXT
; Parses an enquoted text string in zp::line and returns it in mem::spare
; returns the length in .A ($ff if no string was found)
.proc gettext
	ldy #$00
	lda (zp::line),y
	cmp #'"'
	bne @err

	ldx #$00
@l0:	jsr line::incptr
	lda (zp::line),y
	beq @err		; no closing quote
	cmp #'"'
	beq @done
	sta mem::spare,x
	inx
	bne @l0

@done:	jsr line::incptr
	txa
	RETURN_OK
@err:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc

;*******************************************************************************
; GETOPCODE
; Parses zp::line for an instruction and returns information about it if it
; is determined to be an instruction
; IN:
;  - zp::line: the line to parse the opcode from
; OUT:
;  - .A: ASM_OPCODE (on success) else error
;  - .X: the opcode's ID
;  - .C: set if (line) is not an opcode
;  - cc: updated with the cc part of the opcode
.proc getopcode
@optab = r6
@op = r8
	lda #$00
	sta @op
	sta cc

	ldx #<opcodes
	ldy #>opcodes
	stx @optab
	sty @optab+1

@l0:	ldy #$02
@l1:	lda (zp::line),y
	cmp (@optab),y
	bne @next
	dey
	bpl @l1

	; make sure there are no trailing characters
	ldy #$03
	lda (zp::line),y
	beq @done
	jsr util::is_whitespace
	beq @done
	jmp @err

@done:	lda @op
	tax
	cmp #CC_01
	bcc @setcc
	inc cc
	cmp #CC_10
	bcc @setcc
	inc cc
	cmp #CC_IMP
	bcc @setcc
	inc cc

; if we reached this point, instruction is a CC 00 encoding, look up the opcode
; from a table
	sbc #CC_IMP
	tax
	lda opcodetab,x
	tax
	jmp @return

@setcc:	asl
	asl
	asl
	asl
	asl
	tax

@return:
	; update line ptr and return
	lda zp::line
	clc
	adc #$03
	sta zp::line
	bcc :+
	inc zp::line+1
:	RETURN_OK

@next:	lda @optab
	clc
	adc #$03
	sta @optab
	bcc :+
	inc @optab+1
:	inc @op
	lda @op
	cmp #NUM_OPCODES
	bcc @l0

@err:	RETURN_ERR ERR_ILLEGAL_OPCODE
.endproc

;*******************************************************************************
; IS INDIRECT
; Checks if the contents of zp::line represent an indirect operand
; IN:
;   - zp::line: the operand to check
; OUT:
;   - .Z: set if the operand IS indirect (completely enclosed in a parens)
.proc is_indirect
@cnt=r0
@len=r1
	ldy #$00
	sty @cnt
	lda (zp::line),y
	cmp #'('
	bne @no		; if doesn't start with a '(', not indirect

@l0:	; check if opening paren is closed before end of line
	lda (zp::line),y
	jsr islineterminator
	beq @no		; at the effective end of line and unbalanced
	cmp #'('
	bne :+
	inc @cnt
:	cmp #')'
	bne :+
	dec @cnt
	beq @closed
:	iny
	bne @l0		; branch always

@closed:
	; parens are balanced, if there are any more '(' or ')' the expression
	; is not indirect or invalid (unbalanced) respectively
	iny
@l1:	lda (zp::line),y
	jsr islineterminator
	beq @yes

	cmp #'('
	beq @no
	cmp #')'
	beq @no
	iny
	bne @l1

@no:	lda #$ff
@yes:	rts
.endproc

;*******************************************************************************
; IS_DIRECTIVE
; Checks if the contents of (zp::line) represent a directive
; OUT:
;   - .C: set if (zp::line) is not a directive
.proc is_directive
	ldy #$00
	lda (zp::line),y
	cmp #'.'
	bne @no
@yes:	clc			; zp::line is a directive
	rts
@no:	sec			; not a directive
	rts
.endproc

;*******************************************************************************
; GETDIRECTIVE
; Returns the handler and ID for the directive in (zp::line)
; OUT:
;  - .A:  the ID of the directive (if one is found)
;  - .XY: the address of the directive's handler
;  - .C:  set if the contents of zp::line is not a valid directive
.proc getdirective
@cnt=r2
	ldx #$00
	stx @cnt
@l0:	ldy #$01
@l1:	lda directives,x
	cmp #$00
	beq @found
	cmp (zp::line),y
	bne @next
	inx
	iny
	bne @l1

@next:	cpx #directives_len
	bcc @ok
	RETURN_ERR ERR_INVALID_DIRECTIVE

@ok:	inc @cnt	; move to next directive

@l2:	lda directives,x
	inx
	cmp #$00
	beq @l0
	cpx #directives_len
	bcc @l2
	RETURN_ERR ERR_INVALID_DIRECTIVE

@found: ; make sure there are no trailing characters
	lda (zp::line),y
	beq :+
	jsr util::is_whitespace
	bne @next		; trailing char -> continue

:	; move the line pointer to after the directive
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	jsr line::process_ws

	ldx @cnt
	ldy directive_vectorshi,x
	lda directive_vectorslo,x
	tax
	lda @cnt		; get the ID of the directive
	RETURN_OK
.endproc

;*******************************************************************************
; HANDLE_REPEAT
; Handler for .endrep.
; Generates the repeated assembly block defined between here and the
; corresponding .rep directive
.proc handle_repeat
@itername=$100
	lda zp::verify
	beq :+
	RETURN_OK

:	; close the context
	lda #CTX_REPEAT
	jsr ctx::end
	bcs @err

	; if the number of iterations is zero, we're done
	iszero zp::ctx+repctx::iter_end
	beq @done

	; rewind context to first line and debug-info to that line too
	jsr rewind_ctx_dbg

	; load the parameter (iterator name)
	ldxy #@itername
	jsr ctx::getparams

;--------------------------------------
; define a label with the value of the iteration
@l0:	jsr rewind_ctx_dbg

	; iteration 0: add a symbol for the iterator
	;              this will error if the symbol is already defined
	; iteration n: "set" (replace) the iterator's value
	lda zp::ctx+repctx::iter
	sta zp::label_value
	lda zp::ctx+repctx::iter+1
	sta zp::label_value+1
	ora zp::label_value
	beq @l1			; label already defined in repeat

	ldxy #@itername
	lda #SEG_ABS
	sta zp::label_segmentid
	lda #$01		; define label as 16-bit (ABSOLUTE)
	sta zp::label_mode

@set:	jsr lbl::set		; successive iterations- set (replace)
	bcs @err

;--------------------------------------
; assemble all lines for the iteration
@l1:	incw __asm_linenum
	jsr ctx::getline	; get a line to assemble
	bcs @err
	cmp #$00
	beq @next		; if at the end, continue to next iteration

	; assemble line read from context
	lda #FINAL_BANK_MAIN	; bank doesn't matter for ctx
	jsr __asm_tokenize	; assemble context line
	bcc @l1			; ok -> repeat
@err:	rts			; return err

@next:	; increment iterator and repeat if more iterations left
	incw zp::ctx+repctx::iter
	ldxy zp::ctx+repctx::iter
	cmpw zp::ctx+repctx::iter_end
	bne @l0

@done:  jsr ctx::pop		; pop the context

	; did we close all contexts?
	lda __ctx_active
	bne :+
	jsr lbl::popscope	; all contexts popped, pop the scope
	clc
:	rts
.endproc

;*******************************************************************************
; REWIND CTX DBG
; Rewinds the context and debug source line by the number of lines that are
; rewound.
.proc rewind_ctx_dbg
@tmp=r0
	; before rewinding, move debug line back to line we're repeating
	lda ctx::numlines	; get # of lines we're rewinding
	sta @tmp
	lda __asm_linenum
	sec
	sbc @tmp
	sta __asm_linenum
	lda __asm_linenum+1
	sbc #$00
	sta __asm_linenum+1

	jmp ctx::rewind
.endproc

;*******************************************************************************
; HANDLE CTX
; If a context is active, copies the contents of the asmbuffer to it
; OUT:
;  - .Z: set if the line was handled by this handler
;  - .C: set on error
.proc handle_ctx
	lda ctx::active
	beq @ok		; no contexts -> continue

	ldxy #asmbuffer

	; check if the active context is "open" or "closed"
	; open:   write to context
	; closed: write to parent context (nested) OR
	;         return for assembler to handle (not nested)
	cmp #$02		; activectx < 2?
	bcc @toplevel		; if yes, we're using first context (top)

@nested:
	lda ctx::open		; is context open?
	bne @write_ctx		; if yes, write it to the context buffer

;--------------------------------------
; context is open, reduce the current iterator to its constant value
; and write it to the parent context's buffer
@buff=$100+LINESIZE
	ldxy #@buff
	jsr ctx::getparams	; get the active iterator's name
	ldxy #@buff
	jsr sub_label		; and replace uses with its value in asmbuffer
	ldxy #mem::asmbuffer

	jsr ctx::write_parent	; write disassembled line to PARENT's ctx buff
	jmp @ctx_done		; errcheck and return

@toplevel:
	lda ctx::open
	beq @ok		; no context open -> continue

@write_ctx:
	jsr ctx::write	; copy the line to the context

@ctx_done:
	bcs @done
	lda #$00	; flag that the context was handled
	skw
@ok:	lda #$01	; flag context NOT handled
	clc		; no error
@done:	rts
.endproc

;*******************************************************************************
; DEFINEBYTE
; Defines 0 or more bytes and stores them in (asmresult)
; OUT:
;  - .A: the number of bytes written
.proc definebyte
	jsr line::process_ws
	jsr expr::eval
	bcs @text				; invalid expr- try text
	cpy #$00
	beq @ok
	RETURN_ERR ERR_OVERSIZED_OPERAND

@ok:	; store the extracted value
	ldy #$00
	txa
	jsr writeb_with_reloc
	bcs @ret
	jsr incpc
	jmp @commaorws

@text:	jsr gettext
	bcs @err
	; store the extracted text
	tay
	tax
	beq @done
	dex
	dey
:	lda mem::spare,y
	jsr writeb
	bcs @ret
	dey
	bpl :-

	; update program pointers
	inx
	txa
	jsr addpc

@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #';'		; comment?
	beq @done
	jsr line::incptr
	cmp #','
	beq definebyte
	jsr util::is_whitespace
	beq @commaorws

	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; DEFINEWORD
; Parses zp::line for a word value and stores it to zp::asmresult if possible.
; OUT:
;  - .C: set if a word could not be parsed
.proc defineword
	jsr line::process_ws
	jsr expr::eval
	bcs @err

	; store the extracted value
	tya				; .A=MSB
	ldy #$00
	jsr writew_with_reloc

	lda #$02
	jsr addpc

@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #';'
	beq @done
	jsr line::incptr
	cmp #','
	beq defineword
	jsr util::is_whitespace
	beq @commaorws
	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; IMPORTZP
; Imports the label following this directive as a zeropage label reference
; e.g. `IMPORT LABEL`
; The label is assumed to be in the zeropage ($00-$ff) address range.
; The actual resolution of the label will happen when the object code is linked
.proc importzp
	lda #$00		; ZP

	skw
	; fall through to import
.endproc

;*******************************************************************************
; IMPORT
; Imports the label following this directive
; e.g. `IMPORT LABEL`
; The label is assumed to be in the absolute ($100-$ffff) address range.
; The actual resolution of the label will happen when the object code is linked
.proc import
	lda #$01			; ABS
	sta zp::label_mode

	; define a label for the import so that references to it succeed
	lda #SEG_UNDEF			; UNDEF (external)
	sta zp::label_segmentid

	lda #$00			; dummy value
	sta zp::label_value
	sta zp::label_value+1

	ldxy zp::line
	jmp lbl::add
.endproc

;*******************************************************************************
; EXPORT
; Exports the label following this directive
; e.g. `EXPORT LABEL`
.proc export
	;  TODO
	lda zp::pass
	cmp #$02
	beq :+
	RETURN_OK		; exports are done in pass 2

:	; if producing an object file, add to its EXPORTs
	ldxy zp::line
	JUMP FINAL_BANK_LINKER, obj::add_export
.endproc

;*******************************************************************************
; DIRECTIVE SEG ZP
; Handles the `.SEGZP` directive
; This directive is only valid when assembling to object. It creates a new
; SECTION in the object file, closing the current one (if one exists)
; The section created must be placed into the zeropage by the linker
.proc directive_segzp
	lda #$00		; zeropage addressing
	skw

	; fall through to directive_seg
.endproc

;*******************************************************************************
; DIRECTIVE SEG
; Handles the `.SEG` directive
; This directive is only valid when assembling to object. It creates a new
; SECTION in the object file, closing the current one (if one exists)
.proc directive_seg
@name=$100
	lda #$01		; absolute addressing
	sta __asm_segmode

	; get the name of the segment
	jsr util::parse_enquoted_line
	bcc @add
	rts		; error

@add:	ldxy __asm_linenum
	stxy dbgi::srcline

	lda #$01
	sta pcset	; mark PC set (linker will take care of setting it)

	; close the current section (if any)
	jsr obj::close_section

	; create a new SECTION for the parsed SEGMENT name
	lda __asm_segmode
	CALL FINAL_BANK_LINKER, obj::add_section

	; set PC to base of this SECTION (0 if new, or where we left off
	; if not) so that labels will be relative to the SEGMENT
	stxy zp::virtualpc

	sta __asm_segmentid		; set SEGMENT id

	; if pass 2, create new block for debug info
	lda zp::pass
	cmp #$02
	bne @done

	; end current BLOCK of debug info (if one is open)
	ldxy zp::virtualpc	; current address
	jsr dbgi::endblock	; end the current block
	jsr dbgi::set_seg_id	; and set it for debug info too

	; create a new BLOCK of debug info at zp::virtualpc
	jsr dbgi::newblock	; start new block for included file

@done:	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; INCBINFILE
; Includes the enquoted binary file
; The contents of the file are inserted directly as binary values at the
; current assembly address.
.proc incbinfile
@filename=$100
@offset=r0
@size=r0
@size_specified=r2
	lda zp::verify
	beq @cont

	; don't include a file when verifying
	lda #ASM_DIRECTIVE
	clc			; ok
@ret0:	rts

@cont:	ldxy #@filename
	stxy r0
	ldxy zp::line
	jsr util::parse_enquoted_line
	bcs @ret0
	stxy zp::line
	jsr line::incptr

	ldxy #@filename
	jsr file::exists
	bcc @exists
@erropen:
	RETURN_ERR ERR_FAILED_OPEN_INCLUDE

@exists:
	ldxy #@filename
	jsr file::open_r
	bcs @erropen
	pha		; save file handle
	tax
	jsr krn::chkin	; CHKIN (use file as input)

	lda #$00
	sta @size_specified

	; look for the optional offset parameter
	jsr line::process_ws
	;lda (zp::line),y
	beq @l0			; end of line -> continue
	cmp #';'
	beq @l0			; comment (';') -> continue

	cmp #','
	bne @err		; unexpected character
	jsr line::incptr
	jsr expr::eval
	bcs @err
	stxy @offset

	; read @offset many bytes and throw them away
:	jsr file::readb
	bcs @err
	decw @offset
	iszero @offset
	bne :-

	; get the optional size parameter
	jsr line::process_ws
	;lda (zp::line),y
	beq @l0			; end of line -> continue
	cmp #';'
	beq @l0			; comment (';') -> continue
	cmp #','
	bne @err		; unexpected character
	jsr line::incptr
	jsr expr::eval
	bcs @err
	stxy @size
	iszero @size
	beq @eof		; if size is 0, we're done
	inc @size_specified

@l0:	; read the binary file contents byte-by-byte
	jsr file::readb
	bcs @err
	ldy file::eof
	bne @eof		; loop until EOF

	;ldy #$00
	jsr writeb		; write the byte
	bcs @err
	jsr incpc
	lda @size_specified
	beq @l0

	; if a size was given, decrement the counter and loop if !0
	decw @size
	iszero @size
	bne @l0

@eof:	clc			; return without err
@err:	pla			; restore file handle
	php			; save success status flag
	jsr file::close		; cleanup (close the file)
	plp			; restore success flag
	lda #ASM_DIRECTIVE
@ret:	rts
.endproc

;*******************************************************************************
; INCLUDEFILE
; Include file assembles the contents of the given file.
;
; Also generates debug info; the debug info generated will depend on the pass
; Pass 1:
;  Gets the number of lines/segments in the file
; Pass 2:
;  Stores the corresponding lines for addresses of assembled code
.proc includefile
@filename=$100
	jsr util::parse_enquoted_line
	bcs :+				; -> rts (failed to parse filename)

	ldxy #@filename

; entry point for assembling a given file
.export __asm_include
__asm_include:
@fname=rc
@err=savereg
@readfile:
	stxy @fname

	lda zp::verify
	beq @inc
	lda #ASM_DIRECTIVE	; format type
	clc			; don't include a file when verifying
:	rts

@inc:	sta @err		; @err = 0

	; save current file
	lda dbgi::file
	pha

	ldxy @fname
	jsr file::exists
	bcc :+
	lda #ERR_FAILED_OPEN_INCLUDE
@reterr:
	plp		; clean stack
	sec
	rts

:	ldxy @fname
	jsr file::open_r	; open the file we are including
	bcs @reterr

	pha		; save the id of the file we're working on (for closing)
	sta zp::file

	; add the filename to debug info (if it isn't yet), reset line number
	; and finally create a new block of debug information
	ldxy @fname
	jsr dbgi::setfile
	ldxy #1
	stxy dbgi::srcline
	stxy __asm_linenum

	lda zp::pass
	cmp #$02
	bne @doline		; only create new block in pass 2

	; end current file's block and start a new one at the current address
	lda pcset
	beq @doline
	ldxy zp::virtualpc	; current address
	jsr dbgi::endblock	; end the current block
	ldxy zp::virtualpc	; current address
	jsr dbgi::newblock	; start new block for included file

; read a line from file
@doline:
	ldxy #mem::spare
	lda zp::file
	jsr file::getline	; read a line from the file
	bcc @asm
	ldx file::eof
	bne @close		; failed to get a line and not at end of file
	sta @err
	bne @close		; branch always

; assemble the line
@asm:	ldxy #mem::spare
	lda zp::file
	pha

	lda #FINAL_BANK_MAIN	; any bank that is valid (low mem is used)
	jsr __asm_tokenize_pass
	bcc @ok
	jsr errlog::log
	bcc @ok
@fatal:	sta @err		; too many errors or fatal error

@ok:	pla			; restore the file ID we included from
	sta zp::file		; and temporarily store it
	bcs @close

@next:	incw __asm_linenum	; next line
	lda file::eof		; EOF?
	beq @doline		; repeat til we are

@close: pla			; get the file ID for the include file to close
	jsr file::close		; close the file

	pla			; restore debug file ID
	sta dbgi::file

	lda zp::pass
	cmp #$02
	bne @done		; if not pass 2, don't mess with debug info

	; create new block in file we included from (if PC is set)
	lda pcset
	beq @done
	ldxy zp::virtualpc
	jsr dbgi::endblock	; end the block for the included file
	ldxy zp::virtualpc
	jsr dbgi::newblock	; start a new block in original file
@done:
	lda @err
	lda #$00	; get err code
	cmp #$01	; set carry if >= 1
	rts
.endproc

;*******************************************************************************
; DEFINEORG
; Hanldes the .ORG directive.
; Parses an expression for a value and sets the asmresult and virtualpc
; addresses to it
; e.g.: `.ORG $1000` or `ORG $1000+LABEL`
.proc defineorg
	jsr line::process_ws
	jsr expr::eval
	bcs @ret		; error

	stxy zp::asmresult
	stxy zp::virtualpc
	lda pcset
	bne @chkorg

	; PC isn't set yet, set TOP to the new PC
	inc pcset
	stxy top
	stxy origin
	bne @done		; branch always

@chkorg:
	; check if new PC is lower than current base origin
	cpy origin+1
	beq :+			; check LSB
	bcs @done
	bcc @set
:	cpx origin
	bcs @done
@set:	stxy origin

	cpy #$00
	beq :+
	ldy #$01		; absolute
:	sty __asm_segmode	; set segment mode

@done:	; set segment to ABSOLUTE (we know the exact address of all labels
	; declared within a .ORG "section")
	lda #SEG_ABS
	sta __asm_segmentid

	lda #ASM_ORG
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; SET PC
; Sets the address to assemble at next time tokenize is called.
; This is only useful in the context of assembling a line directly to
; memory.  It should not be used when assembling a file/buffer
; IN:
;   - .XY: the address to assemble the next instruction to
.export __asm_set_pc
.proc __asm_set_pc
	stxy zp::asmresult
	stxy zp::virtualpc
	lda #$01
	sta pcset
	rts
.endproc

;*******************************************************************************
; DEFINE_PSUEDO_ORG
; Hanldes the .RORG directive.
; Parses an expression for a value and sets the virtualpc  address to it.
; Note that the physical assembly target (asmresult) is unaffected.
; e.g.: `.RORG $1000` or `RORG $1000+LABEL`
.proc define_psuedo_org
	jsr line::process_ws
	jsr expr::eval
	bcs @ret		; error

	; TODO: require expression to resolve in pass 1
	stxy zp::virtualpc
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; REPEAT
; generates assembly for the parameterized code between this directive
; and the lines that follow until '.endrep'
; EXAMPLE:
;   .rep 10,I
;       asl
;   .endrep
;   will produce 10 'asl's
.proc repeat
@buff=r0
	lda zp::verify
	bne @done		; don't handle when NOT verifying

	; if there are no open contexts, start a non-renderable scope
	ldy ctx::active
	bne :+
	sty @buff+1		; 0
	iny			; 1
	sty @buff		; 1
	dey			; 0
	ldx #@buff
	jsr lbl::setscope	; set scope to a binary (unrenderable) prefix

:	lda #CTX_REPEAT
	jsr ctx::push	; push a new context

	jsr expr::eval  ; get the number of times to repeat the code
	bcs @ret	; error evaluating # of reps expression

@ok:	stxy zp::ctx+repctx::iter_end	; set number of iterations
	jsr line::process_ws		; .Y=0
	;ldy #$00
	;lda (zp::line),y

	; initialize iterator (number of repititons)
	sty zp::ctx+repctx::iter
	sty zp::ctx+repctx::iter+1
	sty zp::label_value
	sty zp::label_value+1

	cmp #','
	beq @getparam
	RETURN_ERR ERR_UNEXPECTED_CHAR ; comma must follow the # of reps

@getparam:
	; get the name of the iterator
	jsr line::incptr
@saveparam:
	ldxy zp::line
	jsr ctx::addparam
	bcs @ret		; error adding parameter
	ldxy zp::line

	; define the label for the iterator as a constant with value 0
	lda #SEG_ABS
	sta zp::label_segmentid
	lda #$01		; define label as 16-bit (ABSOLUTE)
	sta zp::label_mode
	jsr lbl::set
	bcs @ret

@cont:	stxy zp::line		; update line pointer to after parameter

@done:	clc			; ok
@ret:	lda #ASM_DIRECTIVE
	rts
.endproc

;*******************************************************************************
; MACRO
; Begins the definition of a macro, which will continue until '.endmac' is
; EXAMPLE:
;   .mac add8 A, B
;       lda #A
;       clc
;       adc #B
;   .endmac
;   will define a macro that can be used like:
;   add8 10, 20
.proc macro
	lda #CTX_MACRO
	jsr ctx::push	; push a new context

	lda zp::pass
	cmp #$02
	bcs @done	; macro definition handled in pass 1

; get the first parameter (the name)
@getname:
	jsr line::process_ws
	jsr islineterminator
	bne @storename
	RETURN_ERR ERR_NO_MACRO_NAME

@storename:
	ldxy zp::line
	jsr ctx::addparam
	bcs @ret		; return err
	stxy zp::line		; update line pointer

@getparams:
	jsr line::process_ws	; .Y=0
	;lda (zp::line),y
	jsr islineterminator
	beq @done
	ldxy zp::line
	jsr ctx::addparam
	stxy zp::line

	; look for the comma or line-end
	jsr line::process_ws	; sets .Y to 0
	;lda (zp::line),y
	jsr islineterminator
	beq @done
	cmp #','
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR

:	jsr line::incptr
	bne @getparams
@done:	lda #ASM_DIRECTIVE
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; CREATE_MACRO
; This is the handler for the .endmac directive
; It uses the active context to finish creating a macro from that context.
.proc create_macro
	; close the macro context
	lda #CTX_MACRO
	jsr ctx::end
	bcs @ret

	lda zp::pass
	cmp #$02
	bcs @done		; done, macros are defined in pass 1
	lda zp::verify
	bne @done		; and only when NOT verifying

	jsr ctx::rewind

	; fill $100 with param data
	ldxy #$100
	jsr ctx::getparams

	; get the context data (the macro definition)
	pha
	jsr ctx::getdata
	lda #<$100
	sta r0
	lda #>$100
	sta r0+1
	pla

	; create the macro
	CALL FINAL_BANK_MACROS, mac::add

@done:	; done with this context, disable it
	jsr ctx::pop		; cleanup; pop the context
	lda #ASM_DIRECTIVE
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; DISASSEMBLE
; disassembles the given instruction
; IN:
;  - .A:  0=disassemble to string, !0=don't disassemble to string
;  - .XY: address of the instruction to disassemble
;  - r0:  address of the buffer to disassemble to (if A != 0)
; OUT:
;  - .A:   the size of the instruction that was disassembled
;  - .X:   the address modes for the instruction
;  - .C:   clear if instruction was successfully disassembled
;  - (r0): the (0-terminated) disassembled instruction string (if A IN=0)
.export __asm_disassemble
.proc __asm_disassemble
@opaddr=ra
	pha			; save nostr flag
	stxy @opaddr		; opcode
	jsr vmem::load
	sta opcode
	ldxy @opaddr		; operand
	lda #$01
	jsr vmem::load_off
	sta operand
	ldxy @opaddr		; operand byte 2
	lda #$02
	jsr vmem::load_off
	sta operand+1
	pla			; restore nostr flag

	; fall through to disasm
.endproc

;*******************************************************************************
; DISASM
; Entrypoint for disassembling values in opcode and operand directly
; IN:
;   - .A:      0=disassemble to string, !0=don't disassemble to string
;   - r0:      address of the buffer to disassemble to (if A != 0)
;   - ra:      address of instruction
;   - opcode:  opcode of the instruction to disassemble
;   - operand: operand of instruction to disassemble
; OUT:
;  - .A:   the size of the instruction that was disassembled
;  - .X:   the address modes for the instruction
;  - .C:   clear if instruction was successfully disassembled
;  - (r0): the (0-terminated) disassembled instruction string (if A IN=0)
.proc disasm
@dst=r0
@cc=r2
@optab=r7
@cc8=r7
@xxy=r7
@cc8_plus_aaa=r7
@modes=r7
@bbb=r8
@nostr=r9
@illegals=r7
@opaddr=ra
@op=opcode
@operand=operand

	sta @nostr

; check for single byte opcodes
@chksingles:
	lda @op
	ldx #num_opcode_singles-1
:	cmp opcode_singles,x
	beq @implied_or_jsr
	dex
	bpl :-
	bmi @chkillegals

@implied_or_jsr:
	pha
	txa	; * 3 to get offset in opcode string table
	sta @op
	asl
	adc @op
	tax

	ldy #3
:	lda opcode_singles_strings,x
	jsr @appendch
	inx
	dey
	bne :-
	pla
	cmp #$20	; JSR
	bne @implied_

	lda #' '
	jsr @appendch

	lda #MODE_ABS
	sta @modes
	jmp @absolute

@implied_:
	lda #$00
	jsr @appendch		; 0-terminate
	lda #$01
	ldx #MODE_IMPLIED
	clc			; ok
@ret:	rts

@chkillegals:
	; check if the opcode is "illegal"
	ldxy #illegal_opcodes
	stxy @illegals
	ldy #num_illegals-1
	lda @op
:	cmp (@illegals),y
	beq @ret		; if illegal, quit with .C set
	dey
	bpl :-

; check for branches/exceptions
@checkbranch:
	lda @op
	and #$1f
	cmp #$10
	bne @not_branch
@branch:
	; get bits 5, 6 and 7 to determine branch type
	lda @op
	asl
	rol
	rol
	rol
	and #$07
	sta @xxy
	asl
	adc @xxy
	tax
	ldy #$02
:	lda opcode_branches,x
	jsr @appendch
	inx
	dey
	bpl :-

@get_branch_target:
	; calculate target address PC+2+operand
	; sign extend the operand
	lda @operand
	bpl :+
	lda #$ff
	skw
:	lda #$00
	sta @operand+1

	; operand + opaddr + 2
	lda @operand
	clc
	adc @opaddr
	sta @operand
	lda @operand+1
	adc @opaddr+1
	sta @operand+1
	lda #$02
	clc
	adc @operand
	sta @operand
	lda @operand+1
	adc #$00
	sta @operand+1
	lda #MODE_ABS
	sta @modes
	jsr @cont 	; @operand now contains absolute address, render it
	lda #$02	; size is 2
	rts

@not_branch:
	lda @op
	and #$03	; get cc
	sta @cc
	; get opcodes table offset (each block is 8 opcodes)
	asl
	asl
	asl
	sta @cc8

	; get aaa - opcode offset (each mneumonic is 3 bytes)
	lda @op
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc @cc8
	cmp #(opcode_branches-opcodes)/3
	bcs @invalid
	sta @cc8_plus_aaa
	asl
	adc @cc8_plus_aaa
	bne :+
	sec
@invalid:
	rts			; optab code 0 is invalid

:	adc #<opcodes
	sta @optab
	lda #>opcodes
	adc #$00
	sta @optab+1

	; write the opcode (optab),aaa to the destination
	ldy #$00
:	lda (@optab),y
	jsr @appendch
	iny
	cpy #$03
	bne :-

@get_addrmode:
	; get bbb and find the addressing mode for the instruction
	lda @op
	cmp #$6c	; handle JMP (ind) - it doesn't match "normal" encoding
	bne :+
	lda #MODE_INDIRECT | MODE_ABS
	sta @modes
	bne @cont

:	lsr
	lsr
	and #$07
	sta @bbb

	; get the cc offset into the bbb_modes table
	lda @cc
	asl
	asl
	asl
	adc @bbb	; add bbb to get the table position of our instruction
	tax

	lda bbb_modes,x
	sta @modes
	and #MODE_IMPLIED
	beq @cont	; if not implied, go on
@implied:
	lda #$00
	jsr @appendch	; 0-terminate
	lda #$01	; 1 byte in size
	ldx @modes
	RETURN_OK

@cont:  ; add a space before operand
	lda #' '
	jsr @appendch

	; draw the opcode
	ldy #$00
@drawop:
	lda @modes
	and #MODE_INDIRECT
	beq :+
@indirect:
	lda #'('
	jsr @appendch

:	lda @modes
	and #MODE_IMMEDIATE
	beq :+
@immediate:
	lda #'#'
	jsr @appendch

:	lda @modes
	and #MODE_ZP
	beq :+
@zeropage:
	lda #'$'
	jsr @appendch

	lda @operand
	jsr util::hextostr
	tya
	jsr @appendch
	txa
	jsr @appendch

:	lda @modes
	and #MODE_ABS
	beq @chkindexed

@absolute:
	lda #'$'
	jsr @appendch

	lda @operand+1
	jsr util::hextostr
	tya
	jsr @appendch
	txa
	jsr @appendch

	lda @operand
	jsr util::hextostr
	tya
	jsr @appendch
	txa
	jsr @appendch

@chkindexed:
	lda @modes
	and #MODE_X_INDEXED
	beq :+
@xindexed:
	lda #','
	jsr @appendch
	lda #'x'
	jsr @appendch

:	lda @modes
	and #MODE_INDIRECT
	beq :+
@indirect2:
	lda #')'
	jsr @appendch

:	lda @modes
	and #MODE_Y_INDEXED
	beq @done
@yindexed:
	lda #','
	jsr @appendch
	lda #'y'
	jsr @appendch

@done:  ldx #$02
	lda @modes
	and #MODE_ZP
	bne :+
	inx
:	lda #$00
	jsr @appendch

	txa			; .A = size
	ldx @modes		; .X = address modes
	RETURN_OK

;--------------------------------------
; APPEND CH
; Appends a character to the disassembled instruction
@appendch:
	sty savereg
	ldy @nostr
	bne :+
	;ldy #$00
	sta (@dst),y
	incw @dst
:	ldy savereg
	rts
.endproc

;*******************************************************************************
; ASSEMBLE_MACRO
; Takes the contents of (line) and expands it to the corresponding macro.
; IN:
;  - .A the id of the macro to assemble
.proc assemble_macro
@cnt=zp::macros+$0e
@id=zp::macros+$0f
@params=zp::macros
	sta @id
	ldx #$fe	; -2

	; read all the parameters for the macro
@l0:	ldy #$00
	inx
	inx
@l1:	lda (zp::line),y
	beq @done
	iny
	jsr util::is_whitespace
	bne @l1

	stx @cnt
	jsr line::process_ws
	jsr expr::eval
	bcc @setparam
	rts		; return err

@setparam:
	txa
	ldx @cnt
	sta @params,x
	tya
	sta @params+1,x

	ldy #$00
@nextparam:
	lda (zp::line),y 	; read until comma or endline
	beq @done		; 0 (end of line) we're done, assemble
	cmp #';'		; ';' (comment) - also done
	beq @done
	jsr line::incptr
	cmp #','
	beq @l0
	jsr util::is_whitespace
	beq @nextparam
	RETURN_ERR ERR_INVALID_MACRO_ARGS

@done:	lda @id
	JUMP FINAL_BANK_MACROS, mac::asm
.endproc

;*******************************************************************************
; DO_IF
; handles .IF during assembly
; OUT:
;  - .C: set if error
.proc do_if
	lda ifstacksp
	cmp #MAX_IFS
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	; evaluate the condition for the .IF
	; TODO: make sure expression resolvable in pass 1
	jsr expr::eval
	bcs @done
	txa
	bne @true
	tya
	beq @false

@true:	ldx #$01

@false:	; store the TRUE/FALSE value to the if stack
	txa
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
	lda #ASM_DIRECTIVE
	clc			; ok
@done:	rts
.endproc

;*******************************************************************************
; DO_ENDIF
; Handles .ENDIF during assembly
.proc do_endif
	lda ifstacksp
	bne :+
	RETURN_ERR ERR_UNMATCHED_ENDIF

:	dec ifstacksp
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; DO_ELSE
; handles .ELSE during assembly
.proc do_else
	ldx ifstacksp
	lda #$01
	eor ifstack,x
	sta ifstack,x
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; DO_IFDEF
; handles the .IFDEF directive during assembly
.proc do_ifdef
	lda ifstacksp
	cmp #MAX_IFS
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	; check if the label exists
	ldxy zp::line
	jsr lbl::find
	lda #$00
	bcs :+		; label not defined
	lda #$01

:	; store TRUE/FALSE to the if stack
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
@done:
	jsr line::process_word
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; ISLINETERMINATOR
; IN:
;  .A: the character to check
; OUT:
;  - .Z: set if the .A is a 0 or ';'
.proc islineterminator
	cmp #$00
	beq :+
	cmp #';'
:	rts
.endproc

;*******************************************************************************
; IS_LDX_STX
; Checks if the given opcode is a LDX/STX
; OUT:
;  - .C: clear if the given opcode is a LDX/STX
.proc is_ldx_stx
	pha
	lda cc
	cmp #$02	; only applicable if cc = %10
	bne @no

	lda opcode
	cmp #$80	; aaa = 100 STX ($8)
	beq @yes
	cmp #$a0	; aaa = 101 LDX ($a)
	beq @yes
@no:	sec
	skb
@yes:	clc
	pla
	rts
.endproc

;*******************************************************************************
; DEFINECONST
; Hanldes the .EQ directive
; Effective on 1st pass only
.proc defineconst
	lda zp::pass
	cmp #$01
	beq :+
	lda #ASM_DIRECTIVE
	RETURN_OK

:	ldxy zp::line
	jsr lbl::isvalid
	bcs @err

	lda zp::line		; save label name's address
	pha
	lda zp::line+1
	pha

	; read to next whitespace (move past the constant name)
	jsr line::process_word

@cont:	jsr line::process_ws	; eat whitespace
	inc zp::pass		; require label predefinition for constants
	jsr expr::eval		; get constant value
	dec zp::pass		; set pass back to correct value
	bcc @ok

	; clean the stack and return error
	plp
	plp
	sec
@err:	rts

@ok:	stxy zp::label_value

	; restore label address
	pla
	tay
	pla
	tax

	lda __asm_segmentid
	pha			; save section ID
	lda #SEG_ABS
	sta __asm_segmentid	; temporarily set section to ABS

	lda zp::label_value+1
	beq :+			; if MSB is 0, use ZP mode
	lda #$01

:	jsr add_label
	pla			; restore section ID
	sta __asm_segmentid

	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; ADD LABEL
; Adds a label with the given mode
; IN:
;   - .A:  the address mode for the label (0=ZP, 1=ABS), $ff=infer
;   - .XY: address of the label to add
; OUT:
;   - .C: set on error
.proc add_label
	cmp #$ff
	bne @add	; if size is not inferred, just add the label

@seg:	; if .SEG is defined, use its mode
	lda __asm_segmode	; 0=ZP, 1=ABS
	bpl @add		; branch always

@direct:
	; .ORG
	lda zp::virtualpc+1
	beq @add
	lda #$01		; ABS

@add:	sta zp::label_mode
	lda __asm_segmentid
	sta zp::label_segmentid
	jmp lbl::add
.endproc

;*******************************************************************************
; ADD_PC
; Adds the given value to the virtual PC and asmresult pointers
; IN:
;  - .A: the value to add to the assembly pointers (virtualpc and asmresult)
.proc addpc
	ldx zp::verify
	beq :+
	rts

:	pha
	clc
	adc zp::asmresult
	sta zp::asmresult
	tax
	lda zp::asmresult+1
	bcc :+
	inc zp::asmresult+1

:	tay
	pla
	clc
	adc zp::virtualpc
	sta zp::virtualpc
	bcc update_top
	inc zp::virtualpc+1
	bcs update_top		; branch always
.endproc

;*******************************************************************************
; INCPC
; Updates the asmresult and virtualpc pointers by 1
.proc incpc
	ldx zp::verify
	beq :+
	rts

:	incw zp::asmresult
	incw zp::virtualpc

	; fall through to update_top
.endproc

;*******************************************************************************
; UPDATE TOP
; Sets the top of the program to the given PC if it is higher than the current
; TOP
.proc update_top
	; update the top pointer if we are at the top of the program
	ldxy zp::asmresult
	cmpw top
	bcc :+
	stxy top
:	rts
.endproc

;*******************************************************************************
; WRITEB
; Stores a byte to (zp::asmresult),y
; Also checks if the origin has been set
; IN:
;  - .A: the value to write to (zp::asmresult),y
;  - .Y: the offset from (zp::asmresult) to write to
; OUT:
;  - .C: set on error, clear on success
.proc writeb
@savex=re
@savey=rf
	sta zp::bankval

	lda zp::verify
	bne @ok			; if just verifying, don't write

	lda pcset
	bne :+
	RETURN_ERR ERR_NO_ORIGIN

:	stx @savex
	sty @savey
	tya			; .A = offset
	ldxy zp::asmresult

	jsr vmem::writable
	bcc :+
	lda #ERR_PC_TARGET_UNWRITABLE
	;sec
	rts			; address is not writable

:	jsr vmem::store_off
@done:	ldx @savex
	ldy @savey
:				; <- write_reloc
@ok:	clc
:	rts
.endproc

;*******************************************************************************
; WRITEB WITH RELOC
; Writes the given byte out with relocation information
; IN:
;   - .A: the byte to write
;   - .Y: the offset from asmresult to write to
.proc writeb_with_reloc
	jsr writeb
	bcs :-			; -> rts

	lda #$00		; 0=zeropage
	; fall through to write_reloc
.endproc

;*******************************************************************************
; WRITE RELOC
; If assembling to object code, writes the relocation information
; IN:
;   - .A:                       size of the value to relocate (1 or 2)
;   - .Y:                       offset from zp::asmresult to apply relocation
;   - expr::require_relocation: !0 if we should use symbol as base address
;   - expr::contains_global:    !0 if symbol should be used as relocation base
;   - expr::global_id:          symbol ID to relocate relative to (if relevant)
;   - expr::global_op:          operation to apply the relocation with
;   - expr::global_postproc:    postprocessing to apply to global (if relevant)
.proc write_reloc
	ldx __asm_mode
	beq :--				; DIRECT mode -> ok (no relocation)

	ldx zp::pass
	cpx #$01
	beq :--				; -> ok (no relocation on pass 1)
	jmp obj::addreloc		; create/append relocation entry
.endproc

;*******************************************************************************
; WRITEW WITH RELOC
; Writes the given word out with relocation information
; IN:
;   - .XA: the word to write
;   - .Y: the offset from asmresult to write to
.proc writew_with_reloc
	pha
	txa
	jsr writeb		; write LSB
	pla			; restore MSB
	bcs :-			; -> rts

	iny			; next byte
	jsr writeb		; write MSB
	bcs :-			; -> rts

	lda #$01		; 1=ABS
	dey			; restore .Y
	jmp write_reloc
.endproc

;*******************************************************************************
; READB
; Reads a byte from (zp::asmresult),y
; IN:
;  - .Y: the offset from (zp::asmresult) to read from vmem
; OUT:
;  - .A: contains the byte from (zp::asmresult),y
.proc readb
@savex=re
@savey=rf
	stx @savex
	sty @savey
	tya			; .A = offset to load
	ldxy zp::asmresult
	jsr vmem::load_off	; load the byte from VMEM
	ldy @savey
	ldx @savex
	rts
.endproc

;*******************************************************************************
; SUB LABEL
; Substitutes a symbol name in the asmbuffer with its value.
; If the label is not found, does nothing
; This procedure is used to reduce lines before they are stored to the context
; buffer.
; e.g. "LDA A+B+$10" becomes "LDA $1000+B+$10"
; IN:
;   - .XY:       address of name of label to replace
;   - asmbuffer: buffer to find/replace the symbol in
; OUT:
;   - asmbuffer: updated buffer with symbol replaced
.proc sub_label
@cnt         = r0
@replace_idx = r1
@val         = r2
@backup      = r4
@len         = r6
@label       = zp::str0
@line        = zp::str2
@buff        = mem::spare
	stxy @label
	lda #<mem::asmbuffer
	sta @line
	lda #>mem::asmbuffer
	sta @line+1

@find:	ldxy @label
	jsr find_label
	bcs @done	; not found -> exit
	stxy @line	; @line = address of label to substitute

	; look up the address of the label we are substituting
	ldxy @label
	jsr lbl::addr
	bcs @done	; label doesn't exist -> exit
	stxy @val	; save the value of the symbol for later

	; get offset to start backup at (line+strlen(@label))
	ldxy #@buff
	stxy @backup

	; back up rest of line
	ldx #$00
	ldy @len
:	lda (@line),y
	sta @buff,x
	inx
	iny
	cpy #LINESIZE
	bne :-

	; replace label with its hex value in the line
	ldy #$00
	lda #'$'
	sta (@line),y
	inc @line
	lda @val+1
	jsr @write_hex
	lda @val
	jsr @write_hex

	; copy backup[replace_idx:LINESIZE] to asmbuffer[@cnt]
	; to append rest of line after our replacement
	ldy #$00
@l1:	lda (@backup),y
	sta (@line),y
	beq @find	; repeat procedure to replace next occurrence (if any)
	iny
	cpy #LINESIZE
	bcc @l1

@done:	rts

;--------------------------------------
; WRITE HEX
; helper to convert/write a hex value to buffer
@write_hex:
	jsr util::hextostr
	tya
	ldy #$00
	sta (@line),y
	txa
	iny
	sta (@line),y
	inc @line
	inc @line
	sty @cnt
	rts
.endproc

;*******************************************************************************
; FIND LABEL
; Searches for the given label in the given buffer
; e.g. when called with "A", returns the address "LDA A+B+$10"
;                                                     ^
; IN:
;   - .XY:      address of name of label to replace
;   - zp::str2: address of buffer to find the symbol in
; OUT:
;   - .XY:      address of the next occurrence of the label label
;   - zp::str0: the given label
.proc find_label
@cnt         = r0
@replace_idx = r1
@val         = r2
@backup      = r4
@len         = r6
@label       = zp::str0
@line        = zp::str2
@buff        = mem::spare
	stxy @label
	jsr str::len
	sta @len

	ldy #$00
	sty @cnt
@l0:	; read until we're NOT on a separator
	lda (@line),y
	jsr util::isseparator
	bne @check	; found a non-separator char -> check if it's our label
	inc @line
	inc @cnt
	lda @cnt
	cmp #LINESIZE
	bcc @l0
@notfound:
	rts

@check:	; check if we're now pointing to the label
	lda @len
	jsr str::compare
	beq @found

	; read until we ARE on a separator
	ldy #$00
@l1:	lda (@line),y
	jsr util::isseparator
	beq @l0		; we are back on a separator, continue to outer loop
	inc @line
	inc @cnt
	lda @cnt
	cmp #LINESIZE
	bcc @l1
	rts

@found: ; found the string (label)
	ldxy @line
	RETURN_OK
.endproc

;*******************************************************************************
; REPORT
; Writes a report about the current assembly state
; This is used after completing assembly to inform the user about the number
; of labels, etc. used
.export __asm_report
.proc __asm_report

.endproc
