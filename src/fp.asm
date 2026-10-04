;*******************************************************************************
; FP.ASM
; Floating point support for the expression evaluator, built on the Commodore
; BASIC ROM's floating point package.
;
; Floats are handled in the BASIC ROM's 5-byte format.  Values enter this
; module either by parsing a literal (fp::parse) or by promoting a 16-bit
; integer (fp::fromwide) and executed via fp::binop.
;*******************************************************************************

FP_IMPL = 1

.include "errors.inc"
.include "fp.inc"
.include "macros.inc"
.include "zeropage.inc"

.macpack longbranch

;*******************************************************************************
; BASIC ROM ENTRY POINTS (VIC-20)
; Addresses are from the VIC-20 ROM disassembly; the label names in comments
; are the LAB_xxxx names used there.
.if FP_SUPPORTED

FAC1_EXP   = $61	; FAC1 exponent ($62-$65 mantissa, $66 sign)
FAC1_SIGN  = $66
FAC1_ROUND = $70	; rounding byte; every pack applies it
FAC_SCRATCH_LEN = FAC1_ROUND-FAC1_EXP+1

FP_GIVAYF  = $d391	; LAB_D391: signed integer .AY (A=hi) -> FAC1
FP_FSUBM   = $d850	; LAB_D850: FAC1 = (.AY) - FAC1
FP_FADDM   = $d867	; LAB_D867: FAC1 = FAC1 + (.AY)
FP_FMULTM  = $da28	; LAB_DA28: FAC1 = FAC1 * (.AY)
FP_MUL10   = $dae2	; LAB_DAE2: FAC1 = FAC1 * 10
FP_DIV10   = $dafe	; LAB_DAFE: FAC1 = FAC1 / 10
FP_FDIVM   = $db0f	; LAB_DB0F: FAC1 = (.AY) / FAC1
FP_MOVFM   = $dba2	; LAB_DBA2: FAC1 = (.AY)         (A=lo, Y=hi)
FP_MOVMF   = $dbd4	; LAB_DBD4: (.XY) = FAC1         (X=lo, Y=hi)
FP_QINT    = $dc9b	; LAB_DC9B: FAC1 -> signed 32-bit big endian in $62-$65
FP_ROM_INT = $dccc	; LAB_DCCC: FAC1 = INT(FAC1)
FP_FCOMP   = $dc5b	; compare FAC1 with (.AY): -1, 0, +1
FP_ADDIGIT = $dd7e	; LAB_DD7E: FAC1 = .A + FAC1  (.A is a binary digit 0-9)
FP_FOUT    = $dddd	; format FAC1 at $0100 (leading space or minus)
FP_ROM_SQRT = $df71
FP_ROM_SIN = $e268
FP_ROM_COS = $e261
FP_ROM_LOG = $d9ea
FP_ROM_EXP = $dfed

BASIC_IERROR = $0300	; vector LAB_C437 jumps through to report an error
BASIC_ERR_DIV0 = $14	; BASIC error $14: division by zero

; The outer bounds of the ROM's zeropage use.  Nothing it can reach from the
; entry points below touches anything outside these; what each individual
; operation actually saves is the much smaller set in `zpranges'.
ZPA_START = $02
ZPA_LEN   = $29-$02+1
ZPA_END   = ZPA_START+ZPA_LEN-1
ZPB_START = $4e
ZPB_LEN   = $8f-$4e+1
ZPB_END   = ZPB_START+ZPB_LEN-1

; ...and the one byte it touches outside them.  FOUT's digit loop borrows
; BASIC's "current variable pointer low byte" as its powers-of-ten table index
; (LAB_DE97: STY $47 / LAB_DEB2: LDY $47), which lands on zp::gui+7 - the item
; count of whatever debugger LIST window happens to be open.  It is written
; before it is read, so it only needs saving, not clearing.
ZPC_START = $47
ZPC_LEN   = 1

;*******************************************************************************
; PROTECTED OPERATIONS
; The id passed to `protect' selects both the routine to run and the zeropage
; that routine's ROM path needs saved, so the two cannot disagree.
FPP_BUILD   = 0		; do_build:   MUL10, ADDIGIT, DIV10, MOVMF
FPP_BINOP   = 1		; do_binop:  MOVFM, F{ADD,SUB,MULT,DIV}M, FCOMP, GIVAYF
FPP_UNARY   = 2		; do_unary:  above plus INT, SQRT, SIN, COS, LOG, EXP
FPP_FORMAT  = 3		; do_format: MOVFM, FOUT
FPP_COUNT   = 4

ZPSAVE_MAX = 44		; the largest of the sets below (FPP_UNARY)

;*******************************************************************************
; IRQ SAFE
; Asserts that the given zeropage location is outside all of the ROM's windows.
; The ROM trashes those windows with interrupts enabled, so anything the IRQ
; path touches has to live elsewhere (see (3) above; the handler itself is in
; vic20/irq.asm)
.macro FP_IRQ_SAFE loc
	.assert (loc<>ZPC_START) && ((loc<ZPA_START) || ((loc>ZPA_END) && (loc<ZPB_START)) || (loc>ZPB_END)), error, "IRQ zeropage overlaps the FP ROM's window"
.endmacro

	FP_IRQ_SAFE $c5			; KERNAL: key count of the last scan
	FP_IRQ_SAFE $c6			; KERNAL: keyboard buffer index
	FP_IRQ_SAFE zp::key		; KERNAL: key count of this scan
	FP_IRQ_SAFE zp::keytab		; KERNAL: decode table pointer
	FP_IRQ_SAFE zp::keytab+1
	FP_IRQ_SAFE r5			; blink_update's cell pointer (= keytab)
	FP_IRQ_SAFE r6

.endif

;*******************************************************************************
.segment "FP_BSS"

.export __fp_arg1
__fp_arg1: .res FP_SIZE		; left operand for fp::binop

.export __fp_arg2
__fp_arg2: .res FP_SIZE		; right operand for fp::binop

.export __fp_val
__fp_val:  .res FP_SIZE		; result of every fp:: routine

.if FP_SUPPORTED

tmpf:   .res FP_SIZE		; scratch packed float

;-------------------------------------------------------------------------------
; literal scanning state (filled in by fp::parse, consumed by do_build)
digits:  .res FP_MAX_DIGITS	; mantissa digits, decimal point removed
ndigits: .byte 0		; number of entries in digits
nfrac:   .byte 0		; how many of those are after the decimal point
dexp:    .byte 0		; signed exponent from the E suffix
expneg:  .byte 0		; !0 if the E suffix was negative
idx:     .byte 0		; digit index used while building
shift:   .byte 0		; remaining powers of 10 to apply

;-------------------------------------------------------------------------------
; protected-window state
fpvec:   .word 0		; routine `protect' should run
fpp:     .byte 0		; which FPP_ operation is in flight
fpop:    .byte 0		; operator for do_binop
fpsp:    .byte 0		; stack pointer to unwind to on a ROM error
rngi:    .byte 0		; offset of the next range in zpranges
rngend:  .byte 0		; offset one past this operation's last range
zpend:   .byte 0		; end (exclusive) of the range being copied
errcode: .byte 0		; error code to hand back
romerr:  .byte 0		; BASIC's error number, as passed in .X
errvec:  .word 0		; previous contents of $0300
zpsave:  .res ZPSAVE_MAX
unarysign: .byte 0
strsave: .res 32		; FOUT uses the bottom of the hardware stack page

.endif

;*******************************************************************************
; The FP code has its own segment, but it must be linked into the same bank as
; EXPR (see FINAL_BANK_FP) so that expr.asm can call it directly.
.segment "FP"

;*******************************************************************************
; PARSE
; Parses the float literal at zp::line and leaves the packed result in fp::val.
; IN:
;   - zp::line: the literal to parse (a LEX_FLOAT token)
; OUT:
;   - fp::val:  the packed value
;   - zp::line: updated to point past the literal
;   - .A:       the error code on failure
;   - .C:       set on error
.export __fp_parse
.proc __fp_parse
.if FP_SUPPORTED
	lda #$00
	sta ndigits
	sta nfrac
	sta dexp
	sta expneg

	ldy #$00
@int:	lda (zp::line),y
	jsr isdigit
	bcs @dot
	jsr adddigit
	jcs @toolong
	iny
	bne @int

@dot:	cmp #'.'
	bne @exp
	iny
@frac:	lda (zp::line),y
	jsr isdigit
	bcs @exp
	jsr adddigit
	bcs @toolong
	inc nfrac
	iny
	bne @frac

@exp:	cmp #'e'
	beq :+
	cmp #'E'
	bne @scanned

:	iny
	lda (zp::line),y
	cmp #'+'
	beq @esign
	cmp #'-'
	bne @firstdigit
	inc expneg
@esign:	iny

@firstdigit:
	; Require a digit even when the mantissa already identified a float.
	; In particular, "1.0e" and "1.0e+" must not become 1.0.
	lda (zp::line),y
	jsr isdigit
	bcc @edigits
	RETURN_ERR ERR_INVALID_EXPRESSION

@edigits:
	lda (zp::line),y
	jsr isdigit
	bcs @esign_done

	; dexp = dexp*10 + digit; two digits of exponent is all we allow (the
	; ROM's range is only about 1e38 anyway)
	sec
	sbc #'0'		; the digit's VALUE, not its character
	pha
	lda dexp
	cmp #$0a
	bcs @toolong_pla
	asl			; *2
	sta shift
	asl
	asl			; *8
	clc
	adc shift		; *10
	sta dexp
	pla
	clc
	adc dexp
	sta dexp
	iny
	bne @edigits

@esign_done:
	lda expneg
	beq @scanned
	lda #$00
	sec
	sbc dexp
	sta dexp

@scanned:
	; move the line pointer past the literal
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	lda #FPP_BUILD
	jmp protect

@toolong_pla:
	pla
@toolong:
	RETURN_ERR ERR_OVERSIZED_OPERAND
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

;*******************************************************************************
; FROM WIDE
; Packs an unsigned 24-bit integer without calling the BASIC ROM.
; IN:
;   - .A:  high byte of 24-bit value
;   - .XY: low word
; OUT:
;   - fp::val: floating-point value
;   - .C:      clear
.export __fp_fromwide
.proc __fp_fromwide
.if FP_SUPPORTED
	sta __fp_val
	sta __fp_val+1
	sty __fp_val+2
	stx __fp_val+3
	ora __fp_val+2
	ora __fp_val+3
	beq @done
	lda #$98
	sta __fp_val

@normalize:
	lda __fp_val+1
	bmi @done
	asl __fp_val+3
	rol __fp_val+2
	rol __fp_val+1
	dec __fp_val
	bne @normalize

@done:	and #$7f
	sta __fp_val+1
	lda #$00
	sta __fp_val+4
	clc
	rts
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

;*******************************************************************************
; TO WIDE
; Converts an integral float to an unsigned 24-bit integer.
; IN:
;   - fp::val: floating-point value
; OUT:
;   - .A:  high byte
;   - .XY: low word
;   - .C:  set
;   - .A:  error code on fractional or out-of-range value
.export __fp_towide
.proc __fp_towide
.if FP_SUPPORTED
	lda __fp_val
	beq @zero
	cmp #$81
	bcc @fraction
	cmp #$99
	bcs @range
	eor #$ff
	clc
	adc #$a1
	tax			; shift four-byte mantissa down to an integer

	ldy #$03
@copy:	lda __fp_val+1,y
	sta tmpf,y
	dey
	bpl @copy
	lda tmpf
	ora #$80
	sta tmpf

@shift:	lsr tmpf
	ror tmpf+1
	ror tmpf+2
	ror tmpf+3
	bcs @fraction
	dex
	bne @shift
	lda __fp_val+1
	bmi @range
	ldx tmpf+3
	ldy tmpf+2
	lda tmpf+1
	clc
	rts
@zero:	tax
	tay
	clc
	rts
@fraction:
	RETURN_ERR ERR_NOT_INTEGRAL
@range:	RETURN_ERR ERR_OVERSIZED_OPERAND
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

;*******************************************************************************
; BINOP
; Applies a binary operator to fp::arg1 (left) and fp::arg2 (right).
; IN:
;   - .A:       the operator ('+', '-', '*' or '/')
;   - fp::arg1: the left operand
;   - fp::arg2: the right operand
; OUT:
;   - fp::val: the result
;   - .A:      the error code on failure
;   - .C:      set on error
.export __fp_binop
.proc __fp_binop
.if FP_SUPPORTED
	sta fpop
	lda #FPP_BINOP
	jmp protect
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

.export __fp_unary
.proc __fp_unary
.if FP_SUPPORTED
	sta fpop
	lda #FPP_UNARY
	jmp protect
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

.import __expr_floatstr
.export __fp_format
.proc __fp_format
.if FP_SUPPORTED
	ldx #31
:	lda $100,x
	sta strsave,x
	dex
	bpl :-
	lda #FPP_FORMAT
	jsr protect
	php
	pha
	ldx #31
:	lda strsave,x
	sta $100,x
	dex
	bpl :-
	pla
	plp
	rts
.else
	RETURN_ERR ERR_INVALID_EXPRESSION
.endif
.endproc

.if FP_SUPPORTED

;*******************************************************************************
; PROTECT
; Runs one FPP_ operation with the zeropage that operation needs saved and the
; ROM's error vector pointed at our own handler.
; The caller's interrupt-enable state is left exactly as it was found; see (3)
; at the top of this file for why the ROM call does not need it masked.
; IN:
;   - .A: the FPP_ operation to run
; OUT:
;   - .A: the error code on failure
;   - .C: set on error
.proc protect
	sta fpp
	tax
	lda fpvecs_lo,x		; the routine that goes with this operation
	sta fpvec
	lda fpvecs_hi,x
	sta fpvec+1

	jsr save_zp
	jsr clear_fac
	jsr hook_error

	tsx
	stx fpsp		; where error_entry unwinds to

	jsr docall
	bcs out
	lda #$00		; no error

out:	sta errcode
	php			; hold on to the result carry
	jsr unhook_error
	jsr restore_zp
	lda errcode
	plp
	rts

docall:	jmp (fpvec)

;-------------------------------------------------------------------------------
; the routine behind each FPP_ id, in the same order as zpset_start
fpvecs_lo:
	.byte <do_build, <do_binop, <do_unary, <do_format
fpvecs_hi:
	.byte >do_build, >do_binop, >do_unary, >do_format
.assert fpvecs_hi - fpvecs_lo = FPP_COUNT, error, "fpvecs does not match FPP_COUNT"

;-------------------------------------------------------------------------------
; entered from inside the ROM by way of JMP ($0300).  .X holds BASIC's error
; number and the stack still holds the ROM's frames, so unwind before doing
; anything else.
error_entry:
	stx romerr
	ldx fpsp
	txs
	lda romerr
	cmp #BASIC_ERR_DIV0
	bne :+
	lda #ERR_DIVIDE_BY_ZERO
	bne @error_done
:	cmp #$0f		; BASIC overflow
	bne @domain
	lda #ERR_OVERSIZED_OPERAND
	bne @error_done
@domain:
	lda #ERR_INVALID_EXPRESSION	; e.g. SQRT(-1) or LOG(0)
@error_done:
	sec
	bcs out			; branch always
.endproc

;*******************************************************************************
; CLEAR FAC
; Wipes the ROM's floating point scratch ($61-$70: FAC1, FAC2, the sign
; comparison and the rounding byte).
;
; This matters more than it looks.  Those bytes belong to Monster (zp::labels is
; at $57, zp::ctx at $70), so on entry they hold whatever the assembler last put
; there - the FP package would otherwise start every operation from a garbage
; state.  The rounding byte is the worst of them: every pack applies it, so a
; stale $70 quietly shifts the result by one ulp, which is what made INT(2.0)
; pack to different bytes than 2.0 and turned "2.0" into "non integral value".
.proc clear_fac
	lda #$00
	ldx #FAC_SCRATCH_LEN-1
:	sta FAC1_EXP,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; ZEROPAGE RANGES
; What each operation has to save, as (first address, length) pairs.  These are
; the addresses that operation's ROM path can actually reach, traced through the
; ROM listing from the entry points named against each FPP_ id above; saving the
; ROM's whole working area instead would be three to five times the copying on
; every call.
;
; Two constraints on anything added here:
;   - every set must cover $61-$70, because `clear_fac' wipes all of it before
;     each operation regardless of which one it is
;   - a gap of a byte or two is cheaper to copy through than to describe, since
;     each range costs about 35 cycles of setup
;   - a "#<LAB_5C" style immediate in the ROM listing is a pointer to a packed
;     float, so it claims five bytes, not one; that is what the $4e and $57/$5c
;     pack destinations in the unary set are
ZPRLEN .set 0

.macro ZPR addr, len
	.byte addr, len
	ZPRLEN .set ZPRLEN+len
.endmacro

; closes a set: checks it still fits in zpsave and starts the count over
.macro ZPREND
	.assert ZPRLEN <= ZPSAVE_MAX, error, "zeropage save set exceeds zpsave"
	ZPRLEN .set 0
.endmacro

zpranges:
zpr_build:				; 25 bytes
	ZPR $22,8			; misc temps and the product area
	ZPR $56,1			; FAC temp store
	ZPR $61,16			; FAC1, FAC2, sign compare, rounding
	ZPREND
zpr_binop:				; 26 bytes
	ZPR $0d,1			; GIVAYF, via do_compare
	ZPR $22,8
	ZPR $56,1
	ZPR $61,16
	ZPREND
zpr_unary:				; 44 bytes
	ZPR $07,1
	ZPR $12,1			; comparison flag, used by SIN/COS
	ZPR $22,8
	ZPR $4e,5			; the transcendentals pack FAC1 into $4e,
	ZPR $56,29			; ..$72: $57 and $5c as well, then FAC1/2
	ZPREND
zpr_format:				; 31 bytes
	ZPR $22,8
	ZPR $47,1			; FOUT's powers of ten index (gui::num)
	ZPR $56,1
	ZPR $5d,21			; ..$71: FAC temps, FAC1/2, output index
	ZPREND
zpranges_end:

; offset of each set in zpranges; each set ends where the next one starts, so
; the sentinel at the end belongs to the list
zpset_start:
	.byte zpr_build-zpranges
	.byte zpr_binop-zpranges
	.byte zpr_unary-zpranges
	.byte zpr_format-zpranges
	.byte zpranges_end-zpranges
.assert * - zpset_start = FPP_COUNT+1, error, "zpset_start does not match FPP_COUNT"

;*******************************************************************************
; INIT RANGES
; Points the range walker at the set for the operation in flight.
; OUT:
;   - .Y:      zero, the running offset into zpsave
;   - rngi/rngend: the operation's slice of zpranges
.proc init_ranges
	ldx fpp
	lda zpset_start,x
	sta rngi
	lda zpset_start+1,x
	sta rngend
	ldy #$00
	rts
.endproc

;*******************************************************************************
; NEXT RANGE
; Steps the walker on to the next range.
; OUT:
;   - .X:   the first address of the range
;   - zpend: one past its last address
;   - .C:   set when the set is exhausted
; CLOBBERS: .A
.proc next_range
	ldx rngi
	cpx rngend
	bcs @done		; if no more ranges, we're done

	inc rngi
	inc rngi

	lda zpranges,x		; first address of the range
	clc
	adc zpranges+1,x	; + its length = one past its last address
	sta zpend

	lda zpranges,x
	tax			; .X = first address
	clc			; more ranges to come
@done:	rts
.endproc

;*******************************************************************************
; SAVE ZP
; Copy the ZP locations used by the active operation for later restore
.proc save_zp
	jsr init_ranges
@range:	jsr next_range
	bcs @done
@copy:	lda $00,x		; zeropage, indexed by the address itself
	sta zpsave,y
	iny
	inx
	cpx zpend
	bne @copy
	beq @range		; branch always
@done:	rts
.endproc

;*******************************************************************************
; RESTORE ZP
; Restores the ZP locations that were clobbered by the operation
.proc restore_zp
	jsr init_ranges
@range:	jsr next_range
	bcs @done
@copy:	lda zpsave,y
	sta $00,x
	iny
	inx
	cpx zpend
	bne @copy
	beq @range		; branch always
@done:	rts
.endproc

;*******************************************************************************
; HOOK ERROR
; Redirect BASIC's error vector ($0300) to error_entry
.proc hook_error
	lda BASIC_IERROR
	sta errvec
	lda BASIC_IERROR+1
	sta errvec+1
	lda #<protect::error_entry
	sta BASIC_IERROR
	lda #>protect::error_entry
	sta BASIC_IERROR+1
	rts
.endproc

;*******************************************************************************
; UNHOOK ERROR
; Restores the error vector ($0300)
.proc unhook_error
	lda errvec
	sta BASIC_IERROR
	lda errvec+1
	sta BASIC_IERROR+1
	rts
.endproc

;*******************************************************************************
; DO BUILD
; Builds FAC1 from the digits collected by fp::parse and packs it into fp::val.
.proc do_build
	; FAC1 is already zero: protect clears the whole FP scratch on entry
	lda #$00
	sta idx

@digits:
	lda idx
	cmp ndigits
	bcs @scale
	jsr FP_MUL10		; FAC1 *= 10
	ldx idx
	inc idx			; (FP_ADDIGIT clobbers .X)
	lda digits,x
	jsr FP_ADDIGIT		; FAC1 += digit
	jmp @digits

@scale:	; apply the decimal point and the E exponent together
	lda dexp
	sec
	sbc nfrac
	sta shift
	beq @pack
	bmi @down

@up:	jsr FP_MUL10
	dec shift
	bne @up
	beq @pack

@down:	lda #$00
	sec
	sbc shift
	sta shift
@downl:	jsr FP_DIV10
	dec shift
	bne @downl

@pack:	jmp pack_result
.endproc

;*******************************************************************************
; DO BINOP
; Applies fpop to fp::arg1 and fp::arg2, leaving the result in fp::val.
.proc do_binop
	lda fpop
	cmp #FP_EQ
	bcc @arithmetic
	cmp #FP_GE+1
	bcs @badop
	jmp do_compare

@arithmetic:
	; ROM subtraction and division take (AY) as the left operand
	lda #<__fp_arg2
	ldy #>__fp_arg2
	jsr FP_MOVFM
	lda #<__fp_arg1
	ldy #>__fp_arg1
	ldx fpop
	cpx #'+'
	bne @sub
	jsr FP_FADDM
	jmp @pack
@sub:	cpx #'-'
	bne @mul
	jsr FP_FSUBM
	jmp @pack
@mul:	cpx #'*'
	bne @div
	jsr FP_FMULTM
	jmp @pack
@div:	cpx #'/'
	bne @badop
	jsr FP_FDIVM

@pack:	jmp pack_result

@badop:	RETURN_ERR ERR_INVALID_EXPRESSION
.endproc

;*******************************************************************************
; COMPARE
; Compare packed values directly; subtraction could overflow or underflow.
.proc do_compare
	lda #<__fp_arg1
	ldy #>__fp_arg1
	jsr FP_MOVFM
	lda #<__fp_arg2
	ldy #>__fp_arg2
	jsr FP_FCOMP
	clc
	adc #$01		; -1, 0, +1 -> bit-table index 0, 1, 2
	tax
	lda @bits,x
	ldx fpop
	and @masks-FP_EQ,x
	ldy #$00
	cmp #$00
	beq :+
	iny
:	lda #$00
	jsr FP_GIVAYF
	jmp pack_result

;-------------------------------------------------------------------------------
@bits:	.byte 1,2,4
@masks: .byte 2,5,1,3,4,6 ; ==, !=, <, <=, >, >=
.endproc

;*******************************************************************************
; UNARY FUNCTIONS
; Rounding functions retain the float type, including signed results.
.proc do_unary
	lda #<__fp_val
	ldy #>__fp_val
	jsr FP_MOVFM
	lda FAC1_SIGN
	and #$80
	sta unarysign
	lda fpop
	cmp #FP_FLOOR
	beq @floor
	cmp #FP_CEIL
	beq @ceil
	cmp #FP_SQRT
	beq @sqrt
	cmp #FP_SIN
	beq @sin
	cmp #FP_COS
	beq @cos
	cmp #FP_LOG
	beq @log
	cmp #FP_EXP
	beq @exp
	lda #$00
	sta FAC1_SIGN		; ABS, TRUNC, ROUND operate on magnitude first
	lda fpop
	cmp #FP_ABS
	beq @pack
	cmp #FP_ROUND
	bne @truncate
	lda #<c_half
	ldy #>c_half
	jsr FP_FADDM
@truncate:
	jsr FP_ROM_INT
	lda #$00
	sta FAC1_ROUND
	lda unarysign
	sta FAC1_SIGN
	jmp pack_result
@ceil:	lda FAC1_SIGN
	eor #$80
	sta FAC1_SIGN
	jsr FP_ROM_INT
	lda FAC1_SIGN
	eor #$80
	sta FAC1_SIGN
	jmp @clear_round
@floor:
	jsr FP_ROM_INT
@clear_round:
	lda #$00
	sta FAC1_ROUND
	beq @pack
@sqrt:	jsr FP_ROM_SQRT
	jmp @pack
@sin:	jsr FP_ROM_SIN
	jmp @pack
@cos:	jsr FP_ROM_COS
	jmp @pack
@log:	jsr FP_ROM_LOG
	jmp @pack
@exp:	jsr FP_ROM_EXP
@pack:	jmp pack_result
.endproc
c_half: .byte $80,$00,$00,$00,$00

;*******************************************************************************
; FORMAT
; Strip the ROM's leading space and publish a shared, null-terminated string.
.proc do_format
	lda #<__fp_val
	ldy #>__fp_val
	jsr FP_MOVFM
	jsr FP_FOUT
	ldx #$00
	ldy #$00
	lda $100
	cmp #' '
	bne @copy
	inx
@copy:	lda $100,x
	sta __expr_floatstr,y
	beq @done
	inx
	iny
	cpy #23
	bcc @copy
	lda #$00
	sta __expr_floatstr,y
@done:	RETURN_OK
.endproc

;*******************************************************************************
; PACK RESULT
; Keep zero canonical for emission and comparisons, regardless of the ROM's
; scratch mantissa after an operation producing zero (including underflow).
.proc pack_result
	lda FAC1_EXP
	bne @nonzero
	ldx #FP_SIZE-1
:	sta __fp_val,x
	dex
	bpl :-
	RETURN_OK
@nonzero:
	ldxy #__fp_val
	jsr FP_MOVMF
	RETURN_OK
.endproc

;*******************************************************************************
; ADDDIGIT
; Appends the ASCII digit in .A to the mantissa buffer.
; OUT:
;   - .C: set if the buffer is full
.proc adddigit
	pha
	lda ndigits
	cmp #FP_MAX_DIGITS
	bcs @full
	tax
	pla
	sec
	sbc #'0'
	sta digits,x
	inc ndigits
	clc
	rts

@full:	pla
	sec
	rts
.endproc
.endif

;*******************************************************************************
; ISDIGIT
; IN:
;   - .A: the character to test
; OUT:
;   - .C: clear if .A is '0'-'9'
.proc isdigit
	cmp #'0'
	bcc @no
	cmp #'9'+1
	bcs @no
	clc
	rts
@no:	sec
	rts
.endproc
