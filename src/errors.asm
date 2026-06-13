;*******************************************************************************
; ERRORS.ASM
; This file contains the error definitions for errors that may occur during
; assembly or some other situations (such as loading a file).  The errors
; are stored in a compressed, 5-bits per character, format.  You can generate
; compressed errors with the utils/compress-text.py tool.
;*******************************************************************************

.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "text.inc"

.ifdef ultimem
.include "vic20/ultimem/banks.inc"
.endif

.ifdef fe3
.RODATA
.else
.segment "ERRORS"
.endif
;*******************************************************************************
err_no_err:
	.byte 0

err_stack_underflow:
	;.byte "stack underflow",0
	.byte $9d,$1, $1a,$db, $ab,$84, $2c,$86, $63,$d7, $0

err_stack_overflow:
	;.byte "stack overflow",0
	.byte $9d,$1,$1a,$db,$7d,$85,$91,$8c,$7d,$c0

err_line_too_long:
	;.byte "oversized line",0
	.byte $7d,$85,$94,$c9,$d1,$44,$db,$9,$71,$40

err_invalid_expression:
	;.byte "invalid expression",0
	.byte $4b,$96,$b,$9,$26,$c5,$c4,$12,$2c,$d3,$4b,$ce,$0

err_invalid_args_for_macro:
	;.byte "invalid macro args",0
	.byte $4b,$96,$b,$9,$26,$cd,$8,$d2,$7e,$c1,$91,$d3,$0

err_syntax:
	;.byte "syntax error",0
	.byte $9e,$4e,$a0,$58,$d9,$52,$93,$d2,$0

err_invalid_directive:
	;.byte "invalid directive",0
	.byte $4b,$96,$b,$9,$26,$c4,$4c,$85,$1d,$9,$b1,$40

err_undefined_label:
	;.byte "label undefined",0
	.byte $60,$42,$2b,$1b,$ab,$84,$29,$89,$71,$44,$0

err_unmatched_endif:
	;.byte "endif with no if",0
	.byte $2b,$84,$49,$9b,$ba,$54,$46,$ce,$7e,$c9,$30,$0
err_too_many_macros:
	; .byte "too many macros",0
	.byte $a3,$cf,$db,$41,$76,$5b,$68,$43,$93,$d3,$0

;*******************************************************************************
err_unaligned_label:
	;.byte "label not left aligned",0
	.byte $60,$42,$2b,$1b,$73,$d4,$db,$5,$35,$1b,$b,$9,$3b,$85,$20,$0

err_illegal_opcode:
	;.byte "invalid opcode",0
	.byte $4b,$96,$b,$9,$26,$cf,$80,$cf,$21,$40

err_illegal_addrmode:
	;.byte "invalid addr mode",0
	.byte $4b,$96,$b,$9,$26,$c1,$21,$12,$db,$4f,$21,$40

err_oversized_operand:
	;.byte "oversized operand", 0
	.byte $7d,$85,$94,$c9,$d1,$44,$db,$d0,$2c,$81,$71,$0

err_illegal_label:
	;.byte "invalid label",0
	.byte $4b,$96,$b,$9,$26,$cc,$8,$85,$60,$0

err_label_too_long:
	; .byte "label too long",0
	.byte $60,$42,$2b,$1b,$a3,$cf,$db,$f,$71,$c0

err_unexpected_char:
	;.byte "unexpected char", 0
	.byte $ab,$85,$c4,$5,$1d,$5,$26,$c3,$40,$52,$0

err_value_expected:
	;.byte "expected value", 0
	.byte $2e,$10,$28,$d4,$29,$1b,$b0,$4c,$a9,$40

err_io:
	;.byte "io error"
	.byte $4b,$db,$2c,$92,$7c,$80

err_no_macro_name:
	;.byte "no macro name",0
	.byte $73,$db,$68,$43,$93,$db,$70,$4d,$28,$0

err_unresolvable_label:
	;.byte "label unresolvable",0
	.byte $60,$42,$2b,$1b,$ab,$92,$2c,$cf,$65,$81,$13,$5,$0

err_cyclic_include:
	;.byte "cyclic include",0
	.byte $1e,$43,$62,$43,$da,$4e,$1b,$15,$21,$40

err_failed_to_open_include:
	;.byte "failed to open include file",0
	.byte $30,$49,$61,$44,$dd,$f,$db,$d0,$2b,$9b,$4b,$83,$65,$44,$2e,$c6,$4b,$5,$0

err_overlapping_segments:
	;.byte "segment overlap",0
	.byte $99,$47,$69,$4e,$a6,$cf,$b1,$52,$60,$50,$0

err_max_files_exceeded:
	;.byte "too many files",0
	.byte $a3,$cf,$db,$41,$76,$5b,$32,$4c,$2c,$c0

err_param_name_too_long:
	;.byte "param name too long",0
	.byte $80,$52,$b,$5b,$70,$4d,$2e,$d4,$7b,$db,$63,$ce,$38,$0

err_line_not_found:
	;.byte "line not found for address",0
	.byte $62,$4e,$2e,$ce,$7d,$1b,$33,$d5,$71,$1b,$33,$d2,$d8,$44,$24,$85,$9c,$c0

err_no_origin:
	;.byte "origin unset",0
	.byte $7c,$89,$3a,$4e,$dd,$4e,$99,$54,$0

err_branch_out_of_range:
	;.byte "range error",0
	.byte $90,$4e,$39,$5b,$2c,$92,$7c,$80

err_file_not_found:
	;.byte "file not found",0
	.byte $32,$4c,$2e,$ce,$7d,$1b,$33,$d5,$71,$0

err_unknown_segment:
	;.byte "unknown segment",0
	.byte $ab,$8b,$73,$d7,$76,$d3,$29,$cd,$2b,$94,$0

err_import_undefined:
	; .byte "import undefined",0
	.byte $4b,$50,$7c,$94,$dd,$4e,$21,$46,$4b,$85,$20,$0

err_pc_target_unwritable:
	; .byte "pc target unwritable",0
	.byte $80,$db,$a0,$52,$39,$54,$dd,$4e,$bc,$89,$a0,$42,$61,$40

err_cannot_reduce:
	; .byte "cannot reduce expression", 0
	.byte $18,$4e,$73,$d4,$dc,$85,$25,$43,$2e,$c5,$c4,$12,$2c,$d3,$4b,$ce,$0

err_too_many_open_files:
	;.byte "too many open files",0
	.byte $a3,$cf,$db,$41,$76,$5b,$7c,$5,$76,$c6,$4b,$5,$98,$0

err_logical_file_in_use:
	;.byte "logical file in use",0
	.byte $63,$c7,$48,$c1,$66,$c6,$4b,$5,$da,$4e,$dd,$53,$28,$0

err_drive_did_not_respond:
	;.byte "no drive response",0
	.byte $73,$db,$24,$89,$b1,$5b,$91,$53,$83,$ce,$99,$40

err_file_too_big:
	; .byte "file too big",0
	.byte $32,$4c,$2e,$d4,$7b,$db,$12,$47,$0

err_unnamed_buffer:
	;.byte "unnamed buffer",0
	.byte $ab,$8e,$b,$45,$26,$c2,$a9,$86,$2c,$80

err_too_many_buffers:
	;.byte "too many open buffers",0
	.byte $a3,$cf,$db,$41,$76,$5b,$7c,$5,$76,$c2,$a9,$86,$2c,$93,$0

err_no_filename:
	;.byte "no file name",0
	.byte $73,$db,$32,$4c,$2e,$ce,$b,$45,$0

err_no_open_scope:
	; .byte "no open scope",0
	.byte $73,$db,$7c,$5,$76,$d3,$1b,$d0,$28,$0

err_label_already_defined:
	.byte $60,$42,$2b,$1b,$b,$12,$28,$44,$ce,$c4,$29,$89,$71,$44,$0

err_too_many_labels:
	.byte $68,$58,$db,$1,$11,$4c,$9e,$c5,$c0,$c5,$29,$5,$20,$0

err_label_unknown_pass1:
	.byte $60,$42,$2b,$1b,$9,$4,$91,$53,$9e,$ce,$7d,$1b,$4b,$86,$2c,$92,$29,$0

err_invalid_command:
	.byte $4b,$96,$b,$9,$26,$c3,$7b,$4d,$b,$84,$0

err_copy_too_big:
	.byte $1b,$d0,$ce,$d3,$2b,$5,$1d,$9,$7b,$9b,$a3,$cf,$d8,$89,$38,$0

err_buffer_name_exists:
	.byte $15,$46,$31,$52,$db,$81,$69,$5b,$2e,$9,$9d,$13,$0

err_buffer_full:
	.byte $15,$46,$31,$52,$d9,$95,$63,$0

err_too_many_global_refs:
.byte $2e,$10,$96,$c3,$b,$9b,$40,$56,$2e,$cf,$73,$19,$dc,$5b,$3b,$f,$10,$4c,$dc,$85,$30,$0

err_too_many_segments:
; "too many segments"
.byte $a3,$cf,$db,$41,$76,$5b,$99,$47,$69,$4e,$a4,$c0

err_section_too_small:
; .byte "section overrun",0
.byte $99,$43,$a2,$4f,$76,$cf,$b1,$52,$95,$4e,$0

err_addrmode_mismatch:
; "conflicting address modes for symbol"
.byte $1b,$ce,$33,$9,$1d,$9,$71,$db,$9,$4,$91,$53,$9e,$cd,$79,$5,$9e,$c6,$7c,$9b,$9e,$4d,$13,$cc,$0

err_already_exported:
; "multiple definitions for symbol"
.byte $6d,$4c,$a2,$50,$61,$5b,$21,$46,$4b,$89,$a2,$4f,$74,$db,$33,$d2,$dc,$d9,$68,$8f,$60,$0

err_unknown_type:
; "unknown type"
.byte $ab,$8b,$73,$d7,$76,$d4,$cc,$5,$0

err_unexpected_type:
; "unexpected type"
.byte $ab,$85,$c4,$5,$1d,$5,$26,$d4,$cc,$5,$0

err_oom:
; "out of memory"
.byte $7d,$54,$db,$c6,$db,$45,$6b,$d2,$c8,$0

err_ctx_full:
; "context full"
.byte $1b,$ce,$a1,$58,$a6,$c6,$ab,$c,$0

err_no_matching_scope:
; .byte "no matching scope",0
.byte $73,$db,$68,$54,$1a,$9,$71,$db,$98,$cf,$81,$40

;*******************************************************************************
.linecont +
.define errors \
	err_no_err, \
	err_stack_underflow, \
	err_stack_overflow, \
	err_line_too_long, \
	err_invalid_expression, \
	err_invalid_args_for_macro, \
	err_syntax, \
	err_invalid_directive, \
	err_undefined_label, \
	err_unmatched_endif, \
	err_too_many_macros, \
	err_unaligned_label, \
	err_illegal_opcode, \
	err_illegal_addrmode, \
	err_oversized_operand,\
	err_illegal_label, \
	err_label_too_long, \
	err_unexpected_char, \
	err_value_expected, \
	err_io, \
	err_no_macro_name, \
	err_unresolvable_label, \
	err_cyclic_include, \
	err_failed_to_open_include, \
	err_overlapping_segments, \
	err_max_files_exceeded, \
	err_param_name_too_long, \
	err_line_not_found, \
	err_no_origin, \
	err_branch_out_of_range, \
	err_file_not_found, \
	err_unknown_segment, \
	err_import_undefined, \
	err_pc_target_unwritable, \
	err_cannot_reduce, \
	err_too_many_open_files, \
	err_logical_file_in_use, \
	err_drive_did_not_respond, \
	err_file_too_big, \
	err_unnamed_buffer, \
	err_too_many_buffers, \
	err_no_filename, \
	err_no_open_scope, \
	err_label_already_defined, \
	err_too_many_labels, \
	err_label_unknown_pass1, \
	err_invalid_command, \
	err_copy_too_big, \
	err_buffer_name_exists, \
	err_buffer_full, \
	err_too_many_global_refs, \
	err_too_many_segments, \
	err_section_too_small, \
	err_addrmode_mismatch, \
	err_already_exported, \
	err_unknown_type, \
	err_unexpected_type, \
	err_oom, \
	err_ctx_full, \
	err_no_matching_scope
.linecont -
errorslo: .lobytes errors
errorshi: .hibytes errors
NUM_ERRORS=*-errorshi

err_unknown_err: .byte $aa,$ce,$7d,$ce,$d9,$52,$93,$d2,$0

.CODE
;*******************************************************************************
; GET
; Returns the address of the given error id's error message
; IN:
;  - .A: the ID of the error to get the message for
; OUT:
;  -.XY: the address of the error message
.export __err_get
.proc __err_get
	tax
	cpx #NUM_ERRORS
	bcc :+
	ldx #<err_unknown_err
	ldy #>err_unknown_err
	bne @uncompress		; branch always

:
.ifdef ultimem
@err=r0
@compressed_buff=mem::spare+120
	; bank in the errors
	lda $9ffe
	pha
	lda #ERRORS_BANK
	sta $9ffe	; bring errors into BLK5

	; copy the error to a buffer
	lda errorslo,x
	sta @err
	lda errorshi,x
	sta @err+1
	ldy #$00
@l0:	lda (@err),y
	sta @compressed_buff,y
	beq :+
	iny
	bne @l0

:	pla
	sta $9ffe
	ldxy #@compressed_buff
.else
	ldy errorshi,x
	lda errorslo,x
	tax
.endif

@uncompress:
	jmp str::uncompress
.endproc
