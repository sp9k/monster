.include "asm.inc"
.include "draw.inc"
.include "key.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "screen.inc"
.include "target.inc"
.include "text.inc"

START_ROW = 0

.if .defined(c64)
.define DEBUGGER_START "$cf00"
.define DEBUGGER_STOP  "$cfff"
.elseif .defined(vic20)
.define DEBUGGER_START "$9800"
.define DEBUGGER_STOP  "$9fff"
.endif

.CODE

;*******************************************************************************
.export __help_show
.proc __help_show
	JUMP FINAL_BANK_HELP, show
.endproc

.segment "HELP"

;*******************************************************************************
; SHOW
; Displays information about the system
;   DEBUGGER   start-addr, stop-addr
;   LABELS      used/max
;   ANON LABELS used/max
.proc show
	CALLMAIN scr::save

	lda asm::pcset
	bne @prog

@noprog:
	lda #START_ROW
	ldxy #@noasm
	jsr print
	jmp @debugger

@prog:
; print the memory area used by the user's assembled program
	; push the top address
	lda asm::top
	pha
	lda asm::top+1
	pha

	; push the start address
	lda asm::origin
	pha
	lda asm::origin+1
	pha

	ldxy #@program_msg
	lda #START_ROW
	jsr print

; print the memory area used by the debugger
@debugger:
	ldxy #@debugger_msg
	lda #START_ROW+1
	jsr print

; print the number of labels used and how many are available
@labels:
	; push the number of labels used
	lda lbl::num
	pha
	lda lbl::num+1
	pha

	ldxy #@labels_msg
	lda #START_ROW+2
	jsr print

; print the number of anonymous labels used and how many are available
@anon_labels:
	; push the number of anonymous labels used
	lda lbl::numanon
	pha
	lda lbl::numanon+1
	pha

	ldxy #@alabels_msg
	lda #START_ROW+3
	jsr print

	lda #START_ROW+4
	CALLMAIN scr::clrline

	lda #COLOR_RVS
	ldx #START_ROW+4
	CALLMAIN draw::hline

	CALLMAIN key::waitch
@done:
	CALLMAIN scr::restore
	rts

.PUSHSEG
.RODATA
@noasm:        .byte "program     no assembly",0
@program_msg:  .byte "program     $", ESCAPE_VALUE, "-$", ESCAPE_VALUE, 0
@debugger_msg: .byte "debugger    ", DEBUGGER_START, "-", DEBUGGER_STOP, 0
@labels_msg:   .byte "labels      ", ESCAPE_VALUE_DEC, "/", .string(MAX_LABELS), 0
@alabels_msg:  .byte "anon labels ", ESCAPE_VALUE_DEC, "/", .string(MAX_ANON), 0
.POPSEG
.endproc

;*******************************************************************************
; PRINT
; Dispatches to text::print
.proc print
.ifdef vic20
@ret=mem::sparevec
@a=r0
	sta @a
	pla
	sta @ret
	pla
	sta @ret+1

	CALLMAIN text::render_ind
.else
	CALLMAIN text::render
.endif
	lda @a
	CALLMAIN text::print

	lda @ret+1
	pha
	lda @ret
	pha
	rts
.endproc
