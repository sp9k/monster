; Bank-safe, stack-independent transfers for the debugger's low memory.
.include "banks.inc"
.include "../../macros.inc"
.include "../../ram.inc"
.include "../../memory.inc"
.import prog00, dbg00, progvecs
.CODE
.macro transfer src, dst, count, from, to, continuation
	lda #<(src)
	sta read+1
	lda #>(src)
	sta read+2
	lda #<(dst)
	sta write+1
	lda #>(dst)
	sta write+2
	lda #<count
	sta remaining
	lda #>count
	sta remaining+1
	lda #from
	sta frombank+1
	lda #to
	sta tobank+1
	ldxy #continuation
	stxy resume
	jmp copy
.endmacro
.export __debug_save_user_zp
.proc __debug_save_user_zp
	stxy mem::sparevec
	transfer prog00+DBGVECS, progvecs, DBGVECS_SIZE, FINAL_BANK_SIM, FINAL_BANK_MAIN, @save
@save:
	transfer $0000, prog00, $400, FINAL_BANK_MAIN, FINAL_BANK_SIM, @vectors
@vectors:
	transfer progvecs, prog00+DBGVECS, DBGVECS_SIZE, FINAL_BANK_MAIN, FINAL_BANK_SIM, finish
.endproc
.export __debug_restore_user_zp
.proc __debug_restore_user_zp
	stxy mem::sparevec
	transfer prog00, $0000, $400, FINAL_BANK_SIM, FINAL_BANK_MAIN, finish
.endproc
.export __debug_save_debug_zp
.proc __debug_save_debug_zp
	stxy mem::sparevec
	transfer $0000, dbg00, $400, FINAL_BANK_MAIN, FINAL_BANK_SIM, finish
.endproc
.export __debug_restore_debug_low
.proc __debug_restore_debug_low
	stxy mem::sparevec
	; Native NMIs are disabled on entry. Restore both debugger vectors so
	; the USER-only handler is never left active as the debugger banks out.
	transfer dbg00+$100, $100, $300, FINAL_BANK_SIM, FINAL_BANK_MAIN, @done
@done:
	lda #$82
	sta $911e
	jmp finish
.endproc
.export __debug_restore_debug_zp
.proc __debug_restore_debug_zp
	transfer dbg00, $0000, $100, FINAL_BANK_SIM, FINAL_BANK_MAIN, @done
@done: rts
.endproc
finish:
	jmp (mem::sparevec)

.segment "FE3CFG"
; This loop must not use the stack or zero page: both may be replaced.
copy:
	ldy #0
frombank:
	lda #0
	sta $9c02
read:
	lda $ffff,y
	tax
tobank:
	lda #0
	sta $9c02
	txa
write:
	sta $ffff,y
	inc read+1
	bne :+
	inc read+2
:
	inc write+1
	bne :+
	inc write+2
:
	lda remaining
	bne :+
	dec remaining+1
:
	dec remaining
	lda remaining
	ora remaining+1
	bne copy
	lda #FINAL_BANK_MAIN
	sta $9c02
	jmp (resume)
.align 2
resume: .word 0
remaining: .word 0
