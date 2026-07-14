.import install_trace_nmi
.import __fastcopy_restore_debug_state
.import __fastcopy_restore_prog_visual
.import __fastcopy_restore_prog_state
.import __fastcopy_save_debug_state
.import __fastcopy_save_prog_state

.scope bsp
	install_tracer      = install_trace_nmi
	restore_debug_state = __fastcopy_restore_debug_state
	restore_prog_state  = __fastcopy_restore_prog_state
	restore_prog_visual = __fastcopy_restore_prog_visual
	save_debug_state    = __fastcopy_save_debug_state
	save_prog_state     = __fastcopy_save_prog_state

	; aliases
	save_debug_visual    = __fastcopy_save_debug_state
	restore_debug_visual = __fastcopy_restore_debug_state
.endscope

;*******************************************************************************
; TRACE ON
; This macro enables the NMI (or other method) required to interrupt a "trace"
.macro TRACE_ON
	; enable NMIs
	; this lets us catch the user's signal that they wish to interrupt
	; a "trace"
	lda #$82
	sta $911e
.endmacro

;*******************************************************************************
; TRACE OFF
; This macro disables the NMI (or other method) responsible for performing a
; "trace".
.macro TRACE_OFF
	; disable NMIs
	; this prevents an interrupt from occurring while we are running
	; a simualted "step" in the debugger
	lda #$7f
	sta $911e
.endmacro

;*******************************************************************************
; SAVE IO
; This macro saves Vic-20 specific I/O (VIAs etc.) upon returning to the
; debugger from a "GO" command
.macro SAVE_IO
	; capture the pending interrupt state before the timer reads below
	; VIA #1's CA1 flag ($02) is masked out (the debugger uses CA1)
	lda $911d
	and #$ff-$02
	sta sim::via1+$d
	lda $912d
	sta sim::via2+$d

	; copy the timer counters, T1 latches, and DDRs (offsets $2-$9)
	; re-read the hi byte after the lo byte: if it changed, the lo byte
	; wrapped mid-read; read again
	ldx #$08	; $8=T2 counter, $6=T1 latches, $4=T1 counter, $2=DDRs
:	ldy $9111,x
	lda $9110,x
	sta sim::via1,x
	tya
	cmp $9111,x		; hi byte still the same?
	bne :-			; if not (overflow) retry
	sta sim::via1+1,x
:	ldy $9121,x
	lda $9120,x
	sta sim::via2,x
	tya
	cmp $9121,x
	bne :-
	sta sim::via2+1,x
	dex
	dex
	bne :--			; repeat for next register pair

	; capture the ACRs: the simulator's timer emulation depends on them
	; (T1 free-run vs one-shot and T2 pulse-count mode)
	lda $911b
	sta sim::via1+$b
	lda $912b
	sta sim::via2+$b

	; capture VIA #2's IER; the simulator dispatches IRQs from it.
	; VIA #1's IER is left alone: the debugger arms it (CA1/RESTORE)
	; itself while the user program runs
	lda $912e
	sta sim::via2+$e
.endmacro
