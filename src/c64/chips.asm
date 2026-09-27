;******************************************************************************
; CHIPS.ASM
; This file contains the C64 VIC and CIA simulation routines.
; Updates chip registers, timers, and interrupts for each CPU bus cycle.
; See c64-simulation.md for timing details.
;******************************************************************************

.include "reu.inc"
.include "../macros.inc"
.include "../zeropage.inc"
.include "../sim6502.inc"
.include "../asmflags.inc"
.import prog00, __sim_step_cycles
.import __sim_bus_hold
.macpack longbranch

;******************************************************************************
.ifdef PAL
CPL = 63
LINES = 312
SPR_CHECK = 54
.else
CPL = 65
LINES = 263
SPR_CHECK = 55
.endif

;******************************************************************************
; simulated chip registers and state, stored beneath KERNAL
.segment "DEBUGGER"

.export __chips_cia
.export __chips_vic
.export __chips_synced
.export __chips_line
.export __chips_cycle

;******************************************************************************
state:
__chips_vic:    .res 64, $00
__chips_cia:    .res 32, $00
masks:          .res 2, $00
latchlo:        .res 4, $00
latchhi:        .res 4, $00
delay:          .res 4, $00
count_pipe:     .res 4, $00
shot_pipe:      .res 4, $00
loaded:         .res 4, $00
nmi_pending:    .byte $00
__chips_line:   .word $0000
__chips_cycle:  .byte $00
__chips_synced: .byte $00
den:            .byte $00
matched:        .byte $00
sprite_rows:    .res 8, $00
sprite_phase:   .res 8, $00
state_end:

bus_x:        .byte $00
bus_y:        .byte $00
write_cycles: .byte $00
irq_now:      .byte $00
irq_previous: .byte $00
irq_earlier:  .byte $00
bus_active:   .byte $00
valid:        .byte $00

;******************************************************************************
vic   = __chips_vic
cia   = __chips_cia
line  = __chips_line
cycle = __chips_cycle

;******************************************************************************
; INIT
; Initializes the simulated VIC and CIA registers from the saved I/O data.
; Resets the raster position and interrupt masks, and copies the captured
; CIA counters to the timer latches.
.export __chips_init
.proc __chips_init
@index=r6
	lda #$00
	ldx #state_end-state-1
:	sta state,x
	dex
	cpx #$ff
	bne :-

	; copy the saved VIC registers from REU memory
	ldx #$00
@vic:	stx @index
	ldy #$d0
	lda #^REU_VMEM_IO
	jsr load_snapshot
	ldx @index
	sta vic,x
	inx
	cpx #$40
	bne @vic

	; clear the raster compare value and VIC interrupt flags and mask
	lda vic+$11
	and #$7f
	sta vic+$11
	lda #$00
	sta vic+$12
	sta vic+$19
	sta vic+$1a

	; copy the saved CIA registers from REU memory
	ldx #$00
@cia:	stx @index
	txa
	and #$0f
	tax
	ldy #$dc
	lda @index
	cmp #$10
	bcc :+
	iny
:	lda #^REU_VMEM_IO
	jsr load_snapshot
	ldx @index
	sta cia,x
	inx
	cpx #$20
	bne @cia

	lda #$00
	sta cia+$d
	sta cia+$1d

	; copy the CIA counters to the timer latches
	ldx #$03
@seed:	ldy counters,x
	lda cia,y
	sta latchlo,x
	lda cia+1,y
	sta latchhi,x

	ldy controls,x
	lda cia,y
	and #$ef		; clear the LOAD bit
	sta cia,y
	and #$01
	beq @seedshot
	txa
	and #$01
	beq @seedta
	lda cia,y
	and #$60
	jmp @seedclock
@seedta:
	lda cia,y
	and #$20
@seedclock:
	bne @seedshot
	lda #$03		; fill both count stages
	sta count_pipe,x

@seedshot:
	lda cia,y
	and #$08
	beq :+
	lda #$03
	sta shot_pipe,x
:
	dex
	bpl @seed

	lda #$01
	sta valid
	rts
.endproc

;******************************************************************************
; BEGIN
; Prepares the chip clock for the next simulated CPU instruction.
; Clears the cycle count and interrupt samples, and initializes the chips
; if the saved state is no longer valid.
.export __chips_begin, __chips_invalidate
.proc __chips_begin
	lda #$00
	sta __chips_synced
	sta write_cycles
	sta bus_active
	sta irq_now
	sta irq_previous
	sta irq_earlier

	lda valid
	bne :+
	jmp __chips_init
:	rts
.endproc

;******************************************************************************
; INVALIDATE
; Marks the simulated chip state as invalid after native execution.
; The next __chips_begin reloads the saved registers.
.proc __chips_invalidate
	lda #$00
	sta valid
	rts
.endproc

;******************************************************************************
; MAPPED
; Checks whether a virtual address refers to a simulated VIC or CIA register.
; Uses the user's $00 and $01 registers to check whether I/O is banked in.
; IN:
;  - .XY: the virtual address
; OUT:
;  - .C: set if the address refers to a simulated chip
;  - .A, .X, .Y: unaffected
.export __chips_mapped
.proc __chips_mapped
	cpy #$d0
	bcc @no
	cpy #$d4
	bcc @port
	cpy #$dc
	bcc @no
	cpy #$de
	bcs @no

@port:	pha
	lda prog00
	eor #$ff
	ora prog00+1
	pha
	and #$03
	beq @noram
	pla
	and #$04
	beq @noio
	pla
	sec
	rts

@noram:
	pla
@noio:	pla
@no:	clc
	rts
.endproc

;******************************************************************************
; IO BEGIN
; Saves the value and address for a chip register access. Converts the
; address, including register mirrors, to a VIC or CIA array offset.
; IN:
;  - .A:  the value to write (unused for reads)
;  - .XY: the virtual address of a VIC or CIA register
; OUT:
;  - .X: the register offset ($00-$3f for VIC, $00-$1f for CIA)
;  - .Y: the original address high byte
.proc io_begin
@x=r8
@y=r9
@value=ra
	sta @value
	stx @x
	sty @y

	ldx @x
	ldy @y
	cpy #$dc
	bcc @vic
	txa
	and #$0f
	cpy #$dd
	bcc :+
	ora #$10
:	tax
	rts

@vic:	txa
	and #$3f
	tax
	rts
.endproc

;******************************************************************************
; READ
; Reads a simulated VIC or CIA register. Clears the CIA interrupt flags
; when reading an interrupt control register.
; IN:
;  - .XY: the virtual address of a VIC or CIA register
; OUT:
;  - .A: the value read
;  - .C: clear
;  - .N, .Z: set from the value read
;  - .X, .Y: unaffected
.export __chips_read
.proc __chips_read
	jsr io_begin
	cpy #$dc
	bcc @vic
	txa
	and #$0f
	cmp #$0d
	bne @cia

	lda cia,x		; read the pending CIA interrupts
	pha
	lda #$00
	sta cia,x
	pla
	jmp io_done

@cia:	lda cia,x
	jmp io_done

@vic:	jsr vic_read
	jmp io_done
.endproc

;******************************************************************************
; VIC READ
; Reads a simulated VIC register. Returns the current raster position for
; $11/$12 and supplies the unused bits in the interrupt registers.
; IN:
;  - .X: the VIC register offset ($00-$3f)
; OUT:
;  - .A: the value read, or $ff for an unused register
;  - .X, .Y: unaffected
.proc vic_read
	cpx #$11
	beq @high
	cpx #$12
	beq @low
	cpx #$19
	beq @irq
	cpx #$1a
	beq @mask
	cpx #$2f
	bcs @unused
	lda vic,x
	rts

@high:	lda line+1
	lsr
	lda vic+$11
	and #$7f
	bcc :+
	ora #$80
:	rts

@low:	lda line
	rts

@irq:	lda vic+$19
	and vic+$1a
	and #$0f
	beq :+
	lda #$80
:	ora vic+$19
	ora #$70
	rts

@mask:	lda vic+$1a
	ora #$f0
	rts

@unused:
	lda #$ff
	rts
.endproc

;******************************************************************************
; IO DONE
; Finishes a chip register access by restoring the address saved by io_begin.
; IN:
;  - .A: the value read or written
; OUT:
;  - .A: unaffected
;  - .XY: the original virtual address
;  - .C: clear
;  - .N, .Z: set from .A
.proc io_done
@x=r8
@y=r9
	ldx @x
	ldy @y
	clc
	pha
	pla
	rts
.endproc

;******************************************************************************
; WRITE
; Writes a simulated VIC or CIA register. Updates timer latches, control
; bits, interrupt masks and flags, and the raster compare value.
; IN:
;  - .XY: the virtual address of a VIC or CIA register
;  - .A:  the value to write
; OUT:
;  - .A, .X, .Y: unaffected
;  - .C: clear
;  - .N, .Z: set from the value written
.export __chips_write
.proc __chips_write
@value=ra
@index=r6
@tmp=r7
	jsr io_begin
	cpy #$dc
	jcc @vic
	txa
	and #$0f
	cmp #$0d
	beq @icr
	cmp #$0e
	bcs @control
	cmp #$04
	bcc @plain
	cmp #$08
	bcs @plain

	; get the timer number and select the low or high latch byte
	stx @index
	jsr timer_index
	lda @index
	and #$01
	bne @high

	lda @value
	sta latchlo,x
	lda loaded,x
	jeq @done
	ldy counters,x
	lda @value
	sta cia,y		; update the reloaded counter
	jmp @done

@high:	lda @value
	sta latchhi,x
	ldy controls,x
	lda cia,y
	and #$01
	beq :+
	lda loaded,x
	beq @done
:
	jsr reload
	jmp @done

@plain:
	lda @value
	sta cia,x
	jmp @done

@icr:	txa
	lsr
	lsr
	lsr
	lsr
	tax
	lda @value
	bpl @clear
	and #$1f
	ora masks,x
	sta masks,x
	jmp @done

@clear:
	and #$1f
	eor #$ff
	and masks,x
	sta masks,x		; clear the selected mask bits
	jmp @done

@control:
	stx @index
	txa
	and #$11		; $0e/$0f/$1e/$1f -> timer 0/1/2/3
	and #$01
	sta @tmp
	txa
	lsr
	lsr
	lsr
	and #$02
	ora @tmp
	tax

	ldy @index
	lda @value
	and #$ef
	sta cia,y

	lda delay,x
	and #$02		; keep the second force-load stage
	sta delay,x
	lda @value
	and #$10
	beq @done
	inc delay,x		; set the first force-load stage

@done:	lda @value
	jmp io_done

@vic:	cpx #$19
	beq @ack
	cpx #$1e
	beq @done
	cpx #$1f
	beq @done
	cpx #$2f
	bcs @done

	lda @value
	sta vic,x
	cpx #$17
	bne :+

	ldx #$07
@unexpand:
	lda @value
	and bits,x
	bne @unext
	lda #$01
	sta sprite_phase,x
@unext:
	dex
	bpl @unexpand
	jmp @done
:
	cpx #$11
	beq @compare
	cpx #$12
	bne @done
@compare:
	jsr raster_compare
	jmp @done

@ack:	lda @value
	and #$0f
	eor #$ff
	and vic+$19
	sta vic+$19
	jmp @done
.endproc

;******************************************************************************
; TIMER INDEX
; Converts a CIA timer low/high register offset to a timer number.
; IN:
;  - .X: the offset in the CIA register array ($04-$07 or $14-$17)
; OUT:
;  - .X: the timer number ($00/$01 = CIA1 A/B, $02/$03 = CIA2 A/B)
.proc timer_index
@tmp=r7
	txa
	lsr
	and #$01
	sta @tmp
	txa
	lsr
	lsr
	lsr
	and #$02
	ora @tmp
	tax
	rts
.endproc

;******************************************************************************
; RELOAD
; Copies a timer's low and high latch bytes to its counter.
; IN:
;  - .X: the timer number ($00-$03)
.proc reload
	ldy counters,x
	lda latchlo,x
	sta cia,y
	lda latchhi,x
	sta cia+1,y
	rts
.endproc

;******************************************************************************
; IRQ LATCH
; Sets ICR bit 7 if a pending CIA interrupt source is enabled by its mask.
; For CIA2, also records an NMI when bit 7 changes from clear to set.
; IN:
;  - .X: the CIA number ($00 = CIA1, $01 = CIA2)
.proc irq_latch
	ldy icrs,x
	lda cia,y
	and masks,x
	and #$1f
	beq @done
	lda cia,y
	bmi @done
	ora #$80
	sta cia,y

	cpx #$01
	bne @done
	lda #$01
	sta nmi_pending
@done:	rts
.endproc

;******************************************************************************
; FLUSH
; Copies the simulated VIC and CIA registers to the saved I/O data in REU
; memory. Includes the CIA interrupt status and leaves pending interrupts
; set. Returns immediately if the chip state is invalid.
.export __chips_flush
.proc __chips_flush
@index=r6
	lda valid
	bne :+
	rts
:
	ldx #$00
@vic:	stx @index
	jsr vic_read
	ldx @index
	ldy #$d0
	jsr store_snapshot
	ldx @index
	inx
	cpx #$40
	bne @vic

	ldx #$00
@cia:	stx @index
	lda cia,x
	pha
	txa
	and #$0f
	tax
	ldy #$dc
	lda @index
	cmp #$10
	bcc :+
	iny
:	pla
	jsr store_snapshot
	ldx @index
	inx
	cpx #$20
	bne @cia
	rts
.endproc

;******************************************************************************
; STORE SNAPSHOT
; Stores one register value in the saved I/O data in REU memory.
; IN:
;  - .XY: the I/O address
;  - .A:  the value to store
.proc store_snapshot
	stxy reu::reuaddr
	pha
	lda #^REU_VMEM_IO
	sta reu::reuaddr+2
	pla
	jmp reu::store1
.endproc

;******************************************************************************
; LOAD SNAPSHOT
; Reads one register value from the saved I/O data in REU memory.
; IN:
;  - .XY: the I/O address
;  - .A:  the REU bank containing the saved I/O data
; OUT:
;  - .A: the value read
.proc load_snapshot
	stxy reu::reuaddr
	sta reu::reuaddr+2
	jmp reu::load1
.endproc

;******************************************************************************
; chip timing routines, stored beneath I/O on cartridge builds
.ifdef CART
.segment "CHIPCODE"
.endif

;******************************************************************************
; CAPTURE
; Copies the physical CIA registers to the saved I/O data in REU memory.
; Called after native execution stops, while the user's I/O is still banked
; in. Reading ICR here acknowledges the stopped program's CIA interrupts.
.export __chips_capture
.proc __chips_capture
	ldxy #$dc00
	stxy reu::c64addr
	stxy reu::reuaddr
	lda #^REU_VMEM_IO
	sta reu::reuaddr+2
	ldxy #16
	stxy reu::txlen
	jsr reu::store

	ldxy #$dd00
	stxy reu::c64addr
	stxy reu::reuaddr
	jmp reu::store
.endproc

;******************************************************************************
; SYNC
; Advances the VIC and CIAs by the unsynced CPU cycles. Waits for BA on
; reads and adds the wait cycles to the stopwatch. Processes writes
; immediately. Returns before the final raster advance for an active
; CPU bus access; __chips_end_cycle performs that advance.
.export __chips_sync
.proc __chips_sync
@pending=rd
@stolen=re
@aec_low=rf
	lda __sim_step_cycles
	sec
	sbc __chips_synced
	jeq @done
	sta @pending
	lda __sim_step_cycles
	sta __chips_synced

@loop:	jsr vic_clock
	jsr cia_clock

	lda write_cycles
	bne @cpu		; process the write cycle
	lda @stolen
	beq @cpu
	lda bus_active
	beq @stall
	lda @aec_low
	bne @stall
	ldx bus_x
	ldy bus_y
	; repeat the I/O read during the BA warning
	jsr __sim_bus_hold

@stall:
	inc sim::stopwatch
	bne @next
	inc sim::stopwatch+1
	bne @next
	inc sim::stopwatch+2
	jmp @next

@cpu:	jsr sample_interrupt
	dec @pending
 	lda @pending
	bne @next
	lda bus_active
	bne @done		; return before the final raster advance

@next:	jsr advance_clock
	lda @pending
	bne @loop
@done:	rts
.endproc

;******************************************************************************
; ADVANCE CLOCK
; Advances the raster position by one cycle and wraps at the end of a frame.
; Updates sprite Y expansion at its clock position and checks for a raster
; interrupt when the line changes.
.proc advance_clock
	; update sprite Y expansion on cycle 56
	lda cycle
	cmp #55
	bne @advance
	ldx #$07
@expand:
	lda sprite_rows,x
	beq @enext
	lda vic+$17
	and bits,x
	beq @enext
	lda sprite_phase,x
	eor #$01
	sta sprite_phase,x
@enext:
	dex
	bpl @expand

@advance:
	inc sim::raster
	bne :+
	inc sim::raster+1
:	inc cycle
	lda cycle
	cmp #CPL
	bne @done

	lda #$00
	sta cycle
	inc line
	bne :+
	inc line+1

:	lda line+1
	cmp #>LINES
	bne @compare
	lda line
	cmp #<LINES
	bne @compare

	lda #$00
	sta line
	sta line+1
	sta sim::raster
	sta sim::raster+1
	sta den

@compare:
	jsr raster_compare
@done:	rts
.endproc

;******************************************************************************
; READ CYCLE
; Clocks the chips for one CPU read cycle and waits for BA to go high.
; Returns before the memory read. __chips_end_cycle completes the cycle
; after the read.
; IN:
;  - .XY: the virtual address to read
; OUT:
;  - .A, .X, .Y, .P: unaffected
;  - r0-r5: unaffected
.export __chips_read_cycle, __chips_write_cycle
__chips_read_cycle:
	php
	pha
	lda #$00
	beq bus_cycle

;******************************************************************************
; WRITE CYCLE
; Clocks the chips for one CPU write cycle, regardless of BA.
; Returns before the memory write. __chips_end_cycle completes the cycle
; after the write.
; IN:
;  - .XY: the virtual address to write
;  - .A:  the value to write
; OUT:
;  - .A, .X, .Y, .P: unaffected
;  - r0-r5: unaffected
__chips_write_cycle:
	php
	pha
	lda #$01

;******************************************************************************
; BUS CYCLE
; Saves the bus address, updates the CPU cycle count and stopwatch, and
; calls __chips_sync. Restores .A and .P from the stack before returning.
; Entered from read_cycle/write_cycle with .P and .A pushed on the stack.
; IN:
;  - .A:  $00 for a read, $01 for a write
;  - .XY: the virtual address
bus_cycle:
	sta write_cycles
	stx bus_x
	sty bus_y
	inc bus_active

	txa
	pha
	tya
	pha

	inc __sim_step_cycles
	inc sim::stopwatch
	bne :+
	inc sim::stopwatch+1
	bne :+
	inc sim::stopwatch+2

:	jsr __chips_sync
	lda #$00
	sta write_cycles

	pla
	tay
	pla
	tax
	pla
	plp
	rts

;******************************************************************************
; END CYCLE
; Finishes a CPU bus cycle after the read or write has taken effect.
; Advances the raster position to the next cycle and clears bus_active.
; OUT:
;  - .A, .X, .Y, .P: unaffected
.export __chips_end_cycle
.proc __chips_end_cycle
	php
	pha
	txa
	pha
	tya
	pha

	jsr advance_clock
	lda #$00
	sta bus_active

	pla
	tay
	pla
	tax
	pla
	plp
	rts
.endproc

;******************************************************************************
; SAMPLE INTERRUPT
; Shifts the interrupt history and records the current IRQ and NMI state.
; Records IRQ only when the CPU's interrupt-disable flag is clear.
; Keeps three samples for __chips_take_interrupt.
.proc sample_interrupt
	lda irq_previous
	sta irq_earlier
	lda irq_now
	sta irq_previous
	lda #$00
	sta irq_now

	lda sim::reg_p
	and #$04
	bne @nmi
	lda cia+$d
	bmi @irq
	lda vic+$19
	and vic+$1a
	and #$0f
	beq @nmi
@irq:	inc irq_now

@nmi:	lda nmi_pending
	beq @done
	lda irq_now
	ora #$02
	sta irq_now
@done:	rts
.endproc

;******************************************************************************
; TAKE INTERRUPT
; Selects an interrupt from the samples saved during the CPU instruction.
; Normally uses the next-to-last cycle. A taken branch without a page
; crossing uses the opcode cycle instead. NMI takes priority over IRQ.
; Consumes the pending NMI when it is selected.
; OUT:
;  - .C:  set if an interrupt should be taken
;  - .XY: the vector address ($fffa for NMI, $fffe for IRQ) if .C is set
.export __chips_take_interrupt
.proc __chips_take_interrupt
	lda sim::op
	and #$1f
	cmp #$10
	bne @normal
	lda __sim_step_cycles
	cmp #3
	bne @normal
	lda irq_earlier
	jmp @check

@normal:
	lda irq_previous

@check:
	lsr
	pha
	and #$01
	beq @irq
	pla
	lda #$00
	sta nmi_pending
	ldxy #$fffa
	sec
	rts

@irq:	pla
	bcc @no
	ldxy #$fffe
	sec
	rts

@no:	clc
	rts
.endproc

;******************************************************************************
; NMI VECTOR
; Replaces the IRQ vector with the NMI vector if an NMI is pending, then
; clears the pending NMI. Called before reading the vector's low byte.
; IN:
;  - .XY: the IRQ vector address
; OUT:
;  - .XY: $fffa if an NMI was pending, otherwise unaffected
.export __chips_nmi_vector
.proc __chips_nmi_vector
	lda nmi_pending
	beq @done
	lda #$00
	sta nmi_pending
	ldxy #$fffa
@done:	rts
.endproc

;******************************************************************************
; CIA CLOCK
; Updates both CIA interrupt outputs and clocks all four timers.
; Processes timer A before timer B in each CIA.
.proc cia_clock
@timer=r6
@underflow=rb		; two CIA underflow flags
	; update CIA interrupt outputs from the previous cycle's flags
	ldx #$00
	jsr irq_latch
	ldx #$01
	jsr irq_latch

	lda #$00
	sta @underflow
	sta @underflow+1
	ldx #$00
@loop:	stx @timer
	jsr tick_timer
	ldx @timer
	inx
	cpx #4
	bne @loop
	rts
.endproc

;******************************************************************************
; TICK TIMER
; Clocks one CIA timer, including delayed count and force-load requests.
; Reloads the counter on underflow, sets its interrupt flag, and stops the
; timer if one-shot mode is active.
; IN:
;  - .X: the timer number ($00-$03)
;  - r6: the same timer number, saved by cia_clock
.proc tick_timer
@timer=r6
@tmp=r7
@underflow=rb		; two CIA underflow flags
	lda #$00
	sta loaded,x

	; decrement a nonzero counter if the second count stage is set
	lda count_pipe,x
	and #$02
	beq @input
	ldy counters,x
	lda cia,y
	ora cia+1,y
	beq @input

	lda cia,y
	bne :+
	lda cia+1,y
	sec
	sbc #$01
	sta cia+1,y
:	lda cia,y
	sec
	sbc #$01
	sta cia,y

@input:
	lda count_pipe,x
	asl
	and #$02
	sta count_pipe,x

	ldy controls,x
	lda cia,y
	and #$01
	beq @check
	lda cia,y
	sta @tmp
	txa
	and #$01
	bne @tb
	lda @tmp
	and #$20		; test timer A CNT mode
	bne @check
	beq @count

@tb:	lda @tmp
	and #$60
	beq @count
	cmp #$20		; timer B CNT mode
	beq @check
	txa
	lsr
	tay
	lda @underflow,y		; get this CIA's timer A underflow flag
	beq @check
@count:
	inc count_pipe,x

@check:
	lda count_pipe,x
	and #$02
	beq @force
	ldy counters,x
	lda cia,y
	ora cia+1,y
	bne @force

	jsr timer_load
	lda shot_pipe,x
	beq @flag
	ldy controls,x
	lda cia,y
	and #$fe
	sta cia,y		; clear START for one-shot mode
	lda #$00
	sta count_pipe,x

@flag:	txa
	and #$01
	sta @tmp
	txa
	lsr
	tax			; CIA index
	ldy @tmp
	bne :+
	lda #$01
	sta @underflow,x
:	lda timer_flags,y
	ldy icrs,x
	ora cia,y
	sta cia,y
	ldx @timer

@force:
	lda delay,x
	and #$02
	beq @shift
	jsr timer_load
@shift:
	lda delay,x
	asl
	and #$02
	sta delay,x

@shot:
	lda shot_pipe,x
	asl
	and #$02
	sta shot_pipe,x
	ldy controls,x
	lda cia,y
	and #$08
	beq @done
	inc shot_pipe,x
@done:	rts
.endproc

;******************************************************************************
; TIMER LOAD
; Reloads a CIA timer from its latches, sets its reload flag, and clears
; the second count stage.
; IN:
;  - .X: the timer number ($00-$03)
.proc timer_load
	jsr reload
	lda #$01
	sta loaded,x
	lda count_pipe,x
	and #$01		; clear the second count stage
	sta count_pipe,x
	rts
.endproc

;******************************************************************************
; RASTER COMPARE
; Compares the current raster line with the VIC's programmed compare value.
; Sets matched and the raster interrupt flag on a new match. Clears matched
; when the line differs from the compare value.
.proc raster_compare
	lda vic+$11
	asl
	lda #$00
	rol
	cmp line+1
	bne @no
	lda vic+$12
	cmp line
	bne @no

	lda matched
	bne @done
	inc matched
	lda vic+$19
	ora #$01
	sta vic+$19
@done:	rts

@no:	lda #$00
	sta matched
	rts
.endproc

;******************************************************************************
; VIC CLOCK
; Updates badline and sprite bus requests for the current raster cycle.
; BA goes low three cycles before AEC. With zero-based cycle numbers,
; badline matrix reads occupy cycles 14-53 and BA is low at cycles 11-53.
; OUT:
;  - re: nonzero while BA is low (CPU reads must wait)
;  - rf: nonzero while AEC is low (the VIC owns the bus)
.proc vic_clock
@stolen=re
@aec_low=rf
	lda #$00
	sta @stolen
	sta @aec_low

	lda line+1
	bne @sprites
	lda line
	cmp #$30
	bne :+
	lda vic+$11
	and #$10
	ora den
	sta den

:	lda den
	beq @sprites
	lda line
	cmp #$30
	bcc @sprites
	cmp #$f8
	bcs @sprites
	eor vic+$11
	and #$07
	bne @sprites

	lda cycle
	cmp #11
	bcc @sprites
	cmp #54
	bcs @sprites
	inc @stolen
	cmp #14
	bcc @sprites
	inc @aec_low

@sprites:
	lda cycle
	cmp #SPR_CHECK
	beq @start
	cmp #SPR_CHECK+1
	beq @start
	cmp #15
	beq @advance
	jmp @dma

@start:
	ldx #$00
@s:	lda sprite_rows,x
	bne @snext
	lda vic+$15
	and bits,x
	beq @snext
	txa
	asl
	tay
	lda vic+1,y
	cmp line
	bne @snext

	lda #21
	sta sprite_rows,x
	lda #$01
	sta sprite_phase,x
@snext:
	inx
	cpx #8
	bne @s
	jmp @dma

@advance:
	ldx #$07
@a:	lda sprite_rows,x
	beq @anext
	lda vic+$17
	and bits,x
	beq @row
	lda sprite_phase,x
	beq @anext
@row:	dec sprite_rows,x
@anext:
	dex
	bpl @a

@dma:	ldx #$07
@d:	lda sprite_rows,x
	beq @dnext
	lda cycle
	sec
	sbc dma_start,x
	bcs :+
	adc #CPL		; wrap the cycle offset to this raster line
:	cmp #5
	bcs @dnext
	inc @stolen
	cmp #3
	bcc @dnext
	inc @aec_low
@dnext:
	dex
	bpl @d
	rts
.endproc

;******************************************************************************
; INTERRUPT
; Returns the vector for a pending NMI or an enabled IRQ from the current
; chip state. Checks NMI first and clears nmi_pending when selected.
; OUT:
;  - .C:  set if an interrupt should be taken
;  - .XY: the vector address ($fffa for NMI, $fffe for IRQ) if .C is set
.export __chips_interrupt
.proc __chips_interrupt
	lda nmi_pending
	beq @irq
	lda #$00
	sta nmi_pending
	ldxy #$fffa
	sec
	rts

@irq:	lda sim::reg_p
	and #$04
	bne @no
	lda cia+$d
	bmi @yes
	lda vic+$19
	and vic+$1a
	and #$0f
	beq @no

@yes:	ldxy #$fffe
	sec
	rts

@no:	clc
	rts
.endproc

;******************************************************************************
counters:    .byte $04,$06,$14,$16
controls:    .byte $0e,$0f,$1e,$1f
icrs:        .byte $0d,$1d
timer_flags: .byte $01,$02
bits:        .byte $01,$02,$04,$08,$10,$20,$40,$80

;******************************************************************************
; sprite BA start cycles: PAL 54, NTSC 55, two cycles per sprite
dma_start:
.repeat 8, S
.byte (SPR_CHECK+2*S) .mod CPL
.endrepeat
