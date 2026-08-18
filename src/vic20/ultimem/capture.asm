;*******************************************************************************
; CAPTURE.ASM
; Frame capture and cycle-exact replay
;
; While the simulator runs, every store that can affect the visible screen
; (screen/character RAM, VIC registers, and color RAM) is appended to a chain of
; UltiMem banks along with the frame cycle it occurred on (see capture_store).
; The recorded frame is then turned into cycle-exact 6502 code in the OUTPUT
; banks (gen_frame) where it is replayable as a raster-stable routine (as the
; program would appear in its current frame of execution).
;*******************************************************************************

.include "banks.inc"
.include "../frame.inc"
.include "../../macros.inc"
.include "../../ram.inc"
.include "../../zeropage.inc"

;*******************************************************************************
; SIMULATOR STATE
; State owned by the simulator (sim6502.asm) that the capture engine reads.
.import __sim_raster		; CPU cycles elapsed within the current frame
.import __sim_frame_cyc		; length of the current frame in CPU cycles
.import __sim_step_cycles	; cycles executed by the current STEP
.import __sim_tracing		; !0 if a TRACE (not a STEP) is running
.import __sim_do_step		; the simulator's internal single-step entry
.import __sim_vmem_load		; the simulator's virtual/physical read wrapper
.import __sim_op		; opcode of the instruction being executed

frame_cyc   = __sim_frame_cyc
step_cycles = __sim_step_cycles
tracing     = __sim_tracing
step        = __sim_do_step
vmem_load   = __sim_vmem_load

;*******************************************************************************
.BSS

;*******************************************************************************
; FRAME CAPTURE CONSTANTS
; A record carries the value the address held BEFORE the write (prev) as well as
; the value written.  prev is what makes pixel-0 state recoverable: the first
; record naming an address holds that address' value at the top of the frame.
; addr(2) + value(1) + prev(1) + cycle(2) + opcode(1)
CAPTURE_RECORD_SZ = 7

; The opcode is kept rather than just the source register it implies: the
; register falls out of it in one masked compare (see rec_regof), and holding
; the whole instruction is what lets a read-modify-write be re-emitted as
; itself instead of being turned into a load and a store (see RMW RE-EMISSION).
; REG_ANY is what rec_regof answers for those -- they take their value from
; memory and leave A/X/Y alone, so there is no register to mirror.
REG_A   = 0
REG_X   = 1
REG_Y   = 2
REG_ANY = 3
CAPTURE_WIN      = $2000	; BLK1 window the capture bank is mapped into
CAPTURE_END      = $4000	; one 8KB bank ($2000 bytes) -> $2000..$3fff

;*******************************************************************************
; PIXEL-0 STATE
; Every tracked address is given a dense index so its pixel-0 value and its two
; status bits can be looked up in flat arrays:
;
;   $1000-$1FFF  screen / character RAM  -> $0000-$0FFF
;   $9400-$97FF  color RAM               -> $1000-$13FF
;   $9000-$900F  VIC registers           -> $1400-$140F
;
; The arrays live in SHADOW_BANK, mapped into BLK2 for the whole of gen_frame.
; cap_read_record borrows BLK1 and gen_emit borrows BLK1/2/3, but both restore
; what they found, so the mapping survives underneath them.
SHADOW_WIN    = $4000		; BLK2 window SHADOW_BANK is mapped into
SHIDX_SCREEN  = $0000		; index base: $1000-$1FFF
SHIDX_COLOR   = $1000		; index base: $9400-$97FF
SHIDX_VIC     = $1400		; index base: $9000-$900F
SHIDX_COUNT   = $1410		; 5136 tracked addresses

SHADOW_VAL    = SHADOW_WIN			; $4000: pixel-0 value per index
SHADOW_BMP_SZ = SHIDX_COUNT/8			; 642 bytes
SHADOW_DIRTY  = SHADOW_VAL+SHIDX_COUNT		; $5410: address written this frame
; $5692: address whose FIRST write is a read-modify-write.  Those get restored
; ahead of everything else, because re-emitting an RMW (see RMW RE-EMISSION)
; reads the address back, so its pixel-0 value has to actually be there.  A
; normal store overwrites whatever it finds and does not care; an RMW that reads
; a stale value produces a wrong one, and compounds it every pass.
;
; "First write" means first write in the FRAME in full-frame mode (build_shadow),
; and first write INSIDE THE STRIP in exact-window mode (window_rmw_scan) -- the
; writes ahead of the strip are dropped rather than replayed, so a frame-scoped
; answer is wrong in both directions there.  See WINDOW RMW SCAN.
SHADOW_RMW    = SHADOW_DIRTY+SHADOW_BMP_SZ
; $5914: window_rmw_scan's "already written inside the strip" scratch, which is
; what turns a first-write test into a first-write-in-the-window test.  Only
; exact-window mode builds it.
SHADOW_WSEEN  = SHADOW_RMW+SHADOW_BMP_SZ
; $5b96: address the VIC actually FETCHES somewhere in this frame, built by
; fetch_walk from the pixel-0 VIC registers and the frame's own $9000-$900f write
; history.  A clear bit means the chip never reads that address under this
; frame's geometry, so no write to it is observable and none has to be replayed
; OR restored -- see VISIBILITY.
SHADOW_FETCH  = SHADOW_WSEEN+SHADOW_BMP_SZ
SHADOW_END    = SHADOW_FETCH+SHADOW_BMP_SZ	; $5e18 (fits BLK2's $4000-$5FFF)

.assert SHADOW_END <= $6000, error, "pixel-0 arrays overflow the BLK2 window"

;*******************************************************************************
; CONFIRMATION
; One rule replaces the three the filter used to run:
;
;     A WRITE MATTERS IF, AND ONLY IF, SOME READ OF THAT ADDRESS CONFIRMS IT.
;
; Walk reads and writes together in cycle order and keep, per address, the most
; recent write nothing has read yet.  A new write to the same address replaces
; it -- the old one was superseded before anything looked.  A read of the address
; confirms whatever is pending and empties the slot.
;
; The old filters are what falls out of that, not additions to it:
;   - an address the chip never reads has nothing to confirm its writes, so all
;     of them go.  That was the spatial filter.
;   - a write after the address' last read is never confirmed either.  That was
;     the temporal filter, and the deadline table it needed is gone.
;   - a write superseded before the next read is dropped by the write that
;     replaced it.  That is the coalescing, and it is the reason the cycle
;     accounting closes: those cycles become idle time, which is what the wrap
;     writes need.
;
; The answer is per RECORD, not per address, so it lands in a bitmap indexed by
; the record's ordinal in the chain.  Both the analysis and the scheduling loop
; walk the chain from the start in order, so the ordinals agree.
MAX_RECORDS   = NUM_CAPTURE_BANKS*((CAPTURE_END-CAPTURE_WIN)/CAPTURE_RECORD_SZ)
NEED_BMP_SZ   = MAX_RECORDS/8

; BLK3, mapped for the whole of generation
ANALYSIS_WIN  = $6000
NEEDED_BMP    = ANALYSIS_WIN			; the answer: one bit per record
PENDC         = NEEDED_BMP+NEED_BMP_SZ		; unconfirmed write per COLOUR index
PENDC_SZ      = (SHIDX_VIC-SHIDX_COLOR)*2
REWIND_BMP    = PENDC+PENDC_SZ
ANALYSIS_END  = REWIND_BMP+SHADOW_BMP_SZ

; BLK1, mapped only while the analysis runs -- cap_read_record borrows the
; window a record at a time and puts it back, so the table survives underneath.
; By scheduling time it is dead and BLK1 is the OUTPUT bank again.
PENDING_WIN   = $2000
PENDS         = PENDING_WIN			; ...per SCREEN index
PENDS_SZ      = SHIDX_COLOR*2

.assert ANALYSIS_END <= $8000, error, "analysis arrays overflow the BLK3 window"
.assert PENDS_SZ <= $2000, error, "screen pending table overflows the BLK1 window"
.assert (SHIDX_COUNT & 7) = 0, error, "SHIDX_COUNT must be a multiple of 8"

;*******************************************************************************
; REPLAY CODE CONSTANTS
GEN_WIN       = $2000	; base of the output window (BLK1)
GEN_END       = $8000	; end of BLK3 -> three 8KB banks = 24KB

; The output window CANNOT overflow, and it is worth writing down why, because
; the code carries a silent truncation path (gen_frame's @room bail and
; gen_emit's drop) that was long treated as a real hazard.
;
; Every byte emitted here belongs to an instruction that executes exactly once
; inside one frame.  No 6502 instruction exceeds ONE BYTE PER CYCLE -- the best
; ratio is "lda #imm" at 2 bytes in 2 cycles; every 3-byte form costs at least 4
; cycles, and every 1-byte form at least 2.  So the emitted code can never be
; longer in bytes than the frame is in cycles: 16965 on NTSC, 22152 on PAL, and
; 17062 for a field of an interlaced frame.  All three are comfortably inside
; 24576.  Delay loops only widen the margin -- five bytes for up to 1500 cycles.
.assert FRAME_CYCLES <= GEN_END-GEN_WIN, error, "a frame of code cannot fit the output window"
REPLAY_MARGIN = 128	; # of cycles reserved at end of frame

; The generated code's cycle 0 executes on raster line 0, and that is arranged by
; where replay_loop's one-time coarse sync lands rather than by anything here:
; the sync target is chosen so the lead-in the sync path costs puts cycle 0 on
; the frame boundary (see REPLAY_SYNC_LINE).  The timer's period is exactly one
; frame, so every pass afterwards enters on the same line.
;
; That makes an absolute raster line and a replay cycle the same thing scaled by
; CYCLES_PER_LINE (see line_to_cycle), which is what lets the strip be placed
; anywhere in the frame -- including over the borders, where a $900f raster
; effect does most of its work.  It used to enter 11 lines in, and lines above
; that were simply unaddressable: they folded onto cycle 0 and the strip came out
; empty.  If the strip sits low or high by a constant amount, REPLAY_SYNC_LEAD is
; the number to trim.

;*******************************************************************************
capture_ptr:  .word 0		; BLK1-relative write cursor ($2000..$3fff)

;-------------------------------------------------------------------------------
cap_live_bank: .byte 0		; physical bank currently being written
cap_live_idx:  .byte 0		; index of cap_live_bank within capture_chain
cap_prev:      .byte 0		; record's pre-write value (read by cap::record)
cap_reg:       .byte 0		; record's opcode, read before the capture bank is
				; mapped over BLK1

;-------------------------------------------------------------------------------
; !0 = show the frame even though it cannot be exact.  The caller sets this
; after warning, so the second entry skips the check rather than bouncing the
; display back and forth again.
cap_addr:      .word 0		; record's address, held across the prev read

;-------------------------------------------------------------------------------
; VIC PIXEL-0 FALLBACK
; The value each VIC register held when the current frame began, for registers
; the frame itself never writes (those have no shadow entry).  The live register
; cannot serve as that fallback: the replay writes $9000-$900f every pass, so
; "live" is the value the LAST replay left, not the one the frame started with.
; In full-frame mode the seed prologue writes back exactly what it read, so live
; happens to be a fixpoint -- but window mode holds $900f at WIN_MARKER outside
; the strip, and reading that back latched the strip to black on every j/k after
; the first.
;
; It is seeded from the live registers in cap::trigger (the user's VIC is on
; screen by then) and maintained by cap::record: a write makes its register
; dirty for the current frame, so the shadow answers for it and this array is
; not consulted until the frame it started -- by which time this is that frame's
; pixel-0 value.  That keeps it right across a frame boundary, where the capture
; chain is rewound and the record is gone.
vic_pixel0:    .res 16		; $9000-$900f at the top of the current frame

;-------------------------------------------------------------------------------
shadow_saveA:  .byte 0		; gen_frame's saved $9ffa (BLK2) around the shadow
shadow_save2:  .byte 0		; gen_frame's saved $9ff2
gen_save8:     .byte 0		; ...and its saved $9ff8 (BLK1), which the caller
				; left holding the OUTPUT bank the generated code
				; is about to be RUN from

;-------------------------------------------------------------------------------
; PIXEL-0 RESTORE CURSORS
; Restores are emitted as one block, and where that block goes is the whole of
; what makes them correct.  Two things have to hold, and no per-address deadline
; tracking is available to enforce them:
;   - a restore must not erase a write before the beam reaches it (a program
;     writing row 20 while the beam is on row 5 needs that write to survive
;     until row 20 is scanned);
;   - a restore must not undo a write already emitted.
; Spreading restores through the frame breaks both, and repairing that needs the
; draw cycle of every cell -- which needs the video matrix base, and the program
; can move that mid-frame.
;
; Full-frame mode satisfies both by putting the block in the TAIL, past the end
; of the displayed area: the beam has finished with every row, and every write
; has been emitted.  That is the same instant as the head of the next pass --
; the replay loops on the frame period -- so the steady state is a block that
; runs before any of the writes it has to precede.  It used to sit at the head
; instead, which is equally correct but gets a far smaller budget: it had to
; stop at the first scheduled write, so a program that draws early got almost no
; restores at all.  See TAIL SETUP.
;
; Window mode restores its strip completely instead, through gap_fill_restores
; ahead of the strip and a tail block after it.
;
; Within the block the two regions alternate, so a partial restore covers whole
; cells (char + colour) rather than every char and no colours.
rst_cur_s:    .word 0		; screen/char region cursor  (index 0..SHIDX_COLOR)
rst_cur_c:    .word 0		; color region cursor  (SHIDX_COLOR..SHIDX_VIC)
rst_cur:      .word 0		; scan_region working cursor
rst_end:      .word 0		; scan_region region end
rst_turn:     .byte 0		; region restore_next takes from next (0=screen)
rst_cur_r:    .word 0		; scan_rmw cursor over the whole index space
rst_rmw_done: .byte 0		; !0 = the RMW priority pass is finished
need_count:   .word 0		; addresses the restore block is obliged to emit
vic_need:     .byte 0		; VIC registers the seed prologue is obliged to
				; emit.  Kept apart from need_count because the
				; two blocks are separate: scan_region stops at
				; SHIDX_VIC, so the restore block never emits a
				; VIC register, and counting them together
				; charged the frame for restores that never went
				; out on top of the seed that really did.
				; gap filler must leave it alone
restore_val:  .byte 0		; pixel-0 value of the restore just found
restore_addr: .word 0		; address it belongs to

;-------------------------------------------------------------------------------
; EXACT WINDOW MODE
; Shows one strip of the screen exactly, at the cost of everything outside it.
; Screen and colour writes before the window fold into a pre-set block (one
; store per address, at its W_start value); writes inside it are scheduled
; cycle-exact; writes after it are dropped.
;
; VIC registers cannot be folded.  The video matrix address is a running
; counter, so a mid-frame change to $9002/$9003/$9005 shifts every row after it
; -- the chip's fetch position at W_start depends on the whole write history,
; not on the register values there.  So $9000-$900e is mirrored write-for-write
; from cycle 0 through W_end.
;
; $900f is the exception, and the easy one: it has no internal counter, so a
; single write establishes it.  It is held at $00 outside the window as a marker
; that the surrounding rows are not faithful, set to its W_start value at the
; window edge, mirrored inside, and returned to $00 at W_end.
;
; STRIP RANGE
; The strip is positioned by absolute raster line, not by character row.  Rows
; would have to be measured from the display start, which means trusting a
; decode of $9001 -- and if that is off, every line above the assumed start
; becomes unreachable.  Lines are clamped only by the FRAME: the strip runs
; anywhere from line 0 to the last line, borders included, which is where a
; $900f raster effect does most of its work.  Two things had to give for that:
;
;   - the replay's cycle 0 now executes on raster line 0 (see REPLAY_SYNC_LINE).
;     It used to enter 11 lines in, and line_to_cycle folded everything above
;     that onto cycle 0, so a strip at the top came out empty rather than early.
;   - the setup block cannot always precede the strip any more.  When the strip
;     is at the top there is nothing in front of it, so the seed prologue goes
;     at the END of the pass instead -- which, in a loop locked to the frame
;     period, is the same instant as the head of the next one.  See SETUP DECIDE.
;
; STRIP HEIGHT
; Resized at runtime with '+' / '-' (see replay_loop), because the height that
; works depends entirely on how much the program draws.  The cost of a taller
; strip is restore budget, not correctness: the pre-set block has to re-establish
; every address the frame touched, and it can only run in the cycles OUTSIDE the
; strip, so each extra 8-line row costs 8*CYCLES_PER_LINE/6 = 86 addresses'
; worth of it.  On an NTSC frame that is ~2450 addresses at 4 rows and ~1750 at
; 12, against 1012 for a program that rewrites every cell and colour on the
; screen.  Running out degrades rather than corrupts -- the addresses that do
; not fit simply keep the value they had -- so growing the strip until the rows
; around it start to smear is a reasonable way to find the limit for a program.
;
; The height is bounded by the restore budget and by the row count win_lines can
; hold, not by the displayed area: a strip taller than the screen is a reasonable
; thing to ask for once the borders are in range, since it shows a border effect
; and the rows it runs into at the same time.  window_bounds re-derives the
; ceiling on every generation, so it tracks a program that changes the display
; geometry.  Row height follows $9003 bit 0 (8 or 16 lines), so the strip lands
; on character boundaries in both modes -- the tiling still starts from the
; display, and only the two end slots, out in the borders, sit off that grid.
;
; win_lines is a byte, so the strip cannot span more than 255 raster lines, and
; the two LIM constants keep win_rows*disp_rowlines inside it -- without them a
; nonsense row count in $9003 would wrap to some smaller strip.  They are also
; what bounds the strip against the frame now that the display no longer does:
; 248 and 240 lines both leave room inside FRAME_LINES.  You cannot reach either
; of them with '+'; the restore budget stops you long before.

; Last line the strip may end on: the last line of the frame.  The strip is
; positioned against the FRAME, not the displayed area -- the borders are where a
; $900f raster effect does most of its work, and bounding the strip to the
; display made exactly those rows unshowable.  window_close drops its synthetic
; $900f write when the strip runs to here, since there are no rows below it to
; mark and the write would land past gen_frame_end.

; $900f value held outside the strip: black background, black border, and bit 3
; SET.  Bit 3 is the normal/reverse flag and reverse is the 0 state, so $00
; would fill the unfaithful rows with solid blocks instead of black.


;-------------------------------------------------------------------------------
; STRIP POSITION
; The position of record is win_slot -- a tile index, not a line.  win_line is
; derived from it every generation as disp_start + (win_slot-SLOT_ORIGIN)*
; win_lines, clamped into range.
;
; Keeping the index rather than the line is what makes the tiling stable.  The
; clamps at each end of the frame land on lines that are not on the tiling grid
; (the frame is not a whole number of strips, and neither is the display).
; Stepping back from a clamped LINE shifts every later position by the remainder,
; so j/k stopped round-tripping and the strip no longer sat on the rows it had
; before.  Stepping the INDEX cannot drift: slot n always means the same rows,
; and the clamped end views are just what the first and last slots look like when
; they run into the frame.
;
; The origin stays at the display start even though the strip now ranges over the
; whole frame: that is what keeps the slots on character-row boundaries where
; there are characters to align to.  The two end slots are the off-grid ones, and
; they sit in the borders where nothing has rows anyway.


; displayed-area geometry, decoded from the frame's pixel-0 $9001/$9003 by
; display_geom once per generation
disp_rowlines: .byte 0		; raster lines per character row (8 or 16)
disp_rows:     .byte 0		; character rows the program is displaying
disp_start:    .word 0		; first displayed raster line
disp_end:      .word 0		; first raster line past the displayed area.  It
				; no longer bounds the strip (that ended when the
				; strip was allowed over the borders); what reads
				; it now is tail_start, which will not put the
				; full-frame tail block any higher than this.
				; the strip, so it goes after it (see SETUP DECIDE)
setup_cost: .byte 0		; cycles the tail seed block costs

;-------------------------------------------------------------------------------
.export __cap_gen_size		; diagnostic: 0 means generation emitted nothing
__cap_gen_size:
gen_size:       .word 0		; number of generated code bytes
gen_ptr:        .word 0		; output cursor
gen_full:       .byte 0		; !0 = output window full.  Nothing warns about
				; this: gen_frame's @room bail and gen_emit's drop
				; both truncate the frame silently, and
				; will_be_exact does not predict it (see the note
				; on its limits).
gen_cyc:        .word 0		; current cycle reached by emitted code
gen_read_ptr:   .word 0		; scheduler: READ cursor into the capture buffer
gen_frame_end:  .word 0		; scheduler: last cycle we schedule

;-------------------------------------------------------------------------------
; VISIBILITY
; The replay only has to satisfy one thing: at every cycle the VIC FETCHES an
; address, memory holds the captured value.  Nothing else is observable.  So a
; write to an address the chip never reads this frame does not have to be
; replayed -- and, because it is never replayed, does not have to be restored
; either.  Both obligations vanish together.
;
; That is worth a great deal.  On the usual layout (matrix at $1e00, character
; generator in ROM at $8000) the chip reads ~500 of the 4096 bytes of $1000-$1fff
; and ~500 of the 1024 colour nybbles.  Everything else in those ranges is
; ordinary working RAM that the frame may hammer as hard as it likes for free.
;
; This was rejected once, and for a good reason (see the note that used to stand
; on need_count): $1000-$1fff is matrix, character data and plain RAM at the same
; time, which of those a given address is depends on $9002/$9005, and the program
; can move them mid-frame.  A register SNAPSHOT genuinely cannot answer it.
;
; But the capture chain is the complete, cycle-ordered history of every
; $9000-$900f write in the frame, and the chip's fetch position is a function of
; exactly that.  fetch_walk replays the history against a model of the chip's
; counters and marks what gets read.  The information was never missing; it was
; never walked.
;
; MODEL.  Taken from the 6560/6561 reimplementation at
; github.com/sodiumlb/ocula-pivic-firmware, which is gate-level and is the
; authority here.  The parts that matter:
;
;   screen_addr = screen_mem_start + VMC, where
;       screen_mem_start = (($9005 & $f0) << 6) | (($9002 & $80) << 2)
;   colour comes from $9400 + (screen_addr & $3ff) -- the SAME address, fetched
;       in the SAME cycle, wrapping in the 1K aperture.  Matrix cell and colour
;       cell are one thing, which is why they share a bit here.
;   char data = ($9005 & $0f) << 10  +  (code << (3 + $9003.0))  +  cell depth
;
; VMC is a free-running 12-bit counter, NOT base + row*cols + col.  It is latched
; into VML at the last raster line of a character row and reloaded from VML at
; HC=2 of every line -- which is why every raster line of a row re-fetches the
; same matrix bytes, and why a mid-frame $9002 change shifts every row after it
; rather than re-indexing the ones before.  Modelling the counter is the only way
; to get that right; arithmetic on the register values is not.
;
; The chip resets VML and VMC during vertical sync, so the frame opens with both
; at zero.  $9003's row count is latched once, at VC=0, well before the matrix
; opens -- so the pixel-0 value is the one that counts no matter what the frame
; does to $9003 later.  Columns are latched per line at HC=2.
;
; GRANULARITY.  The walk steps per RASTER LINE, not per cycle: everything that
; selects what gets fetched is latched at the top of a line, so 261 iterations
; answer the same question 16965 would.  Marking happens on the first line of
; each character row (and again if the geometry moved underneath it), since every
; line of a row fetches the same cells.
;
; A geometry write landing INSIDE the displayed area is the one case a per-line
; model cannot resolve -- it changes the fetch position partway along a line the
; walk has already committed to.  Rather than guess, vis_off is raised and the
; whole filter stands down for that frame, which is exactly today's behaviour.
; __cap_geom_unstable counts it, so how often real programs actually do this is a
; measurement rather than a guess.
;
; CONSERVATISM.  Every approximation here marks MORE than the chip reads, never
; less: the spurious cell past the end of each row is marked, the character
; generator is taken as a whole span rather than per glyph, and any doubt raises
; vis_off.  A wrongly kept write costs cycles.  A wrongly dropped one is visible
; corruption, and would be found by looking at the screen rather than by any
; assert.
; Exported so the filter can be turned off from the monitor and a frame compared
; against itself.  That comparison is the whole verification story for this: with
; it off the replay must be byte-identical to what the engine built before, and
; with it on the PICTURE must be identical while the counters below drop.  No
; assert can check that -- only looking at the screen can.
.export __cap_vis_enable
__cap_vis_enable:
vis_enable:  .byte 0		; !0 = the filter is allowed to run
vis_off:     .byte 0		; !0 = stood down for this frame (see above)

fw_regs:     .res 16		; live $9000-$900f as the walk crosses the frame
fw_vmc:      .word 0		; video matrix counter
fw_vml:      .word 0		; ...and the latch it reloads from each line
fw_cdc:      .byte 0		; cell depth counter (0..7 or 0..15)
fw_vcc:      .byte 0		; vertical cell counter (character rows left)
fw_inmat:    .byte 0		; !0 = the matrix is open vertically
fw_line:     .word 0		; raster line the walk is on
fw_lineend:  .word 0		; first cycle past that line
fw_held:     .byte 0		; !0 = rec_* holds a record for a later line
fw_done:     .byte 0		; !0 = the record chain is exhausted
fw_remark:   .byte 0		; !0 = geometry moved, so re-mark this line
fw_cbmark:   .word 0		; char base the generator span was last marked for
fw_cbseen:   .byte 0		; !0 = fw_cbmark holds a base (so 0 is a real one)
fw_cell:     .word 0		; VIC address of the cell being marked
fw_i:        .byte 0		; cell counter within the line
fw_dl:       .byte 0		; deadline group this row's cells get

; The scheduling loop's cursor through the deadline table's unit.  Records arrive
; in cycle order, so the group is stepped rather than divided out: a division per
; record would be thousands of them, and this is an add and a compare.
bmp_count:     .word 0		; bmp_popcount result
rewind_cost:   .word 0		; rewind_est*6, the cycles reserved for them
rewind_est:    .word 0		; upper bound on the rewind set, known BEFORE
				; scheduling so its cycles can be reserved
vis_group:     .byte 0		; four-line group the current record sits in
vis_group_end: .word 0		; first cycle of the NEXT group
dl_saveC:      .byte 0		; gen_frame's saved $9ffc (BLK3) around the table
dl_save2:      .byte 0		; ...and its saved $9ff2

; decoded from fw_regs by fw_decode
fw_cols:     .byte 0		; $9002 & $7f
fw_rows:     .byte 0		; ($9003 & $7e) >> 1
fw_lastline: .byte 0		; last cell-depth value: 7, or 15 if $9003.0
fw_originy:  .byte 0		; $9001
fw_originx:  .byte 0		; $9000 bits 0-6
fw_ncells:   .byte 0		; cells this line actually fetches
fw_vadv:     .byte 0		; ...and what VMC therefore advances by
fw_over:     .byte 0		; !0 = the row overran the line, so which row an
				; address belongs to is only known to within a
				; cell -- see the deadline slack in fw_mark_cells
fw_savex:    .byte 0		; fw_consume's register index across a mark
cg_blocks:   .byte 0		; 1K blocks of $1000-$1fff holding character data
fw_ord:      .word 0		; 1-based ordinal of the record in rec_*
sched_ord:   .word 0		; ...and the scheduling loop's own count
fw_vmbase:   .word 0		; screen_mem_start, a 14-bit VIC address
fw_cbbase:   .word 0		; char_mem_start, a 14-bit VIC address

;-------------------------------------------------------------------------------
; TAIL SETUP
; Full-frame mode emits NOTHING at the head of the pass.  The seed prologue, the
; index-register seed and the pixel-0 restores all go at the END, which in a loop
; locked to the frame period is the same instant as the head of the next pass.
;
; That is what makes the frame's opening writes reachable.  The prologue is up to
; ~100 cycles and used to run before the first scheduled write; a program whose
; first store lands inside that window had it emitted late by the difference, and
; no amount of register priming could fix it -- gen_cyc was simply already past
; the deadline.  Moved to the tail, cycle 0 of the pass is the first write's.
;
; The block cannot merely follow the last write, though: a program that stops
; drawing mid-screen would have the restores erase rows before the beam reached
; them, and the seed would change VIC registers mid-frame.  So it starts at the
; later of the displayed area's end and the last write -- out in the bottom
; border and vblank, where nothing it does is visible this pass.
;
; Room is reserved rather than hoped for: gen_frame_end is pulled in by
; setup_cost, so scheduling stops early enough that the seed always fits.  The
; writes that displaces are in the bottom border, which build_shadow already
; drops as a matter of course.
; Restores use BOTH ends.  The tail alone is not enough: the displayed area sits
; low in the frame, so once REPLAY_MARGIN and the reservation are taken out there
; is often less room below it than there is above it, and a tall display leaves
; none at all.  The head is idle by construction up to the first scheduled write,
; and a BUDGETED block there cannot steal a cycle from anything -- which is the
; difference between it and the prologue, whose unconditional ~100 cycles are
; what had to move in the first place.
last_tc:        .word 0		; cycle of the last record in the frame
tail_cyc:       .word 0		; cycle the tail block may begin at
tail_room:      .word 0		; cycles between there and the reservation
head_cyc:       .word 0		; cycle the head block must stop by (== its room,
				; since nothing precedes it).  Clamped to tail_cyc
				; in full_frame_budget -- neither of the bounds
				; head_room applies is bounded by the frame, and
				; unclamped the two regions could overlap and get
				; their cycles counted twice.
restore_room:   .word 0		; head_cyc + tail_room -- what the restores really
				; get, and what will_be_exact must measure against
restored_count: .word 0		; restores the block actually emitted
record_count:   .word 0		; records in the frame, for the interlace split
				; (frame_cyc - REPLAY_MARGIN)

;-------------------------------------------------------------------------------
; scratch vars for frame generator record creation.
; rec_addr..rec_tc+1 must stay contiguous and in on-disk record order:
; plan_preloads saves/restores them as one CAPTURE_RECORD_SZ-byte block.
rec_addr         = zp::debuggertmp	; current record pointer
rec_val          = zp::debuggertmp+2	; current record value
rec_prev         = zp::debuggertmp+3	; value the address held at pixel 0
rec_tc           = zp::debuggertmp+4	; current record cycle
rec_op           = zp::debuggertmp+6	; opcode of the storing instruction
gen_loop_count   = zp::debuggertmp+7	; emit_delay: loop iteration count
.assert gen_loop_count <= zp::debuggertmp+7, error, "rec_* overflows debuggertmp"

; pixel-0 lookup scratch.  Nothing here survives a cap_read_record or gen_emit
; call (both use r0-r5), so it is only ever live within one address lookup.
shidx            = r0			; dense index of the address being looked up
shptr            = r2			; bitmap byte holding its bit
shmask           = r4			; the bit within that byte
pslot            = ra			; pend_slot / confirm_idx pointer.  Must be
					; zeropage -- it is used indirect-indexed,
					; and the .BSS it started in is RAM123.
lc               = r6			; lines_to_cycles in/out

gen_delay_cycles = r6			; emit_delay: remaining cycles to burn
gen_div_scratch  = r8			; temp scratchpad

;-------------------------------------------------------------------------------
; DELAY LOOP COST
; A delay loop costs cost*kk + bias cycles for kk iterations; both terms depend
; on where the loop is emitted (see loop_cost), so they are recomputed for every
; loop rather than assumed.
gen_loop_cost:   .byte 0		; cycles per iteration (5, or 6 across a page)
gen_loop_bias:   .byte 0		; constant term (1, or 0 across a page)
gen_loop_full:   .word 0		; cycles of a full 250-iteration loop
gen_loop_thresh: .word 0		; gen_loop_full + 6

;-------------------------------------------------------------------------------
gen_src_bank:    .byte 0		; current bank of the read cursor
gen_src_ptr:     .word 0		; end cursor
gen_src_end_idx: .byte 0		; bank index captured frame ends in
gen_read_idx:    .byte 0		; bank index read cursor is currently in

;-------------------------------------------------------------------------------
; INTERLACE TWO-SET OUTPUT
; A large interlaced frame's replay code can exceed the 24KB single-window
; ceiling, so it may be split at the field seam: field 1 (tc < gen_seam)
; generates into OUTPUT set A, field 2 (tc >= gen_seam) into set B, joined by a
; resident bank-flip stub that runs from $a000 and remaps BLK1/2/3 to set B.
FLIP_CYCLES = 24		; jmp-to-flip (3) + stub: 3x lda#/sta (18) + jmp (3)
gen_interlaced: .byte 0		; !0 = split this frame into two field sets
gen_field2:     .byte 0		; !0 = generator has crossed the seam into set B
gen_seam:       .word 0		; field-boundary cycle (frame_cyc/2)
gen_out1:       .byte 0		; OUTPUT bank gen_emit maps into BLK1 (set A or B)
gen_out2:       .byte 0		; ... BLK2
gen_out3:       .byte 0		; ... BLK3

;*******************************************************************************
; FRAME-SCOPED REGISTER SCHEDULER
; A model of the generated program's A/X/Y is kept live across the WHOLE frame
; (regmodel_val/regmodel_known), not reset per write.  The model MIRRORS the
; program's own register use: each record carries the register its value was
; stored from (rec_reg), and the replay puts the value in that same register
; rather than searching for a convenient one.
;
; That is what makes the tight cases representable.  Two stores 4 cycles apart
; have no instruction between them, so both values were already live in the real
; machine's registers -- and the real machine's loads of them therefore sit in
; earlier gaps, which is exactly the room the preloader needs.  Mirroring turns
; the availability of that room into a structural property instead of something
; a heuristic has to get lucky with.  A read-modify-write names no register --
; its value comes from memory and A/X/Y are untouched -- and rather than borrow
; one it is re-emitted as itself whenever its memory input is known to be
; initialized; see RMW RE-EMISSION.  Only an RMW whose input cannot be trusted
; borrows a register, using RMW PICK REG to minimize the resulting pressure.
;
; Each captured write is scheduled in cycle order:
;   - value already in a register  -> a bare store, padded to the exact cycle
;         gap 4    : "st? abs"      (4c), pad 0
;         gap 5    : "sta abs,x/y"  (5c indexed; value must be in A, index off a
;                    known non-A register: operand = addr - index-value), pad 0
;         gap >= 6 : "st? abs" (4c) + (gap-4) register-safe pad (always >= 2)
;   - value cold                   -> "ld? #value" (2c) then the bare store; the
;         load is placed during earlier slack (see below), so the store still
;         lands on its exact cycle
;         gap 6    : "ld? #value" (2c) + "st? abs" (4c), pad 0
;         gap 7    : "ld? <value>" (3c, identity zeropage) + "st? abs" (4c),
;                    pad 0 -- this is the "lda zp / sta abs" cadence (3+4), the
;                    tightest run a real program can hold.  A 2c load would
;                    leave a lone cycle, which has no instruction; see the
;                    IDENTITY ZEROPAGE notes for why 3c loads exist at all
;         gap >= 8 : "ld? #value" (2c) + (gap-6) pad + "st? abs" (4c)
; Two mechanisms give zero drift:
;   1. Cross-write cache: a register already holding the value it is about to
;      store is not reloaded, so a run reusing <= 3 values (1,2,3,1,2,...) loads
;      each once and every later write of it is a bare store -- however long the
;      run.  The check is against that value's OWN register: a copy sitting in
;      another one cannot be used without breaking the mirror.
;   2. Lookahead preloading (plan_preloads/emit_gap_to): the slack of a >= 6
;      cycle gap is spent loading UPCOMING values into their registers rather
;      than padding with nops, so they are in place before the tight run that
;      needs them.  Only the FIRST upcoming use of each register is preloaded --
;      a later one would just be overwritten by the earlier store on its way.
; Only the dex/bne bulk delay (emit_delay, used for large gaps) clobbers a
; register (X); short gaps use register-safe padding, so modeled A/Y survive
; across the frame and X survives across short gaps.

; register model: the generated program's A/X/Y at the current emission point
regmodel_val:       .res 3	; modeled byte in register A(0)/X(1)/Y(2)
regmodel_known:     .res 3	; !0 if regmodel_val[i] holds a known/trusted value

; per-write scheduler scratch (survives gen_emit/emit_delay -> file-scope bss)
sched_slack:        .word 0	; rec_tc - gen_cyc for the current write (signed)
sched_entry:        .word 0	; cycle where the current write's load/store begins
sched_cost:         .byte 0	; current store cost (4 or 5)
sched_op:           .byte 0	; current store opcode
sched_cold:         .byte 0	; !0 if the current value must be loaded first
sched_zpload:       .byte 0	; !0 = load it with the 3-cycle "ld? <value>" form
				; off the identity zeropage instead of "ld? #value"
sched_reg:          .byte 0	; register holding (or to hold) the current value
sched_reserved:     .byte 0	; register reserved for the current value ($ff=none)
sched_index_bias:   .byte 0	; setup_index_store: index value biasing the operand
sched_ldcost:       .byte 0	; cycles of the cold load just emitted (2 or 3)
sched_orig_addr:    .word 0	; unmodified address for the scheduled-write bitmap
				; (indexed stores bias rec_addr, this does not)
rmw_pref:           .byte 0	; rmw_pick_reg: preference-order cursor
rmw_reg:            .byte 0	; rmw_room: register under test
rmw_gap:            .word 0	; rmw_room: cycles to that register's next use
rmw_deadline:       .word 0	; rmw_count_stores: cycle to stop counting at
rmw_n:              .byte 0	; rmw_count_stores: running count
rmw_count:          .word 0	; first-touch RMW addresses in the replayed span
				; (the frame, or the strip in window mode)
rmw_ok:             .byte 0	; !0 = their restores fit, so re-emission is safe
rmwt_reg:           .byte 0	; rmw_gap_of: register under test
rmwt_best:          .byte 0	; rmw_pick_worst: best candidate so far
rmwt_gap:           .word 0	; ...and its distance to its next use

;-------------------------------------------------------------------------------
; FULL-FRAME REGISTER SEED
; What the tail block loads into A/X/Y, captured from the pending-use slots at
; generation start.  Held separately from pend_val because scheduling advances
; the slots as it consumes records, so by emission time they describe some later
; point in the frame -- see SEED PEND MODEL.
seed_val:           .res 3	; value to load
seed_have:          .res 3	; !0 = that register is seeded at all
seed_i:             .byte 0	; loop index over the three

;-------------------------------------------------------------------------------
; DRIFT COUNTERS
; Writes this generation could not place on their exact cycle, split by cause so
; a nonzero total says which mechanism to go after.  Both are bounded, tracked
; drift -- the replay stays usable -- but both are meant to be rare, and the only
; honest way to know whether they are is to count them on real programs rather
; than argue about reachability.  Exported so they can be read from the monitor
; at the addresses in labels.txt.  Reset per generation.
.export __cap_late_cold
__cap_late_cold:    .word 0	; schedule_write @coldtight: value cold with fewer
				; than 6 cycles to load and store it
.export __cap_late_neg
__cap_late_neg:     .word 0	; other timing miss: the write cannot finish by its
					; deadline on arrival, or a lone padding cycle makes
					; the emitted code land one cycle early

;-------------------------------------------------------------------------------
; VISIBILITY COUNTERS
; What the filter actually did, for the same reason the drift counters exist: the
; only honest way to know whether dropping invisible writes is worth anything is
; to count it on real programs.  drop_space over kept is the win; geom_unstable
; is how often a per-line model was not enough and the filter stood down.
; Exported so they can be read from the monitor.  Reset per generation.
.export __cap_drop_space
__cap_drop_space:   .word 0	; records dropped: the VIC never reads that address
.export __cap_drop_time
__cap_drop_time:    .word 0	; records dropped: the beam had already left that
				; address for the last time before the write landed
.export __cap_kept
__cap_kept:         .word 0	; records the filter let through
.export __cap_geom_unstable
__cap_geom_unstable: .word 0	; live-base geometry writes landing mid-line inside
				; the display, where the line is marked under both
				; the old and the new base.  It no longer stands
				; anything down -- the name is kept because it is
				; still the number worth watching

;-------------------------------------------------------------------------------
; RASTER LOCK DIAGNOSTICS
; The drift counters cannot see this: they compare the scheduler's model against
; itself, so a pass that is the wrong LENGTH, or a lock that is not holding,
; reports zero from both.  These measure the lock itself.
;
; skip/passes is the whole question.  The fine slide is the only thing that
; corrects sub-frame phase; @sync alone just waits for a T1 underflow.  If the
; pass overruns its frame, T1 has already underflowed and wrapped by the time
; the slide is computed, (rbase - $9124) falls outside REPLAY_SLIDE_MAX, and the
; correction is skipped -- every pass, leaving nothing to re-anchor phase.
;   skip ~= passes -> the lock is broken; look at pass length and REPLAY_MARGIN
;   skip ~= 0      -> the lock holds; the drift is inside the generated code,
;                     which means emit_delay is emitting a different number of
;                     cycles than gen_advance recorded
; NOT reset per generation -- these accumulate across the whole viewing session,
; because what matters is the rate, and the viewer cannot be left without
; RESTORE working.
; Did the generated pass actually outgrow its frame?  The accounting says it
; cannot: mirroring a visible store is never dearer than the store the program
; itself made, an invisible one hands its cycles back, and the wrap writes are
; bounded by the number of addresses the chip reads.  This is here to prove that
; on real programs rather than to guard against it -- nothing acts on it, and
; nothing is dropped or reserved to keep it at zero.
.export __cap_overrun
__cap_overrun:   .word 0	; passes whose generated code ran past the frame

.export __cap_sync_skip
__cap_sync_skip: .word 0	; passes that ran with NO fine-sync correction
.export __cap_passes
__cap_passes:    .word 0	; replay loop iterations

sched_saved_rec:    .res CAPTURE_RECORD_SZ	; rec_* held across a forward scan

; emit_gap_to state
gap_target:         .word 0	; cycle to advance gen_cyc to
gap_bulk:           .word 0	; gap delta, then the bulk delay (delta - preload cycles)
gap_preload_cycles: .byte 0	; cycles the planned preloads consume (2*preload_count)
gap_dropped:        .byte 0	; !0 = a 1-cycle bulk could not be emitted, so the
				; gap came up a cycle short of gap_target

; plan_preloads state
preload_reg:        .res 3	; planned preload target registers
preload_val:        .res 3	; planned preload values
preload_count:      .byte 0	; number of planned preloads (0..3)
preload_free:       .byte 0	; free-register bitmap (bit i set = register i free)
preload_zp:         .res 3	; !0 = emit that preload as the 3-cycle zp form
preload_saved_rd:   .word 0	; saved gen_read_ptr across a scan
preload_saved_idx:  .byte 0	; saved gen_read_idx (scan may hop chain banks)
preload_saved_bank: .byte 0	; saved gen_src_bank across a scan

;-------------------------------------------------------------------------------
; PENDING-USE SLOTS
; What each register is next wanted for, and where its own scan of the record
; stream has got to.  There is no lookahead window: each register walks the
; frame on its OWN cursor, stopping at the next record that names it, and
; resuming from there once that record has been emitted.  Every record is
; therefore visited at most once per register -- three linear passes over the
; frame in total, which is less work than the fixed 24-record rescan this
; replaced, and unlike that scan it can see a use arbitrarily far ahead.
;
; That distance is the whole point.  The cycles to load a register are always
; there -- the program's own change of it cost at least the two we need, in some
; gap between the last store that used it and the next one -- but a windowed
; scan standing in that gap could not see far enough to know the register would
; be wanted, and spent the cycles on padding instead.
pend_valid:  .res 3		; !0 = pend_val/pend_tc hold that register's next use
pend_val:    .res 3		; value it will need to hold
pend_tc:     .res 6		; cycle of the store that needs it (word per reg)
pend_ptr:    .res 6		; that register's read cursor (word per reg)
pend_idx:    .res 3		; ...its chain bank index
pend_bank:   .res 3		; ...its physical bank
pend_best:   .byte 0		; pend_pick: best candidate so far
pend_cmp:    .word 0		; pend_sooner: incumbent's deadline
pend_chosen: .byte 0		; pend_refill: register being refilled
pend_saved_rec: .res CAPTURE_RECORD_SZ	; rec_* held across a refill

rbase:   .byte 0		; replay frame-sync: nop-slide base (timer-lo - lead)

; the loop's own saved BLK1/2/3 mapping.  'z' single-steps the program from
; inside the loop, which runs the simulator + code generator; those reuse
; cap_save* and remap the BLK windows, so the loop stashes its OUTPUT mapping.
replay_save8: .byte 0		; saved $9ff8 (BLK1)
replay_saveA: .byte 0		; saved $9ffa (BLK2)
replay_saveC: .byte 0		; saved $9ffc (BLK3)
replay_save2: .byte 0		; saved $9ff2 (BLK config)
replay_col:   .byte 0		; saved VIA2 keyboard column drive ($9120)
replay_rowbit: .byte 0		; row bit the current matrix scan is testing

; debugger's VIA2 Timer 1 state backup
replay_jmpvec: .word 0		; fine-sync indirect-jump vector.  Deliberately NOT
				; zp::jmpaddr: page 0 holds the identity table while
				; the replay is locked to the raster, so nothing in
				; the loop may use a zeropage variable.  "jmp (abs)"
				; is 5 cycles either way, so the sync is unchanged.
zp_save8:     .byte 0		; zp_install/zp_restore: saved $9ff8 (BLK1)
zp_save2:     .byte 0		; ... and $9ff2
replay_ier1:  .byte 0		; saved $911e (VIA1 IER -- the NMI enables)
replay_acr:   .byte 0		; saved $912b (VIA2 ACR)
replay_t1ll:  .byte 0		; saved $9126 (VIA2 T1 latch lo)
replay_t1lh:  .byte 0		; saved $9127 (VIA2 T1 latch hi)

; RESTORE (VIA1 CA1) latch; see RESTORE ARM / POLL RESTORE
.export __cap_abort		; diagnostic: !0 means a RESTORE unwound generation
__cap_abort:
cap_abort:    .byte 0		; !0 = RESTORE seen; unwind out of generation
poll_ctr:     .byte 0		; poll_restore's 1-in-256 gate
progress_col: .byte 0		; border colour the working indicator is showing

;*******************************************************************************
.segment "SIM"

;*******************************************************************************
; RESET
; Rewinds the capture chain to its head position. This is the first bank in the
; chain of banks and the base address in that bank.
.export __cap_reset
__cap_reset:
.proc reset
	lda #CAPTURE_BANK		; capture_chain[0]
	sta cap_live_bank
	lda #$00
	sta cap_live_idx
	lda #<CAPTURE_WIN
	sta capture_ptr
	lda #>CAPTURE_WIN
	sta capture_ptr+1
	rts
.endproc

;*******************************************************************************
; RECORD
; Records a store if it targets an address that can affect the display.
; Stores to any other address are ignored
; Called from vmem_store BEFORE the write lands, so a read of the target here
; still returns the pre-write value -- that is where cap_prev comes from.
; IN:
;   - .XY: address that was written
;   - .A:  value that was written
.export __cap_record
__cap_record:
.proc record
	pha
	jsr in_capture_range	; .C set if the address is tracked
	bcc @done

	; a VIC register write makes that register dirty for the current frame,
	; so the shadow answers for it from here on and vic_pixel0 is not read
	; again until the frame this write starts -- where this IS the pixel-0
	; value.  (in_capture_range only passes $90xx for .X < $10.)
	cpy #$90
	bne :+
	sta vic_pixel0,x

:	; read the value the address holds now, before the store overwrites it.
	; vmem_load clobbers .X/.Y (and leaves BLK1 on MAIN, which every other
	; virtual access here already tolerates), so stash the address first.
	stxy cap_addr
	jsr vmem_load
	sta cap_prev
	ldxy cap_addr

	pla
	jmp capture_store	; append the address/value to store
@done:	pla
	rts
.endproc

;*******************************************************************************
; IN CAPTURE RANGE
; Checks if the target address may affect the screen contents ($1000-$1FFF,
; VIC registers, $9000-$900F, and color RAM, $9400-$97FF).
; IN:
;   - .XY: address
; OUT:
;   - .C: set if this address is one we capture
; Preserves .A, .X, .Y.
.proc in_capture_range
	cpy #$10
	bcc @vic		; < $1000
	cpy #$20
	bcc @yes		; $1000-$1FFF (screen / character memory)
@vic:	cpy #$90
	bne @color
	cpx #$10		; $9000-$900F (VIC registers)
	bcc @yes
	bcs @no
@color:	cpy #$94		; $9400-$97FF (color RAM)
	bcc @no
	cpy #$98
	bcc @yes
@no:	clc
	rts
@yes:	sec
	rts
.endproc

;*******************************************************************************
; CAPTURE STORE
; Appends a (address, value, prev, frame-cycle) record to the live capture bank.
; When the current bank fills it advances to the next bank in capture_chain.
; The frame boundary is handled by tick_raster, which rewinds capture to the
; head of the chain.
; Records are 6 bytes and the window is $2000..$3fff (1365 records/bank).
; The shortest store to a captured address is 4 cycles (zeropage is not in
; range), so a frame holds at most frame_cyc/4 records: 4241 (NTSC), 5538
; (PAL), 8531 (NTSC interlaced).  8 banks = 10920, so @overflow is unreachable.
; IN:
;   - .XY: address
;   - .A:  value
;   - cap_prev: the value the address held before this write
CAPTURE_LIMIT = CAPTURE_END - CAPTURE_RECORD_SZ + 1
NUM_CAPTURE_BANKS = 8
capture_chain:
	.byte CAPTURE_BANK, CAPTURE_BANK2, CAPTURE_BANK3
	.byte CAPTURE_BANK4, CAPTURE_BANK5, CAPTURE_BANK6
	.byte CAPTURE_BANK7, CAPTURE_BANK8
.proc capture_store
@i=r0
@addr=r2			; address being recorded
@val=r4				; value being recorded
@tc=r7				; computed frame-cycle timestamp
@save8=r5			; saved $9ff8 (BLK1 bank) during the append
@save2=r6			; saved $9ff2 (BLK config) during the append
	sta @val
	stxy @addr

	; timestamp = raster position at the end of this store instruction
	lda __sim_raster
	clc
	adc step_cycles
	sta @tc
	lda __sim_raster+1
	adc #$00
	sta @tc+1

	; grab the opcode NOW, while the normal mapping is still up: @store
	; borrows BLK1 for the capture bank, and __sim_op must not be read from
	; underneath that
	lda __sim_op
	sta cap_reg

	; advance to the next bank if a full record no longer fits in the
	; current window ($2000..$3fff)
	lda capture_ptr+1
	cmp #>CAPTURE_LIMIT
	bcc @store
	bne @full
	lda capture_ptr
	cmp #<CAPTURE_LIMIT
	bcc @store

@full:	; current bank full, move to the next one
	lda cap_live_idx
	cmp #NUM_CAPTURE_BANKS-1
	bcs @overflow
	inc cap_live_idx
	ldx cap_live_idx
	lda capture_chain,x
	sta cap_live_bank
	lda #<CAPTURE_WIN
	sta capture_ptr
	lda #>CAPTURE_WIN
	sta capture_ptr+1
	jmp @store

@overflow:
	; OOM.  Structurally unreachable (see the record-count arithmetic above),
	; so the record is simply dropped rather than tracked.

@ret:	; restore registers and return
	ldxy @addr
	lda @val
	rts

;------------------------------------------------------------------------------
; write the record (addr, val, cycle) to the capture bank
@store: ; map the live capture bank into the BLK1 window
	lda $9ff8
	sta @save8
	lda $9ff2
	sta @save2
	lda cap_live_bank
	sta $9ff8
	lda #$57			; BLK1 = RAM r/w
	sta $9ff2

	; write the record at capture_ptr
	lda capture_ptr
	sta @i
	lda capture_ptr+1
	sta @i+1
	ldy #$00
	lda @addr
	sta (@i),y
	iny
	lda @addr+1
	sta (@i),y
	iny
	lda @val
	sta (@i),y
	iny
	lda cap_prev
	sta (@i),y
	iny
	lda @tc
	sta (@i),y
	iny
	lda @tc+1
	sta (@i),y
	iny
	lda cap_reg			; the storing instruction's opcode
	sta (@i),y

	; restore the borrowed BLK1 mapping
	lda @save2
	sta $9ff2
	lda @save8
	sta $9ff8

	; advance the write cursor and record count
	lda capture_ptr
	clc
	adc #CAPTURE_RECORD_SZ
	sta capture_ptr
	bcc :+
	inc capture_ptr+1
:	jmp @ret
.endproc

;*******************************************************************************
; Everything below runs only at GENERATION time -- never from the per-store
; capture path and never during the cycle-exact replay itself -- so it lives in
; its own ROM bank (SIM/ROMB was full).  All shared state is capture.asm's .BSS,
; which the linker places in RAM123; that region is not bank switched, so both
; halves see the same variables.  Anything in ROM (capture_chain) has to be
; duplicated instead -- see gen_chain.
.segment "SIMCAP"

.include "capture-helpers.inc"


;*******************************************************************************
; CLEAR BITMAPS
; Zeroes DIRTY and RMW (the value array needs no clearing -- an entry is only
; ever read when its dirty bit is set).  SHADOW_WSEEN is not touched here:
; window_rmw_scan owns it and clears it itself, so a full-frame generation never
; pays for it.
.proc clear_bitmaps
	lda #<SHADOW_DIRTY
	ldy #>SHADOW_DIRTY
	jsr clear_bmp
	lda #<SHADOW_RMW
	ldy #>SHADOW_RMW
	jmp clear_bmp
.endproc

;*******************************************************************************
; SCHEDULED RESET / MARK
; SHADOW_WSEEN is scratch while window_rmw_scan identifies first touches inside
; the strip.  Once that scan is finished it becomes the scheduling pass's
; "already written by replay" bitmap in both modes.  An RMW on an address marked
; here can always be re-emitted: this pass itself established the value it will
; read, independently of the restore budget.
.proc scheduled_reset
	lda #<SHADOW_WSEEN
	ldy #>SHADOW_WSEEN
	jmp clear_bmp
.endproc

.proc scheduled_mark
	ldxy sched_orig_addr
	jsr shadow_index
	jsr shadow_bitptr
	lda #<SHADOW_WSEEN
	ldy #>SHADOW_WSEEN
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	ora shmask
	sta (shptr),y
	rts
.endproc

;*******************************************************************************
; BUILD SHADOW
; Walks every record in the captured frame and records, for each distinct
; address, the value it held at pixel 0 -- that is the prev of the FIRST record
; naming it, so a set dirty bit means "already have it, skip".
; The walk is deliberately raw (next_record_raw): a write in the frame's tail
; headroom is dropped from the replay, so restoring its address to the pixel-0
; value is not just harmless but correct -- the write happened below the
; visible area.
; Assumes SHADOW_BANK is already mapped into BLK2 (gen_frame does this).
.proc build_shadow
@vp=r6
	jsr clear_bitmaps
	lda #$00
	sta rmw_count
	sta rmw_count+1
	sta need_count
	sta need_count+1
	sta vic_need
	sta last_tc
	sta last_tc+1
	sta record_count
	sta record_count+1
	jsr gen_read_reset

@loop:	jsr poll_restore	; RESTORE -> abandon the walk
	bne @out
	jsr next_record_raw
	bcs :+
@out:	rts
:	lda rec_tc		; records are in cycle order, so the last one to
	sta last_tc		; land here is the frame's last write (see TAIL
	lda rec_tc+1		; SETUP; tail_start clamps it into range)
	sta last_tc+1
	incw record_count

	ldxy rec_addr
	jsr shadow_index
	jsr shadow_test_dirty
	bne @loop		; first touch already seen -> this is not pixel 0

	; claim it and store the pre-write value
	ldy #$00
	lda (shptr),y
	ora shmask
	sta (shptr),y


	lda shidx		; @vp = SHADOW_VAL + shidx
	clc
	adc #<SHADOW_VAL
	sta @vp
	lda shidx+1
	adc #>SHADOW_VAL
	sta @vp+1
	lda rec_prev
	sta (@vp),y

	; Every address the frame touches is an obligation.  Working out which
	; ones are actually observable is not safe: $1000-$1fff is screen matrix,
	; character data and plain RAM all at once, which of those a given
	; address is depends on $9005/$9002, and the program can move those
	; mid-frame -- so an address that is not part of the matrix when it is
	; written can be part of it by the time the beam arrives.  Count them all
	; and let the display extent fall out of the total instead.
	jsr bump_need

	; a first touch that is a read-modify-write reads this value back when it
	; is re-emitted, so its restore cannot be one of the ones the budget drops.
	; Frame-scoped, which is what full-frame mode wants; exact-window mode
	; throws this away and rebuilds it against the strip (see WINDOW RMW SCAN).
	jsr rec_regof
	cmp #REG_ANY
	bne @loop

	; ...but a VIC register has no such restore to drop.  Its pixel-0 value
	; comes from the seed prologue, which is unconditional and unbudgeted, so
	; a re-emitted RMW on one is safe however tight the frame gets.  Counting
	; it here charged rmw_check_budget against room it never asks for, and
	; that switch is frame-global -- a raster effect written "inc $900f"
	; could talk the frame out of re-emitting every OTHER RMW in it.  The bit
	; is dead weight too: scan_rmw stops at SHIDX_VIC and never reads it.
	lda shidx+1
	cmp #>SHIDX_VIC
	bcs @loop

	inc rmw_count
	bne :+
	inc rmw_count+1
:
	jsr shadow_bitptr
	lda #<SHADOW_RMW
	ldy #>SHADOW_RMW
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	ora shmask
	sta (shptr),y
	jmp @loop
.endproc

;*******************************************************************************
; BUMP NEED
; One more address the frame is obliged to re-establish, charged to whichever
; block will actually do it: the seed prologue for a VIC register, the restore
; block for anything else.
; IN: shidx = the address' index.  Preserves .X/.Y; clobbers .A.
.proc bump_need
	lda shidx+1
	cmp #>SHIDX_VIC
	bcs @vic
	inc need_count
	bne :+
	inc need_count+1
:	rts
@vic:	inc vic_need
	rts
.endproc

;*******************************************************************************
; VIC DIRTY
; Whether the frame writes VIC register .X at all.  A register it never writes
; needs nothing done to it: the replay's only VIC writes are the frame's own
; records, so a clean register keeps its pixel-0 value pass after pass.
; OUT: .Z set if clean.  IN/OUT: .X preserved (shadow_test_dirty leaves it
; alone); clobbers .A/.Y and r0-r4.
.proc vic_dirty
	txa			; shidx = SHIDX_VIC + .X
	sta shidx
	lda #>SHIDX_VIC		; (SHIDX_VIC is page-aligned, so .X is the low
	sta shidx+1		;  byte outright)
	jmp shadow_test_dirty
.endproc



;*******************************************************************************
; VIS REMAP
; Puts BLK1/2/3 back after a far call into the visibility bank.
;
; A far CALL runs __ultimem_select_bank on the way in AND, through pop_bank, on
; the way back -- so all three windows come back holding whatever the bank table
; says for FINAL_BANK_SIM_CAP, not what the caller had arranged.  Restoring only
; BLK2 here is how the generated code came to be executed out of the simulator's
; virtual memory image: BLK1 still held Ram(33) when replay_loop did its
; "jsr CAP_RUN_ADDR", so the replay jumped into the user's program instead of
; into the frame it had just generated.  BLK3 was equally wrong, which left the
; whole scheduling pass reading fetch deadlines out of unrelated RAM.
;
; Nothing announces either failure.  Restore all three, together, in one place.
.proc vis_remap
	lda gen_save8			; BLK1: the caller's OUTPUT bank
	sta $9ff8
	lda #SHADOW_BANK		; BLK2: pixel-0 arrays
	sta $9ffa
	lda #ANALYSIS_BANK		; BLK3: fetch deadlines
	sta $9ffc
	lda #$7f			; all three RAM r/w
	sta $9ff2
	rts
.endproc

;*******************************************************************************
; VIS KEEP
; Is this write observable?  One question now, not three: did a read of its
; address ever confirm it?
;
; Character data is the exception and has to be.  Which byte of the generator the
; chip reads depends on the screen codes on the line, so a read of it cannot be
; attributed to an address the way a matrix or colour read can.  Writes there are
; kept whenever the generator lives in RAM at all.
;
; IN:  .XY = address, sched_ord = the record's 1-based ordinal
; OUT: .C set to keep.  Clobbers .A/.Y and r0-r7.
.proc vis_keep
@p=r6
	lda vis_enable
	beq @yes
	lda vis_off
	bne @yes

	cpy #$90			; VIC registers are read continuously
	bcs @yes

	tya				; character data?
	lsr
	lsr				; addr >> 10, for $1000-$1fff
	and #$03
	tax
	lda bitmask_tab,x
	and cg_blocks
	bne @yes

	lda sched_ord			; bit (sched_ord-1) of NEEDED_BMP
	sec
	sbc #$01
	sta @p
	lda sched_ord+1
	sbc #$00
	sta @p+1
	lda @p
	and #$07
	tay
	lda bitmask_tab,y
	sta shmask
	lda @p+1
	lsr
	sta @p+1
	lda @p
	ror
	sta @p
	lsr @p+1
	ror @p
	lsr @p+1
	ror @p
	lda @p
	clc
	adc #<NEEDED_BMP
	sta @p
	lda @p+1
	adc #>NEEDED_BMP
	sta @p+1
	ldy #$00
	lda (@p),y
	and shmask
	beq @no
@yes:	sec
	rts
@no:	incw __cap_drop_space
	clc
	rts
.endproc


;*******************************************************************************
; RESTORE RESET
; Rewinds both restore cursors to the head of their regions.
.proc restore_reset
	lda #$00
	sta rst_cur_s
	sta rst_cur_s+1
	sta rst_cur_c
	sta rst_turn
	sta rst_cur_r		; the RMW priority pass runs first, from index 0
	sta rst_cur_r+1
	sta rst_rmw_done
	lda #>SHIDX_COLOR
	sta rst_cur_c+1
	rts
.endproc

;*******************************************************************************
; SHADOW ADDR
; index -> the address it stands for (the inverse of shadow_index).  Only the
; two memory regions occur here; VIC indices are never restored this way, the
; seed prologue covers them.
; IN:  shidx
; OUT: restore_addr
.proc shadow_addr
	lda shidx
	sta restore_addr
	lda shidx+1
	cmp #>SHIDX_COLOR
	bcs @color
	clc			; screen/char: addr = idx + $1000
	adc #$10
	sta restore_addr+1
	rts
@color:	clc			; color: addr = idx - $1000 + $9400
	adc #$84
	sta restore_addr+1
	rts
.endproc


;*******************************************************************************
; SCAN REGION
; Advances one region's cursor to the next address that still needs restoring:
; dirty (so we know its pixel-0 value) and not yet stored to by the replay.
; Empty groups of 8 are skipped a whole bitmap byte at a time, so a sparse frame
; costs one pass over the bitmap rather than one test per address.
; IN:
;   - .A: region (0 = screen/char, !0 = color)
; OUT:
;   - .C set: shidx holds the index, and the cursor is left past it
; Clobbers .A/.X/.Y.
.proc scan_region
	tax
	bne @color

	lda rst_cur_s		; screen/char: [0, SHIDX_COLOR)
	sta rst_cur
	lda rst_cur_s+1
	sta rst_cur+1
	lda #<SHIDX_COLOR
	sta rst_end
	lda #>SHIDX_COLOR
	sta rst_end+1
	jmp @scan

@color:	lda rst_cur_c		; color: [SHIDX_COLOR, SHIDX_VIC)
	sta rst_cur
	lda rst_cur_c+1
	sta rst_cur+1
	lda #<SHIDX_VIC
	sta rst_end
	lda #>SHIDX_VIC
	sta rst_end+1

@scan:	lda rst_cur+1		; region exhausted?
	cmp rst_end+1
	bcc @more
	lda rst_cur
	cmp rst_end
	bcc @more
	jsr @save
	clc
	rts

@more:	lda rst_cur		; shidx = rst_cur
	sta shidx
	lda rst_cur+1
	sta shidx+1

	lda rst_cur		; on a byte boundary?
	and #$07
	bne @one

	jsr shadow_bitptr	; whole group of 8 clean -> skip it
	lda #<REWIND_BMP
	ldy #>REWIND_BMP
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	bne @one
	lda rst_cur
	clc
	adc #$08
	sta rst_cur
	bcc @scan
	inc rst_cur+1
	jmp @scan

@one:	jsr shadow_bitptr	; still needs putting back?
	lda #<REWIND_BMP
	ldy #>REWIND_BMP
	jsr shadow_test_at
	beq @next		; no -- the preamble's value is still standing

	jsr @bump		; found: leave the cursor past it
	jsr @save
	sec
	rts

@next:	jsr @bump
	jmp @scan

;-------------------------------------------------------------------------------
@bump:	inc rst_cur
	bne :+
	inc rst_cur+1
:	rts

@save:	ldx rst_end+1		; write the cursor back to its region
	cpx #>SHIDX_COLOR
	bne @savec
	lda rst_cur
	sta rst_cur_s
	lda rst_cur+1
	sta rst_cur_s+1
	rts
@savec:	lda rst_cur
	sta rst_cur_c
	lda rst_cur+1
	sta rst_cur_c+1
	rts
.endproc

;*******************************************************************************
; SCAN RMW
; Advances the priority cursor to the next address marked in SHADOW_RMW -- the
; ones whose first write is a read-modify-write (frame-scoped in full-frame mode,
; strip-scoped in window mode).  Those restores cannot be among the ones the
; budget drops, because the re-emitted RMW reads the value back (see RMW
; RE-EMISSION).
; The address' dirty bit is cleared as it goes out, so the ordinary region scan
; does not restore it a second time.
; OUT: .C set: shidx holds the index, cursor left past it.  Clobbers .A/.X/.Y.
.proc scan_rmw
@scan:	lda rst_cur_r+1		; whole index space walked?
	cmp #>SHIDX_VIC
	bcc @more
	lda rst_cur_r
	cmp #<SHIDX_VIC
	bcc @more
	clc
	rts

@more:	lda rst_cur_r
	sta shidx
	lda rst_cur_r+1
	sta shidx+1

	lda rst_cur_r		; on a byte boundary?
	and #$07
	bne @one

	jsr shadow_bitptr	; whole group of 8 clear -> skip it
	lda #<SHADOW_RMW
	ldy #>SHADOW_RMW
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	bne @one
	lda rst_cur_r
	clc
	adc #$08
	sta rst_cur_r
	bcc @scan
	inc rst_cur_r+1
	jmp @scan

@one:	jsr shadow_bitptr
	lda #<SHADOW_RMW
	ldy #>SHADOW_RMW
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	and shmask
	beq @next

	; Found one.  Mark it written-by-replay rather than clearing its dirty
	; bit: DIRTY means "the frame touched this address, so SHADOW_VAL holds
	; its pixel-0 value", and replay_preamble walks it AFTER gen_frame has
	; run.  Clearing it here deleted every RMW address from the preamble --
	; which for an RMW is the one address whose starting value it cannot do
	; without.  WSEEN is the honest signal: this pass does write it.
	jsr shadow_bitptr
	lda #<SHADOW_WSEEN
	ldy #>SHADOW_WSEEN
	jsr shadow_addbase
	ldy #$00
	lda (shptr),y
	ora shmask
	sta (shptr),y
	jsr @bump
	sec
	rts

@next:	jsr @bump
	jmp @scan

@bump:	inc rst_cur_r
	bne :+
	inc rst_cur_r+1
:	rts
.endproc

;*******************************************************************************
; RESTORE NEXT
; The next pending restore, alternating regions so a cell and its colour go out
; together.  If one region is exhausted the other simply keeps supplying.
; OUT: .C set: restore_addr / restore_val hold the store to emit
.proc restore_next
	lda rst_rmw_done	; the RMW addresses come out first -- see SHADOW_RMW
	bne @regions
	jsr scan_rmw
	bcs @found
	inc rst_rmw_done	; exhausted; from here on it is the regions only

@regions:
	lda rst_turn		; scan_region clobbers .X, so no loop counter
	jsr scan_region
	bcs @alt

	lda rst_turn		; that region is exhausted -- try the other
	eor #$01
	sta rst_turn
	lda rst_turn
	jsr scan_region
	bcc @none

@alt:	lda rst_turn		; alternate for the next call
	eor #$01
	sta rst_turn

@found:	jsr shadow_addr		; shidx -> restore_addr
	lda shidx		; restore_val = SHADOW_VAL[shidx]
	clc
	adc #<SHADOW_VAL
	sta shptr
	lda shidx+1
	adc #>SHADOW_VAL
	sta shptr+1
	ldy #$00
	lda (shptr),y
	sta restore_val
	sec
	rts

@none:	clc			; every region exhausted
	rts
.endproc







;*******************************************************************************
; LINES TO CYCLES
; IN/OUT: lc = raster lines in, CPU cycles out.
.proc lines_to_cycles
@acc=r8
@mul=ra
	lda #$00
	sta @acc
	sta @acc+1
	ldx #$08		; shift-and-add over the constant's bits
	lda #CYCLES_PER_LINE
	sta @mul
@ml:	lsr @mul
	bcc :+
	lda @acc
	clc
	adc lc
	sta @acc
	lda @acc+1
	adc lc+1
	sta @acc+1
:	asl lc
	rol lc+1
	dex
	bne @ml
	lda @acc
	sta lc
	lda @acc+1
	sta lc+1
	rts
.endproc

;*******************************************************************************
; DISPLAY LINE
; OUT: lc = the raster line the displayed area begins at.
; Taken from the frame's pixel-0 $9001 (vertical origin), not the live one: a
; program that moves the origin mid-frame still starts this frame where it was
; at the top.  The doubling assumes $9001 counts two raster lines per step; it
; is only used to place the strip initially, so if it is wrong the strip just
; starts in the wrong place and j/k walk it back.
.proc display_line
	ldx #$01		; $9001
	jsr shadow_vic_val
	asl			; origin counts two raster lines per step
	sta lc
	lda #$00
	rol			; .C from the shift is the high bit
	sta lc+1
	rts
.endproc

;*******************************************************************************
; LINE TO CYCLE
; Converts an absolute raster line into the replay's cycle position.  The replay
; enters on line 0 (see REPLAY_SYNC_LINE), so the two are the same measurement
; and this is only a scale -- kept as its own name because the call sites are
; converting a POSITION, and the day the entry line moves again this is where the
; bias goes back.
;
; It used to subtract an 11-line entry offset and floor the result at zero, which
; collapsed every line above the entry point onto cycle 0: a strip placed there
; came out with win_start == win_end and dropped the whole frame rather than
; showing the top rows.
; IN/OUT: lc
MAX_CONV_LINES = $ffff / CYCLES_PER_LINE
.proc line_to_cycle
	; Clamp first.  The callers derive lines by ARITHMETIC -- disp_end is
	; $9001*2 + rows*rowlines, and both terms are program-controlled -- so a
	; nonsense geometry can ask for a line far past the frame.
	; lines_to_cycles is a 16-bit shift-and-add with no overflow check, and a
	; wrap there comes back as a plausible-looking SMALL cycle, which is worse
	; than a large one: it silently moved the tail block up into the display.
	lda lc+1
	cmp #>MAX_CONV_LINES
	bcc @ok
	bne @clamp
	lda lc
	cmp #<MAX_CONV_LINES
	bcc @ok
@clamp:	lda #<MAX_CONV_LINES
	sta lc
	lda #>MAX_CONV_LINES
	sta lc+1
@ok:	jmp lines_to_cycles
.endproc

;*******************************************************************************
; DISPLAY START
; OUT: lc = the replay cycle the displayed area begins at.
.proc display_start
	jsr display_line
	jmp line_to_cycle
.endproc

;*******************************************************************************
; DISPLAY GEOM
; Decodes the frame's pixel-0 display geometry into disp_*: where the displayed
; area starts and ends in raster lines, how tall a character row is, and how
; many rows the program is showing.  Everything comes from the shadow, so a
; program that changes the geometry mid-frame is still described as it was at
; the top of the frame -- which is the geometry the replay reproduces.
; OUT: disp_rowlines, disp_rows, disp_start, disp_end
.proc display_geom
@h=r8				; displayed height in raster lines
	ldx #$03		; $9003: bit 0 = char height, bits 1-6 = rows
	jsr shadow_vic_val
	pha
	and #$01
	beq @c8
	lda #16			; 8x16 characters
	bne @setrl		; (always)
@c8:	lda #8			; 8x8 characters
@setrl:	sta disp_rowlines
	pla
	and #$7e		; rows (bit 7 is the raster LSB, read-only)
	lsr a
	sta disp_rows

	sta @h			; @h = disp_rows * disp_rowlines
	lda #$00
	sta @h+1
	ldx #$03		; *8 ...
	lda disp_rowlines
	cmp #16
	bne @sh
	inx			; ...or *16
@sh:	asl @h
	rol @h+1
	dex
	bne @sh

	jsr display_line	; lc = first displayed raster line
	lda lc			; disp_end = disp_start + height
	sta disp_start
	clc
	adc @h
	sta disp_end
	lda lc+1
	sta disp_start+1
	adc @h+1		; (the stores above leave .C alone)
	sta disp_end+1
	rts
.endproc





;*******************************************************************************
; RMW CHECK ROOM
; Decides whether re-emitting read-modify-writes is safe this frame at all.
;
; Each one that is the first touch of its address depends on a restore having
; put the pre-write value there, and restores are budgeted: what does not fit is
; simply not emitted.  For an ordinary store that only means the address keeps a
; stale value until something overwrites it, but an RMW READS what it finds, so
; a missed restore makes it compute from the wrong number -- and it does that
; again next pass, from its own previous answer.  The error grows.
;
; So if the frame has more of them than the block has room for, re-emission is
; switched off entirely and every RMW borrows a register instead.  That trades a
; bounded cycle of drift for a value that runs away, which is the right way
; round.  Reaching this needs a program with more first-touch RMWs than the
; restore block can initialize; register pressure then decides whether borrowing
; one introduces drift.
;
; The room is the caller's to measure because the modes restore in different
; places: full-frame mode has only the vblank block before rst_budget, while
; exact-window mode has the slack ahead of the strip, or the run behind it when
; the strip is at the top (see WINDOW RMW BUDGET).  What they share is that
; scan_rmw hands these addresses out FIRST, so fitting rmw_count*6 into the block
; that runs before the strip is what makes every one of them land.
; IN: .XY = cycles available to the restores they depend on
.proc rmw_check_room
@need=r0
@room=r2
	stxy @room
	lda #$00
	sta rmw_ok

	lda rmw_count		; need = rmw_count * 6
	asl
	sta @need
	lda rmw_count+1
	rol
	sta @need+1
	bcs @no			; more than 32767 of them -- not happening
	lda @need		; *3
	asl
	tax
	lda @need+1
	rol
	tay
	bcs @no
	txa
	clc
	adc @need
	sta @need
	tya
	adc @need+1
	sta @need+1
	bcs @no

	lda @room+1
	cmp @need+1
	bcc @no
	bne @yes
	lda @room
	cmp @need
	bcc @no
@yes:	inc rmw_ok
@no:	rts
.endproc


;*******************************************************************************
; EMIT RESTORES
; Emits the vblank restore block: "lda #prev / sta abs" per address, 6 cycles
; and 5 bytes each, until gen_cyc would pass rst_budget, the output window runs
; low, or nothing is left to restore.  Whatever does not fit simply keeps the
; value it had -- the same behaviour as before any of this existed, so a short
; budget degrades rather than corrupts.
; Runs before any write is scheduled, so .A is free and the register model is
; reset immediately afterwards.
.proc emit_restores
@lp:	jsr poll_restore	; RESTORE -> stop filling; the caller unwinds
	bne @quit
	jsr emit_one_restore
	bcc @all
	inc restored_count
	bne @lp
	inc restored_count+1
	jmp @lp
@all:	rts			; nothing left: every wrap write emitted

@quit:	rts			; RESTORE: the block is half-built, but the replay
				; it belongs to is never going to run
.endproc

;*******************************************************************************
; EMIT ONE RESTORE
; Emits a single "lda #prev / sta abs" (6 cycles, 5 bytes) and advances gen_cyc.
; OUT: .C set if one was emitted; clear if nothing is pending or the output
;      window is too close to full to keep going.
.proc emit_one_restore
	lda gen_ptr+1		; leave room for the trailing rts
	cmp #>(GEN_END-$100)
	bcs @none

	jsr restore_next
	bcc @none

	lda #$a9		; lda #restore_val
	jsr gen_emit
	lda restore_val
	jsr gen_emit
	lda #$8d		; sta restore_addr
	jsr gen_emit
	lda restore_addr
	jsr gen_emit
	lda restore_addr+1
	jsr gen_emit

	lda #6
	jsr gen_advance
	sec
	rts
@none:	clc
	rts
.endproc



;*******************************************************************************
; GEN RESET
; Resets the replay code generator's output cursor and byte count.
.proc gen_reset
	lda #<GEN_WIN
	sta gen_ptr
	lda #>GEN_WIN
	sta gen_ptr+1
	lda #$00
	sta gen_full
	sta gen_size
	sta gen_size+1
	sta gen_cyc
	sta gen_cyc+1
	; generation starts in OUTPUT set A (field 1 / single-set frames)
	lda #OUTPUT_BANK
	sta gen_out1
	lda #OUTPUT_BANK2
	sta gen_out2
	lda #OUTPUT_BANK3
	sta gen_out3
	rts
.endproc

;*******************************************************************************
; GEN EMIT
; Appends the byte in .A to the generated replay code
.proc gen_emit
@i=r0
@save8=r2
@saveA=r3
@saveC=r4
@save2=r5
	pha
	lda gen_full
	bne @done		; output bank is full; drop

	; save curent bank configuration
	lda $9ff8
	sta @save8
	lda $9ffa
	sta @saveA
	lda $9ffc
	sta @saveC
	lda $9ff2
	sta @save2

	; swap in ouput banks
	lda gen_out1		; active OUTPUT set (A=field 1, B=field 2)
	sta $9ff8
	lda gen_out2
	sta $9ffa
	lda gen_out3
	sta $9ffc
	lda #$7f		; BLK1/2/3 all RAM r/w
	sta $9ff2

	lda gen_ptr
	sta @i
	lda gen_ptr+1
	sta @i+1
	ldy #$00
	pla			; restore byte to write
	pha
	sta (@i),y

	lda @save2
	sta $9ff2
	lda @saveC
	sta $9ffc
	lda @saveA
	sta $9ffa
	lda @save8
	sta $9ff8

	incw gen_ptr
	incw gen_size
	lda gen_ptr+1
	cmp #>GEN_END
	bcc @done
	lda #$01		; OOM
	sta gen_full
@done:	pla
	rts
.endproc

;*******************************************************************************
; LOOP COST
; Works out what one iteration of the delay loop emit_loop is about to emit will
; actually cost, which is NOT a constant.
;
; The loop's "bne" is a taken backward branch, so it costs 4 cycles instead of 3
; whenever it crosses a page.  With the loop based at b, "dex" sits at b+2, the
; branch operand at b+4, and the branch's next PC at b+5; the penalty applies
; when b+5 and the target b+2 are in different pages -- that is, when the low
; byte of b is 251, 252 or 253.  The final (not taken) branch is 2 cycles either
; way, so the constant term moves too:
;   normal    : 2 + 2*kk + 3*(kk-1) + 2 = 5*kk + 1
;   page cross: 2 + 2*kk + 4*(kk-1) + 2 = 6*kk
; Three bases in every 256 land on this.  Unaccounted, a 250-iteration loop then
; runs 250 cycles long -- a quarter of a frame of raster displacement appearing
; out of nowhere, purely from where the loop happened to be emitted.
; OUT: gen_loop_cost / gen_loop_bias / gen_loop_full / gen_loop_thresh
.proc loop_cost
	ldx gen_ptr
	cpx #251
	bcc @five
	cpx #254
	bcs @five

	lda #6				; branch crosses a page
	sta gen_loop_cost
	lda #0
	sta gen_loop_bias
	ldxy #1500			; 250*6 + 0
	stxy gen_loop_full
	ldxy #1506
	stxy gen_loop_thresh
	rts

@five:	lda #5
	sta gen_loop_cost
	lda #1
	sta gen_loop_bias
	ldxy #1251			; 250*5 + 1
	stxy gen_loop_full
	ldxy #1257
	stxy gen_loop_thresh
	rts
.endproc

;*******************************************************************************
; EMIT LOOP
; Emits a "LDX #.X : DEX : BNE *-1" delay loop (5 bytes), which runs for
; gen_loop_cost*.X + gen_loop_bias CPU cycles (.X iterations; or if .X=0, 256).
; loop_cost must have been called with gen_ptr at the loop's base.
; IN:
;   - .X: number of iterations (0=256) to delay
.proc emit_loop
	txa
	pha
	lda #$a2		; ldx #imm
	jsr gen_emit
	pla			; iteration count
	jsr gen_emit
	lda #$ca		; dex
	jsr gen_emit
	lda #$d0		; bne
	jsr gen_emit
	lda #$fd		; -3 -> back to dex
	jmp gen_emit
.endproc

;*******************************************************************************
; EMIT DELAY
; Emits instructions that consume exactly gen_delay_cycles CPU cycles.  gen_delay_cycles must be
; 0 or >= 2 (a lone 1 cycle is not representable).  Long delays use dex/bne
; loops (each 1251 cycles for 250 iterations); the tail uses bit $00 (3) and
; nop (2).  Loops clobber X in the generated code.
.proc emit_delay
@bulk:	; loop cost depends on the emission address, and every loop emitted here
	; shifts it by 5 bytes -- so re-derive it for each one
	jsr loop_cost

	; while gen_delay_cycles >= gen_loop_thresh, burn a full 250-iteration loop
	lda gen_delay_cycles+1
	cmp gen_loop_thresh+1
	bcc @small
	bne @burn
	lda gen_delay_cycles
	cmp gen_loop_thresh
	bcc @small
@burn:	ldx #250
	jsr emit_loop		; gen_loop_full cycles
	lda gen_delay_cycles
	sec
	sbc gen_loop_full
	sta gen_delay_cycles
	lda gen_delay_cycles+1
	sbc gen_loop_full+1
	sta gen_delay_cycles+1
	jmp @bulk

@small:	; below the threshold.  If < 6, no loop fits; go straight to the tail.
	lda gen_delay_cycles+1
	bne @sized		; >= 256 -> a loop fits
	lda gen_delay_cycles
	cmp #6
	bcc @tail

@sized:	; one loop of kk = floor((gen_delay_cycles-bias)/cost) iterations, capped
	; at 250 (the loop runs cost*kk + bias cycles)
	lda gen_delay_cycles		; work = gen_delay_cycles - bias
	sec
	sbc gen_loop_bias
	sta gen_div_scratch
	lda gen_delay_cycles+1
	sbc #0
	sta gen_div_scratch+1
	lda #0
	sta gen_loop_count		; kk
@div:	lda gen_loop_count
	cmp #250
	bcs @haskk		; cap at 250
	lda gen_div_scratch+1
	bne @deci		; work >= 256 -> >= cost
	lda gen_div_scratch
	cmp gen_loop_cost
	bcc @haskk
@deci:	lda gen_div_scratch
	sec
	sbc gen_loop_cost
	sta gen_div_scratch
	bcs :+
	dec gen_div_scratch+1
:	inc gen_loop_count
	jmp @div

@haskk:	; leftover = gen_delay_cycles - (cost*kk + bias) = gen_div_scratch.
	; leftover must be 0 or >= 2 -- a lone 1 cycle is not representable -- so if
	; it is exactly 1, drop one loop iteration and give those cycles to the
	; tail: +cost, plus the bias when that empties the loop entirely.
	; (kk >= 1 here: @small sends everything below 6 to the tail, so work is
	; always at least cost.)
	lda gen_div_scratch
	cmp #1
	bne @emit		; leftover != 1 -> fine
	dec gen_loop_count		; back off one loop iteration
	lda gen_loop_cost
	clc
	adc #1			; leftover = 1 + cost
	ldx gen_loop_count
	bne :+
	clc
	adc gen_loop_bias	; loop gone entirely -> its bias comes back too
:	sta gen_div_scratch
@emit:	lda gen_loop_count
	beq @leftover		; kk==0 -> skip the loop
	tax
	jsr emit_loop

@leftover:
	; remaining cycles = gen_div_scratch -> tail
	lda gen_div_scratch
	sta gen_delay_cycles
	lda #0
	sta gen_delay_cycles+1

@tail:	; pad gen_delay_cycles (0, or 2..6) with bit $00 (3) for parity + nop (2)
	lda gen_delay_cycles
	and #1
	beq @nops
	lda #$24		; bit $00 (3 cycles)
	jsr gen_emit
	lda #$00
	jsr gen_emit
	lda gen_delay_cycles
	sec
	sbc #3
	sta gen_delay_cycles
@nops:	lda gen_delay_cycles
	cmp #2
	bcc @done
	lda #$ea		; nop (2 cycles)
	jsr gen_emit
	lda gen_delay_cycles
	sec
	sbc #2
	sta gen_delay_cycles
	jmp @nops
@done:	rts
.endproc


;*******************************************************************************
; GEN ADVANCE
; Adds .A cycles to the emitted-code cycle position gen_cyc.  Preserves .X/.Y.
.proc gen_advance
	clc
	adc gen_cyc
	sta gen_cyc
	bcc :+
	inc gen_cyc+1
:	rts
.endproc


;*******************************************************************************
; NEXT RECORD
; next_record_raw, but stopping at gen_frame_end so the frame's tail headroom is
; never scheduled.  Records are monotonic in tc, so the first record at/after
; gen_frame_end means no later record is wanted either.
; OUT: .C set if rec_* holds a schedulable record.
.proc next_record
	jsr next_record_raw
	bcc @none
	lda rec_tc+1
	cmp gen_frame_end+1
	bcc @ok
	bne @none
	lda rec_tc
	cmp gen_frame_end
	bcs @none
@ok:	sec
	rts
@none:	clc
	rts
.endproc

;*******************************************************************************
; REG LDOP /
; Maps a register index ([0,1,2] corresponds to [A,X,Y]) to the lda imm. opcode
; for it.
.proc reg_ldop
	cmp #1
	beq @x
	bcs @y			; 2 -> Y
	lda #$a9		; 0 -> lda #imm
	rts
@x:	lda #$a2		; ldx #imm
	rts
@y:	lda #$a0		; ldy #imm
	rts
.endproc

;*******************************************************************************
; REC REGOF
; The register the current record's store took its value from, decoded from
; rec_op.
;
; All seven STA forms ($81/$85/$8d/$91/$95/$99/$9d) share bits 7,6,5 = 100 and
; bits 1,0 = 01, so one masked compare covers them; the only other opcode the
; mask admits is $89, which is not a legal instruction.  STX ($86/$8e/$96) and
; STY ($84/$8c/$94) fall out the same way against $e7, their strays being the
; illegal $9e/$9c.  The simulator rejects illegal opcodes before the store ever
; runs, so the masks are exact.  Anything else is a read-modify-write.
; OUT: .A = REG_A / REG_X / REG_Y / REG_ANY
.proc rec_regof
	lda rec_op
	and #$e3
	cmp #$81
	beq @a
	lda rec_op
	and #$e7
	cmp #$86
	beq @x
	cmp #$84
	beq @y
	lda #REG_ANY
	rts
@a:	lda #REG_A
	rts
@x:	lda #REG_X
	rts
@y:	lda #REG_Y
	rts
.endproc

;*******************************************************************************
; REG LDOP ZP
; Maps a register index ([0,1,2] corresponds to [A,X,Y]) to the ZEROPAGE load
; opcode for it -- 3 cycles rather than the immediate form's 2.  The operand is
; the value itself: page 0 holds the identity table during replay, so ZP[v] is
; v.  This is the only 3-cycle load of an arbitrary byte the 6502 has, and it is
; what makes odd-length gaps representable.
.proc reg_ldop_zp
	cmp #1
	beq @x
	bcs @y			; 2 -> Y
	lda #$a5		; 0 -> lda zp
	rts
@x:	lda #$a6		; ldx zp
	rts
@y:	lda #$a4		; ldy zp
	rts
.endproc

;*******************************************************************************
; REG STOP
; Maps a register index ([0,1,2] corresponds to [A,X,Y]) to the store abs opcode
; for it.
.proc reg_stop
	cmp #1
	beq @x
	bcs @y			; 2 -> Y
	lda #$8d		; 0 -> sta abs
	rts
@x:	lda #$8e		; stx abs
	rts
@y:	lda #$8c		; sty abs
	rts
.endproc

;*******************************************************************************
; REGISTER-INDEX BIT TABLES
; bit i (=1<<i) and its complement, indexed by register (0=A,1=X,2=Y).  Used by
; the preload planner's free-register bitmap.
regbit_tab:    .byte $01, $02, $04
regbit_inv_tab: .byte $fe, $fd, $fb

;*******************************************************************************
; REGMODEL RESET
; Marks every modeled register (A/X/Y) UNKNOWN.  Called at generate start (the
; base-state prologue clobbers A, so nothing is trusted afterwards).
.proc regmodel_reset
	lda #$00
	sta regmodel_known
	sta regmodel_known+1
	sta regmodel_known+2
	rts
.endproc

;*******************************************************************************
; SEED INDEX REG
; Emits "ldy #$00 : ldx #$00" and models them.  A 5-cycle gap can only be hit by
; an indexed store, and a 7-cycle read-modify-write by the ,x form; both need a
; known index register to bias the operand against.  With nothing modeled yet
; the frame's opening writes have none and come up a cycle short.  Four cycles
; of vblank buys both for the whole frame.
;
; Costs nothing in mirroring terms: neither register holds a captured value
; until the first store that names it, and that store overwrites the seed like
; any other.
.proc seed_index_reg
	lda #$a0		; ldy #$00
	jsr gen_emit
	lda #$00
	jsr gen_emit

	lda #$a2		; ldx #$00 -- the re-emitted read-modify-writes
	jsr gen_emit		; want an index register too
	lda #$00
	jsr gen_emit

	jsr seed_index_model
	lda #$04
	jmp gen_advance
.endproc

;*******************************************************************************
; SEED INDEX MODEL
; Says .X/.Y hold $00 WITHOUT emitting the loads.
;
; Full-frame mode emits the setup block at the tail of the pass (see TAIL SETUP),
; so by the time cycle 0 comes round the registers already hold what the previous
; pass's tail left in them.  The model has to say so from the start of generation,
; or the frame's opening writes lose the indexed 5c store and the 7c ,x
; read-modify-write for want of an index the generated code demonstrably has.
.proc seed_index_model
	lda #$00
	sta regmodel_val+1
	sta regmodel_val+2
	lda #$01
	sta regmodel_known+1
	sta regmodel_known+2
	rts
.endproc

;*******************************************************************************
; SEED PEND MODEL
; Full-frame register seed.  Decides what the tail block will put in A/X/Y and
; models it, without emitting anything -- seed_pend_regs emits the matching loads
; at the end of the pass, which is the same instant as cycle 0 of the next one.
;
; The values come from the pending-use slots, so each register is seeded with the
; value its FIRST store in the frame needs.  That is what closes the frame-start
; @coldtight: the frame's opening writes used to be cold by construction, because
; regmodel_reset had just marked everything unknown, and a first write inside the
; first 6 cycles had nowhere to get its value from.  Seeding $00 (which is all
; this used to do, for the index registers) made them KNOWN but almost never
; USEFUL; seeding the value that is actually wanted makes the first store of each
; register a bare store.
;
; A register nothing wants is still seeded to $00 if it is .X or .Y, so the
; indexed 5c store and the 7c ,x read-modify-write keep an index to bias against.
; .A has no such secondary use, so it is left cold rather than spending 2 cycles
; of the reservation on a value no store will read.
;
; MUST agree with seed_pend_regs exactly -- the model is a promise about code
; that has not been emitted yet.  Both read seed_val/seed_have, which is captured
; here and never touched again; pend_val itself cannot be used at emission time
; because scheduling advances the slots as it consumes records.
.proc seed_pend_model
	lda #$00
	sta seed_i

@l:	ldx seed_i
	lda pend_valid,x
	beq @none

	lda pend_val,x			; the value its first store needs
	sta seed_val,x
	sta regmodel_val,x
	lda #$01
	sta seed_have,x
	sta regmodel_known,x
	bne @next			; (always)

@none:	lda #$00			; nothing wants it this frame
	sta seed_val,x
	sta regmodel_val,x
	cpx #REG_A
	beq @cold			; .A: not worth 2 cycles
	lda #$01
	sta seed_have,x
	sta regmodel_known,x
	bne @next			; (always)
@cold:	sta seed_have,x			; .A = 0 in both -> unseeded, unknown
	sta regmodel_known,x

@next:	inc seed_i
	lda seed_i
	cmp #$03
	bcc @l
	rts
.endproc

;*******************************************************************************
; SEED PEND REGS
; Emits the loads seed_pend_model promised.  Goes LAST in the tail block: the
; restores and the seed prologue ahead of it both clobber .A, so the registers
; have to be established after them to still hold this when the loop wraps.
.proc seed_pend_regs
	lda #$00
	sta seed_i

@l:	ldx seed_i
	lda seed_have,x
	beq @next

	txa
	jsr reg_ldop			; ld? #imm for that register
	jsr gen_emit
	ldx seed_i
	lda seed_val,x
	jsr gen_emit
	lda #$02
	jsr gen_advance

@next:	inc seed_i
	lda seed_i
	cmp #$03
	bcc @l
	rts
.endproc

;*******************************************************************************
; SETUP INDEX STORE
; Sets up a 5-cycle indexed store of the accumulator (the value must already be
; in A).  Picks a known non-A register to index off (X via "sta abs,x", else Y
; via "sta abs,y"), writes the opcode to sched_op, and biases rec_addr by the index
; register's value so the store lands at the original address (operand =
; addr - index-value; the store does not disturb the index register).
; OUT: .C set if an index register was available (sched_op / rec_addr set); clear
;      if neither X nor Y is known.
.proc setup_index_store
	lda regmodel_known+1
	beq @tryy
	lda #$9d		; sta abs,x
	sta sched_op
	lda regmodel_val+1
	jmp @bias
@tryy:	lda regmodel_known+2
	beq @none
	lda #$99		; sta abs,y
	sta sched_op
	lda regmodel_val+2
@bias:	sta sched_index_bias		; operand = addr - index value
	lda rec_addr
	sec
	sbc sched_index_bias
	sta rec_addr
	lda rec_addr+1
	sbc #$00
	sta rec_addr+1
	sec
	rts
@none:	clc
	rts
.endproc

;*******************************************************************************
; PLAN PRELOADS
; Queues the loads this gap's slack should be spent on, into preload_reg /
; preload_val (preload_count entries) for emit_gap_to to emit.
;
; There is no scan here any more: each register's next use is already known from
; its own cursor (see PENDING-USE SLOTS), so this only has to decide the ORDER
; and how many fit.  Order is by deadline, nearest store first -- see pend_pick
; for why anything else can strand a register that had cycles available to it.
; The current write's own register is excluded; it loads on demand.
.proc plan_preloads
	lda #$00
	sta preload_count
	sta preload_zp
	sta preload_zp+1
	sta preload_zp+2

	jsr pend_advance	; retire slots whose store is not ahead of us

	lda #$07		; A, X and Y
	sta preload_free
	lda sched_reserved
	cmp #$ff
	beq @loop
	tax
	lda preload_free
	and regbit_inv_tab,x
	sta preload_free

@loop:	lda preload_free
	beq @done

	; room for another 2-cycle load?  2*(preload_count+1) <= delta (gap_bulk)
	lda gap_bulk+1
	bne @room		; delta >= 256 -> plenty
	lda preload_count
	clc
	adc #1
	asl			; preload_count <= 2 here, so no overflow
	cmp gap_bulk
	beq @room
	bcs @done
@room:
	jsr pend_pick		; .X = soonest-needed register
	bcc @done

	ldy preload_count
	txa
	sta preload_reg,y
	lda pend_val,x
	sta preload_val,y
	inc preload_count
	lda preload_free	; claimed
	and regbit_inv_tab,x
	sta preload_free
	jmp @loop
@done:	rts
.endproc

;*******************************************************************************
; PEND RESET
; Points every register's scan at the head of the chain and finds its first use.
.proc pend_reset
	ldx #$02
@l:	lda #$00
	sta pend_valid,x
	sta pend_idx,x
	lda #CAPTURE_BANK		; gen_chain[0]
	sta pend_bank,x
	txa
	asl
	tay
	lda #<CAPTURE_WIN
	sta pend_ptr,y
	lda #>CAPTURE_WIN
	sta pend_ptr+1,y
	dex
	bpl @l

	ldx #$02
@f:	jsr pend_refill
	dex
	bpl @f
	rts
.endproc

;*******************************************************************************
; PEND REFILL
; Walks register .X's own cursor forward to the next record that names it, and
; caches that record's value and cycle.  Marks the slot invalid once the frame
; runs out.  rec_* and the shared read cursor are left exactly as found.
; IN: .X = register
.proc pend_refill
	stx pend_chosen
	ldx #CAPTURE_RECORD_SZ-1	; hold the caller's record
@sv:	lda rec_addr,x
	sta pend_saved_rec,x
	dex
	bpl @sv
	lda gen_read_ptr
	sta preload_saved_rd
	lda gen_read_ptr+1
	sta preload_saved_rd+1
	lda gen_read_idx
	sta preload_saved_idx
	lda gen_src_bank
	sta preload_saved_bank

	; swap in this register's cursor
	ldx pend_chosen
	txa
	asl
	tay
	lda pend_ptr,y
	sta gen_read_ptr
	lda pend_ptr+1,y
	sta gen_read_ptr+1
	lda pend_idx,x
	sta gen_read_idx
	lda pend_bank,x
	sta gen_src_bank

	lda #$00
	sta pend_valid,x

@scan:	jsr next_record			; C=0 -> no more records this frame
	bcc @out
	jsr rec_regof
	cmp pend_chosen
	bne @scan			; not this register's

	ldx pend_chosen			; found it
	lda rec_val
	sta pend_val,x
	txa
	asl
	tay
	lda rec_tc
	sta pend_tc,y
	lda rec_tc+1
	sta pend_tc+1,y
	ldx pend_chosen
	lda #$01
	sta pend_valid,x

@out:	; park the cursor where the walk stopped, so the next refill resumes here
	ldx pend_chosen
	txa
	asl
	tay
	lda gen_read_ptr
	sta pend_ptr,y
	lda gen_read_ptr+1
	sta pend_ptr+1,y
	lda gen_read_idx
	sta pend_idx,x
	lda gen_src_bank
	sta pend_bank,x

	lda preload_saved_rd		; give the caller back its cursor
	sta gen_read_ptr
	lda preload_saved_rd+1
	sta gen_read_ptr+1
	lda preload_saved_idx
	sta gen_read_idx
	lda preload_saved_bank
	sta gen_src_bank
	ldx #CAPTURE_RECORD_SZ-1
@rs:	lda pend_saved_rec,x
	sta rec_addr,x
	dex
	bpl @rs
	ldx pend_chosen
	rts
.endproc

;*******************************************************************************
; PEND ADVANCE
; Retires any slot whose store is not in the future any more.  Doing it by cycle
; rather than by matching the record covers the ones window mode drops: those
; are never emitted, so nothing would otherwise consume them and the slot would
; stall for the rest of the frame.
.proc pend_advance
	ldx #$02
@r:	lda pend_valid,x
	beq @nx
	txa
	asl
	tay
	lda pend_tc+1,y			; pend_tc <= rec_tc -> retire it
	cmp rec_tc+1
	bcc @ret
	bne @nx
	lda pend_tc,y
	cmp rec_tc
	beq @ret
	bcs @nx
@ret:	jsr pend_refill			; preserves .X
	jmp @r				; the next one may be stale too
@nx:	dex
	bpl @r
	rts
.endproc

;*******************************************************************************
; PEND PICK
; Chooses which register to preload next: the one whose store comes SOONEST.
;
; Deadline order is what makes the placement complete rather than merely better.
; A gap holding two spare cycles with two registers pending can only serve one;
; spend it on the register whose store is far away and the near one is stranded,
; because the far one's own spare cycles lie in a later gap that the near one
; can no longer reach.  Nearest-first never does that -- with unit-cost loads,
; release times and deadlines, earliest-deadline-first finds a legal placement
; whenever one exists, and one always does (see the FRAME-SCOPED REGISTER
; SCHEDULER notes on why the cycles are guaranteed to be there).
;
; Registers that need nothing are dropped from preload_free as they are found,
; so repeated calls make progress.
; OUT: .C set and .X = register to preload; .C clear if there is nothing to do.
.proc pend_pick
	ldx #$00
@f:	lda preload_free
	and regbit_tab,x
	beq @fn				; already taken or dropped
	lda pend_valid,x
	beq @drop			; nothing known to be coming
	lda regmodel_known,x
	beq @fn				; unknown -> it does need loading
	lda regmodel_val,x
	cmp pend_val,x
	bne @fn				; wrong value -> it does need loading
@drop:	lda preload_free		; already holds it: nothing to do
	and regbit_inv_tab,x
	sta preload_free
@fn:	inx
	cpx #3
	bcc @f

	lda #$ff
	sta pend_best
	ldx #$00
@b:	lda preload_free
	and regbit_tab,x
	beq @bn
	ldy pend_best
	cpy #$ff
	beq @take			; first candidate
	jsr pend_sooner			; is .X's deadline before pend_best's?
	bcc @bn
@take:	stx pend_best
@bn:	inx
	cpx #3
	bcc @b

	ldx pend_best
	cpx #$ff
	beq @none
	sec
	rts
@none:	clc
	rts
.endproc

;*******************************************************************************
; PEND SOONER
; .C set if register .X's deadline is earlier than register pend_best's.
; Preserves .X.
.proc pend_sooner
	lda pend_best			; copy the incumbent's deadline out; both
	asl				; indices are dynamic, so one at a time
	tay
	lda pend_tc,y
	sta pend_cmp
	lda pend_tc+1,y
	sta pend_cmp+1

	txa
	asl
	tay
	lda pend_tc+1,y
	cmp pend_cmp+1
	bcc @yes
	bne @no
	lda pend_tc,y
	cmp pend_cmp
	bcc @yes
@no:	clc
	rts
@yes:	sec
	rts
.endproc

;*******************************************************************************
; SCAN BEGIN / SCAN END
; Bracket a forward walk of upcoming records: save the current record and read
; cursor (the walk may hop chain banks) and put them back afterwards.
.proc scan_begin
	ldx #CAPTURE_RECORD_SZ-1
@sv:	lda rec_addr,x
	sta sched_saved_rec,x
	dex
	bpl @sv
	lda gen_read_ptr
	sta preload_saved_rd
	lda gen_read_ptr+1
	sta preload_saved_rd+1
	lda gen_read_idx
	sta preload_saved_idx
	lda gen_src_bank
	sta preload_saved_bank
	rts
.endproc

.proc scan_end
	lda preload_saved_rd
	sta gen_read_ptr
	lda preload_saved_rd+1
	sta gen_read_ptr+1
	lda preload_saved_idx
	sta gen_read_idx
	lda preload_saved_bank
	sta gen_src_bank
	ldx #CAPTURE_RECORD_SZ-1
@rs:	lda sched_saved_rec,x
	sta rec_addr,x
	dex
	bpl @rs
	rts
.endproc

;*******************************************************************************
; RMW PICK REG
; Finds a register a read-modify-write can borrow.  An RMW takes its value from
; memory and leaves A/X/Y alone, so whichever we take we clobber, and the cost
; is paid by the next store that wanted it: that store turns cold and has to be
; reloaded between our store and its own.
;
; So the test is whether the interval [rec_tc, next use of that register] has
; room for a 2-cycle reload on top of the stores that already have to fit in it.
; Each of those costs at least 4, so the interval needs 4*records + 2.  Registers
; whose next use is far away, or which have no next use at all, are free.
;
; The count is deliberately optimistic -- some of those intervening stores need
; loads of their own, which this does not subtract.  That errs towards borrowing,
; and borrowing is the safe direction: getting it wrong costs a bounded, tracked
; cycle of drift, where re-emitting instead leans on the address having been
; restored, and a missed restore is a wrong value that compounds every pass.
;
; OUT: .C set and .X = a register that can be spared; .C clear if none can.
RMW_FAR = 32			; beyond this the next use cannot be crowded
.proc rmw_pick_reg
	ldy #$00
@try:	ldx any_pref,y
	sty rmw_pref
	jsr rmw_room
	bcs @found
	ldy rmw_pref
	iny
	cpy #$03
	bcc @try
	clc
	rts
@found:	rts
.endproc

;*******************************************************************************
; RMW GAP OF
; rmw_gap = cycles from the current record to register .X's next use.  A register
; with no pending use answers $ffff (nothing can crowd it); one whose next use is
; somehow already behind us answers 0 (take it last).  Preserves .X.
.proc rmw_gap_of
	stx rmwt_reg
	lda pend_valid,x
	beq @free

	txa
	asl
	tay
	lda pend_tc,y
	sec
	sbc rec_tc
	sta rmw_gap
	lda pend_tc+1,y
	sbc rec_tc+1
	sta rmw_gap+1
	bcc @behind
	ldx rmwt_reg
	rts

@free:	lda #$ff
	sta rmw_gap
	sta rmw_gap+1
	ldx rmwt_reg
	rts

@behind:
	lda #$00
	sta rmw_gap
	sta rmw_gap+1
	ldx rmwt_reg
	rts
.endproc

;*******************************************************************************
; RMW PICK WORST
; Every register is wanted too soon to spare -- rmw_pick_reg just said so -- and
; one has to be taken anyway.  Take the one whose next use is FURTHEST away: the
; store that loses its value then has the most room to reload before it needs it,
; so the drift this costs has the best chance of being absorbed instead of
; propagating into the run behind it.
;
; .X is the incumbent, which is what this used to take unconditionally, and it
; stays the answer whenever nothing strictly beats it.  FIELD TRANSITION also
; uses this helper to choose which register its fixed-cost bank-flip stub should
; clobber; there the same furthest-next-use rule applies without an RMW.
; OUT: .X = the register to borrow
.proc rmw_pick_worst
	ldx #REG_X
	jsr rmw_gap_of
	lda rmw_gap
	sta rmwt_gap
	lda rmw_gap+1
	sta rmwt_gap+1
	lda #REG_X
	sta rmwt_best

	ldx #REG_A
	jsr @cmp
	ldx #REG_Y
	jsr @cmp
	ldx rmwt_best
	rts

@cmp:	txa
	pha
	jsr rmw_gap_of
	lda rmw_gap+1		; strictly greater than the incumbent?
	cmp rmwt_gap+1
	bcc @no
	bne @yes
	lda rmw_gap
	cmp rmwt_gap
	bcc @no
	beq @no
@yes:	pla
	sta rmwt_best
	lda rmw_gap
	sta rmwt_gap
	lda rmw_gap+1
	sta rmwt_gap+1
	rts
@no:	pla
	rts
.endproc

;*******************************************************************************
; RMW ROOM
; .C set if register .X can be reloaded between the current record's store and
; the next store that needs it.  Preserves .X.
.proc rmw_room
	stx rmw_reg		; every exit restores .X from here
	lda pend_valid,x
	beq @yes		; never wanted again -- free to take

	txa			; room = pend_tc[X] - rec_tc
	asl
	tay
	lda pend_tc,y
	sec
	sbc rec_tc
	sta rmw_gap
	lda pend_tc+1,y
	sbc rec_tc+1
	sta rmw_gap+1
	bcc @no			; already behind us

	lda rmw_gap+1
	bne @yes		; >= 256 cycles away
	lda rmw_gap
	cmp #RMW_FAR
	bcs @yes

	; close enough to be crowded: count the stores that have to fit first
	jsr rmw_count_stores	; .A = records in (rec_tc, pend_tc[X]]
	asl			; 4 * count
	asl
	clc
	adc #2			; ...plus the reload itself
	bcs @no			; overflowed a byte -> no room
	cmp rmw_gap
	beq @yes
	bcs @no
@yes:	ldx rmw_reg
	sec
	rts
@no:	ldx rmw_reg
	clc
	rts
.endproc

;*******************************************************************************
; RMW COUNT STORES
; Records with a cycle in (rec_tc, rmw_gap + rec_tc].  The interval is under
; RMW_FAR cycles and every store spans at least 4, so this reads at most eight
; records and cannot become quadratic over the frame.
; OUT: .A = count
.proc rmw_count_stores
	lda rec_tc		; deadline = rec_tc + rmw_gap
	clc
	adc rmw_gap
	sta rmw_deadline
	lda rec_tc+1
	adc rmw_gap+1
	sta rmw_deadline+1

	lda #$00
	sta rmw_n
	jsr scan_begin
@l:	jsr next_record
	bcc @done
	lda rec_tc+1		; past the deadline?
	cmp rmw_deadline+1
	bcc @in
	bne @done
	lda rec_tc
	cmp rmw_deadline
	beq @in
	bcs @done
@in:	inc rmw_n
	lda rmw_n
	cmp #16			; the interval cannot really hold this many
	bcc @l
@done:	jsr scan_end
	lda rmw_n
	rts
.endproc

any_pref: .byte REG_X, REG_Y, REG_A

;*******************************************************************************
; EMIT GAP TO
; Advances the emitted-code cycle position (gen_cyc) to gap_target, preserving the
; register model.  The bulk of the gap is a dex/bne delay (register-safe only
; below 6 cycles) and the last cycles preload upcoming cold values into free
; registers (plan_preloads) so they are hot before the run that needs them.
; sched_reserved (unless $ff) is excluded from the free pool.  Does nothing if already
; at or past gap_target.
.proc emit_gap_to
	; delta = gap_target - gen_cyc; <= 0 -> at/behind the target, nothing to do
	lda gap_target
	sec
	sbc gen_cyc
	sta gap_bulk
	lda gap_target+1
	sbc gen_cyc+1
	sta gap_bulk+1
	bcc @ret		; target < gen_cyc
	lda gap_bulk
	ora gap_bulk+1
	bne @plan
	rts			; delta == 0
@ret:	rts

@plan:	lda #$00
	sta gap_dropped

	; gap_bulk currently holds the delta (used by plan_preloads for room checks).
	; If the bulk delay might clobber X, free it here so the planner can
	; repreload its value for the upcoming run.
	;
	; The test is whether a loop is POSSIBLE, not whether it is certain.  It
	; used to be 12 -- the point past which even three preloads (6 cycles) still
	; leave a >= 6 cycle dex loop -- which left a hole between 6 and 11: fewer
	; preloads than the maximum get queued, the bulk comes out >= 6 anyway, and
	; the loop goes out with X never having been offered to the planner.  @xlost
	; below then correctly marks X unknown, but by then the cycles are spent, so
	; a store wanting X a few cycles later lands cold with no room -- straight
	; into @coldtight.  Freeing at 6 costs at worst one needless 2-cycle reload
	; when no loop materialises, and X physically still holds its value in that
	; case, so the pessimism is only ever a wasted load and never a wrong one.
	lda gap_bulk+1
	bne @freex
	lda gap_bulk
	cmp #6
	bcc @doplan
@freex:	lda #$00
	sta regmodel_known+1
@doplan:
	jsr plan_preloads

	; bulk = delta - 2*preload_count
	lda preload_count
	asl
	sta gap_preload_cycles
	lda gap_bulk
	sec
	sbc gap_preload_cycles
	sta gap_bulk
	lda gap_bulk+1
	sbc #$00
	sta gap_bulk+1

	; A bulk of exactly 1 cycle has no instruction that fills it.  If anything
	; is being preloaded, stretch one of those loads from the 2-cycle
	; immediate to the 3-cycle identity-zeropage form and the odd cycle is
	; gone -- no drift, and nothing dropped.  With no preload to stretch, the
	; cycle is charged as a shortfall further down.
	lda gap_bulk+1
	bne @bulkok
	lda gap_bulk
	cmp #1
	bne @bulkok
	lda preload_count
	beq @bulkok
	lda #$01
	sta preload_zp		; the first queued load absorbs it
	lda #$00
	sta gap_bulk
@bulkok:
	; emit the bulk delay (0, or >= 2 cycles)
	lda gap_bulk
	ora gap_bulk+1
	beq @loads
	lda gap_bulk+1
	bne @big
	lda gap_bulk
	cmp #2
	bcs @big

	; bulk == 1: a lone cycle has no instruction, so this gap lands one cycle
	; short.  RECORD it -- gen_cyc is the generator's only notion of where the
	; emitted code is, so silently claiming gap_target would make every later
	; slack a cycle optimistic and the shortfall would compound for the rest of
	; the frame.  Charged below, the next write with >= 2 cycles of pad absorbs
	; it and the code realigns.
	lda #$01
	sta gap_dropped
	incw __cap_late_neg	; this call will land one cycle early; make zero honest
	bne @loads		; (always)

@big:	lda gap_bulk
	sta gen_delay_cycles
	lda gap_bulk+1
	sta gen_delay_cycles+1
	jsr emit_delay
	; a delay >= 6 cycles uses a dex loop that clobbers X in the generated code
	lda gap_bulk+1
	bne @xlost
	lda gap_bulk
	cmp #6
	bcc @loads
@xlost:	lda #$00
	sta regmodel_known+1

@loads:	; bulk done -> gen_cyc = gap_target - (what the loads will cost) - 1 if a
	; cycle was dropped above; the loads bring it back up to where the code
	; really is
	lda preload_count
	asl
	clc
	adc gap_dropped
	clc
	adc preload_zp		; a stretched load costs one cycle more
	sta gap_preload_cycles
	lda gap_target
	sec
	sbc gap_preload_cycles
	sta gen_cyc
	lda gap_target+1
	sbc #$00
	sta gen_cyc+1

	; emit each planned preload: ld? #value (2 cycles), update the model.
	; reg_ldop/gen_emit/gen_advance all preserve X, so it stays the loop index.
	ldx #$00
@pl:	cpx preload_count
	bcs @done
	lda preload_zp,x
	bne @zp
	lda preload_reg,x
	jsr reg_ldop		; ld? #value, 2 cycles (preserves X)
	ldy #$02
	bne @op			; (always)
@zp:	lda preload_reg,x
	jsr reg_ldop_zp		; ld? <value>, 3 cycles -- absorbs an odd cycle
	ldy #$03
@op:	sty gap_preload_cycles	; reuse: the bulk arithmetic is finished with it
	jsr gen_emit
	lda preload_val,x
	jsr gen_emit
	ldy preload_reg,x		; model[reg] = value
	lda preload_val,x
	sta regmodel_val,y
	lda #$01
	sta regmodel_known,y
	lda gap_preload_cycles
	jsr gen_advance
	inx
	bne @pl			; preload_count <= 3, never wraps
@done:	rts
.endproc

;*******************************************************************************
; SCHEDULE WRITE
; Schedules the current record (rec_addr/rec_val/rec_tc) so its store completes
; at exactly rec_tc: caches the value in a register (loading it during earlier
; slack if cold), fills the gap up to the write (preloading upcoming values),
; then emits the store in the exact 4- or 5-cycle form.
.proc schedule_write
	lda #$00		; cleared here, not per path: @demote reaches the
	sta sched_zpload	; cold code without going through @coldreg
	lda rec_addr		; indexed stores bias rec_addr in place; retain the
	sta sched_orig_addr	; real address for scheduled_mark and RMW safety
	lda rec_addr+1
	sta sched_orig_addr+1

	; slack = rec_tc - gen_cyc (signed).  A borrow out of the high byte means
	; the cycle this write had to land on is already behind us, so it cannot
	; be placed however the value is handled.  Count that immediately; the same
	; general timing counter is also charged below when the deadline is still
	; ahead but too close, or a lone padding cycle cannot be represented.
	lda rec_tc
	sec
	sbc gen_cyc
	sta sched_slack
	lda rec_tc+1
	sbc gen_cyc+1
	sta sched_slack+1
	bcs :+
	incw __cap_late_neg
:
	; REGISTER MIRRORING
	; Which register carries the value is not ours to choose: the program's
	; own store named one, and using the same one is what makes the tight
	; cases reachable at all.  A gap-4 store had no instruction in front of
	; it, so its value was already live in that register on the real machine
	; -- which means the real machine's load of it sits in some earlier gap,
	; and that gap is exactly the room our preload needs.  Mirroring turns
	; that into a structural guarantee.
	;
	; Storing from whatever register happens to hold the value would forfeit
	; it: A=w,X=v with "stx/sta/stx" four cycles apart works under mirroring,
	; but if the first store opportunistically used .A (because v was already
	; there) then the second overwrites .A with w and the third is left with v
	; nowhere and no cycles to reload it.
	jsr rec_regof
	cmp #REG_ANY
	bne :+
	jmp @rmw		; nothing to mirror -- re-emit the instruction
:	tax
	stx sched_reg

	; hot only if THAT register holds it; a copy sitting in another register
	; is no use, since taking it would break the mirror
	lda regmodel_known,x
	beq @coldreg
	lda regmodel_val,x
	cmp rec_val
	beq @hot

@coldreg:
	lda #$01
	sta sched_cold
	lda sched_reg
	sta sched_reserved
	jmp @cold

@hot:	lda #$00
	sta sched_cold
	lda sched_reg		; the current value has to survive the gap before
	sta sched_reserved	; its store.  pend_advance exposes this register's
				; NEXT value to the planner, so leaving it free here
				; would let that future load overwrite a hot current
				; value before it was stored.

	; a large gap uses a dex delay that clobbers X; if the value is hot in X
	; and the gap is large, reload it (into X) rather than trust the delay
	lda sched_reg
	cmp #1
	bne @hotdec
	lda sched_slack+1
	bne @demote
	lda sched_slack
	cmp #10
	bcc @hotdec
@demote:
	lda #$01
	sta sched_cold
	lda #$01		; keep it in X; the cold path reloads after the delay
	sta sched_reserved
	jmp @cold

;-------------------------------------------------------------------------------
; HOT: value in sched_reg
@hotdec:
	lda sched_slack+1
	bne @hotbare		; slack >= 256 -> bare 4c + pad
	lda sched_slack
	cmp #4
	bcs :+
	incw __cap_late_neg	; deadline is ahead, but the 4c store cannot reach it
:	lda sched_slack
	cmp #5
	beq @hot5
	jmp @hotbare		; slack == 4, >= 6, or < 4 -> bare 4c

@hot5:	; slack == 5: an indexed 5c store (value in A) hits it exactly; else a bare
	; 4c store leaves an unrepresentable 1c pad (absorbed as <=1c drift)
	lda sched_reg
	bne @hotbare		; value not in A -> cannot index
	lda #$05
	sta sched_cost
	jsr setup_index_store	; sets sched_op, biases rec_addr
	bcc @hotbare		; no index register -> fall back to bare
	jsr set_entry
	jmp @go

@hotbare:
	lda #$04
	sta sched_cost
	lda sched_reg
	jsr reg_stop
	sta sched_op
	jsr set_entry
	jmp @go

;-------------------------------------------------------------------------------
; COLD: value to be loaded into sched_reg
@cold:	lda sched_slack+1
	bne @coldld		; slack >= 256 -> load 2 + store 4
	lda sched_slack
	cmp #7
	beq @cold7
	cmp #6
	bcc @coldtight		; slack < 6 -> cannot fit load+store

@coldld:; entry = rec_tc - 6 (load 2 + store 4)
	lda #$04
	sta sched_cost
	lda sched_reg
	jsr reg_stop
	sta sched_op
	lda rec_tc
	sec
	sbc #6
	sta sched_entry
	lda rec_tc+1
	sbc #$00
	sta sched_entry+1
	jmp @go

@cold7:	; slack == 7: an immediate load + store is 6 and leaves an unrepresentable
	; 1-cycle pad.  The identity zeropage makes the load 3 cycles instead --
	; "ld? <value>" -- so load + store is exactly 7.  This is the cadence of
	; "lda zp / sta abs" (3+4), the tightest run a real program can hold, and
	; unlike the indexed 5-cycle store it works from any register and needs no
	; index register, so it is always available.
	lda #$01
	sta sched_zpload
	lda #$04
	sta sched_cost
	lda sched_reg
	jsr reg_stop
	sta sched_op
	lda rec_tc
	sec
	sbc #7
	sta sched_entry
	lda rec_tc+1
	sbc #$00
	sta sched_entry+1
	jmp @go

@coldtight:
	; slack < 6: no room for load+store -- this value should have been
	; preloaded.  Mirroring says a real machine had it in that register
	; already, so the room to load it existed somewhere earlier; reaching here
	; means the preloader did not get it there.  Emit now and accept the
	; bounded, self-correcting drift, and count it -- the remaining ways in
	; (rmw_pick_worst borrowing a register with an imminent use, and register
	; pressure crowding a preload out) are rare enough that a nonzero total is
	; worth going and looking at.
	incw __cap_late_cold
	lda #$04
	sta sched_cost
	lda sched_reg
	jsr reg_stop
	sta sched_op
	lda gen_cyc
	sta sched_entry
	lda gen_cyc+1
	sta sched_entry+1

;-------------------------------------------------------------------------------
@go:	; A hot value living in .A has to survive the gap -- pre-set restores
	; clobber .A, so lock them out for this one gap.  A cold value is
	; reloaded after the gap anyway, and .X/.Y are never touched.
	lda #$00
	lda sched_cold
	bne :+
	lda sched_reg
	bne :+

:	; advance to the entry cycle, preloading upcoming cold values into free regs
	lda sched_entry
	sta gap_target
	lda sched_entry+1
	sta gap_target+1
	jsr emit_gap_to

	; cold: emit the on-demand load of the current value, either as the
	; 2-cycle immediate or the 3-cycle identity-zeropage form.  Both take the
	; value itself as the operand -- ZP[v] == v is the whole point of the
	; table -- so only the opcode and the cycle count differ.
	lda sched_cold
	beq @store
	ldx sched_reg
	lda sched_zpload
	beq @imm
	txa
	jsr reg_ldop_zp		; ld? <value> (3 cycles)
	ldx #$03
	bne @ldemit		; (always)
@imm:	txa
	jsr reg_ldop		; ld? #value (2 cycles)
	ldx #$02
@ldemit:
	stx sched_ldcost
	jsr gen_emit
	lda rec_val
	jsr gen_emit
	ldx sched_reg		; model[sched_reg] = rec_val
	lda rec_val
	sta regmodel_val,x
	lda #$01
	sta regmodel_known,x
	lda sched_ldcost
	jsr gen_advance

@store:	lda sched_op		; st? abs, or sta abs,x/y for the indexed 5c form
	jsr gen_emit
	lda rec_addr		; (already biased for indexed stores)
	jsr gen_emit
	lda rec_addr+1
	jsr gen_emit
	lda sched_cost
	jsr gen_advance
	jmp scheduled_mark

;-------------------------------------------------------------------------------
; RMW RE-EMISSION
; Check if a RMW operation is needed to maintain cycle exactness.  This will
; be the case if all registers are needed immediately after the RMW (inc/dec)
; is executed. e.g. "inc $1e00 : sta $1e01 : stx $1e02 : sty $1e03"
;
; To be correct, the affected address must have either
;   a) been written earlier in the replayed span
;   b) been restored before that span began (see SHADOW_RMW)
;
; VIC registers are always restored, so we can always assume the (b) case is true
; for them.
;
; "The replayed span" is the frame in full-frame mode and the strip in
; exact-window mode; both are ruled on by rmw_ok, which the two modes compute
; against their own restore block (rmw_check_room / window_rmw_budget).  Window
; mode used to be excluded here and always borrow a register -- but it is the
; mode with the LARGEST restore budget, and the one where the drift that borrowing
; costs is least acceptable, so excluding it had it exactly backwards.
@rmw:	; Prefer the real RMW whenever its input is known to be initialized.  It
	; costs the same six cycles as a cold load/store substitute and, unlike the
	; substitute, preserves all three registers.  Borrowing first used to turn a
	; register that was live across the original RMW cold merely because
	; rmw_room believed it could be reloaded later; that estimate is deliberately
	; optimistic and can strand the register in a tight run.
	lda rmw_ok
	bne @rmwgo
	jsr @input_safe		; VIC, restored, or written earlier this pass?
	bcs @rmwgo

	; The RMW cannot safely read memory: borrow a register if one has room to be
	; reloaded, or take the least-bad one as the bounded-drift fallback.
	jsr pend_advance	; make sure the next-use slots are current
	jsr rmw_pick_reg	; any free registers?
	bcs @rmwreg		; if so, continue and use it

@rmwtake:
	jsr rmw_pick_worst	; take one regardless and eat the drift: a cycle
				; late beats a value that compounds every pass
@rmwreg:
	stx sched_reg
	lda regmodel_known,x
	beq :+
	lda regmodel_val,x
	cmp rec_val
	bne :+
	jmp @hot
:	jmp @coldreg

; Whether this particular RMW can read its input safely even though the
; frame-global restore test failed.
;
; VIC registers are established by the unbudgeted seed.  A prior scheduled
; write establishes an ordinary address directly.  Finally, restore emission
	; clears SHADOW_DIRTY as each priority RMW address goes out, so a clear dirty
	; bit here means this address's restore has already been emitted from that block.
; OUT: .C set if safe to re-emit.
@input_safe:
	lda rec_addr+1
	cmp #$90
	bne @memory
	lda rec_addr
	cmp #$10
	bcc @safe

@memory:
	ldxy rec_addr
	jsr shadow_index
	jsr shadow_bitptr
	lda #<SHADOW_WSEEN
	ldy #>SHADOW_WSEEN
	jsr shadow_test_at
	bne @safe

	jsr shadow_test_dirty
	beq @safe
	clc
	rts
@safe:	sec
	rts

@rmwgo:	; Cost is ours to choose, not the original instruction's: the absolute
	; form is 6 cycles and the ,x form 7, and either can be built from either
	; by flipping bit 4 of the opcode and biasing the operand.  slack-6 lands
	; on 0 or >=2 for every slack except exactly 7, so 7 is the only one that
	; wants the indexed form -- and there the gap is 0, which is what keeps
	; this safe: a longer gap could emit a dex delay, and that would leave .X
	; holding something other than the value the operand was biased against.
	lda sched_slack+1
	bne @rmwabs
	lda sched_slack
	cmp #7
	bne @rmwabs
	lda regmodel_known+1
	beq @rmwabs		; .X unknown -- nothing to bias against

	lda #$07
	sta sched_cost
	lda rec_op
	ora #$10		; -> the ,x form
	sta sched_op
	lda rec_addr		; operand = address - our own .X
	sec
	sbc regmodel_val+1
	sta rec_addr
	lda rec_addr+1
	sbc #$00
	sta rec_addr+1
	jmp @rmwent

@rmwabs:
	lda #$06
	sta sched_cost
	lda rec_op
	and #$ef		; -> the absolute form
	sta sched_op

@rmwent:
	lda sched_slack+1
	bne :+			; negative was counted on entry; positive >=256 fits
	lda sched_slack
	cmp sched_cost
	bcs :+
	incw __cap_late_neg	; deadline is ahead, but the RMW cannot finish by it
:
	lda #$00		; no register is touched, so nothing is cold and
	sta sched_cold		; nothing needs to survive the gap
	lda #$ff
	sta sched_reserved
	jsr set_entry
	lda sched_entry
	sta gap_target
	lda sched_entry+1
	sta gap_target+1
	jsr emit_gap_to
	jmp @store
.endproc

;*******************************************************************************
; SET ENTRY
; sched_entry = rec_tc - sched_cost (the cycle a hot store begins).
.proc set_entry
	lda rec_tc
	sec
	sbc sched_cost
	sta sched_entry
	lda rec_tc+1
	sbc #$00
	sta sched_entry+1
	rts
.endproc

;*******************************************************************************
; REWIND COST CALC
; rewind_cost = rewind_est * 6 -- one "lda #v / sta abs" pair each.  Saturates
; rather than wrapping: a wrapped cost would reserve almost nothing and put the
; silent-drop failure straight back.
.proc rewind_cost_calc
@t=r0
	lda rewind_est
	asl
	sta @t
	lda rewind_est+1
	rol
	sta @t+1
	bcs @max			; *2
	lda @t
	asl
	sta rewind_cost
	lda @t+1
	rol
	sta rewind_cost+1
	bcs @max			; *4
	lda rewind_cost
	clc
	adc @t
	sta rewind_cost
	lda rewind_cost+1
	adc @t+1
	sta rewind_cost+1
	bcc @done			; *4 + *2
@max:	lda #$ff
	sta rewind_cost
	sta rewind_cost+1
@done:	rts
.endproc

;*******************************************************************************
; GEN FRAME
; Turns the captured frame into cycle-exact replay code in OUTPUT_BANK using a
; frame-scoped register scheduler (see the FRAME-SCOPED REGISTER SCHEDULER notes
; above): a base-state prologue re-establishes every VIC register at its pixel-0
; value, then each captured write is scheduled in cycle order - a value already
; modeled in A/X/Y is a bare store, a cold value is loaded during earlier slack
; (preloaded ahead of tight runs), and stores land on their exact frame cycle.
; OUT: OUTPUT_BANK holds gen_size bytes of 6502 ending in RTS.
.proc gen_frame
	lda #$00
	sta restored_count
	sta restored_count+1
	sta __cap_late_cold		; drift counters are per generation, so
	sta __cap_late_cold+1		; what they report is this frame's
	sta __cap_late_neg
	sta __cap_late_neg+1
	sta __cap_drop_space
	sta __cap_drop_space+1
	sta __cap_kept
	sta __cap_kept+1
	sta __cap_geom_unstable
	sta __cap_geom_unstable+1
	sta __cap_drop_time
	sta __cap_drop_time+1
	sta __cap_overrun
	sta __cap_overrun+1
	sta vis_off			; the filter runs until something in this
					; frame gives it a reason not to
	jsr gen_reset

	; Map the pixel-0 arrays into BLK2 and the fetch deadlines into BLK3 for
	; the whole of generation.  Callers run the generated code straight
	; afterwards with the OUTPUT banks still expected in BLK1/2/3, so both
	; have to be put back before returning.
	lda $9ff8
	sta gen_save8
	lda $9ffa
	sta shadow_saveA
	lda $9ffc
	sta dl_saveC
	lda $9ff2
	sta shadow_save2
	lda #SHADOW_BANK
	sta $9ffa
	lda #ANALYSIS_BANK
	sta $9ffc
	lda #$7f			; BLK1/2/3 all RAM r/w
	sta $9ff2

	lda #<REWIND_BMP		; nothing needs putting back until the
	ldy #>REWIND_BMP		; scheduling pass says so
	jsr clear_bmp

	; Split selection is made after build_shadow has counted the records.  Most
	; interlaced raster programs fit one output set and should not pay for a
	; mid-frame bank switch merely because a worst-case capture might not.
	lda #$00
	sta gen_field2
	sta gen_interlaced

	; Reserve REPLAY_MARGIN cycles of tail headroom: gen_frame_end = frame_cyc -
	; REPLAY_MARGIN is the latest cycle we will schedule a write at.  Writes
	; past it (bottom border / vblank) are dropped so the loop's per-frame
	; overhead fits inside one frame period and the raster lock holds.
	lda frame_cyc
	sec
	sbc #<REPLAY_MARGIN
	sta gen_frame_end
	lda frame_cyc+1
	sbc #>REPLAY_MARGIN
	sta gen_frame_end+1

	; Recover the frame's pixel-0 state.  The caller selected the frame extent
	; (gen_src_end_idx/gen_src_ptr) via gen_src_live; build_shadow walks it and
	; leaves, for every address the frame touches, the value it held at the top
	; of the frame.  An empty capture yields no records and renders the
	; seed-only frame.
	jsr build_shadow
	lda cap_abort		; check if user interrupted the generation
	beq :+
	jmp @rts		; user interrupt, we're done

	; Work out what the chip actually reads this frame (see VISIBILITY) and
	; drop the restore obligations for everything it does not.  This runs
	; after build_shadow because it needs the frame's pixel-0 VIC registers,
	; which is what build_shadow's dirty bitmap makes shadow_vic_val answer
	; with; the scheduling loop then skips the same records through
	; vis_visible.
:	CALL FINAL_BANK_SIM_VIS, __cap_vis_analyze

	jsr vis_remap			; ALL THREE windows, not just BLK2

	lda cap_abort
	beq :+
	jmp @rts

:	jsr interlace_split_decide

	; ...then rewind the cursors for the scheduling pass below: the record
	; cursor to chain[0] at offset CAPTURE_WIN, and the restore cursors to
	; the head of each memory region.
	jsr gen_read_reset
	jsr restore_reset

	; Settle the strip's bounds before anything is emitted.  Where the setup
	; block goes at the tail (see TAIL SETUP), so the frame's first write
	; owns cycle 0.
	lda #$00

	; FULL-FRAME: the SETUP block always goes at the tail (see TAIL SETUP), so
	; the frame's first write owns cycle 0.  Reserve its cycles at the end of
	; the frame rather than hoping they are there: scheduling stops setup_cost
	; early, and the writes that displaces are down in the bottom border,
	; which is already the region build_shadow drops as a matter of course.
	jsr setup_cost_calc
	lda gen_frame_end
	sec
	sbc setup_cost
	sta gen_frame_end
	bcs :+
	dec gen_frame_end+1
:	jsr full_frame_budget	; -> head_cyc, tail_room, restore_room, rmw_ok

@head:	jsr scheduled_reset	; WSEEN tracks writes actually emitted by replay
	lda #$00		; the ordinal counts with the record cursor both
	sta sched_ord		; paths above have just rewound
	sta sched_ord+1

	; FULL-FRAME: no prologue here -- that is what moved to the tail, because
	; it is unconditional and would spend its ~100 cycles whether or not the
	; frame had them to spare.  The restores are a different case: the block
	; is budgeted, it stops at the first scheduled write, so it can only ever
	; consume cycles that are idle anyway and cannot push a write late.  The
	; tail alone is not enough room (see the note by head_cyc), so take both.
	jsr emit_restores

	; .A is never trusted at cycle 0.  In window mode the prologue that just
	; ran clobbered it; in full-frame the previous pass's tail did, leaving
	; whatever its last restore or seed store put there.
	jsr regmodel_reset

	; start each register's own walk of the frame (see PENDING-USE SLOTS).
	; Must come after the gen_read_reset above -- both window and full-frame
	; paths settle the shared cursor first, and pend_reset borrows it -- and
	; before the full-frame seed below, which takes its values straight out
	; of the slots this fills.
	jsr pend_reset

	jsr seed_pend_model	; the tail emits these loads

@loop:	jsr poll_restore		; user interrupt?
	beq @room
	jmp @rts			; user interrupt, exit

@room:	; stop cleanly if we are within 256 bytes of the end of the output window,
	; so the trailing RTS always fits.
	lda gen_ptr+1
	cmp #>(GEN_END-$100)
	bcc @next
	jmp @end
@next:	jsr next_record		; rec_* = next write; C=0 -> end of frame
	bcs @have
	jmp @end

@have:	; Nothing the VIC reads depends on this address, or the beam had already
	; finished with it -- either way the write cannot be seen, so do not spend
	; a store on it, and (vis_prune having let go of its dirty bit for the
	; never-fetched case) do not spend a restore on it either.
	incw sched_ord
	ldxy rec_addr
	jsr vis_keep
	bcs @vis
	jmp @loop
@vis:	incw __cap_kept

.ifndef PAL
	; interlaced: the first write at/after the seam closes field 1 and
	; switches generation to OUTPUT set B (field 2)
	lda gen_interlaced
	beq @write
	lda gen_field2
	bne @write			; already switched
	lda rec_tc+1
	cmp gen_seam+1
	bcc @write
	bne @switch
	lda rec_tc
	cmp gen_seam
	bcc @write
@switch:
	jsr field_transition
@write:
.endif
	jsr schedule_write
	jmp @loop

@end:	;-----------------------------------------------------------------------
	; TAIL (see TAIL SETUP) (see TAIL SETUP)
	; Out past the displayed area first: down here the beam has finished with
	; every row, so a restore cannot erase one before it is scanned and the
	; seed cannot move a VIC register mid-screen.
	lda tail_cyc
	sta gap_target
	lda tail_cyc+1
	sta gap_target+1
	jsr emit_gap_to		; no-op if the last write already ran past it

	; Restores get everything up to the reservation.  This is a far larger
	; budget than the old vblank block had -- that one stopped at the first
	; scheduled write, so a program drawing early got almost no restores at
	; all; this one has the whole bottom border and vblank.
	jsr emit_restores

	; ...then the setup block itself, in the cycles reserved for it.  Order
	; within the tail does not matter -- nothing here is visible this pass --
	; but the register seed goes last so A/X/Y are what seed_pend_model
	; claimed when the loop wraps into cycle 0.
	jsr emit_seed
	jsr seed_pend_regs


	; Did it fit?  Nothing here acts on the answer -- no reservation, no
	; budget, no truncation.  gen_cyc past the frame end means the pass
	; genuinely outgrew its frame, and the accounting says that needs a
	; program which both saturates the frame with visible change points and
	; still needs a wrap on nearly every address.  If this is ever non-zero on
	; real code, THAT is the point to design for it.
	lda gen_cyc+1
	cmp gen_frame_end+1
	bcc @fit
	bne @over
	lda gen_cyc
	cmp gen_frame_end
	bcc @fit
@over:	incw __cap_overrun
@fit:

@rts:	lda #$60		; rts -- end of the replay chunk
	jsr gen_emit

	jsr progress_off	; done working; give the border back

	; hand BLK1/BLK2/BLK3 back to the caller's mapping.  BLK1 matters most:
	; the caller runs the generated code out of it the moment this returns.
	lda shadow_save2
	sta $9ff2
	lda gen_save8
	sta $9ff8
	lda shadow_saveA
	sta $9ffa
	lda dl_saveC
	sta $9ffc
	rts
.endproc

;*******************************************************************************
; EMIT SEED
; Base-state prologue: re-establish the VIC registers the frame WRITES at their
; pixel-0 values, so a register it only changes mid-screen still starts correct.
; The immediate is the value the register held at the TOP of this frame, not its
; live value: a program that splits $900F mid-screen leaves the bottom-half
; colour live, and seeding that would paint the whole frame with it until the
; split re-ran.
;
; Only the written ones, because this block is the frame's entire lead time and
; it was spending all of it on nothing.  The replay's only VIC writes are the
; frame's own records, so a register the frame never writes is never disturbed by
; the replay either -- it holds its pixel-0 value pass after pass, and re-seeding
; it stores back exactly what is already there.  At 6 cycles a register that was
; 90 dead cycles for a frame that touches one, which for a raster-effect loop is
; the difference between being schedulable and not: the restores and the first
; write both have to fit after this block, and rst_budget is often only a line or
; two in.  ($9004 is read-only and $9003 bit 7 is the raster LSB, so two of the
; sixteen never did anything at all.)
;
; Normally these stores run at the very top of the frame, in the vblank/top
; border -- well before the visible area -- so they cost only lead time, not
; accuracy.  When the strip is up there instead they run at the END of the pass;
; see SETUP DECIDE.  gen_cyc is advanced 6 per store either way so the timed
; writes stay scheduled against the true frame cycle; a skipped register advances
; nothing, which is what buys the room back.  vic_need is the matching count,
; kept by build_shadow so will_be_exact and setup_decide can charge exactly this.
.proc emit_seed
	ldx #$00
@seed:	cpx #$0f		; $900f is seeded unconditionally: progress_step
	beq @emit		; borrows the border between passes, so it is
				; never left at pixel 0 for us
	jsr vic_dirty		; a register the frame never writes needs nothing
	beq @skip		; -- see VIC DIRTY

@emit:	lda #$a9		; lda #<pixel-0 $9000+x>
	jsr gen_emit
	jsr shadow_vic_val	; preserves .X
	jsr gen_emit
	lda #$8d		; sta $9000+x (absolute)
	jsr gen_emit
	txa
	jsr gen_emit
	lda #$90
	jsr gen_emit
	lda gen_cyc		; gen_cyc += 6 (the lda#/sta just emitted)
	clc
	adc #6
	sta gen_cyc
	bcc @skip
	inc gen_cyc+1

@skip:	inx
	cpx #$10
	bne @seed
	rts
.endproc


;*******************************************************************************
; SETUP COST CALC
; setup_cost = (vic_need + 1) * 6 + 6.  The +1 covers $900f, which the prologue
; emits whether the frame wrote it or not; charging it twice when the frame did
; write it only costs a few cycles of pessimism.  The +6 is the register seed at
; its worst -- three 2-cycle loads (seed_pend_regs); window mode's fixed pair
; costs 4, so it is charged 2 cycles it will not spend.
; Both modes charge the same figure -- window_rmw_budget measures its RMW ruling
; in it, and full-frame reserves it at the end of the frame (see TAIL SETUP).
.proc setup_cost_calc
@cost=r0
	lda vic_need
	clc
	adc #$01
	sta @cost
	asl			; *2
	adc @cost		; *3  (asl left .C clear -- vic_need <= 16)
	asl			; *6
	clc
	adc #$06		; ...plus the register seed, at its worst (three
	sta setup_cost		; 2-cycle loads; window mode's fixed pair is 4, so
	rts			; charging 6 there is 2 cycles of pessimism)
.endproc

;*******************************************************************************
; TAIL START
; The earliest cycle the full-frame tail block may begin at (see TAIL SETUP):
; past the displayed area, so a restore cannot erase a row before the beam
; reaches it and the seed cannot move a VIC register mid-screen, and past the
; last write the frame schedules, so it cannot undo one.
;
; Clamped to gen_frame_end: a program displaying almost the whole frame can put
; the end of its display past the reservation, and the block has to start
; somewhere the seed still fits.  Restores simply come up empty there.
; OUT: tail_cyc.  Assumes SHADOW_BANK is mapped (display_geom reads it).
.proc tail_start
	jsr display_geom	; -> disp_end (first line past the display)
	lda disp_end
	sta lc
	lda disp_end+1
	sta lc+1
	jsr line_to_cycle	; lc = that line as a replay cycle
	lda lc
	sta tail_cyc
	lda lc+1
	sta tail_cyc+1

	lda last_tc+1		; ...but never before the last scheduled write
	cmp tail_cyc+1
	bcc @cap
	bne @take
	lda last_tc
	cmp tail_cyc
	bcc @cap
@take:	lda last_tc
	sta tail_cyc
	lda last_tc+1
	sta tail_cyc+1

@cap:	lda gen_frame_end+1	; ...and never past the reservation
	cmp tail_cyc+1
	bcc @clamp
	bne @done
	lda gen_frame_end
	cmp tail_cyc
	bcs @done
@clamp:	lda gen_frame_end
	sta tail_cyc
	lda gen_frame_end+1
	sta tail_cyc+1
@done:	rts
.endproc

;*******************************************************************************
; HEAD ROOM
; The cycle the head restore block has to stop by -- and because full-frame mode
; emits nothing before it, also exactly how many cycles it gets.
;
; Two things bound it.  The first scheduled write, because a restore must not
; undo one and running past it would push it late besides; and the start of the
; displayed area, because past there the beam is scanning rows, and a row
; restored after it has been scanned shows the previous pass's leftovers rather
; than the pixel-0 value.  Leaves the record cursor rewound.
; OUT: head_cyc
.proc head_room
	jsr display_start	; lc = cycle the displayed area begins at
	lda lc
	sta head_cyc
	lda lc+1
	sta head_cyc+1

	jsr next_record		; ...but never past the first write
	php			; .C = a record exists
	pha
	jsr gen_read_reset	; peeking must not consume it
	pla
	plp
	bcc @done

	lda rec_tc+1
	cmp head_cyc+1
	bcc @first
	bne @done
	lda rec_tc
	cmp head_cyc
	bcs @done
@first:	lda rec_tc
	sta head_cyc
	lda rec_tc+1
	sta head_cyc+1
@done:	rts
.endproc

;*******************************************************************************
; FULL FRAME BUDGET
; Settles both restore regions and rules on read-modify-write re-emission against
; their TOTAL.
;
; The two are disjoint and independently correct: the head runs before anything
; is drawn and before any write is emitted, the tail after everything is drawn
; and every write is out.  A first-touch RMW's restore is equally good from
; either -- in steady state the tail of one pass precedes the next pass's writes
; just as the head does -- so what the ruling has to measure is the sum.
;
; Getting this wrong is what broke the inexactness warning once already: the room
; is a DURATION, and rst_budget is an absolute stop-by cycle.  They coincided
; while the only block was at the head and started from cycle 0.  They do not
; coincide for the tail, and will_be_exact comparing against the wrong one made
; every frame look exact.
; OUT: head_cyc, tail_cyc, tail_room, restore_room, rmw_ok
.proc full_frame_budget
	jsr tail_start		; -> tail_cyc (already clamped to gen_frame_end)
	jsr head_room		; -> head_cyc

	; Clamp the head to where the tail begins.  head_room bounds head_cyc by
	; the display start and the first write, and NEITHER is bounded by the
	; frame: display_start is $9001*2 scaled, so a program that parks a large
	; value there -- or moves it mid-frame, leaving a pixel-0 value nothing
	; sensible -- hands out a head budget of cycles that do not exist.  Worse,
	; without this the head and tail regions can overlap and the same cycles
	; get counted twice, which is how restore_room came out larger than a whole
	; frame.  tail_cyc is already <= gen_frame_end, so this bounds both.
	lda tail_cyc+1
	cmp head_cyc+1
	bcc @clamp
	bne @sized
	lda tail_cyc
	cmp head_cyc
	bcs @sized
@clamp:	lda tail_cyc
	sta head_cyc
	lda tail_cyc+1
	sta head_cyc+1

@sized:	sec			; tail_room = gen_frame_end - tail_cyc
	lda gen_frame_end
	sbc tail_cyc
	sta tail_room
	lda gen_frame_end+1
	sbc tail_cyc+1
	sta tail_room+1
	bcs :+
	lda #$00		; tail_start clamps, so this cannot fire -- but a
	sta tail_room		; negative room would sail straight past the
	sta tail_room+1		; comparisons below as a huge one

:	clc			; restore_room = head_cyc + tail_room
	lda head_cyc
	adc tail_room
	sta restore_room
	lda head_cyc+1
	adc tail_room+1
	sta restore_room+1
	bcc :+
	lda #$ff		; saturate rather than wrap
	sta restore_room
	sta restore_room+1

:	ldx restore_room
	ldy restore_room+1
	jmp rmw_check_room
.endproc

;*******************************************************************************
; INTERLACE SPLIT DECIDE
; A two-field NTSC capture only needs the resident bank flip when its generated
; code may exceed the ordinary 24KB OUTPUT set.  Avoiding the split removes 24
; cycles of work that did not exist in the captured program and is therefore the
; only way to make a busy seam exact.
;
; The no-split threshold is deliberately conservative.  Per record, reserve up
; to 8 bytes for load/store/local padding and 5 more for a distinct-address
; restore.  1800*13 = 23400, leaving 1176 bytes for delay loops, seed/setup and
; the trailing RTS; a whole 34125-cycle empty gap needs under 150 bytes of delay
; loops.  Captures above the threshold retain the two-set path.
INTERLACE_ONESET_RECORDS = 1800
.proc interlace_split_decide
.ifndef PAL
	lda frame_cyc+1
	cmp #>FRAME_CYCLES_INT
	bne @done
	lda frame_cyc
	cmp #<FRAME_CYCLES_INT
	bne @done

	lda record_count+1
	cmp #>INTERLACE_ONESET_RECORDS
	bcc @done		; conservatively proven to fit one set
	bne @split
	lda record_count
	cmp #<INTERLACE_ONESET_RECORDS
	bcc @done

@split:
	inc gen_interlaced
	lda frame_cyc+1		; gen_seam = frame_cyc / 2
	lsr
	sta gen_seam+1
	lda frame_cyc
	ror
	sta gen_seam
.endif
@done:	rts
.endproc

;*******************************************************************************
; FIELD TRANSITION
; Writes the code to transisition the first half of an interlaced (2-frame)
; capture.
.proc field_transition
	; The bank flip has to load three constants, but it need not always destroy
	; A.  Pick the register whose next captured use is furthest away and jump to
	; the matching A/X/Y version of the resident stub.  In particular, the first
	; field-2 store's own register has a zero-distance pending use and loses to
	; either of the others, so a tight opening write stays hot across the seam.
	jsr rmw_pick_worst	; leaves the choice in rmwt_best as well as .X

	; jmp replay_field_flip (ends field 1, still in set A)
	lda #$4c
	jsr gen_emit
	ldx rmwt_best
	lda #<replay_field_flip_a
	cpx #REG_A
	beq :+
	lda #<replay_field_flip_x
	cpx #REG_X
	beq :+
	lda #<replay_field_flip_y
:
	jsr gen_emit
	ldx rmwt_best
	lda #>replay_field_flip_a
	cpx #REG_A
	beq :+
	lda #>replay_field_flip_x
	cpx #REG_X
	beq :+
	lda #>replay_field_flip_y
:
	jsr gen_emit

	; gen_cyc += FLIP_CYCLES (jmp + stub at replay time)
	lda gen_cyc
	clc
	adc #FLIP_CYCLES
	sta gen_cyc
	bcc :+
	inc gen_cyc+1

:	; set banks to generate to to the "B" group
	lda #OUTPUT_B_BANK
	sta gen_out1
	lda #OUTPUT_B_BANK2
	sta gen_out2
	lda #OUTPUT_B_BANK3
	sta gen_out3

	; reset gen pointer to base of the new banks' output region
	lda #<GEN_WIN
	sta gen_ptr
	lda #>GEN_WIN
	sta gen_ptr+1

	; Only the chosen register is cold on the far side of the seam.  The replay
	; runs straight through, so the other two retain their modeled values.
	ldx rmwt_best
	lda #$00
	sta regmodel_known,x

	inc gen_field2
	rts
.endproc




;*******************************************************************************
; IDENTITY ZEROPAGE
; The replay's one unrepresentable quantity is a lone cycle: a gap of 1 has no
; instruction that fills it.  It shows up whenever the program's own cadence is
; odd -- "lda $30 : sta $1000" is 3+4, so consecutive stores sit 7 cycles apart,
; and a 2-cycle immediate load plus a 4-cycle store only makes 6.
;
; The fix is a 3-cycle load of an ARBITRARY byte, and the 6502 has exactly one:
; "lda zp".  Everything else is the wrong length -- "lda abs" and "lda zp,x" are
; 4, transfers are 2, "pla" is 4.  So page 0 is filled with an identity table,
; ZP[i] = i, and "lda <v>" then loads v for any v.  Immediate and zeropage
; become the same load at two different prices, 2 cycles or 3, and every gap
; the program can produce becomes representable (see the FRAME-SCOPED REGISTER
; SCHEDULER notes).
;
; This costs page 0 for the duration, so it can only be installed across code
; that touches no zeropage variable at all.  The raster-locked loop qualifies:
; it runs under sei (no KERNAL IRQ), polls the keyboard through the raw matrix
; registers, switches banks by poking $9ff8-$9ffc directly, and reaches the
; generated code with a plain jsr rather than a bank trampoline.  The one
; exception was the fine-sync's jmp through zp::jmpaddr, now replay_jmpvec.
;
; Everything outside that loop -- gen_frame's r0-r8 and zp::debuggertmp, and the
; CALL trampoline's zeropage bank stack in replay_step -- needs the real page 0
; back, so the key handlers restore it before running and reinstall afterwards.
ZPSAVE_WIN = $2000		; BLK1 window ZPSAVE_BANK is mapped into

;*******************************************************************************
; RESTORE ARM
; Turns RESTORE into a latch that can be polled from anywhere in the capture
; view, including the parts that cannot take an interrupt.
;
; RESTORE is wired to VIA1 CA1.  The IFR flag is set by the edge whether or not
; the IER lets it reach /NMI, so masking VIA1 does not lose the press -- it just
; parks it in $911d until someone looks.  That is what we want, because the two
; places the key has to work in are both places an NMI must not land: the
; raster-locked replay (page 0 is the identity table there, and the KERNAL
; handler's page-0 scratch would quietly corrupt every "ld? <value>"), and code
; generation (a handler that unwound out of it would have to abandon the CALL
; trampoline's bank stack mid-nest).  Polling costs a few cycles per record and
; needs no unwind at all.
;
; Masking VIA1 this early is also what makes the key work during generation --
; before, the enables came down only once generation was already finished.
.proc restore_arm
	lda $911e
	sta replay_ier1
	lda #$7f		; clear every VIA1 interrupt enable
	sta $911e
	lda #$02		; ack any edge already pending: the keypress that
	sta $911d		; opened this view must not also close it
	lda #$00
	sta cap_abort
	sta poll_ctr
	rts
.endproc

;-------------------------------------------------------------------------------
; RESTORE DISARM
; Undoes restore_arm on the way back to the debugger.
.proc restore_disarm
	lda #$02		; a press we never got to still sits in the flag;
	sta $911d		; clear it or it fires as soon as VIA1 is enabled
	lda replay_ier1
	ora #$80		; bit 7 set = "enable the bits below"
	sta $911e
	rts
.endproc

;-------------------------------------------------------------------------------
; POLL RESTORE
; Asks the latch whether RESTORE has been pressed, and remembers the answer in
; cap_abort so callers deep in the generation call tree can each unwind their
; own way.
;
; Called from the per-record loops, so it is gated to one VIA read in 256 calls.
; The full check is ~20 cycles against a per-record cost of a couple hundred,
; which would be a real tax on generation for latency nobody can perceive; 256
; records is well under a frame of wall time either way.
; OUT: .Z clear if generation should give up.  Clobbers .A.
.proc poll_restore
	lda cap_abort		; already decided -- a caller that gave up and
	bne @yes		; returned leaves its own caller still looping,
				; and that one must not have to wait out the
				; gate below to hear about it
	inc poll_ctr
	beq @look		; 1 call in 256 does the real work
@no:	lda #$00		; .Z set -> nothing to report
	rts

@look:	jsr progress_step	; the gate paces the working indicator too

	lda $911d		; VIA1 IFR
	and #$02		; CA1 (RESTORE) edge latched?
	beq @no
	sta cap_abort		; !0; .Z still clear from the AND
@yes:	rts
.endproc

;-------------------------------------------------------------------------------
; PROGRESS STEP / PROGRESS OFF
; A working indicator for the part of the view that has nothing to show yet.
; Generation can take a noticeable moment on a busy frame, and what is on screen
; while it runs is the user's own display, put up by restore_prog_visual and then
; sitting there perfectly still -- indistinguishable from a hang.  So step the
; border one colour per poll: it is driven off the record counter, so its speed
; is the real rate of progress rather than a timer pretending to be one.
;
; The border is free to scribble on.  Every replayed frame re-establishes all 16
; VIC registers from vic_pixel0 in its seed prologue, and the paths that never
; reach a replay hand the display back through restore_debug_state -- so nothing
; downstream reads $900f expecting to find what was there before.  progress_off
; still puts it back, because between the last poll and the replay's first frame
; there is a raster sync long enough to show one frame of the wrong border.
; Clobbers .A, like the poll_restore it hangs off.
.proc progress_step
	inc progress_col
	lda progress_col
	and #$07		; border is $900f bits 0-2
	sta progress_col
	lda $900f
	and #$f8		; the user's background/reverse bits stay put:
	ora progress_col	; only the border says anything
	sta $900f
	rts
.endproc

; Preserves .A and the flags -- it is called from between a compare and the
; branch that reads it.
.proc progress_off
	php
	pha
	lda vic_pixel0+$0f
	sta $900f
	pla
	plp
	rts
.endproc

;-------------------------------------------------------------------------------
; ZP INSTALL
; Stashes page 0 into ZPSAVE_BANK and replaces it with the identity table.
.proc zp_install
	jsr zp_map
	ldx #$00
@l:	lda $00,x		; read first: the store below overwrites it
	sta ZPSAVE_WIN,x
	txa
	sta $00,x
	inx
	bne @l
	jmp zp_unmap
.endproc

;-------------------------------------------------------------------------------
; ZP RESTORE
; Puts Monster's page 0 back, undoing zp_install.
.proc zp_restore
	jsr zp_map
	ldx #$00
@l:	lda ZPSAVE_WIN,x
	sta $00,x
	inx
	bne @l
	jmp zp_unmap
.endproc

;-------------------------------------------------------------------------------
; ZP MAP / ZP UNMAP
; Borrow BLK1 for the stash bank and give it back.  BLK1 holds the generated
; code during replay, so the caller's mapping has to survive.
.proc zp_map
	lda $9ff8
	sta zp_save8
	lda $9ff2
	sta zp_save2
	lda #ZPSAVE_BANK
	sta $9ff8
	lda #$7f			; BLK1/2/3 all RAM r/w
	sta $9ff2
	rts
.endproc

.proc zp_unmap
	lda zp_save2
	sta $9ff2
	lda zp_save8
	sta $9ff8
	rts
.endproc

;*******************************************************************************
; REPLAY PREAMBLE
; Establishes the frame's pixel-0 state for every address the frame touches, once,
; before the raster lock is taken.
;
; This is the point of the whole exercise.  The per-pass restore block has to fit
; inside the frame it is restoring, competing for cycles with the writes it
; exists to support -- which is why EXACT WINDOW MODE exists at all, and why
; "restores that did not fit" was a thing a frame could silently be.  Run ONCE,
; outside the timed loop, there is no budget: this walks the whole dirty bitmap
; and writes every value, however many there are, and costs the replay nothing.
;
; It also needs no table.  build_shadow already left the pixel-0 value of every
; touched address in SHADOW_VAL and marked it in SHADOW_DIRTY; this is just a
; walk of what is already there, so the "preamble table in its own bank" the plan
; called for turned out to be free.
;
; The VIC region is deliberately not walked -- emit_seed owns $9000-$900f, and
; poking the registers here would fight it.
;
; MUST run before zp_install.  Page 0 becomes the identity table there, and r0-rc
; live in page 0.
; Assumes the caller left BLK1/2/3 holding the OUTPUT banks; BLK2 is borrowed and
; handed straight back.
.proc replay_preamble
@p=r2				; SHADOW_DIRTY cursor
@vp=r4				; SHADOW_VAL cursor
@dst=r6				; where the value goes
@bits=r8
@n=r9				; bitmap bytes left
@idx=rb				; index of bit 0 of the current byte
	lda $9ffa
	pha
	lda $9ff2
	pha
	lda #SHADOW_BANK
	sta $9ffa
	lda #$7f
	sta $9ff2

	lda #<SHADOW_DIRTY
	sta @p
	lda #>SHADOW_DIRTY
	sta @p+1
	lda #<SHADOW_VAL
	sta @vp
	lda #>SHADOW_VAL
	sta @vp+1
	lda #$00
	sta @idx
	sta @idx+1
	lda #<VIS_PRUNE_BYTES		; screen + colour; the seed covers VIC
	sta @n
	lda #>VIS_PRUNE_BYTES
	sta @n+1

@lp:	ldy #$00
	lda (@p),y
	beq @next			; eight clean addresses at a time
	sta @bits

	ldx #$00
@bit:	lsr @bits
	bcc @nb
	txa				; shidx = @idx + bit
	clc
	adc @idx
	sta shidx
	lda @idx+1
	adc #$00
	sta shidx+1
	jsr shadow_addr			; -> restore_addr (preserves .X)
	lda restore_addr
	sta @dst
	lda restore_addr+1
	sta @dst+1
	txa
	tay
	lda (@vp),y			; its pixel-0 value
	ldy #$00
	sta (@dst),y
@nb:	inx
	cpx #$08
	bne @bit

@next:	inc @p
	bne :+
	inc @p+1
:	lda @vp
	clc
	adc #$08
	sta @vp
	bcc :+
	inc @vp+1
:	lda @idx
	clc
	adc #$08
	sta @idx
	bcc :+
	inc @idx+1
:	lda @n
	bne :+
	dec @n+1
:	dec @n
	lda @n
	ora @n+1
	bne @lp

	pla
	sta $9ff2
	pla
	sta $9ffa
	rts
.endproc

;*******************************************************************************
; REPLAY LOOP
; Generates code to display every every write for the frame to areas that affect
; the visible screen output (screen, color RAM, and VIC registers).
; Uses a stable interrupt on VIA2 timer 1 to sync the raster position.
CAP_RUN_ADDR  = $2000		; BLK1 window OUTPUT_BANK is mapped into
REPLAY_LEAD      = 7
REPLAY_SLIDE_MAX = 16

; Raster lines the sync path below burns between first seeing REPLAY_SYNC_LINE on
; $9004 and the generated code's cycle 0: ~10.8 to the timer load, plus ~0.7 for
; the underflow poll, fine sync and jsr.
REPLAY_SYNC_LEAD = 11

; $9004 value to sync to, picked so that lead lands cycle 0 on raster line 0 --
; i.e. sync this far BEFORE the frame boundary and arrive exactly on it.  $9004
; is the raster line >> 1, so this is in two-line units and the fine slide below
; takes up the rest.  Syncing to 0 and entering 11 lines in is what used to make
; the top of the frame unreachable to the strip; see the note on line_to_cycle.
REPLAY_SYNC_LINE = (FRAME_LINES-REPLAY_SYNC_LEAD)/2
REPLAY_SYNC_POS  = 6		; horizontal delay before fine syncing

; The sync walks $9004 upward from REPLAY_SYNC_LINE in five steps of one (two
; lines each), and landing cycle 0 on line 0 necessarily puts that walk against
; the top of the counter: on NTSC it ends waiting on 130, which is the last value
; of the frame.  That is exact rather than comfortable -- if an @rdly ever
; overshoots and the walk misses its final value, $9004 wraps and the sync costs
; a whole frame before it catches up.  The assert is what will notice if the
; frame geometry or the walk changes underneath it.
.assert REPLAY_SYNC_LINE+5 <= (FRAME_LINES-1)/2, error, "sync walk runs off the end of the frame"
.proc replay_loop
	; map the OUTPUT banks containing generated frame code into BLK1/2/3
	lda $9ff8
	sta replay_save8
	lda $9ffa
	sta replay_saveA
	lda $9ffc
	sta replay_saveC
	lda $9ff2
	sta replay_save2
	lda #OUTPUT_BANK
	sta $9ff8
	lda #OUTPUT_BANK2
	sta $9ffa
	lda #OUTPUT_BANK3
	sta $9ffc
	lda #$7f			; BLKs 1/2/3 all RAM
	sta $9ff2

	; Save everything @exit puts back before generating anything, because
	; generation can now bail out into @exit (RESTORE).  replay_col included:
	; the poll below is what normally seeds it, and on the bail path the poll
	; never runs.
	lda $9120
	sta replay_col

	; save the debugger's VIA2 Timer 1 config for later restore
	lda $912b
	sta replay_acr
	lda $9126
	sta replay_t1ll
	lda $9127
	sta replay_t1lh

	; build the replay from the LIVE (in-progress) frame.  VIA1 is already
	; masked and the RESTORE latch armed (trigger did both before
	; will_be_exact), so this is interruptible and so is the raster lock
	; below -- neither could take an NMI.
	jsr gen_src_live
	jsr gen_frame
	lda cap_abort
	beq :+
	jmp @exit			; never take the lock; just unwind
:	jsr replay_preamble		; pixel-0 state, once, off the clock

	; lock a free-running VIA2 Timer 1 to the frame period:
	; latch = frame-2; timer period = latch+2 (one frame)
	lda $912b
	and #$3f
	ora #$40			; ACR bit 6: T1 free-run
	sta $912b
	lda frame_cyc
	sec
	sbc #2
	sta $9126			; T1 latch lo
	sec				; nop-slide base = timer-lo - REPLAY_LEAD
	sbc #REPLAY_LEAD
	sta rbase

	; wait for coarse position (raster line)
	ldy #REPLAY_SYNC_LINE
@rs0:	cpy $9004
	bne @rs0

	iny
	iny
@rs1:	cpy $9004
	bne @rs1
	jsr @rdly
	iny
	cpy $9004
	beq @rs2
	nop
	nop
@rs2:	jsr @rdly
	nop
	iny
	cpy $9004
	beq @rs3
	bit $24
@rs3:	jsr @rdly
	nop
	iny
	cpy $9004
	bne @rs4
@rs4:	ldx #REPLAY_SYNC_POS		; horizontal position within the line
@rs5:	dex
	bne @rs5
.ifndef PAL
	nop
	nop
.endif

	lda frame_cyc+1
	sta $9125			; load hi byte of timer

	; page 0 becomes the identity table for as long as we stay locked to the
	; raster; the key handlers below put it back before doing anything that
	; needs a zeropage variable
	jsr zp_install

@loop:	lda gen_interlaced
	beq @sync

	; ilace on: bank in the frame 1 output banks
	lda #OUTPUT_BANK
	sta $9ff8
	lda #OUTPUT_BANK2
	sta $9ffa
	lda #OUTPUT_BANK3
	sta $9ffc

	; wait for the timer to roll over (one frame boundary)
@sync:	lda $912d			; VIA2 IFR
	and #$40			; T1 underflow flag
	beq @sync

	; (rbase - T1C-L) is now how far past the boundary we should
	; enter the LDA-# slide to burn the extra cycles
	sec
	lda rbase
	sbc $9124
	cmp #REPLAY_SLIDE_MAX
	bcs @noslide			; out of range -> skip fine correction
	clc
	adc #<@slide
	sta replay_jmpvec
	lda #>@slide
	adc #$00
	sta replay_jmpvec+1
	jmp (replay_jmpvec)

@slide: ; LDA-# slide; synchronize jitter
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop

;------------------------------------------------------------------------------
@noslide:
	incw __cap_sync_skip
	; falls through -- the branch above is not taken on the corrected path, so
	; that path still costs exactly 2 cycles here and is untouched

;------------------------------------------------------------------------------
@run:	jsr CAP_RUN_ADDR		; execute generated frame replay

	incw __cap_passes		; counted AFTER the run, in the tail margin,
					; so cycle 0 of the generated code is not
					; pushed later by the measurement

	; check RESTORE key
	lda $911d			; VIA1 IFR
	and #$02			; CA1 (RESTORE) edge latched?
	bne @restore			; -> straight back to the debugger

	; poll the keyboard matrix and check for commands to handle
	lda $9120			; save the column drive value
	sta replay_col			; replay_step's generator clobbers it
	lda #$00
	sta $9120			; drive all columns low
	lda $9121			; read rows ($ff = no key)
	cmp #$ff
	bne @key			; a key is down
	lda replay_col
	sta $9120			; restore the column drive
	jmp @loop			; no key -> keep replaying

;-------------------------------------------------------------------------------
; RESTORE
; Return to the debugger
@restore:
	lda $9120
	sta replay_col
	jsr zp_restore			; page 0 back before leaving the lock
	jmp @exit

; The scan pairs below are (row drive for $9120, column bit for $9121): on this
; machine $9120 selects the matrix ROW and $9121 reads the COLUMNS back.
@key:	; A key is down, so we are about to leave the raster lock: stepping needs
	; real zeropage variables (gen_frame's r0-r8 and zp::debuggertmp, and the
	; CALL trampoline's bank stack).  Both paths out -- @step and @exit --
	; pass through here, so this is the only place the table comes down.
	jsr zp_restore

	; 'z' -- single-step the program (row 4, col 1)
	lda #$ef
	ldx #$02
	jsr @scan
	beq @step
	jmp @exit		; any other key leaves

@step:	jsr replay_step			; step the program + rebuild the replay

@rel:	; check RESTORE key (user interrupt)
	lda cap_abort
	beq :+
	jmp @exit			; user wants out -> return

:	; wait for every key to come up before replaying again, so one press
	; does not repeat
	lda #$00
	sta $9120
	jsr @settle
	lda $9121
	cmp #$ff
	bne @rel
	lda replay_col
	sta $9120			; restore the column drive
	jsr replay_preamble		; the shadow was rebuilt -- re-establish
	jsr zp_install			; going back under the raster lock
	jmp @loop

;------------------------------------------------------------------------------
; SCAN
; Drives one keyboard column and tests one row bit.  The matrix needs a moment
; to settle after the column drive changes -- reading $9121 immediately can
; return the previous column's rows, which shows up as a key that never
; registers.
; IN:  .A = column drive, .X = row bit
; OUT: .Z set if that key is down
@scan:	sta $9120
	stx replay_rowbit
	jsr @settle
	lda $9121
	and replay_rowbit
	rts

@settle:
	ldx #$08
:	dex
	bne :-
	rts

;------------------------------------------------------------------------------
@exit:	lda replay_col
	sta $9120			; restore the column drive

	; restore VIA2 Timer 1
	lda replay_t1ll
	sta $9126			; T1 latch lo
	lda replay_t1lh
	sta $9127			; T1 latch hi
	lda replay_acr
	sta $912b			; ACR (restore original T1 mode)
	lda replay_t1lh
	sta $9125			; T1C-H: reload counter from latch

	; VIA1 stays masked past here: trigger disarms the RESTORE latch once the
	; debugger's own state is back up, which is also the last thing that
	; rewrites $911e

	; restore BLK1/2/3 mapping and return to the debugger
	lda replay_save2
	sta $9ff2
	lda replay_saveC
	sta $9ffc
	lda replay_saveA
	sta $9ffa
	lda replay_save8
	sta $9ff8
	rts

;-------------------------------------------------------------------------------
; RDLY
; Delays 2*CYCLES_PER_LINE cycles (including the jsr/rts to from this routine)
@rdly:
.ifdef PAL
	ldx #$19
.else
	ldx #$17
.endif
:	dex
	bne :-
	nop
	rts
.endproc

;*******************************************************************************
; REPLAY FIELD FLIP
; Update banks to the "B" group for the second frame of the interlace code and
; continue execution to complete the interlaced image.  Three equal-cost forms
; let field_transition clobber whichever register is needed furthest in the
; second field while preserving the other two.
.proc replay_field_flip_a
	lda #OUTPUT_B_BANK
	sta $9ff8
	lda #OUTPUT_B_BANK2
	sta $9ffa
	lda #OUTPUT_B_BANK3
	sta $9ffc
	jmp CAP_RUN_ADDR		; -> field 2 code at $2000 in set B
.endproc

.proc replay_field_flip_x
	ldx #OUTPUT_B_BANK
	stx $9ff8
	ldx #OUTPUT_B_BANK2
	stx $9ffa
	ldx #OUTPUT_B_BANK3
	stx $9ffc
	jmp CAP_RUN_ADDR
.endproc

.proc replay_field_flip_y
	ldy #OUTPUT_B_BANK
	sty $9ff8
	ldy #OUTPUT_B_BANK2
	sty $9ffa
	ldy #OUTPUT_B_BANK3
	sty $9ffc
	jmp CAP_RUN_ADDR
.endproc

;*******************************************************************************
; REPLAY STEP
; Single-steps the user program by one instruction and updates the replay to
; capture the updated frame capture to display.
.proc replay_step
	lda #$00
	sta tracing			; step (not trace)
	CALL FINAL_BANK_SIM, step	; perform the step (step lives in SIM)
	; fall through to rebuild the replay from the stepped state
.endproc


;*******************************************************************************
; GEN SRC LIVE
; Set the read extent (gen_src_end_idx/gen_src_ptr) to the in-progress frame.
; The read cursor itself is (re)seeded at the head of the chain in gen_frame.
.proc gen_src_live
	lda cap_live_idx
	sta gen_src_end_idx
	lda capture_ptr
	sta gen_src_ptr
	lda capture_ptr+1
	sta gen_src_ptr+1
	rts
.endproc

;*******************************************************************************
; TRIGGER
; Shows the replay of whatever has been captured so far: swaps the user
; program's screen/VIC/color in and runs the raster-synced replay until a key is
; pressed.
.import __fastcopy_save_debug_state
.import __fastcopy_restore_prog_visual
.import __fastcopy_restore_debug_state
.import __screen_blank
.import __screen_unblank
.export __cap_trigger
__cap_trigger:
.proc trigger
	lda #$01				; visibility filtering on unless the
	sta vis_enable				; monitor has been used to turn it off
						; mid-session (see __cap_vis_enable)

	CALLMAIN __screen_blank			; stop debugger's IRQ/display
	CALLMAIN __fastcopy_save_debug_state	; save debugger's visible state
	CALLMAIN __fastcopy_restore_prog_visual	; show user's screen/VIC/color

	jsr restore_arm				; allow user to interrupt

	; capture the "pixel-0" state (state of VIC registers at top of frame)
	ldx #$0f
:	lda $9000,x
	sta vic_pixel0,x
	dex
	bpl :-

	; No verdict to ask for.  will_be_exact existed to say "this frame cannot
	; be shown faithfully" so the caller could offer the strip view instead;
	; the restore block is unbudgeted now and it had become incapable of
	; answering anything but yes, at the cost of a second full analysis pass
	; per generation.

@go:	sei
	jsr replay_loop

	; make sure raster isn't on last line (262), disabling interlace there
	; would cause issues
	lda #$3f
:	cmp $9004
	bne :-

	; disable interlace mode
	lda $9000
	and #$7f
	sta $9000
	ldx #$05

	; TODO: is this necessary
@settle:
@sw0:	lda $9004
	cmp #$60
	bcc @sw0			; wait until well down the frame
@sw1:	lda $9004
	cmp #$10
	bcs @sw1			; wait for the wrap back to the top
	dex
	bne @settle

	CALLMAIN __fastcopy_restore_debug_state	; restore debugger
	CALLMAIN __screen_unblank
	jsr restore_disarm
	sec					; the frame was shown
	rts
.endproc


;*******************************************************************************
; VISIBILITY ANALYSIS BANK
; The model of where the VIC is reading from (see VISIBILITY, above).  It runs
; once per generation, before a single byte of replay code is emitted, so paying
; a bank switch to get here costs nothing -- and it is what bought ROMSIMCAP its
; headroom back when it was down to 19 bytes.
;
; SIMCAP calls in through vis_analyze and nothing else.  Everything below is
; reached only from there, so the bank stack never nests inside this.
.segment "SIMVIS"

;*******************************************************************************
; SHARED LEAF ROUTINES
; The same source SIMCAP instantiates at file scope, instantiated here under a
; scope instead.  Both banks live at $a000, so a helper cannot be called across
; the boundary; the code below reaches these as vish::<name>.
.scope vish
.include "capture-helpers.inc"
.endscope

;*******************************************************************************
; VIS POLL
; SIMCAP's poll_restore is not reachable from here and is not worth a third copy:
; the gate, the progress indicator and the abort latch are three lines between
; them.  cap_abort is .BSS, so setting it here is what SIMCAP's own poll_restore
; reads when the analysis returns.
; OUT: .Z clear if generation should give up.
.proc vis_poll
	lda cap_abort
	bne @yes

	; The gate is for the BORDER, not for the latch.  poll_restore reads the
	; latch 1 call in 256 because it is called per RECORD -- thousands of
	; times, where ~20 cycles each would be a real tax on generation.  This is
	; called per raster LINE and per bitmap BYTE, a few hundred times, so
	; inheriting that gate meant about one real check per entire walk: a
	; RESTORE press then sat unnoticed for the length of the analysis, which
	; runs twice per generation and on every j/k/+/- regeneration.  Reading
	; $911d every call costs ~10 cycles against the ~200k the analysis spends.
	;
	; Stepping the border every call would strobe it, so that keeps a gate.
	inc poll_ctr
	lda poll_ctr
	and #$1f
	bne @look

	inc progress_col	; keep the border cycling while we think
	lda progress_col
	and #$07
	sta progress_col
	lda $900f
	and #$f8
	ora progress_col
	sta $900f

@look:	lda $911d		; VIA1 IFR
	and #$02		; CA1 (RESTORE) edge latched?
	beq @no
	sta cap_abort		; !0; .Z still clear from the AND
@yes:	rts
@no:	lda #$00
	rts
.endproc

;*******************************************************************************
; VIS REWIND SCAN
; How many addresses will the pass leave holding something other than their
; pixel-0 value?  Those are the wrap writes -- the one obligation the original
; program never had, because it moved on to the next frame while the replay
; loops this one forever.
;
; Only confirmed writes count: an unconfirmed one is never emitted, so it cannot
; leave anything behind.  Records arrive in cycle order, so the last confirmed
; write to an address decides its bit.
.proc vis_rewind_scan
vp=r6
ord=r8				; 1-based, matching the analysis and the scheduler
	lda #<REWIND_BMP
	ldy #>REWIND_BMP
	jsr vish::clear_bmp
	lda #$00
	sta rewind_est
	sta rewind_est+1
	sta ord
	sta ord+1
	jsr vish::gen_read_reset

@lp:	jsr vis_poll
	beq @go
	rts
@go:	jsr vish::next_record_raw
	bcs @have
	jmp @count

@have:	inc ord
	bne :+
	inc ord+1

:	jsr need_test			; confirmed by a read?
	beq @lp

	ldxy rec_addr
	jsr vish::shadow_index
	lda shidx+1			; VIC belongs to the seed, not the wrap
	cmp #>SHIDX_VIC
	bcs @lp

	lda shidx			; leaves it on its pixel-0 value?
	clc
	adc #<SHADOW_VAL
	sta vp
	lda shidx+1
	adc #>SHADOW_VAL
	sta vp+1
	ldy #$00
	lda (vp),y
	cmp rec_val
	php
	jsr vish::shadow_bitptr
	lda #<REWIND_BMP
	ldy #>REWIND_BMP
	jsr vish::shadow_addbase
	ldy #$00
	plp
	beq @same
	lda (shptr),y
	ora shmask
	sta (shptr),y
	jmp @lp
@same:	lda shmask
	eor #$ff
	and (shptr),y
	sta (shptr),y
	jmp @lp

@count:	jsr bmp_popcount
	lda bmp_count
	sta rewind_est
	lda bmp_count+1
	sta rewind_est+1
	rts

;-------------------------------------------------------------------------------
; .Z clear if record ord (1-based) was confirmed.  Clobbers .A/.X/.Y and pslot.
need_test:
	lda ord
	sec
	sbc #$01
	sta pslot
	lda ord+1
	sbc #$00
	sta pslot+1
	lda pslot
	and #$07
	tay
	lda vish::bitmask_tab,y
	sta shmask
	lda pslot+1
	lsr
	sta pslot+1
	lda pslot
	ror
	sta pslot
	lsr pslot+1
	ror pslot
	lsr pslot+1
	ror pslot
	lda pslot
	clc
	adc #<NEEDED_BMP
	sta pslot
	lda pslot+1
	adc #>NEEDED_BMP
	sta pslot+1
	ldy #$00
	lda (pslot),y
	and shmask
	rts
.endproc

;*******************************************************************************
; BMP POPCOUNT
; Bits set in REWIND_BMP over the screen and colour regions.
.proc bmp_popcount
@p=r2
@n=r4
@bits=r6
	lda #<REWIND_BMP
	sta @p
	lda #>REWIND_BMP
	sta @p+1
	lda #$00
	sta bmp_count
	sta bmp_count+1
	lda #<VIS_PRUNE_BYTES
	sta @n
	lda #>VIS_PRUNE_BYTES
	sta @n+1

@lp:	ldy #$00
	lda (@p),y
	beq @next
	sta @bits
	ldy #$08
@bit:	lsr @bits
	bcc :+
	inc bmp_count
	bne :+
	inc bmp_count+1
:	dey
	bne @bit

@next:	inc @p
	bne :+
	inc @p+1
:	lda @n
	bne :+
	dec @n+1
:	dec @n
	lda @n
	ora @n+1
	bne @lp
	rts
.endproc

;*******************************************************************************
; VIS ANALYZE
; The bank's only entry point: work out what the VIC reads this frame, then drop
; the restore obligations for everything it does not.
;
; It maps SHADOW_BANK itself rather than inheriting the caller's mapping.  A far
; CALL reprograms BLK1/2/3 from the table in ultimem.asm on the way in AND on the
; way out, so the caller's pokes are gone by the time this runs and this one's
; are gone by the time the caller resumes -- both sides re-map.  Getting this
; wrong is not a crash, it is an analysis run against whatever bank happened to
; be in BLK2, which would answer plausibly and wrongly.
.export __cap_vis_analyze
.proc __cap_vis_analyze
	lda $9ffa
	pha
	lda $9ffc
	pha
	lda $9ff8
	pha
	lda $9ff2
	pha
	lda #SHADOW_BANK
	sta $9ffa
	lda #ANALYSIS_BANK		; NEEDED is written here and read back by
	sta $9ffc			; the scheduling loop over in SIMCAP
	lda #PENDING_BANK		; ...and BLK1 holds the screen half of the
	sta $9ff8			; pending table for the walk's duration
	lda #$7f			; BLK1/2/3 all RAM r/w
	sta $9ff2

	jsr fetch_walk
	jsr vis_prune
	jsr vis_rewind_scan

	pla
	sta $9ff2
	pla
	sta $9ff8
	pla
	sta $9ffc
	pla
	sta $9ffa
	rts
.endproc

;*******************************************************************************
; PEND SLOT
; Pointer to the two-byte "unconfirmed write" slot for the index in shidx.
; Screen indices live in BLK1, colour in BLK3; both are mapped for the whole of
; the analysis, so no swapping is needed between the two halves of a cell.
; OUT: pslot
.proc pend_slot
@t=r6
	lda shidx+1
	cmp #>SHIDX_COLOR
	bcs @colour

	lda shidx			; PENDS + idx*2
	asl
	sta @t
	lda shidx+1
	rol
	sta @t+1
	jmp @add

@colour:
	lda shidx			; PENDC + (idx-SHIDX_COLOR)*2
	sec
	sbc #<SHIDX_COLOR
	sta @t
	lda shidx+1
	sbc #>SHIDX_COLOR
	sta @t+1
	asl @t
	rol @t+1
	lda @t
	clc
	adc #<PENDC
	sta pslot
	lda @t+1
	adc #>PENDC
	sta pslot+1
	rts

@add:	lda @t
	clc
	adc #<PENDS
	sta pslot
	lda @t+1
	adc #>PENDS
	sta pslot+1
	rts
.endproc

;*******************************************************************************
; PEND WRITE
; Record that shidx has an unconfirmed write, the one whose ordinal is in fw_ord.
; Whatever was pending is simply overwritten: nothing read it, so it never
; mattered.  That overwrite IS the coalescing.
.proc pend_write
	jsr pend_slot
	ldy #$00
	lda fw_ord
	sta (pslot),y
	iny
	lda fw_ord+1
	sta (pslot),y
	rts
.endproc

;*******************************************************************************
; CONFIRM IDX
; The chip just read shidx.  Whatever write is pending there is therefore
; observable: mark its record needed and empty the slot, so the next read does
; not claim it a second time.
.proc confirm_idx
@ord=r8
	jsr pend_slot
	ldy #$00
	lda (pslot),y
	sta @ord
	iny
	lda (pslot),y
	sta @ord+1
	ora @ord
	beq @none			; nothing pending

	lda #$00			; empty the slot
	sta (pslot),y
	dey
	sta (pslot),y

	lda @ord			; ordinals are 1-based so 0 can mean empty
	sec
	sbc #$01
	sta @ord
	lda @ord+1
	sbc #$00
	sta @ord+1

	lda @ord			; bit @ord of NEEDED_BMP
	and #$07
	tay
	lda bitmask_tab,y
	sta shmask
	lda @ord+1
	lsr
	sta pslot+1
	lda @ord
	ror
	sta pslot
	lsr pslot+1
	ror pslot
	lsr pslot+1
	ror pslot
	lda pslot
	clc
	adc #<NEEDED_BMP
	sta pslot
	lda pslot+1
	adc #>NEEDED_BMP
	sta pslot+1
	ldy #$00
	lda (pslot),y
	ora shmask
	sta (pslot),y
@none:	rts
.endproc

;*******************************************************************************
; CLEAR NEEDED
; The answer starts empty: a record is kept only once a read has claimed it.
.proc clear_needed
@p=r6
	lda #<NEEDED_BMP
	sta @p
	lda #>NEEDED_BMP
	sta @p+1
	lda #$00
	ldy #$00
	ldx #>NEED_BMP_SZ
@page:	sta (@p),y
	iny
	bne @page
	inc @p+1
	dex
	bne @page
	ldy #<NEED_BMP_SZ
	beq @done
@rem:	dey
	sta (@p),y
	bne @rem
@done:	rts
.endproc

;*******************************************************************************
; CLEAR PENDING
; Both halves of the unconfirmed-write table.  A stale entry here would confirm a
; write from the previous generation, so this is not optional.
.proc clear_pending
@p=r6
@n=r8
	lda #<PENDS
	sta @p
	lda #>PENDS
	sta @p+1
	ldx #>PENDS_SZ
	jsr @fill
	lda #<PENDC
	sta @p
	lda #>PENDC
	sta @p+1
	ldx #>PENDC_SZ
@fill:	lda #$00
	ldy #$00
:	sta (@p),y
	iny
	bne :-
	inc @p+1
	dex
	bne :-
	rts
.endproc

;*******************************************************************************
; MARK IDX
; Sets the fetch bit for the index in shidx.  Clobbers .A/.Y and shptr/shmask.
.proc mark_idx
	jsr vish::shadow_bitptr
	lda #<SHADOW_FETCH
	ldy #>SHADOW_FETCH
	jsr vish::shadow_addbase
	ldy #$00
	lda (shptr),y
	ora shmask
	sta (shptr),y
	rts
.endproc



;*******************************************************************************
.proc mark_blk1k
@p=r0
@hi=r2
	lsr				; 128 bitmap bytes per block, so the
	sta @hi				; offset is block<<7 -- which does not fit
	lda #$00			; a byte for block 2 and 3, hence the
	bcc :+				; split into high and low halves here
	lda #$80
:	clc
	adc #<SHADOW_FETCH
	sta @p
	lda @hi
	adc #>SHADOW_FETCH
	sta @p+1
	lda #$ff
	ldy #128-1
:	sta (@p),y
	dey
	bpl :-
	rts
.endproc

;*******************************************************************************
; FW DECODE
; Recovers the geometry the chip is running on from fw_regs.  Called at the start
; of the walk and again whenever the frame writes one of the registers below.
; The two base addresses are 14-bit VIC addresses, not CPU ones.
.proc fw_decode
	lda fw_regs+2			; columns: $9002 bits 0-6
	and #$7f
	sta fw_cols

	lda fw_regs+3			; rows: $9003 bits 1-6 ...
	and #$7e
	lsr
	sta fw_rows
	lda fw_regs+3			; ... and bit 0 is the cell height
	and #$01
	beq @c8
	lda #15				; 8x16: cell depth counts 0..15
	bne @setll			; (always)
@c8:	lda #7				; 8x8:  ...0..7
@setll:	sta fw_lastline

	lda fw_regs+1
	sta fw_originy


	; screen_mem_start = (($9005 & $f0) << 6) | (($9002 & $80) << 2)
	; The first term is ($9005 >> 4) << 10, so it lands wholly in the high
	; byte as (v >> 4) << 2; the second is bit 9, high byte bit 1.
	lda #$00
	sta fw_vmbase
	sta fw_cbbase
	lda fw_regs+5
	lsr
	lsr
	lsr
	lsr
	asl
	asl
	sta fw_vmbase+1
	lda fw_regs+2
	bpl :+
	lda fw_vmbase+1
	ora #$02
	sta fw_vmbase+1

:	; char_mem_start = ($9005 & $0f) << 10
	lda fw_regs+5
	and #$0f
	asl
	asl
	sta fw_cbbase+1

	; How many cells does a line actually fetch?
	;
	; The matrix opens four cycles after the horizontal origin matches and
	; spends two cycles per cell, so a line has room for
	;     N = (CYCLES_PER_LINE - origin_x - 3) / 2
	; and a row wider than that never finishes: the new-line signal closes the
	; matrix wherever it got to, and VMC carries on from THERE rather than from
	; base + cols.  This used to stand the whole filter down; modelling it is
	; two subtractions.  VMC advances by one less than the number fetched --
	; the last cell of a row is fetched in the MATRIX_END state, which does not
	; increment.  Verified against the reference model for every origin_x
	; (see test_fetchmodel.py).
	;
	; Below origin_x 3 the matrix never opens at all and the line fetches
	; nothing, which is the chip's behaviour, not an approximation.
	lda fw_regs+0
	and #$7f			; horizontal origin
	sta fw_originx
	cmp #$03
	bcs @room
	lda #$00			; matrix never opens
	sta fw_ncells
	sta fw_vadv
	sta fw_over
	rts

@room:	lda #CYCLES_PER_LINE-3
	sec
	sbc fw_originx
	lsr				; N = (CYCLES_PER_LINE-3-origin_x)/2
	sta fw_ncells

	ldx fw_cols
	inx				; cols+1 cells wanted; wraps to 0 at 255,
	beq @over			; which certainly overruns
	cpx fw_ncells
	bcc @fits
	beq @fits

@over:	lda #$01
	sta fw_over
	; The row does not finish inside the line.  The new-line signal closes
	; the matrix wherever it got to, and on the way it adds ONE MORE
	; increment (the firmware's HC=1 "if state == FETCH_CHAR_DATA" case), so
	; the counter advances by the full cell count rather than one less --
	; consecutive rows abut instead of overlapping by a cell.
	lda fw_ncells
	sta fw_vadv
	rts

@fits:	lda #$00
	sta fw_over
	; The row completes and closes itself through MATRIX_END, which does not
	; increment -- so the last cell of a row is also the first of the next.
	stx fw_ncells
	dex
	stx fw_vadv
	rts
.endproc

;*******************************************************************************
; FW MARK CHARGEN
; Marks the character generator's span, if it has moved since the last time.
;
; The span is taken whole -- 256 glyphs at 8 or 16 bytes each -- rather than per
; glyph.  Working out which glyphs are actually on screen means knowing the
; matrix contents at every instant of the frame, and getting it wrong drops a
; write that shows.  A whole span is 2K or 4K of index space and costs 256 or 512
; bitmap bytes to mark, once per distinct base.
;
; Only VIC blocks 12-15 ($3000-$3FFF) are CPU $1000-$1FFF, which is the only part
; of the character generator that can be WRITTEN and so the only part that can be
; captured.  A generator in ROM at $8000 (VIC blocks 0-3) contributes nothing
; here, and neither do the unconnected blocks 4-7 and 9-11, which read open bus.
.proc fw_mark_chargen
@blk=r6
@n=r7
	lda fw_cbseen
	beq @go
	lda fw_cbbase+1			; unchanged?  already marked
	cmp fw_cbmark+1
	bne @go
	lda fw_cbbase
	cmp fw_cbmark
	beq @done

@go:	lda fw_cbbase
	sta fw_cbmark
	lda fw_cbbase+1
	sta fw_cbmark+1
	lda #$01
	sta fw_cbseen

	; 2K for 8-line cells, 4K for 16-line: two or four 1K blocks.
	lda #$02
	ldx fw_lastline
	cpx #15
	bne :+
	lda #$04
:	sta @n

	lda fw_cbbase+1			; block number = addr >> 10
	lsr
	lsr
	sta @blk

@lp:	lda @blk
	and #$0f			; the counter is 14-bit, so it wraps
	cmp #12				; blocks 12-15 are CPU $1000-$1FFF
	bcc @next
	and #$03
	pha
	tax
	lda bitmask_tab,x		; remember it as character data
	ora cg_blocks
	sta cg_blocks
	pla
	jsr mark_blk1k
@next:	inc @blk
	dec @n
	bne @lp
@done:	rts
.endproc

;*******************************************************************************
; FW READ CELLS
; The cells this raster line reads.  Every read does two things: it marks the
; address as one the chip looks at (which is what vis_prune still wants), and it
; CONFIRMS whatever write is pending there -- that write is now observable, so
; its record is needed.
;
; This runs on every line of a character row, not just the first.  It has to: the
; chip re-reads the matrix on every line, so a write landing mid-row is confirmed
; by the next line's read, and a per-row shortcut would miss exactly the writes
; that the old temporal filter used to have to reason about separately.
;
; Colour is not a second lookup -- it is read by the same fetch, at
; $9400 + (screen_addr & $3ff) -- but it is a separate ADDRESS with its own
; pending write, so both halves are confirmed.
.proc fw_read_cells
	lda fw_ncells
	beq @done
	sta fw_i

	lda fw_vmbase
	clc
	adc fw_vmc
	sta fw_cell
	lda fw_vmbase+1
	adc fw_vmc+1
	sta fw_cell+1

@lp:	lda fw_cell+1			; keep the counter 14-bit
	and #$3f
	sta fw_cell+1

	lda fw_cell			; colour: SHIDX_COLOR + (cell & $3ff)
	sta shidx
	lda fw_cell+1
	and #$03
	ora #>SHIDX_COLOR
	sta shidx+1
	jsr mark_idx
	jsr confirm_idx

	lda fw_cell+1			; matrix: only VIC blocks 12-15 are RAM
	and #$30
	cmp #$30
	bne @next
	lda fw_cell
	sta shidx
	lda fw_cell+1
	and #$0f
	sta shidx+1
	jsr mark_idx
	jsr confirm_idx

@next:	inc fw_cell
	bne :+
	inc fw_cell+1
:	dec fw_i
	bne @lp
@done:	rts
.endproc

;*******************************************************************************
; FW STEP
; Advances the chip model by one raster line, marking what that line fetches.
;
; Marking runs on the first line of a character row only.  Every raster line of a
; row reloads the video matrix counter from the same latch and so fetches the
; same cells; doing it once instead of eight times is the difference between the
; walk costing a tenth of a second and costing one.  fw_remark forces it anyway
; on a line the geometry moved under, where that reasoning does not hold.
.proc fw_step
	lda fw_inmat
	bne @in

	; outside the matrix: does this line open it?  The comparison is
	; (line >> 1) == $9001, so a line above 511 cannot match at all.
	lda fw_line+1
	lsr
	bne @out
	lda fw_line
	ror
	cmp fw_originy
	bne @out

	lda #$01			; the matrix opens here
	sta fw_inmat
	lda #$00
	sta fw_cdc
	lda fw_rows			; the row count was latched back at VC=0,
	sta fw_vcc			; which is why pixel-0 $9003 is the one

	; The line is fetched BEFORE the row budget is consulted.  The chip closes
	; the matrix at HC=1 of the FOLLOWING line, so a $9003 row count of zero
	; still reads one raster line's worth of cells -- and a filter that marked
	; nothing there would drop writes that show.  Checking fw_vcc up here
	; instead of below is exactly that bug, and it is not one any assert would
	; have caught: it needs a frame that blanks the display through $9003.
@in:	lda fw_vml			; HC=2: reload the counter from the latch
	sta fw_vmc
	lda fw_vml+1
	sta fw_vmc+1

	jsr fw_read_cells
	jsr fw_mark_chargen

@adv:	lda #$00
	sta fw_remark

	lda fw_vmc			; the counter ran on by one per cell FETCHED,
	clc				; which is not cols when the row overruns
	adc fw_vadv
	sta fw_vmc
	bcc :+
	inc fw_vmc+1

:	lda fw_cdc			; last line of the character row?
	cmp fw_lastline
	beq @rowend

	lda fw_vcc			; mid-row: only a zero row count reaches
	beq @close			; here with the budget already gone
	inc fw_cdc
	rts

@rowend:
	lda fw_vmc			; latch where the next row starts
	sta fw_vml
	lda fw_vmc+1
	sta fw_vml+1
	lda #$00
	sta fw_cdc
	lda fw_vcc
	beq @close
	dec fw_vcc
	bne @out

@close:	lda #$00
	sta fw_inmat
@out:	rts
.endproc

;*******************************************************************************
; FW CONSUME
; Applies every record whose cycle falls before fw_lineend to the model, holding
; the first one that does not for the next line.  Records are in cycle order and
; so is the walk, so the two are simply merged -- no timeline table is built,
; which matters because a program can write $900f every four cycles and there is
; no bound on how many such writes a frame holds.
.proc fw_consume
@lp:	lda fw_done
	beq :+
	rts				; chain exhausted (own rts: @out is now
:	lda fw_held			; out of branch range from here)
	bne @have
	jsr vish::next_record_raw
	bcs @newrec
	lda #$01			; chain exhausted
	sta fw_done
	rts
@newrec:
	incw fw_ord			; 1-based, so 0 can mean "nothing pending"

@have:	lda rec_tc+1			; past this line?  hold it
	cmp fw_lineend+1
	bcc @take
	bne @hold
	lda rec_tc
	cmp fw_lineend
	bcc @take
@hold:	lda #$01
	sta fw_held
	rts

@take:	lda #$00
	sta fw_held

	lda rec_addr+1			; only VIC register writes move the model
	cmp #$90
	bne @mem
	ldx rec_addr
	cpx #$10
	bcs @mem

	cpx #$04			; $9004 is read-only and $9006-$900f do
	beq @plain			; not select what gets fetched
	cpx #$06
	bcs @plain

	; $9005 -- and $9002 bit 7 with it -- supply screen_mem_start and
	; char_mem_start, which the chip reads AT FETCH TIME.  A write to one
	; partway along a line therefore moves the cells not yet fetched and
	; leaves the ones already fetched where they were.  Mark the line under
	; the OLD geometry here, before the write lands; fw_remark has fw_step
	; mark it again under the new one, and the union is what the chip read
	; across the two halves of the line.
	;
	; Nothing else needs this, which is the part that took a while to see.
	; Columns are latched into HCC at HC=2, rows into VCC at VC=0, character
	; height is read at the HC=1 cell-depth test, and both origins are
	; per-line compares -- so for all of those, applying the write at the next
	; line boundary IS exact, and that is already what this walk does.
	;
	; This used to raise vis_off and abandon filtering for the whole frame,
	; for any geometry write anywhere in the displayed area.
	lda fw_inmat
	beq @geom
	cpx #$05
	beq @old
	cpx #$02
	bne @geom
@old:	incw __cap_geom_unstable	; now a measure of how often the union path
	stx fw_savex			; runs, not of frames given up on
	lda fw_vml			; HC=2 has already reloaded the counter
	sta fw_vmc
	lda fw_vml+1
	sta fw_vmc+1
	jsr fw_read_cells		; clobbers .X
	ldx fw_savex

@geom:	lda rec_val
	sta fw_regs,x
	jsr fw_decode
	lda #$01			; re-mark this line under the new geometry
	sta fw_remark
	jmp @lp

@plain:	lda rec_val
	sta fw_regs,x
	jmp @lp

@mem:	; A screen, character or colour write.  Whatever was pending for this
	; address is simply overwritten -- nothing read it, so it never mattered.
	ldxy rec_addr
	jsr vish::shadow_index
	jsr pend_write
	jmp @lp
@out:	rts
.endproc

;*******************************************************************************
; FETCH WALK
; Builds SHADOW_FETCH: the set of addresses the VIC reads somewhere in this
; frame.  See the VISIBILITY notes for why this is both safe and worth doing.
;
; Must run after build_shadow, which is what makes shadow_vic_val answer with the
; frame's pixel-0 VIC state rather than a stale dirty bitmap.  Leaves the record
; cursor at the end of the chain; the caller rewinds it.
; Assumes SHADOW_BANK is mapped into BLK2.
.proc fetch_walk
	lda #<SHADOW_FETCH
	ldy #>SHADOW_FETCH
	jsr vish::clear_bmp
	jsr clear_needed
	jsr clear_pending

	ldx #$0f			; seed the model with pixel-0 registers
:	jsr vish::shadow_vic_val
	sta fw_regs,x
	dex
	bpl :-
	jsr fw_decode

	; The chip clears the matrix latch and counter during vertical sync, so
	; the frame opens with both at zero however the last one ended.
	lda #$00
	sta fw_vmc
	sta fw_vmc+1
	sta fw_vml
	sta fw_vml+1
	sta fw_cdc
	sta fw_inmat
	sta fw_held
	sta fw_done
	sta fw_remark
	sta fw_cbseen
	sta cg_blocks
	sta fw_ord
	sta fw_ord+1
	sta fw_line
	sta fw_line+1
	lda fw_rows			; latched at VC=0, before the matrix opens
	sta fw_vcc

	lda #<CYCLES_PER_LINE
	sta fw_lineend
	lda #>CYCLES_PER_LINE
	sta fw_lineend+1

	jsr vish::gen_read_reset

@lp:	jsr vis_poll		; user interrupt?
	bne @abort

	jsr fw_consume
	jsr fw_step

	incw fw_line
	lda fw_lineend
	clc
	adc #<CYCLES_PER_LINE
	sta fw_lineend
	lda fw_lineend+1
	adc #>CYCLES_PER_LINE
	sta fw_lineend+1

	; another whole line inside the frame?  The walk is driven by cycles
	; rather than a line count so it needs nothing from the simulator but
	; the frame length -- which is what varies with interlace.
	lda frame_cyc
	cmp fw_lineend
	lda frame_cyc+1
	sbc fw_lineend+1
	bcs @lp

@vic:	; $9000-$900f: every one of them is read continuously, so the whole
	; block is fetched by definition.
	ldx #$0f
:	txa
	sta shidx
	lda #>SHIDX_VIC
	sta shidx+1
	txa
	pha
	jsr mark_idx
	pla
	tax
	dex
	bpl :-
	rts

@abort:	; an interrupted walk leaves a bitmap that is missing marks, and a
	; missing mark drops a write that shows.  Stand the filter down.
	lda #$01
	sta vis_off
	rts
.endproc

;*******************************************************************************
; VIS PRUNE
; Clears the dirty bit of every address the VIC never reads, so the restore block
; is not obliged to re-establish a value nothing can see.  need_count is rebuilt
; from what survives -- it is what the restore budget and will_be_exact are
; measured against, so leaving it counting dropped addresses would keep reporting
; a frame as busy after the work had gone away.
;
; The RMW bitmap is pruned with it.  An RMW on an invisible address is never
; re-emitted, so it cannot need its input restored.
; Assumes SHADOW_BANK is mapped into BLK2.
;
; The three bitmaps share one layout, so this walks them a BYTE at a time rather
; than an address at a time: DIRTY &= FETCH, RMW &= FETCH, and the counts fall
; out of a popcount of what is left.  640 iterations instead of 5120, and no
; index arithmetic at all.
VIS_PRUNE_BYTES = SHIDX_VIC/8		; the VIC region is never pruned -- those
					; addresses are always read, and their
					; obligation belongs to the seed block
					; rather than the restore block
.proc vis_prune
@d=r0
@r=r2
@f=r6
@bits=r8			; @bits+1 is the popcount, so @n cannot be r9
@n=ra
	lda vis_enable
	beq @out
	lda vis_off
	beq @go
@out:	rts

@go:	lda #$00			; recount from what survives
	sta need_count
	sta need_count+1
	sta rmw_count
	sta rmw_count+1

	lda #<SHADOW_DIRTY
	sta @d
	lda #>SHADOW_DIRTY
	sta @d+1
	lda #<SHADOW_RMW
	sta @r
	lda #>SHADOW_RMW
	sta @r+1
	lda #<SHADOW_FETCH
	sta @f
	lda #>SHADOW_FETCH
	sta @f+1

	lda #<VIS_PRUNE_BYTES
	sta @n
	lda #>VIS_PRUNE_BYTES
	sta @n+1

@lp:	jsr vis_poll			; 640 popcounts is long enough to want
	bne @quit			; a way out of

	ldy #$00
	lda (@f),y
	sta @bits			; the mask of survivors in this byte

	lda (@d),y
	and @bits
	sta (@d),y
	jsr @count
	lda need_count
	clc
	adc @bits+1
	sta need_count
	bcc :+
	inc need_count+1

:	ldy #$00
	lda (@r),y
	and @bits
	sta (@r),y
	jsr @count
	lda rmw_count
	clc
	adc @bits+1
	sta rmw_count
	bcc :+
	inc rmw_count+1

:	inc @d
	bne :+
	inc @d+1
:	inc @r
	bne :+
	inc @r+1
:	inc @f
	bne :+
	inc @f+1

:	lda @n
	bne :+
	dec @n+1
:	dec @n
	lda @n
	ora @n+1
	bne @lp
@quit:	rts

@count:	; bits set in .A -> @bits+1 (the mask itself stays in @bits)
	ldx #$00
	ldy #$08
:	lsr
	bcc :+
	inx
:	dey
	bne :--
	stx @bits+1
	rts
.endproc
