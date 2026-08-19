;*******************************************************************************
; Video frame lengths for the VIC-20
; CPU cycles per frame = cycles-per-line * lines
; Used by the simulator to wrap the raster position and by the frame capture
; engine to detect an interlaced frame.
;*******************************************************************************

.ifndef VIC20_FRAME_INC
VIC20_FRAME_INC = 1

; The VIC-I fetches on phi1 and the CPU runs on phi2, so there is no cycle
; stealing and no badlines: every line is exactly CYCLES_PER_LINE cycles
; whether the display is on or off.  Blanking buys beam time, never CPU time.
.ifdef PAL
CYCLES_PER_LINE  = 71
FRAME_LINES      = 312		; PAL (6561) frame, no interlace
FRAME_CYCLES     = CYCLES_PER_LINE*FRAME_LINES		; 22152
.else
CYCLES_PER_LINE  = 65
; 261, and the temptation to "fix" this to 262 is a trap worth documenting.
;
; The 6560's vertical counter DOES take 262 values (0..261): it resets when VC
; equals NTSC_NORM_LAST_LINE, so the count is last+1.  But VC is not a raster
; line -- it increments at HC=29 and resets at HC=62, so the first VC value
; lasts 32 cycles and the last one 33.  Those two make one line between them,
; and 262 VC values span 261 lines of TIME:
;     32 + 260*65 + 33 = 16965
; The firmware says so outright: "unlike the 6561 PAL chip, the VC and raster
; line are NOT equivalent in the 6560".
;
; Do not sanity-check this against broadcast NTSC either.  16965 cycles is
; 60.28Hz, not 59.94 -- the NTSC VIC-20 genuinely runs fast, which is why it is
; fussy on some displays.  PAL is the simple case: on the 6561 VC IS the raster
; line, so 312 lines is both the VC count and the line count.
;
; Cross-checked against github.com/sodiumlb/ocula-pivic-firmware (vic_ntsc.c,
; the HC=29 increment and HC=62 reset; vic_pal.c PAL_LAST_LINE).
;
; That cross-check is now mechanical: src/test_frameperiod.py transcribes both
; chips' VC/HC state machines out of that firmware and counts cycles between
; resets.  It reports 16965 for NTSC, with VC=0 held 32 cycles and VC=261 held
; 33, every other VC value 65 -- and 22152 for PAL, where all 312 are whole.
; Run it before changing any number in this file.
;
; The structural difference, which is what the trap is made of: PAL does
;     if (VC == 311) VC = 0; else VC++;
; at HC=1, so the reset REPLACES an increment and VC visits 312 whole lines.
; NTSC increments at HC=29 and resets at HC=62 as SEPARATE events in the same
; sweep, so VC=0 is not reached by an increment at all -- it exists only for the
; 32 cycles between the reset and the next increment.
FRAME_LINES      = 261		; NTSC (6560) non-interlaced frame
FRAME_LINES_INT  = 525		; NTSC interlaced full frame (both fields);
				; 2*262.5, which is what the half-line counter does
FRAME_CYCLES     = CYCLES_PER_LINE*FRAME_LINES		; 16965
FRAME_CYCLES_INT = CYCLES_PER_LINE*FRAME_LINES_INT	; 34125
.endif

.endif
