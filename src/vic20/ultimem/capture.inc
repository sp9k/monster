;*******************************************************************************
; CAPTURE.INC
; Interface to the frame capture/replay engine (capture.asm).
; The engine lives in the SIM bank (FINAL_BANK_SIM), so callers outside that
; bank must reach it with CALL/JUMP.
;*******************************************************************************

.ifndef CAPTURE_INC
CAPTURE_INC = 1

.import __cap_record
.import __cap_reset
.import __cap_trigger

.scope cap
	; records a simulated store (.XY = address, .A = value) if it can affect
	; the display; ignores it otherwise
	record  = __cap_record

	; rewinds the capture chain to its head, beginning a new (empty) frame
	reset   = __cap_reset

	; shows the captured frame: swaps in the user's screen/VIC/color and runs
	; the raster-synced replay until a key is pressed
	trigger = __cap_trigger

	; set !0 to show a frame that cannot be replayed exactly.  cap::trigger
	; returns .C clear without showing anything when it finds one and this is
	; clear -- the judgement needs pixel-0 VIC state, which only exists once
	; trigger has the user's registers live, so it cannot be made earlier.
.endscope

.endif
