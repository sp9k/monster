;*******************************************************************************
; MEM.ASM (hard8x8)
; RAM reservations used ONLY by the hard8x8 (text mode) port. This is mainly
; state related to the "gutter" (column 0 on the physical screen) which is
; used for breakpoints and to display the source line when debugging.
;*******************************************************************************

.include "layout.inc"

.BSS

;*******************************************************************************
.export __mem_highlight_rows
__mem_highlight_rows: .res SCREEN_HEIGHT

;*******************************************************************************
.export __mem_blink_cell
__mem_blink_cell:  .word 0	; screen-cell address of the blinking gutter cell
.export __mem_blink_row
__mem_blink_row:   .byte 0	; screen row of the blinking cell (IRQ re-validates)
.export __mem_blink_dchar
__mem_blink_dchar: .byte 0	; breakpoint-glyph XOR '>'-marker-glyph
.export __mem_blink_dclr
__mem_blink_dclr:  .byte 0	; breakpoint-color XOR marker-color
.export __mem_blink_active
__mem_blink_active: .byte 0	; !0 if a row is currently blinking (also a lock)
.export __mem_blink_phase
__mem_blink_phase:  .byte 0	; which indicator shows now (0=breakpoint, 1=marker)
.export __mem_blink_cnt
__mem_blink_cnt:    .byte 0	; IRQ frames until the next toggle
