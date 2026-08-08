.include "settings.inc"
.include "../draw.inc"
.include "../screen.inc"
.include "../irq.inc"
.include "../macros.inc"
.include "../memory.inc"

SCREEN_ROWS = 24

.export __prefs_reverse_color
.export __prefs_text_color
.export __prefs_normal_color
.export __prefs_brkon_color
.export __prefs_brkoff_color
.export __prefs_success_color
.export __prefs_select_col
.export __prefs_error_color

.DATA
.export __prefs_palette

__prefs_palette:
palette:
__prefs_text_color:    .byte TEXT_COLOR
__prefs_normal_color:  .byte DEFAULT_900F
__prefs_reverse_color: .byte DEFAULT_RVS
__prefs_brkon_color:   .byte BREAKPOINT_ON_COLOR
__prefs_brkoff_color:  .byte BREAKPOINT_OFF_COLOR
__prefs_success_color: .byte ASM_SUCCESS_COLOR
__prefs_select_col:    .byte GUI_SELECT_COLOR
__prefs_error_color:   .byte ERROR_COLOR

pal_num: .byte 0

; must be in always-visible RAM: called from banked code on the cart build
.segment "DATA"
;*******************************************************************************
; PALETTES
; 0: default
; 1: white-on-black
; 2: black-on-white
; 3: yellow-on-black
palettes:
text_colors:    .byte TEXT_COLOR,           $01, $00, $07
normal_colors:  .byte DEFAULT_900F,         $08, $19, $08
reverse_colors: .byte DEFAULT_RVS,          $00, $11, $00
brkon_colors:   .byte BREAKPOINT_ON_COLOR,  $98, $99, $98
brkoff_colors:  .byte BREAKPOINT_OFF_COLOR, $e8, $e9, $e8
success_colors: .byte ASM_SUCCESS_COLOR,    $00, $11, $00
select_color:   .byte GUI_SELECT_COLOR,     $e8, $e9, $e8
error_colors:   .byte ERROR_COLOR,          ERROR_COLOR, ERROR_COLOR, ERROR_COLOR

NUM_PALETTES = 4
NUM_TABLES   = 8

.CODE

;*******************************************************************************
; NEXT PAL
; Cycles to the next palette definition
.export __prefs_next_pal
.proc __prefs_next_pal
	ldx pal_num
	inx
	cpx #NUM_PALETTES
	bcc set_pal
	ldx #$00
	beq set_pal	; branch always
.endproc

;*******************************************************************************
; PREV PAL
; Cycles to the previous palette definition
.export __prefs_prev_pal
.proc __prefs_prev_pal
	ldx pal_num
	dex
	bpl set_pal
	ldx #NUM_PALETTES-1

	; fall through to set_pal
.endproc

;*******************************************************************************
; SET PAL
; Sets the palette to the one of the given id
; IN:
;   - .X: the id of the palette to switch to
.proc set_pal
@pal=r0
	jsr irq::off
	stx pal_num

	ldxy #palettes
	stxy @pal

	ldx #$00
@l0:	ldy pal_num
	lda (@pal),y
	sta palette,x

	lda @pal
	clc
	adc #NUM_PALETTES
	sta @pal
	bcc :+
	inc @pal+1
:	inx
	cpx #NUM_TABLES
	bne @l0

	jsr scr::clrcolor
	lda __prefs_normal_color
	jsr draw::refresh_colors
	jmp irq::on
.endproc

;*******************************************************************************
; LOAD
; Loads user preferences from the "PREFS" file
.export __prefs_load
.proc __prefs_load
	; TODO:
.endproc
