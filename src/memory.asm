;*******************************************************************************
; MEMORY.ASM
; This file contains reservations for important shared RAM locations used
; by Monster.  Notably this includes the "line" and "assembly" buffers.
;*******************************************************************************

.include "config.inc"
.include "layout.inc"

.import __SPARESIZE__


;*******************************************************************************
.segment "SPARE"
.export __mem_spare
.export __mem_spareend
.assert * & $ff = 0, error, "spare must be algined to page boundary"
__mem_spare: .res SPARESIZE
__mem_spareend:

;*******************************************************************************
; SPAREVEC
; This vector resides in a location unused by user memory
.ifdef ultimem
.segment "ULTICFG"
.endif
.export __mem_sparevec
__mem_sparevec: .word 0

;*******************************************************************************
.BSS
.export __mem_ctxbuffer
__mem_ctxbuffer = $140+40	; buffer for a line of context during assembly

.export __statusline
__statusline = __mem_spare+80

.export __statusinfo
__statusinfo: .res 20

.export __mem_drive_err
__mem_drive_err: .res 23

.export __linesave
__linesave: .res 40

.export __mem_coloron
__mem_coloron: .byte 0

.export __mem_rowcolors
__mem_rowcolors: .res SCREEN_HEIGHT
;.assert >(*) = >(__mem_rowcolors), error, "mem::rowcolors must not cross page boundary (used in stable IRQ)"

.export __mem_rowcolors_idx
__mem_rowcolors_idx: .res 24

.export __mem_rowcolors_save
__mem_rowcolors_save: .res 24

.export __mem_filename
__mem_filename: .res 16	; buffer for output file

.export __mem_findbuff
__mem_findbuff: .res MAX_SEARCH_LEN+1	; +1 for terminating 0

.segment "BSS_NOINIT"
.export __mem_init_sig
__mem_init_sig: .res 4

;*******************************************************************************
; LINEBUFFER
; The linebuffer must live in lower RAM, which is NOT switched out with the
; upper RAM upon switching banks
; This allows the buffer to be manipulated from any bank
.segment "LINEBUFF"

.export __linebuffer
__linebuffer:
.assert * & $ff = $00, error, "line buffer not page-aligned"
	.res LINESIZE

.export __linebuffer2
__linebuffer2:
	.res LINESIZE	; backup buffer for when the linebuffer must be saved

.export __mem_asmbuffer
__mem_asmbuffer:
	.res LINESIZE+1
