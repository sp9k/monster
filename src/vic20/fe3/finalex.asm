;*******************************************************************************
; FINALEX.ASM
; This file contains routines for reading, writing, and executing code in
; different banks.
; The bank code itself resides in low RAM, where it is visible regardless of
; the active bank.
;*******************************************************************************

.include "banks.inc"
.include "../../config.inc"
.include "../../inline.inc"
.include "../../macros.inc"
.include "../../zeropage.inc"

.import __boot_start
.import __ram_src
.import __ram_dst

.import __SETUP_LOAD__
.import __SETUP_RUN__
.import __SETUP_SIZE__

.import __BANKCODE_LOAD__
.import __BANKCODE_RUN__
.import __BANKCODE_SIZE__

.import __BANKCODE2_LOAD__
.import __BANKCODE2_RUN__
.import __BANKCODE2_SIZE__

.import __BSS_LOAD__
.import __BSS_SIZE__

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.import __DEBUGINFO_CODE_LOAD__
.import __DEBUGINFO_CODE_RUN__
.import __DEBUGINFO_CODE_SIZE__

.import __FASTTEXT_LOAD__
.import __FASTTEXT_SIZE__
.import __FASTTEXT_RUN__

.import __MACROCODE_LOAD__
.import __MACROCODE_RUN__
.import __MACROCODE_SIZE__

.import __VSCREEN_LOAD__
.import __VSCREEN_RUN__
.import __VSCREEN_SIZE__

.import __IRQ_LOAD__
.import __IRQ_RUN__
.import __IRQ_SIZE__

.import __LINKER_LOAD__
.import __LINKER_RUN__
.import __LINKER_SIZE__

.import __OBJCODE_LOAD__
.import __OBJCODE_RUN__
.import __OBJCODE_SIZE__

.import __LABELS_LOAD__
.import __LABELS_RUN__
.import __LABELS_SIZE__

.import __FASTCOPY_LOAD__
.import __FASTCOPY_RUN__
.import __FASTCOPY_SIZE__

.import __EXPR_LOAD__
.import __EXPR_RUN__
.import __EXPR_SIZE__

.import __UDGEDIT_LOAD__
.import __UDGEDIT_RUN__
.import __UDGEDIT_SIZE__

.import __CONSOLE_LOAD__
.import __CONSOLE_RUN__
.import __CONSOLE_SIZE__

.import __COPYBUFF_LOAD__
.import __COPYBUFF_RUN__
.import __COPYBUFF_SIZE__

.import __RODATA_LOAD__
.import __RODATA_RUN__
.import __RODATA_SIZE__

.linecont +
.export TOTAL_SIZE
TOTAL_SIZE = __SETUP_SIZE__+__BANKCODE_SIZE__+__BANKCODE2_SIZE__+__DATA_SIZE__+\
	     __FASTTEXT_SIZE__+__MACROCODE_SIZE__+__VSCREEN_SIZE__+ \
	     __IRQ_SIZE__+__LINKER_SIZE__+__LABELS_SIZE__+__UDGEDIT_SIZE__+ \
	     __EXPR_SIZE__ + __CONSOLE_SIZE__+__COPYBUFF_SIZE__+ \
	     __RODATA_SIZE__ + __DEBUGINFO_CODE_SIZE__+__FASTCOPY_SIZE__+ \
	     __OBJCODE_SIZE__
.linecont -

.export __fe3_bank
__fe3_bank = $9c02

;*******************************************************************************
; Rest of cart initialization code (when running from cart)
.ifdef CART
.segment "CART"
	ldx #@end-@unlock
:	lda @unlock-1,x
	sta $200-1,x
	dex
	bne :-

	; run the unlock code
	jmp $200

;-----------------------
@unlock:
	ldx $a000
	stx $a000
	sta $9c02
	cmp $9c02

	; activate ROM bank 0
	lda #$40
	sta $9c02

	; copy SETUP
	ldxy #$2000
	stxy r0
	ldxy #__SETUP_RUN__
	stxy r2

	ldx #>TOTAL_SIZE+1	; # of pages to copy
	ldy #$00

@reloc: ; read from ROM bank and write to RAM bank
	lda (r0),y
	sta (r2),y
	iny
	bne @reloc
	inc r0+1
	inc r2+1
	dex			; next page
	bne @reloc

	jmp __boot_start
@end:
.endif

;*******************************************************************************
; RELOC
; relocates code from 1 address to another
; IN:
;  - .A: destination bank
;  - r0r1: source address
;  - r2r3: dest address
;  - r4:   number of bytes to copy
.macro reloc BANK
@src=r0
@dst=r2
@size=r4
@bank=r6
	sta @bank
	lda @size+1
	beq @lastpage

	ldy #$00
@pageloop:
	lda #BANK
	sta $9c02	; source bank

	lda (@src),y

	ldx @bank
	stx $9c02	; dest bank

	sta (@dst),y
	iny
	bne @pageloop
	inc @src+1
	inc @dst+1
	dec @size+1
	bne @pageloop

@lastpage:
	ldy @size
	beq @done
	dey
:	lda #$a1
	sta $9c02	; source bank
	lda (@src),y
	ldx @bank
	stx $9c02	; dest bank
	sta (@dst),y
	dey
	cpy #$ff
	bne :-

@done:	lda #BANK
	sta $9c02
.endmacro

;*******************************************************************************
; DRAWLOGO
.macro drawlogo
	; set all color to blue
	lda #$06
	ldx #$00
:	sta $9400,x
	dex
	bne :-

.ifdef NTSC
	ldx #$08
	ldy #$30
.else
	ldx #$10
	ldy #$30
.endif
	stx $9000		; horizontal centering

	lda #$30
	sta $9001

	; draw the boot screen
	ldx #18*7
:	lda bootlogo-1,x
	sta $1000-1,x
	dex
	bne :-

	; now update the # of rows and columns to make the screen visible
	lda #18			; # of columns
	sta $9002
	lda #7<<1		; # of rows
	sta $9003
.endmacro

.segment "SETUP"

;*******************************************************************************
; INIT 0
; Relocates the various banks of ROM code to RAM
.export __fe3_init0
.proc __fe3_init0
@cnt=r7
@relocs=r8
	drawlogo

	lda #num_relocs
	sta @cnt
	ldxy #relocs
	stxy @relocs
@reloc:	ldy #$00
	lda (@relocs),y
	sta r0
	iny
	lda (@relocs),y
	sta r0+1

	; destination
	iny
	lda (@relocs),y
	sta r2
	iny
	lda (@relocs),y
	sta r2+1

	; size
	iny
	lda (@relocs),y
	sta r4
	iny
	lda (@relocs),y
	sta r4+1

	; bank
	iny
	lda (@relocs),y

	reloc FINAL_BANK_MAIN

	lda @relocs
	clc
	adc #$07
	sta @relocs
	bcc :+
	inc @relocs+1
:	dec @cnt
	bne @reloc
.endproc

;*******************************************************************************
; INIT1
; CART init code; copy the application from ROM bank 1
.export __fe3_init1
.proc __fe3_init1
	; copy the app and enter it
	lda #$41	; ROM 32k page #1
	sta $9c02

	; copy everything from $2000-$8000
	ldxy #$2000
	stxy r0
	ldx #$60	; 96 pages
	ldy #$00
@l0:	lda (r0),y	; reads from ROM in ROM bank 1
	sta (r0),y	; writes go to RAM in RAM bank 1
	iny
	bne @l0
	inc r0+1	; next page
	cpx #$30
	bne :+
	ldy #$00
:	dex
	bne @l0

	lda #FINAL_BANK_MAIN
	sta $9c02
	rts
.endproc

;*******************************************************************************
; RELOCS
; Table of start and target addresses for segments that need to be relocated
relocs:
; BANK CODE
.word __BANKCODE_LOAD__, __BANKCODE_RUN__, __BANKCODE_SIZE__
.byte FINAL_BANK_MAIN

; BANK CODE 2
.word __BANKCODE2_LOAD__, __BANKCODE2_RUN__, __BANKCODE2_SIZE__
.byte FINAL_BANK_MAIN

; DATA
.word __DATA_LOAD__, __DATA_RUN__, __DATA_SIZE__
.byte FINAL_BANK_MAIN

; DEBUGINFO_CODE
.word __DEBUGINFO_CODE_LOAD__, __DEBUGINFO_CODE_RUN__, __DEBUGINFO_CODE_SIZE__
.byte FINAL_BANK_DEBUG

; FASTTEXT
.word __FASTTEXT_LOAD__, __FASTTEXT_RUN__, __FASTTEXT_SIZE__
.byte FINAL_BANK_FASTTEXT

; MACRO
.word __MACROCODE_LOAD__, __MACROCODE_RUN__, __MACROCODE_SIZE__
.byte FINAL_BANK_MACROS

; IRQ
.word __IRQ_LOAD__, __IRQ_RUN__, __IRQ_SIZE__
.byte FINAL_BANK_MAIN

; SCREEN
.word __VSCREEN_LOAD__, __VSCREEN_RUN__, __VSCREEN_SIZE__
.byte FINAL_BANK_VSCREEN

; LINKER
.word __LINKER_LOAD__, __LINKER_RUN__, __LINKER_SIZE__
.byte FINAL_BANK_LINKER

; OBJCODE
.word __OBJCODE_LOAD__, __OBJCODE_RUN__, __OBJCODE_SIZE__
.byte FINAL_BANK_LINKER

; LABELS
.word __LABELS_LOAD__, __LABELS_RUN__, __LABELS_SIZE__
.byte FINAL_BANK_SYMBOLS

; EXPR
.word __EXPR_LOAD__, __EXPR_RUN__, __EXPR_SIZE__
.byte FINAL_BANK_UDGEDIT

; UDG EDITOR
.word __UDGEDIT_LOAD__, __UDGEDIT_RUN__, __UDGEDIT_SIZE__
.byte FINAL_BANK_UDGEDIT

; CONSOLE
.word __CONSOLE_LOAD__, __CONSOLE_RUN__, __CONSOLE_SIZE__
.byte FINAL_BANK_MONITOR

; COPYBUFF
.word __COPYBUFF_LOAD__, __COPYBUFF_RUN__, __COPYBUFF_SIZE__
.byte FINAL_BANK_BUFF

; FASTCOPY
.word __FASTCOPY_LOAD__, __FASTCOPY_RUN__, __FASTCOPY_SIZE__
.byte FINAL_BANK_FASTCOPY

; RODATA
.word __RODATA_LOAD__, __RODATA_RUN__, __RODATA_SIZE__
.byte FINAL_BANK_MAIN

num_relocs=(*-relocs)/7

;*******************************************************************************
bootlogo:
	.include "bootlogo.dat"

.segment "BANKCODE"

;*******************************************************************************
; STORE_BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; IN:
;  - .XY:         the address to store to
;  - .A:          the bank to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export __ram_store_byte
.proc __ram_store_byte
	pha
	lda #$00
	sta zp::bankoffset
	pla
	; fall through
.endproc

;*******************************************************************************
; STORE_BYTE_REL
; stores the byte given in zp::bankval to the address in .XA in bank .A
; IN:
;  - .XY:            the base address
;  - .A:             the bank to store to
;  - zp::bankoffset: the offset from the base address
;  - zp::bankval:    the byte to write
.export __ram_bank_store_rel
.proc __ram_bank_store_rel
@dst=zp::banktmp
	stxy @dst

	sta $9c02
	lda zp::bankval
	ldy zp::bankoffset
	sta (@dst),y

	ldx #$80
	stx $9c02	; restore bank
	ldxy @dst
	rts
.endproc

;*******************************************************************************
; READ_BYTE
; Returns the byte in bank .A at address .YX
; IN:
;  - .XY: the address to read from
;  - .A:  the bank to read from
; OUT:
;  - .A: the byte that was read
.export __ram_load_byte
.proc __ram_load_byte
	pha
	lda #$00
	sta zp::bankval
	pla
	; fall through
.endproc

;*******************************************************************************
; LOAD_BYTE_OFF
; Returns the byte in bank .A at address .YX plus a given offset
; IN:
;  - .XY: the address to read from
;  - .A: the bank to read from
;  - zp::bankval: the offset to read from
; OUT:
;  - .A: the byte that was read
;  - .Y: contains the offset (same that was given as zp::bankval)
.export __ram_load_byte_off
.proc __ram_load_byte_off
@src=zp::banktmp
	stxy @src
	sta $9c02	; set bank
	ldy zp::bankval
	lda (@src),y
	ldx #$80
	stx $9c02	; restore bank
	ldx @src
	rts
.endproc

;*******************************************************************************
; RETURN TO X
; Sets the bank to the given bank and returns (RTS)
; IN:
;  - .X: the bank to return to
.proc return_to_x
@done:	stx $9c02	; restore bank
	rts
.endproc

;*******************************************************************************
; COPY LINE
; Copies up to LINESIZE bytes from zp::bankaddr0 to zp::bankaddr1 stopping at
; the first $0d or $00
; IN:
;  - .A:            the bank to perform the copy within
;  - zp::bankaddr0: the source address to copy from
;  - zp::bankaddr1: the destination address to copy to
;  OUT:
;   - .Y: the number of bytes copied
;   - .A: the last byte copied
.export __ram_copy_line
.proc __ram_copy_line
	ldx $9c02	; get current bank
	sta $9c02	; set bank to copy within
	ldy #$00
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	beq return_to_x
	cmp #$0d
	beq @done
	iny
	cpy #MAX_LINE_LEN
	bne :-
@done:	beq return_to_x	; branch always (restore bank)
.endproc

.CODE

;*******************************************************************************
; GET BYTE
; When called from a non-MAIN bank, returns the value from the MAIN bank for the
; the given address.
; IN:
;   - .XY: the address to get the value of
; OUT:
;   - .A: the value for the given address in the MAIN bank
; CLOBBERS:
;   - .A, .Y, r0-r1
.export __ram_get_byte
.proc __ram_get_byte
	stxy r0
	ldy #$00
	lda (r0),y
	rts
.endproc

;*******************************************************************************
; MEMCPY
; Writes the memory from one bank/address to another
; The number of bytes is given in .YX and the block # to write to is given in .A
; This routine assumes that IF the memory overlaps, that it will do so from
; the TOP. (dst > src)
; IN:
;  - .XY:      the number of bytes to copy
;  - ram::src: the source address (24-bit)
;  - ram::dst: the destination address (24-bit)
.export __ram_memcpy
.proc __ram_memcpy
@size    = r0
@src     = __ram_src
@dst     = __ram_dst
@bank    = __ram_src+2
@bankdst = __ram_dst+2
	stxy @size
	iszero @size
	beq @done

	decw @size

	; we need to copy from top to bottom- add @size-1 to the dst and src
	ldxy @src
	add16 @size
	stxy @src

	ldxy @dst
	add16 @size
	stxy @dst

	incw @size

@l0:	; read a byte from the source bank/addr
	ldxy @src
	lda @bank
	jsr __ram_load_byte

	; write the byte to the dest bank/addr
	sta zp::bankval
	ldxy @dst
	lda @bankdst
	jsr __ram_store_byte

	; move to the next location
	decw @src
	decw @dst

	decw @size
	lda @size
	bne @l0
	lda @size+1
	bne @l0

@done:	rts
.endproc

.segment "BANKCODE2"
;*******************************************************************************
; PUSH BANK
; Saves the current RAM bank
.export __fe3_push_bank
.proc __fe3_push_bank
	lda $9c02		; get current bank
	ldx zp::banksp
	inc zp::banksp
	sta zp::bankstack,x	; save current bank
	rts
.endproc

;*******************************************************************************
; POP BANK
; Restores the last pushed bank
.export __fe3_pop_bank
.proc __fe3_pop_bank
	dec zp::banksp
	ldx zp::banksp
	lda zp::bankstack,x	; get the caller's bank
	sta $9c02		; restore bank
	rts
.endproc
