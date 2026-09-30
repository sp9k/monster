;*******************************************************************************
; IMAGE.ASM
; This file contains C64/REU-specific procedures for writing the output image
; during linking.
;*******************************************************************************
.include "../image.inc"
.include "../macros.inc"
.include "../ram.inc"
.import __image_page

;*******************************************************************************
.segment "IMAGE_VARS"
dma_byte:   .byte $00
saved_port: .byte $00
fill_chunk: .byte $00

BANKED_SEG "LINKER_AUX", FINAL_BANK_LINKER_AUX

;*******************************************************************************
; BEGIN DMA
; Configures the REU for an immediate one-byte transfer.
; IN:
;   - IRQs masked
; OUT:
;   - None
.proc begin_dma
	lda $01
	sta saved_port
	ora #$06
	sta $01
	lda #<dma_byte
	sta $df02
	lda #>dma_byte
	sta $df03
	lda #$00
	sta $df08
	sta $df0a
	lda #$01
	sta $df07
	rts
.endproc

;*******************************************************************************
; BYTE ADDRESS
; Computes the physical REU address of the image cursor and configures the REU
; to point to it.
; IN:
;   - image::cursor: validated image offset
; OUT:
;   - None
.proc byte_address
	lda image::cursor
	sta $df04
	lda image::cursor+1
	sta $df05

	clc
	lda image::cursor+2
	adc #^IMAGE_REU_BASE
	sta $df06			; set REU to base + cursor
	rts
.endproc

;*******************************************************************************
; LOAD BYTE
; Reads one byte from the REU image.
; IN:
;   - image::cursor: validated image offset.
; OUT:
;   - .A: byte read
.export __image_backend_load
.proc __image_backend_load
	php

	sei
	jsr begin_dma
	jsr byte_address
	lda #$91
	sta $df01
	lda saved_port
	sta $01

	plp
	lda dma_byte
	rts
.endproc

;*******************************************************************************
; STORE BYTE
; Writes one byte to the REU image.
; IN:
;   - .A: byte to store
;   - image::cursor: image offset to store at
; OUT:
;   - None
.export __image_backend_store
.proc __image_backend_store
	php
	sei

	sta dma_byte
	jsr begin_dma
	jsr byte_address
	lda #$90
	sta $df01
	lda saved_port
	sta $01

	plp
	rts
.endproc

;*******************************************************************************
; FILL PAGE
; Fills an REU page using 256-byte transfers.
; IN:
;   - .A: fill byte
;   - __image_page: page index
; OUT:
;   - None
.export __image_backend_fill
.proc __image_backend_fill
	sta dma_byte
	lda #$00
	sta fill_chunk

@chunk:	php
	sei

	jsr begin_dma

	lda #$00
	sta $df04
	sta $df07
	lda #$01
	sta $df08
	lda #$80 ; hold C64 source fixed, advance REU destination
	sta $df0a
	lda __image_page
	and #$07
	asl
	asl
	asl
	asl
	asl
	ora fill_chunk
	sta $df05

	lda __image_page
	lsr
	lsr
	lsr
	clc
	adc #^IMAGE_REU_BASE
	sta $df06

	lda #$90
	sta $df01
	lda saved_port
	sta $01

	plp
	inc fill_chunk
	lda fill_chunk
	cmp #$20
	bne @chunk

	rts
.endproc
