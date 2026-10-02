;*******************************************************************************
; IMAGE.ASM
; Contains UltiMem-specific procedures for writing the output image during
; linking.
;*******************************************************************************

.include "../image.inc"
.include "../macros.inc"
.include "../ram.inc"
.include "../zeropage.inc"
.import __image_page

;*******************************************************************************
.segment "IMAGE_VARS"
saved_config: .byte $00
saved_banklo: .byte $00
saved_bankhi: .byte $00
fill_byte:    .byte $00
.assert * <= $3800, lderror, "image state overlaps linker tables"

;*******************************************************************************
; PAGES
; Table of banks that comprise the linker output image
BANKED_SEG "LINKER_AUX", FINAL_BANK_LINKER_AUX
ptr = zp::bankaddr0
page_index = __image_page
pages:
.repeat IMAGE_PAGES, I
	.byte IMAGE_BANK + I
.endrepeat

;*******************************************************************************
; MAP PAGE
; Maps the selected image page into BLK3.
; IN:
;   - __image_page: page index
;   - IRQs masked
; OUT:
;   - None
.proc map_page
	lda $9ff2
	sta saved_config
	ora #$30
	sta $9ff2

	lda $9ffc
	sta saved_banklo
	lda $9ffd
	sta saved_bankhi

	ldy page_index
	lda pages,y
	sta $9ffc

	lda #$00
	sta $9ffd
	rts
.endproc

;*******************************************************************************
; UNMAP PAGE
; Restores the BLK3 mapping saved by MAP PAGE.
; IN:
;   - saved_banklo, saved_bankhi, saved_config: saved BLK3 mapping
; OUT:
;   - None
.proc unmap_page
	lda saved_banklo
	sta $9ffc
	lda saved_bankhi
	sta $9ffd
	lda saved_config
	sta $9ff2
	rts
.endproc

;*******************************************************************************
; POINT AT BYTE
; Locates the cursor within the mapped image page and sets ptr to it.
; IN:
;   - image::cursor: validated image offset
; OUT:
;   - ptr: address of the byte within BLK3
;   - .Y: $00
.proc point_at_byte
	lda image::cursor
	sta ptr
	lda image::cursor+1
	and #$1f
	ora #$60
	sta ptr+1
	ldy #$00
	rts
.endproc

;*******************************************************************************
; LOAD BYTE
; Reads one byte from the selected UltiMem page.
; IN:
;   - image::cursor: validated offset
;   - __image_page: page index
; OUT:
;   - .A: byte read
.export __image_backend_load
.proc __image_backend_load
	php
	sei
	jsr map_page
	jsr point_at_byte
	lda (ptr),y
	pha
	jsr unmap_page
	pla
	plp
	rts
.endproc

;*******************************************************************************
; STORE BYTE
; Writes one byte to the selected UltiMem page.
; IN:
;   - .A: byte
;   - image::cursor: validated offset
;   - __image_page: page index
; OUT:
;   - None
.export __image_backend_store
.proc __image_backend_store
	php
	sei
	pha

	jsr map_page
	jsr point_at_byte
	pla
	sta (ptr),y
	jsr unmap_page

	plp
	rts
.endproc

;*******************************************************************************
; FILL PAGE
; Fills an UltiMem page in 256-byte chunks.
; IN:
;   - .A: fill byte
;   - __image_page: page index
; OUT:
;   - None
.export __image_backend_fill
.proc __image_backend_fill
	sta fill_byte
	ldx #$00

@chunk:	php
	sei
	jsr map_page
	lda #$00
	sta ptr
	txa
	ora #$60
	sta ptr+1
	ldy #$00
	lda fill_byte

@byte:	sta (ptr),y
	iny
	bne @byte
	jsr unmap_page
	plp
	inx
	cpx #$20
	bne @chunk
	rts
.endproc
