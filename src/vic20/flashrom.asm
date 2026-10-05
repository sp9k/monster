;*******************************************************************************
; FLASHROM.ASM
; This file contains the RAM entry point for the cartridge flasher.
; The writer runs in built-in RAM because staging overwrites UltiMem RAM
; and erasing removes every ROM bank.
;*******************************************************************************

UPDATER = 1

.segment "FLASH_CODE"

;*******************************************************************************
; START
; Reinitializes the KERNAL for the RAM writer and discards Monster's return
; addresses. No Monster code or banked data may be used after entry.
; Does not return.
.export __flash_ram_start
.proc __flash_ram_start
	sei
	cld
	ldx #$ff
	txs				; abandon all Monster return addresses

	lda #$00
	sta $9ff1			; detach RAM123 and IO RAM before staging
	sta $9ff2			; detach all cartridge ROM apertures

	ldx #$00
@clear_zp:
	sta $00,x			; fresh KERNAL workspace and file table count
	inx
	bne @clear_zp

	jsr $ff8a			; restore KERNAL vectors
	lda #<safe_nmi
	sta $0318
	lda #>safe_nmi
	sta $0319
	jsr $fdf9			; VIC KERNAL: initialize VIAs/keyboard timer
	lda #$7f
	sta $911e			; RESTORE must never enter BASIC or cartridge

	lda #$1e
	sta $0288			; screen above the updater, at $1e00
	sta $0284			; top of available application RAM
	lda #$10
	sta $0282
	jsr $e518			; VIC KERNAL: native screen/keyboard state

	jsr __flasher_start
	lda #<msg_reset
	ldy #>msg_reset
	jsr __flasher_printz
.endproc

;*******************************************************************************
; HALT
; Waits in built-in RAM after failure. Successful flashing resets the VIC-20.
.export __flash_halt
.proc __flash_halt
	jmp __flash_halt
.endproc

;*******************************************************************************
; SAFE NMI
; Ignores RESTORE while the writer owns RAM.
.proc safe_nmi
	rti
.endproc

;*******************************************************************************
; PARAMETERS
.segment "FLASH_BSS"
.export __flash_device
__flash_device:		.byte 0

;*******************************************************************************
; SHARED WRITER
.include "../flasher.asm"

.segment "FLASH_RODATA"
msg_reset:		.byte $0d, "   RESET THE VIC-20", 0
