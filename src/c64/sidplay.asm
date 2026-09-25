;*******************************************************************************
; SIDPLAY.ASM
; This file contains the routines to load PSID files, initialize them, and play
; them back.
;*******************************************************************************

.include "../macros.inc"
.include "../ram.inc"
.include "../file.inc"
.include "../kernal.inc"
.include "../errors.inc"

SIDPLAY_IMPL = 1

.include "sidplay.inc"
.macpack longbranch

.ifdef CART
.import __SIDBUFF_RUN__, __SIDBUFF_SIZE__
.assert __SIDBUFF_RUN__ = SID_START, lderror, "SID window start mismatch"
.assert __SIDBUFF_SIZE__ = SID_LIMIT-SID_START, lderror, "SID window size mismatch"

;*******************************************************************************
.segment "SIDBUFF"
.res SID_LIMIT-SID_START

.segment "SIDVARS"
;*******************************************************************************
header:    .res $7c	; PSID header
saved_reu: .res 10	; $df01-$df0a; never use foreground REU scratch
handle:    .byte 0
offset:    .byte 0
loadaddr:  .word 0
endaddr:   .word 0
initaddr:  .word 0
playaddr:  .word 0
song:      .byte 0
cia_timed: .byte 0	; 0 = video-frame IRQ, 1 = CIA timer A IRQ

;*******************************************************************************
.export __sid_zp_reu
__sid_zp_reu = REU_SID_ZP_ADDR
.export __sid_image_reu
__sid_image_reu = REU_SID_IMAGE_ADDR
.DATA
.export __sid_active
__sid_active: .byte 0
paused:       .byte 0

.CODE

;*******************************************************************************
SET_CUR_BANK BANK_NONE
.export __sid_load, __sid_stop, __sid_toggle, __sid_restart, __sid_tick
.export __sid_config_irq, __sid_cia_tick

;*******************************************************************************
; LOAD
; Calls the SID file loader in the directory bank.
; IN:
;  - .XY: the 0-terminated filename
; OUT:
;  - .C: set if loading failed
;  - .A: error code if .C is set
__sid_load: JUMP FINAL_BANK_FILEDIR, load

;*******************************************************************************
; STOP
; Stops playback and prepares the player for a new song to be loaded
.proc __sid_stop
	IO_BEGIN
	jsr clear_sid
	IO_DONE
	rts
.endproc

;*******************************************************************************
; CLEAR SID
; Clears the SID registers and playback state, and disables the raster
; interrupt.
.proc clear_sid
	lda #$00
	sta __sid_active
	sta paused

	ldx #$18
:	sta $d400,x
	dex
	bpl :-

	jsr __sid_config_irq
	rts
.endproc

;*******************************************************************************
; TOGGLE
; Pauses or resumes playback.
; SID registers are write-only, so they are set to their full volume when
; unpaused until the next write to $d418. Does nothing if no song playing.
.proc __sid_toggle
	IO_BEGIN

	lda __sid_active	; song playing?
	bne @pause		; if so, pause it
	lda paused		; song not playing AND paused?
	beq @done		; if so, no active song- we're done

	; activate and unpause the song
	lda #$00
	sta paused
	lda #$01
	sta __sid_active
	lda #$0f
	bne @volume		; branch always

@pause: lda #$01
	sta paused
	lda #$00
	sta __sid_active

@volume:
	sta $d418		; set volume off (if pausing) or to full if not
	jsr __sid_config_irq

@done:	IO_DONE
	rts
.endproc

;*******************************************************************************
; CONFIG IRQ
; Enables the VIC raster interrupt for a "VBI" tune, or disables it when
; stopped, paused, or playing a CIA-based tune.
.proc __sid_config_irq
	lda $d01a
	and #$fe
	sta $d01a		; disable the raster source
	lda #$01
	sta $d019		; discard stale raster event (if any)

	lda __sid_active
	beq @done

	lda cia_timed
	bne @done

	lda $d011
	and #$7f
	sta $d011		; compare line < 256, preserve display settings
	lda #250
	sta $d012
	lda $d01a
	ora #$01
	sta $d01a

@done:	rts
.endproc

;*******************************************************************************
; CIA TICK
; Runs a playback update if the selected tune uses CIA timing.
.proc __sid_cia_tick
	lda cia_timed
	beq @done
	jmp __sid_tick
@done:
	rts
.endproc

;*******************************************************************************
; SAVE REU
; Saves the writable REU registers ($df01-$df0a)
.proc save_reu
	ldx #9
:	lda $df01,x
	sta saved_reu,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; RESTORE REU
; Restores the registers captured by SAVE REU, writing the command register
; last
.proc restore_reu
	ldx #9
:	lda saved_reu,x
	sta $df01,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; SETUP REU
; Configures a 254-byte transfer between CPU $0002-$00ff and the tune's REU
; zero page
.proc setup_reu
	lda #2
	sta $df02		; CPU $0002-$00ff; leave processor port alone
	lda #<REU_SID_ZP_ADDR
	sta $df04
	lda #>REU_SID_ZP_ADDR
	sta $df05
	lda #^REU_SID_ZP_ADDR
	sta $df06
	lda #$fe
	sta $df07
	lda #0
	sta $df03
	sta $df08
	sta $df0a		; increment both addresses
	rts
.endproc

;*******************************************************************************
; SWAPZP
; Swaps the zero pages configured by SETUP REU.
.proc swapzp
	lda #$b2		; immediate SWAP + autoload
	sta $df01
	rts
.endproc

;*******************************************************************************
; RESTART
; Reinitializes the loaded tune and begins playback for it anew.
; OUT:
;  - .C: set if the REU is busy
;  - .A: ERR_IO_ERROR if .C is set
.proc __sid_restart
	lda __sid_active
	ora paused
	beq @none
	ldx #$91		; REU -> C64
	jmp initialize
@none:
	clc
	rts
.endproc

;*******************************************************************************
; START
; Captures the newly loaded song window in the REU, then initializes playback.
; OUT:
;  - .C: set if the REU is busy
;  - .A: ERR_IO_ERROR if .C is set
.proc start
	ldx #$90		; C64 -> REU, before init can modify the payload
	jmp initialize
.endproc

;*******************************************************************************
; SETUP IMAGE
; Configures a transfer of the full SID RAM window to/from its REU snapshot,
; without changing foreground REU parameters without actually transferring
.proc setup_image
	ldxy #SID_START
	stxy $df02
	lda #<REU_SID_IMAGE_ADDR
	sta $df04
	lda #>REU_SID_IMAGE_ADDR
	sta $df05
	lda #^REU_SID_IMAGE_ADDR
	sta $df06
	ldxy #SID_LIMIT-SID_START
	stxy $df07

	lda #$00
	sta $df0a
	rts
.endproc

;*******************************************************************************
; INITIALIZE
; Saves or restores the song window, clears the SID and private zero
; page, and calls init for the default subtune. Restores the caller's memory
; mapping and REU registers before enabling playback.
; IN:
;  - .X: $90 to save the window to REU, or $91 to restore it from REU
; OUT:
;  - .C: set if the REU is busy (playback state remains unchanged)
;  - .A: ERR_IO_ERROR if .C is set
.proc initialize
	php
	sei
	cld

	lda $01
	pha
	lda #$36
	sta $01

	; do not cancel a pending transfer or interfere with REU IRQ users
	lda $df01
	bmi @busy
	lda $df09
	bmi @busy

	txa
	pha
	jsr save_reu
	jsr setup_image
	pla
	sta $df01

	; init state
	jsr clear_sid
	jsr setup_reu

	; zero the private REU page
	ldxy #zero
	stxy $df02
	lda #$80
	sta $df0a

	; bring in the SID's zeropage
	lda #$90
	sta $df01
	jsr setup_reu
	jsr swapzp

	; run the init routine for the song
	lda song
	ldx #$00
	ldy #$00
	jsr callinit

	; save the updated zeropage for the song
	sei
	cld
	lda #$36
	sta $01
	jsr swapzp
	jsr restore_reu

	; set up the IRQ for the song
	lda #$01
	sta __sid_active
	jsr __sid_config_irq
	pla
	sta $01
	plp
	RETURN_OK

@busy:	pla
	sta $01
	plp
	RETURN_ERR ERR_IO_ERROR

callinit:
	jmp (initaddr)
.endproc

;*******************************************************************************
; TICK
; Plays one update if a tune is active and the REU is available.
; Called once per selected hardware interrupt.
.proc __sid_tick
	lda __sid_active
	beq @done
	lda $df01		; is DMA already armed?
	bmi @done		; if so, don't trigger it (it's already delayed)
	lda $df09
	bmi @done		; if REU interrupts enabled, skip playback

	; bring in the SID player's zeropage
	jsr save_reu
	jsr setup_reu
	jsr swapzp

	cld
	lda #$00
	tax
	tay
	jsr callplay		; play the next tick
	sei
	cld

	; save the new SID zeropage and restore Monster's
	lda #$36
	sta $01
	jsr swapzp
	jsr restore_reu
@done:	rts
callplay:
	jmp (playaddr)
.endproc

BANKED_CODE "FILEDIR", FINAL_BANK_FILEDIR

;*******************************************************************************
; READBYTE
; Reads from the selected KERNAL input channel
; OUT:
;  - .C: clear if a byte was read, set on EOF or error
;  - .A: the byte if .C is clear; 0 for EOF, ERR_IO_ERROR for a transport error
.proc readbyte
	jsr krn::readst
	bne @status
	jsr krn::chrin
	bcs @ioerr
	pha
	jsr krn::readst
	and #$bf
	bne @bad
@ok:	RETURN_OK

@bad:	pla
@ioerr:	RETURN_ERR ERR_IO_ERROR

@status:
	cmp #$40		; EOF?
	bne @ioerr

@eof:	lda #$00
	sec
	rts
.endproc

;*******************************************************************************
; LOAD FILE
; Stops the current tune, reads and validates a PSID file, and starts playback.
; IN:
;  - .XY: the 0-terminated filename
; OUT:
;  - .C: set if loading or initialization failed
;  - .A: error code if .C is set
.proc load
@dst=r0
	stxy loadaddr
	CALLMAIN __sid_stop
	ldxy loadaddr
	jsr file::open_r
	bcc :+
	rts

:	sta handle
	tax
	jsr krn::chkin
	jcs @ioerr
	lda #$00
	sta offset

;-------------------------------------------------------------------------------
@header:
	jsr readbyte
	jcs @short
	ldx offset
	sta header,x
	inc offset
	lda offset
	cmp #$76
	bcc @header
	ldx #$03
:	lda header,x
	cmp magic,x
	jne @invalid
	dex
	bpl :-
	lda header+4
	ora header+6
	jne @invalid
	lda header+5
	beq @invalid
	cmp #$05
	bcs @invalid
	cmp #$01
	beq @v1

;-------------------------------------------------------------------------------
@extended:
	jsr readbyte
	jcs @short
	ldx offset
	sta header,x
	inc offset
	lda offset
	cmp #$7c
	bcc @extended
	lda header+$77
	and #$03
	bne @invalid		; MUS / PlaySID-specific samples
	lda header+$76
	ora header+$7a
	ora header+$7b
	bne @invalid		; reserved flags / extra SID chips

@v1:	lda header+7
	cmp offset
	bne @invalid
	jmp @addresses
@invalid:
	lda #ERR_SID_FORMAT
	jmp @error

;-------------------------------------------------------------------------------
@addresses:
	; Songs and default song are 1..256; default must be <= songs.
	ldx #14
	jsr songindex
	bcs @invalid
	sta cia_timed		; temporary last song index
	ldx #16
	jsr songindex
	bcs @invalid
	cmp cia_timed
	bcc :+
	bne @invalid
:	sta song
	; v1 speed bits repeat every 32; v2+ uses bit 31 for songs 32+.
	ldx header+5
	cpx #$01
	beq @speedbit
	cmp #32
	bcc @speedbit
	lda #31
@speedbit:
	and #31
	tax

@shift:
	lsr header+18
	ror header+19
	ror header+20
	ror header+21
	dex
	bpl @shift
	lda #$00
	rol 			; selected speed bit: 0=VBI, 1=CIA
	sta cia_timed
	lda header+9
	sta loadaddr
	lda header+8
	sta loadaddr+1
	ora loadaddr
	bne @gotload
	jsr readbyte
	jcs @short
	sta loadaddr
	jsr readbyte
	jcs @short
	sta loadaddr+1

;-------------------------------------------------------------------------------
@gotload:
	ldxy loadaddr
	cmpw #SID_START
	jcc @address
	cmpw #SID_LIMIT
	jcs @address
	stxy endaddr
	lda header+11
	sta initaddr
	lda header+10
	sta initaddr+1
	ora initaddr
	bne :+
	stxy initaddr
:	lda header+13
	sta playaddr
	lda header+12
	sta playaddr+1
	ora playaddr
	jeq @invalid		; IRQ-installing tunes aren't cooperative

;-------------------------------------------------------------------------------
@payload:
	jsr readbyte
	bcs @eof
	pha
	ldxy endaddr
	cmpw #SID_LIMIT
	bcs @large
	stxy @dst
	ldy #$00
	pla
	sta (@dst),y
	incw endaddr
	jmp @payload

;-------------------------------------------------------------------------------
@large: pla
	lda #ERR_FILE_TOO_BIG
	bne @error

@eof:	cmp #$00
	bne @error

	; entry points must be inside the bytes actually read (also rejects empty)
	ldxy initaddr
	jsr entrycheck
	bcs @address
	ldxy playaddr
	jsr entrycheck
	bcs @address
	lda handle
	jsr file::close
	CALLMAIN start
	rts

@address:
	lda #ERR_SID_ADDRESS
	bne @error

@ioerr:	lda #ERR_IO_ERROR
	bne @error

@short: cmp #$00
	bne @error
	lda #ERR_SID_FORMAT

;-------------------------------------------------------------------------------
@error:	pha
	lda handle
	jsr file::close
	pla
	sec
	rts

;-------------------------------------------------------------------------------
magic: .byte $50,$53,$49,$44
.endproc

;*******************************************************************************
; ENTRYCHECK
; Checks that an init/play entry point is within the loaded payload
; IN:
;  - .XY: entry-point address
; OUT:
;  - .C: set if the address is below loadaddr or at/above endaddr
.proc entrycheck
	cmpw loadaddr
	bcc @bad
	cmpw endaddr
	rts

@bad:	sec
	rts
.endproc

;*******************************************************************************
; SONGINDEX
; Converts the big-endian song number from the PSID header to a zero-based byte
; value.
; IN:
;  - .X: the header offset of the song number's high byte
; OUT:
;  - .A: zero-based song index
;  - .C: set if the song number is invalid
.proc songindex
	lda header,x	; get MSB (big endian)
	beq @low	; check LSB if 0
	cmp #1
	bne @bad	; >1 -> bad header
	lda header+1,x
	bne @bad	; 256 is max value, anything greater -> bad
	lda #$ff	; 255
	RETURN_OK

@low:	lda header+1,x	; get LSB (big endian)
	beq @bad	; if 0 -> bad header
	sec
	sbc #1		; get 0-based value
	RETURN_OK

@bad:	sec
	rts
.endproc

.else
;*******************************************************************************
; NO-OP STUBS
.CODE
.export __sid_load, __sid_stop, __sid_toggle, __sid_restart, __sid_tick
.export __sid_config_irq, __sid_cia_tick
__sid_load:
	RETURN_ERR ERR_SID_FORMAT
__sid_stop:
__sid_toggle:
__sid_restart:
__sid_tick:
__sid_config_irq:
__sid_cia_tick:
	rts
.endif

;*******************************************************************************
zero:	.byte 0
