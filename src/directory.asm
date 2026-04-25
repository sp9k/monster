;*******************************************************************************
; DIRECTORY.ASM
; This file contains the code to list the directory of a disk and provide a
; menu for selecting a file to load as well as supporting routines for getting
; file names from the disk's directory.
;*******************************************************************************

.include "config.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "file.inc"
.include "irq.inc"
.include "kernal.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "settings.inc"
.include "screen.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

.CODE

;*******************************************************************************
; GET BY TYPE
; Returns all files that contain the provided extension
; IN:
;   - .A:  the extension (one character, uppercase)
;   - .XY: the address to the buffer to store to
; OUT:
;   - .A:  number of files returned (or error)
;   - .XY: address of 0-terminated buffer containing 0-terminated filenames
;   - .C:  set on error
.export __dir_get_by_type
.proc __dir_get_by_type
@ext=r5
@file=r6
@resultptr=r7
@cnt=r9
@buff=$100
	sta @ext
	stxy @resultptr

	jsr open_dir		; open the directory "file"
	bcs @ret
	sta @file

	jsr read_disk_name
	lda #$00
	sta @cnt

@l0:	; read a filename
	ldxy #@buff
	jsr read_filename
	bcs @done

	; look for extension (e.g. ".d" or ".o")
	lda @buff-1,y
	cmp #$5a+1		; 'Z'+1
	bcc :+
	;sec
	sbc #$20		; convert to uppercase
:	cmp @ext		; does extension match?
	bne @l0			; if no -> try next
	lda @buff-2,y
	cmp #'.'		; was there actually an extension?
	bne @l0			; if no -> try next

@match:	; filename has the requested extension, append to result
	ldy #$00
@l1:	lda @buff,y
	sta (@resultptr),y
	beq @next
	iny
	bne @l1
@next:	tya
	sec
	adc @resultptr
	sta @resultptr
	bcc @l0
	inc @resultptr+1
	bne @l0

@done:	ldy #$00
	tya
	sta (@resultptr),y	; terminate list
	lda @file
	jsr file::close

@ok:	lda @cnt
	ldxy @resultptr
@ret:	rts
.endproc

;*******************************************************************************
; DIR VIEW
; Enters the directory viewer
; NOTE: this routine is limited to 128 files
; The max supported by the 1541 is 144 and this routine could easily be
; modified to support as many.
; It could also easily be modified to support more (e.g. for the 1581)
.export __dir_view
.proc __dir_view
@file=r8
@line=r8
@row=ra
@select=rb
@cnt=rc			; number of files extracted from listing
@scrollmax=rd		; maximum amount to allow scrolling
@scroll=re
@dirbuff=mem::spare+40		; 0-40 will be corrupted by text routines
@namebuff=mem::spareend-40	; buffer for the file name
@fptrslo=@namebuff-(128*2)	; room for 128 files
@fptrshi=@namebuff-(128)	; room for 128 files
	jsr irq::off
	jsr open_dir
	bcc :+
@err:	jmp irq::on

:	sta @file

	; reset the screen so that we can print the file names normally
	jsr scr::save

	ldxy #@dirbuff+5
	stxy @line

	ldx #$01
	stx @row
	dex			; .X=0
	stx @select
	stx @scroll
	stx @cnt

	; highlight disk name row
	jsr draw::hiline

	; and the bottom (status) row
	ldx #SCREEN_HEIGHT-1
	jsr draw::hiline

;--------------------------------------
; parse the name of the disk
@getdiskname:
	ldx #@dirmsglen
:	lda @dirmsg-1,x
	sta @namebuff-1,x
	dex
	bne :-

	; read the disk name into the name buffer
	ldxy #@namebuff+@dirmsglen-1
	jsr read_disk_name

	; draw the disk name
	ldxy #@namebuff
	lda #$00
	jsr text::print

;--------------------------------------
; parse filenames and render initial view
@getfilenames:
	ldx @cnt
	lda @line+1
	sta @fptrshi,x	; save pointer to this filename
	tay
	lda @line
	sta @fptrslo,x
	tax

	; read a filename into (@line)
	jsr read_filename
	bcs @cont		; eof -> continue
	ldxy @line
	sec			; +1
	adc @line
	sta @line
	bcc :+
	inc @line+1

:	; print the line (if visible)
	lda @row
	cmp #SCREEN_HEIGHT-1
	bcs :+			; if line isn't visible, don't draw
	jsr text::print
	inc @row

:	; next line
	inc @cnt
	bpl @getfilenames
	bmi @exit		; only 127 files allowed
				; TODO: report this error better

;--------------------------------------
; init viewer
@cont:	lda @file
	jsr file::close
	jsr irq::on

	dec @row

	; max a user can scroll is (# of files - SCREEN_HEIGHT-1)
	ldx #$00
	lda @cnt
	cmp #SCREEN_HEIGHT-2
	bcc :+
	;sec
	sbc #SCREEN_HEIGHT-2
	tax
:	stx @scrollmax

	; highlight the first item
	jsr highlight_selection

;--------------------------------------
; main viewer loop
@key:	jsr key::waitch
	cmp #K_QUIT
	bne @checkdown
@exit:  jmp scr::restore

; check the arrow keys (used to select a file)
@checkdown:
	jsr key::isdown
	bne @checkup
@rowdown:
	jsr unhighlight_selection
	inc @select
	lda @select
	cmp @row
	bcc @hiselection
	dec @select

@scrolldown:
	lda @scroll
	cmp @scrollmax
	bcs @hiselection

	inc @scroll

	; scroll up and redraw the bottom line
	ldx #$01
	lda #SCREEN_HEIGHT-1-1
	jsr text::scrollup

	lda @scroll
	clc
	adc @select
	tax
	ldy @fptrshi,x
	lda @fptrslo,x
	tax
	lda #SCREEN_HEIGHT-1-1			; bottom row
	jsr text::print
	jmp @hiselection

@checkup:
	jsr key::isup
	bne @checkret

@rowup:
	jsr unhighlight_selection
	dec @select
	bpl @hiselection
	inc @select		; lowest valid select value is 0
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the bottom line
	lda #1
	ldx #SCREEN_HEIGHT-1-1
	jsr text::scrolldown

	dec @scroll
	lda @scroll
	clc
	adc @select
	tax
	ldy @fptrshi,x
	lda @fptrslo,x
	tax
	lda #1			; top row
	jsr text::print

@hiselection:
	jsr highlight_selection
@nextkey:
	jmp @key

; check the RETURN key (to open a file)
@checkret:
	cmp #$0d		; select file and load
	beq @loadselection

; if 'G', go to bottom of directory list
@checkgototop:
	cmp #$67		; 'g'
	bne @checkbottom
	jsr key::waitch
	cmp #$67		; gg?
	bne @nextkey

	jsr unhighlight_selection

	ldx #$00
	stx @select
	stx @scroll
	beq @redraw		; branch always

; if 'G', go to bottom of directory list
@checkbottom:
	cmp #$47		; 'G'
	bne @nextkey

	jsr unhighlight_selection

	; set scroll to scrollmax
	lda @scrollmax
	sta @scroll

	; set selection (row) to min(SCREEN_HEIGHT-2, @cnt)
	ldx @cnt
	cpx #SCREEN_HEIGHT-2
	bcc :+
	ldx #SCREEN_HEIGHT-2
:	dex
	stx @select
@redraw:
	jsr @refresh
	jmp @hiselection

; user selected a file (RETURN), load it and exit the directory view
@loadselection:
	jsr scr::restore
	lda @select
	clc
	adc @scroll
	tax
	lda @fptrslo,x
	ldy @fptrshi,x
	tax
	jmp edit::load		; load the file

;--------------------------------------
; refresh (redraw) all visible rows
@refresh:
@i=r8
	lda #$01
	sta @i

:	lda @i
	clc
	adc @scroll
	tax
	ldy @fptrshi-1,x
	lda @fptrslo-1,x
	tax
	lda @i
	jsr text::print

	inc @i
	lda @i
	cmp #SCREEN_HEIGHT
	bcs @refresh_done
	adc @scroll
	cmp @cnt
	beq :-
	bcc :-

@refresh_done:
	rts

.PUSHSEG
.RODATA
@dirmsg: .byte "disk:",0
@dirmsglen=*-@dirmsg
.POPSEG
.endproc

;*******************************************************************************
; UNHIGHLIGHT SELECTION
; Unhighlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc unhighlight_selection
@select=rb
	ldx @select
	inx
	jmp draw::resetline	; deselect the current selection
.endproc

;*******************************************************************************
; HIGHLIGHT SELECTION
; Highlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc highlight_selection
@select=rb
	ldx @select
	inx
	jmp draw::hiline	; select the current selection
.endproc

;*******************************************************************************
; OPEN DIR
; Opens the directory "file" for loading
.proc open_dir
	jsr krn::clall
	ldxy #strings::dir
	jsr file::exists
	bcs :+
	ldxy #strings::dir
	jsr file::open_r_prg
	bcs :+
	tax
	jsr krn::chkin
	clc			; ok
:	rts
.endproc

;*******************************************************************************
; READ DISK NAME
; Reads the name of the disk.  Assumes the directory file is open and
; is at the start.
; IN:
;   - .XY: address of buffer to store the name to
.proc read_disk_name
@buff=r0
	stxy @buff

	ldy #8
:	jsr krn::chrin
	dey
	bne :-

	; read until the closing '"'
:	jsr krn::chrin
	cmp #'"'
	beq @done
	sta (@buff),y
	iny
	bne :-

@done:  lda #$00
	sta (@buff),y

	; read until $00 (line terminator)
:	jsr krn::chrin
	cmp #$00
	bne :-

	rts
.endproc

;*******************************************************************************
; READ FILENAME
; Reads one filename from the directory file (assumed to be open)
; IN:
;   - .XY: address of buffer to store to filename to
; OUT:
;   - .A: size of the filename read
;   - .C:  set on error/eof
.proc read_filename
@tmp=r0
@buff=r2
	stxy @buff

	; eat 4 bytes (track, sector and line #)
	ldy #4
:	jsr getb
	dey
	bne :-

	; look for opening "
:	jsr getb
	cmp #'"'
	bne :-

	; read until the closing '"'
	;ldy #$00
:	jsr getb
	cmp #'"'
	beq @done
	sta (@buff),y
	iny
	bne :-
	inc @buff+1
	bne :-

@done:	lda #$00
	sta (@buff),y		; terminate buffer

	; read rest of filename
:	jsr getb
	cmp #$00
	bne :-

	tya
	RETURN_OK

;--------------------------------------
getb:
        jsr krn::readst	; call READST
        bne @eof       	; read error or end of file
        jmp krn::chrin	; call chrin (read byte from directory)
@eof:	pla
	pla
	sec
	rts
.endproc
