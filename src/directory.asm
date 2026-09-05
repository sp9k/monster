;*******************************************************************************
; DIRECTORY.ASM
; This file contains the code to list the directory of a disk and provide a
; menu for selecting a file to load as well as supporting routines for getting
; file names from the disk's directory.
;*******************************************************************************

.include "border.inc"
.include "config.inc"
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
.include "ram.inc"
.include "settings.inc"
.include "screen.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

HEIGHT = SCREEN_HEIGHT-2

;*******************************************************************************
; GEOMETRY
DIR_LCOL     = 0		; column the left border is drawn in
DIR_RCOL     = LINESIZE-1	; column the right border is drawn in
DIR_TEXT_COL = DIR_LCOL+1	; column a row's text starts at

DIR_TOP_ROW  = 0		; top border's row
DIR_NAME_ROW = DIR_TOP_ROW+1	; disk name's row
DIR_FILE_ROW = DIR_NAME_ROW+1	; first filename's row

; lowest row the window may occupy
DIR_MAX_ROW = HEIGHT-1

; number of filenames the window can show at once
DIR_NUM_FILE_ROWS = DIR_MAX_ROW-DIR_FILE_ROW

.assert DIR_MAX_ROW < SCREEN_HEIGHT, error, "directory window doesn't fit"

.ifdef soft4x8
.assert (DIR_LCOL .mod 2) = 0, error, "directory must start on an even column"
.assert ((DIR_RCOL+1) .mod 2) = 0, error, "directory must end on an even column"
.endif

;*******************************************************************************
.ifdef ultimem
.segment "SHAREBSS2"
.else
.BSS
.endif

rowbuf: .res LINESIZE		; row being composed

.CODE
;*******************************************************************************
; MAIN-bank entry points
.export __dir_get_by_type
.export __dir_view

.if .defined(CART) .and .defined(c64)
__dir_get_by_type: JUMP FINAL_BANK_FILEDIR, getbytype
__dir_view:        JUMP FINAL_BANK_FILEDIR, dirview
.else
__dir_get_by_type = getbytype
__dir_view        = dirview
.endif

BANKED_CODE "FILEDIR", FINAL_BANK_FILEDIR

;*******************************************************************************
; GET BY TYPE
; Returns all files that contain the provided extension
; IN:
;   - .A:  the extension (one character, uppercase)
;   - .XY: the address to the buffer to store to
;   - r0:  exclusive end address of the destination buffer
;   - r2:  maximum number of filenames to return
; OUT:
;   - .A:  number of files returned (or error)
;   - .XY: address of the final list terminator
;   - .C:  set on error

.proc getbytype
@ext=r5
@resultend=r6
@file=r8
@resultptr=ra
@cnt=rc
@max=rd
@nextptr=re
@buff=$100
	sta @ext
	stxy @resultptr
	lda r2
	sta @max
	ldxy r0
	stxy @resultend

	ldxy @resultptr
	cmpw @resultend
	bcc :+
	RETURN_ERR ERR_BUFFER_FULL

:	jsr open_dir		; open the directory "file"
	bcc :+
	rts			; propagate open error
:
	sta @file

	ldxy #@buff		; use filename buffer as scratch for disk name
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
	lda @cnt
	cmp @max
	bcs @too_many

	; reserve room for this filename's terminator + list terminator
	tya
	sec			; filename length + 1
	adc @resultptr
	sta @nextptr
	lda @resultptr+1
	adc #$00
	sta @nextptr+1
	ldxy @nextptr
	cmpw @resultend
	bcs @buffer_full

	inc @cnt		; count the match
	ldy #$00
@l1:	lda @buff,y
	sta (@resultptr),y
	beq @next
	iny
	bne @l1
@next:	ldxy @nextptr
	stxy @resultptr
	jmp @l0

@too_many:
	lda #ERR_TOO_MANY_OBJECTS
	bne @abort		; branch always

@buffer_full:
	lda #ERR_BUFFER_FULL

@abort:
	pha
	ldy #$00
	tya
	sta (@resultptr),y	; leave a valid partial list
	lda @file
	jsr file::close
	pla
	sec
	rts

@done:	ldy #$00
	tya
	sta (@resultptr),y	; terminate list

	lda @file
	jsr file::close

@ok:	lda @cnt
	beq @nofiles		; no file has the requested extension -> error
	ldxy @resultptr
	clc
@ret:	rts

@nofiles:
	RETURN_ERR ERR_FILE_NOT_FOUND
.endproc

;*******************************************************************************
; DIR VIEW
; Enters the directory viewer
; NOTE: this routine is limited to 128 files
; The max supported by the 1541 is 144 and this routine could easily be
; modified to support as many.
; It could also easily be modified to support more (e.g. for the 1581)
; OUT:
;   - .C: set on error
;   - .A: error code (on error)

.proc dirview
@line=r8
@row=ra
@select=rb
@cnt=rc			; number of files extracted from listing
@scrollmax=rd		; maximum amount to allow scrolling
@scroll=re
@file=zp::tmp10
@dirbuff=mem::spare+40		; 0-40 will be corrupted by text routines
@namebuff=mem::spareend-40	; buffer for the file name
@fptrslo=@namebuff-(128*2)	; room for 128 files
@fptrshi=@namebuff-(128)	; room for 128 files
	jsr open_dir
	bcc :+
	jsr scr::unblank
	jmp scr::restore

:	sta @file

	; the window is drawn at full width
	lda #$00
	sta text::puts_start
	lda #SCREEN_WIDTH
	sta text::puts_stop

	; reset the screen so that we can print the file names normally

	ldxy #@dirbuff+5
	stxy @line

	ldx #DIR_FILE_ROW
	stx @row
	ldx #$00
	stx @select
	stx @scroll
	stx @cnt

	; draw the window's top border
	lda #DIR_TOP_ROW
	ldx #BORDER_TL
	ldy #BORDER_TR
	jsr border

;-------------------------------------------------------------------------------
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

	; draw the disk name and highlight it
	ldxy #@namebuff
	lda #DIR_NAME_ROW
	jsr printrow
	ldy #DIR_TEXT_COL	; reverse everything between the borders
	ldx #DIR_RCOL
	lda #DIR_NAME_ROW
	CALLMAIN scr::rvsline_part

;-------------------------------------------------------------------------------
; parse filenames and render initial view
@getfilenames:
	; make sure there is room for another (max-length) filename before
	; the file-pointer tables; if not, just show the files we have so far
	ldxy @line
	cmpw #@fptrslo-18
	bcs @cont

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
	cmp #DIR_MAX_ROW
	bcs :+			; if line isn't visible, don't draw
	jsr printrow
	inc @row

:	; next line
	inc @cnt
	bpl @getfilenames
	bmi @cont		; only 128 files allowed; show what we have


;-------------------------------------------------------------------------------
; init viewer
@cont:	lda @file
	jsr file::close
	jsr scr::unblank

	; close the window with its bottom border below the last file
	lda @row
	ldx #BORDER_BL
	ldy #BORDER_BR
	jsr border

	; @row becomes the number of files that are on screen
	lda @row
	sec
	sbc #DIR_FILE_ROW
	sta @row

	; max a user can scroll is (# of files - # of visible rows)
	ldx #$00
	lda @cnt
	cmp #DIR_NUM_FILE_ROWS
	bcc :+
	;sec
	sbc #DIR_NUM_FILE_ROWS
	tax
:	stx @scrollmax

	; highlight the first item
	jsr @toggle

;-------------------------------------------------------------------------------
; main viewer loop
@key:	jsr key::waitch
	cmp #K_QUIT
	beq @exit
	cmp #K_WIN_CLOSE
	bne @checkdown
@exit:  jsr scr::restore
	RETURN_OK

; check the arrow keys (used to select a file)
@checkdown:
	jsr key::isdown
	bne @checkup
@rowdown:
	jsr @toggle
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
	ldx #DIR_FILE_ROW
	lda #DIR_MAX_ROW-1
	jsr text::scrollup

	lda @select
	jsr @getname
	lda #DIR_MAX_ROW-1		; bottom row
	jsr printrow
	jmp @hiselection

@checkup:
	jsr key::isup
	bne @checkret

@rowup: jsr @toggle
	dec @select
	bpl @hiselection
	inc @select		; lowest valid select value is 0
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the top line
	lda #DIR_FILE_ROW
	ldx #DIR_MAX_ROW-1
	jsr text::scrolldown

	dec @scroll
	lda @select
	jsr @getname
	lda #DIR_FILE_ROW	; top row
	jsr printrow

@hiselection:
	jsr @toggle
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

	jsr @toggle

	ldx #$00
	stx @select
	stx @scroll
	beq @redraw		; branch always

; if 'G', go to bottom of directory list
@checkbottom:
	cmp #$47		; 'G'
	bne @nextkey

	jsr @toggle

	; set scroll to scrollmax
	lda @scrollmax
	sta @scroll

	; set selection (row) to min(DIR_NUM_FILE_ROWS, @cnt)
	ldx @cnt
	cpx #DIR_NUM_FILE_ROWS
	bcc :+
	ldx #DIR_NUM_FILE_ROWS
:	dex
	stx @select
@redraw:
	jsr @refresh
	jmp @hiselection

; user selected a file (RETURN), load it and exit the directory view
@loadselection:
	jsr scr::restore
	lda @select
	jsr @getname
	JUMPMAIN edit::load		; load the file

;-------------------------------------------------------------------------------
; GETNAME
; Returns the filename that the given visible row is showing
; IN:
;   - .A: the row's index within the window (0 = its first row)
; OUT:
;   - .XY: the filename
@getname:
	clc
	adc @scroll
	tax
	ldy @fptrshi,x
	lda @fptrslo,x
	tax
	rts

;-------------------------------------------------------------------------------
; TOGGLE
; Reverses the selected filename
@toggle:
@nameptr=r6
	lda @cnt
	beq @toggle_done	; no files -> nothing to highlight

	; measure the selected filename
	lda @select
	jsr @getname
	stxy @nameptr
	ldy #$00
:	lda (@nameptr),y
	beq :+
	iny
	bne :-

	; reverse the columns it occupies
:	tya
	clc
	adc #DIR_TEXT_COL
	tax			; one past the name's last column
	ldy #DIR_TEXT_COL	; the name's first column
	lda @select
	clc
	adc #DIR_FILE_ROW	; the row the name is on
	CALLMAIN scr::rvsline_part
@toggle_done:
	rts

;-------------------------------------------------------------------------------
; refresh (redraw) all visible rows
@refresh:
@i=r8			; index of the visible row to draw (0 = the first one)
	lda #$00
	sta @i

:	lda @i
	jsr @getname
	lda @i
	clc
	adc #DIR_FILE_ROW
	jsr printrow

	inc @i
	lda @i
	cmp #DIR_NUM_FILE_ROWS
	bcs @refresh_done	; no more room in the window
	clc
	adc @scroll
	cmp @cnt
	bcc :-			; more files to draw

@refresh_done:
	rts

; kept in-bank: read directly by banked code
@dirmsg: .byte "disk:",0
@dirmsglen=*-@dirmsg
.endproc

;*******************************************************************************
; BORDER
; Builds and draws one of the window's horizontal borders
; IN:
;  - .A: row to draw the border at
;  - .X: character to draw in the leftmost column
;  - .Y: character to draw in the rightmost column
.proc border
	pha			; save the row
	tya
	pha			; and the characters for both corners
	txa
	pha

	jsr blankrow

	lda #BORDER_HBAR
	ldx #DIR_RCOL-1
:	sta rowbuf,x
	dex
	bne :-			; stop at DIR_LCOL; the corner goes there

	pla			; restore left corner char
	sta rowbuf+DIR_LCOL
	pla			; restore right corner char
	sta rowbuf+DIR_RCOL

	pla			; restore the row
	; fall through to showrow
.endproc

;*******************************************************************************
; SHOWROW
; Draws rowbuf on the given row
; IN:
;  - .A: the row to draw it at
.proc showrow
	ldxy #rowbuf
	CALLMAIN text::puts
	rts
.endproc

;*******************************************************************************
; PRINTROW
; Draws the given string as one of the window's rows
; IN:
;  - .XY: the string to draw
;  - .A:  the row to draw it at
.proc printrow
@src=r0
@row=r2
	sta @row
	stxy @src
	jsr blankrow

	; copy the string into the row; its column is its index in the buffer
	ldy #$00
	ldx #DIR_TEXT_COL
:	lda (@src),y
	beq :+
	sta rowbuf,x
	iny
	inx
	cpx #DIR_RCOL
	bcc :-

:	lda #BORDER_VBAR
	sta rowbuf+DIR_LCOL
	sta rowbuf+DIR_RCOL

	lda @row
	jmp showrow
.endproc

;*******************************************************************************
; BLANKROW
; Fills rowbuf (the filename display buffer) with spaces
.proc blankrow
	lda #' '
	ldx #DIR_RCOL
:	sta rowbuf,x
	dex
	bpl :-
	rts
.endproc

;*******************************************************************************
; OPEN DIR
; Opens the directory "file" for loading
.proc open_dir
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

;-------------------------------------------------------------------------------
getb:	jsr krn::readst	; call READST
        bne @eof       	; read error or end of file
        jmp krn::chrin	; call chrin (read byte from directory)
@eof:	pla
	pla
	sec
	rts
.endproc
