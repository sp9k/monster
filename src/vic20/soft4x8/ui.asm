.include "../settings.inc"
.include "../../asmflags.inc"
.include "../../debug.inc"
.include "../../debuginfo.inc"
.include "../../flags.inc"
.include "../../format.inc"
.include "../../labels.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../sim6502.inc"
.include "../../source.inc"
.include "../../string.inc"
.include "../../strings.inc"
.include "../../text.inc"
.include "../../util.inc"
.include "../../watches.inc"
.include "../../vmem.inc"
.include "../../zeropage.inc"

COLMEM_ADDR=$9400
STATUS_COL=0		; start column for status line

.CODE

;*******************************************************************************
; UPDATE STATUSLINE
; Updates mem::statusline with new information (cursor pos, etc.)
; SIDE-EFFECTS:
;  - mem::statusline: contains the new status info
.export __ui_update_statusline
.proc __ui_update_statusline
@filename=zp::text
@tmp=zp::text
@leftend=zp::text+2
@columnstart = STATUS_COL+3
@linestart   = STATUS_COL+6
@sizestart   = STATUS_COL+13
@modestart   = STATUS_COL
@fmtstart    = STATUS_COL+1
	lda #' '
	ldx #39
@clr:	sta mem::statusline,x
	dex
	bpl @clr

	lda zp::curx
	jsr util::todec8

	; display the column
	ldy #$00
	cpx #'0'
	beq :+
	stx mem::statusline+@columnstart
	iny
:	sta mem::statusline+@columnstart,y

	lda #','
	sta mem::statusline+@columnstart+1,y

	; display current line
	lda text::statusfmt
	beq @fmt_default

@fmtxy:	lda zp::cury
	jsr util::todec8

	; display the row
	ldy #$00
	cpx #'0'
	beq :+
	stx mem::statusline+@linestart
	iny
:	sta mem::statusline+@linestart,y
	jmp @mode

@fmt_default:
	ldxy src::line
	jsr util::todec
	ldx #$00
@l0:	lda mem::spare,x
	beq :+
	sta mem::statusline+@linestart,x
	inx
	bne @l0

:	lda #'/'
	sta mem::statusline+@linestart,x

	stx @tmp

	; display total lines
	ldxy src::lines
	jsr util::todec

	ldx @tmp
	ldy #$00
@l1:	lda mem::spare,y
	beq @mode
	sta mem::statusline+@linestart+1,x
	iny
	inx
	bne @l1

@mode:	; add the editor mode
	lda text::statusmode
	sta mem::statusline+@modestart

	lda fmt::enable
	beq :+
	lda #'f'+$20
	skw
:	lda #' '
	sta mem::statusline+@fmtstart

	ldy #$00
@copyinfo:
	lda mem::statusinfo,y
	beq @copy_filename
	sta mem::statusline+@linestart+2,x
	iny
	inx
	cpx #38
	bcc @copyinfo

@copy_filename:
	; filename
	lda src::activebuff
	jsr src::filename
	stxy @filename
	jsr str::len
	tay
	dey
	beq @drive
	ldx #39
:	lda (@filename),y
	sta mem::statusline,x
	dex
	dey
	bpl :-

; display a '*' if the file is dirty
	jsr src::isdirty
	beq @drive
	lda #'*'
	sta mem::statusline,x

; display the drive name followed by a colon to the left of the filename
@drive:	lda #':'
	sta mem::statusline-1,x

	stx @tmp
	lda zp::device
	jsr util::todec8
	ldy @tmp
	sta mem::statusline-2,y
	cpx #'0'
	beq :+
	txa
	dey
	sta mem::statusline-2,y
:	lda #'#'
	sta mem::statusline-3,y
@done:	rts
.endproc

;*******************************************************************************
; REGS CONTENTS
; Returns a line containing the contents of the registers
; OUT: mem::linebuffer: a line of text containing the values for each tegister
;      matches the format: PC  A  X  Y  SP NV-BDIZC ADDR
.export __ui_regs_contents
.proc __ui_regs_contents
@tmp=zp::disasm+6
@flag=zp::disasm+7
@buff=mem::linebuffer2
	ldy #39
	lda #' '
:	sta @buff,y
	dey
	bpl :-

	; draw .Y
	lda sim::reg_y
	jsr util::hextostr
	sty @buff+11
	stx @buff+12

	; draw .X
	lda sim::reg_x
	jsr util::hextostr
	sty @buff+8
	stx @buff+9

	; draw .A
	lda sim::reg_a
	jsr util::hextostr
	sty @buff+5
	stx @buff+6

	; draw .P (status)
	lda sim::reg_p
	sta @tmp
	lda #$a1
	sta @flag
	ldx #$00

@getstatus:
	and @tmp
	bne :+
	lda #'0'
	skw
:	lda #'1'
	sta @buff+17,x
:	lsr @flag
	inx
	cpx #2
	beq :-
	lda @flag
	bne @getstatus

@getsp:
	lda sim::reg_sp
	jsr util::hextostr
	sty @buff+14
	stx @buff+15

@getaddr:
	lda sim::pc+1
	jsr util::hextostr
	sty @buff
	stx @buff+1

	lda sim::pc
	jsr util::hextostr
	sty @buff+2
	stx @buff+3

; if registers were affected, highlight them (GUI only)
	lda __debug_interface
	bne @colordone

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_A
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(21*$b)+3
	stx COLMEM_ADDR+(21*$b)+4

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_X
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(21*$b)+5

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_Y
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(21*$b)+6
	stx COLMEM_ADDR+(21*$b)+7

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_STACK
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(21*$b)+8

	; if memory was WRITTEN to, highlight it as well
	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_STORE
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(22*$b)+13
	stx COLMEM_ADDR+(22*$b)+14

@colordone:
; if memory was loaded or stored, show the effective address
@memaddr:
	lda sim::affected
	and #OP_LOAD|OP_STORE
	bne :+
	lda #'-'
	sta @buff+26
	sta @buff+27
	sta @buff+28
	sta @buff+29
	bne @clk

:	lda sim::effective_addr+1
	jsr util::hextostr
	sty @buff+26
	stx @buff+27
	lda sim::effective_addr
	jsr util::hextostr
	sty @buff+28
	stx @buff+29

@clk:	lda dbg::sw_valid
	bne :+
	; if stopwatch is invalid, show ???
	lda #'?'
	sta @buff+37
	sta @buff+38
	sta @buff+39
	bne @print

:	ldx sim::stopwatch
	ldy sim::stopwatch+1
	lda sim::stopwatch+2
	jsr util::todec24

	; set the last character of the stopwatch always
	lda $100+7
	sta @buff+32+7

	; ignore leading zeroes
	ldx #$00
:	inx
	cpx #$07
	beq @print
	lda $100,x
	cmp #'0'
	beq :-

@cpyclk:
	lda $100,x
	sta @buff+32,x
	cpx #$07
	inx
	bcc @cpyclk

@print:	lda #$00
	sta @buff+40
	ldxy #@buff
	rts
.endproc

;*******************************************************************************
; RENDER BREAKPOINT
; Returns the rendered string for the given breakpoint.
; This is displayed in both the TUI and GUI breakpoint viewer
; IN:
;  - .A: the breakpoint to get the string for
; OUT:
;  - .XY: the rendered string for that watch
;  - .C:  set if there is no breakpoint for the given ID
.export __ui_render_breakpoint
.proc __ui_render_breakpoint
@offset=zp::tmp14
@format_str=zp::tmp15
@namebuff=mem::spare+40
	sta @offset
	tax

	cpx dbg::numbreakpoints
	bcs @datadone

	; push the address of the breakpoint
	ldy dbg::breakpointshi,x
	lda dbg::breakpointslo,x
	pha
	tax
	tya
	pha

	; get/push the symbol name for this address (if there is one)
	jsr lbl::by_addr
	bcc @getname
@noname:
	lda #>strings::question_marks
	pha
	lda #<strings::question_marks
	pha
	jmp @lineno

@getname:
	lda #>@namebuff
	pha
	sta r0+1
	lda #<@namebuff
	pha
	sta r0
	jsr lbl::getname

@lineno:
	ldx @offset
	; check if there is a file ID for this breakpoint
	lda dbg::breakpoint_fileids,x
	cmp #$ff
	bne :+

	ldxy #strings::breakpoints_line_noname
	stxy @format_str
	jmp @print

:	; get the line number and file name
	lda dbg::breakpoint_lineslo,x
	pha
	lda dbg::breakpoint_lineshi,x
	pha

	lda dbg::breakpoint_fileids,x
	jsr dbgi::get_filename
	tya
	pha
	txa
	pha

	ldxy #strings::breakpoints_line
	stxy @format_str

@print:
	; display a symbol if the breakpoint is active
	ldx @offset
	ldy #BREAKPOINT_OFF_CHAR
	lda dbg::breakpoint_flags,x
	beq :+
	dey				; ldy #BREAKPOINT_CHAR
:	sty strings::breakpoints_line

	; push the breakpoint ID
	lda @offset
	pha

	; print the breakpoint info
	ldxy @format_str
	jsr text::render

@datadone:
	rts
.endproc

;*******************************************************************************
; RENDER WATCH
; Returns the rendered string for the given watch.
; This is displayed in both the TUI and GUI watch viewer
; IN:
;  - .A: the watch to get the string for
; OUT:
;  - .XY: the rendered string for that watch
.export __ui_render_watch
.proc __ui_render_watch
@id=r2		; id of the watch to get
@range=r3	; if !0, start != stop (watch is a range)
@start=r4	; start address
@stop=r6	; stop address (same as start if NOT range)
@val=r8		; value of watch (if NOT range)
@prev=r9	; previous value of watch (if NOT range)
	sta @id
	jsr watch::getdata
	tax			; save flags
	lda #$00
	sta @range

	lda @start
	cmp @stop
	beq :+
	inc @range		; flag that start != stop

:	lda @start+1
	cmp @stop+1
	beq :+
	inc @range		; flag that start != stop

:	; print the watch info
	lda @range		; is it a range of addresses?
	beq @valline		; not a range, continue

; if the stat address != stop address, print start and stop
@rangeline:
	; push the stop address
	lda @stop
	pha
	lda @stop+1
	pha

	; push the start address
	lda @start
	pha
	lda @start+1
	pha

	; push SPACE if not dirty or '!' if dirty
	ldy #' '
	txa				; get flags
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value
	ldy #'!'			; dirty
:	tya
	pha

	; if start addr != stop addr, print the address range
	ldx #<strings::watches_range_line
	ldy #>strings::watches_range_line
	bne @getdatadone

; if the start address == stop address, just print the one address and its val
@valline:
	; push current value of the watch
	lda @val
	pha

	txa				; get flags

	; is this watch dirty?
	ldxy #strings::watches_line	; default to "clean" string
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value

	; dirty, push previous value
	lda @prev
	pha				; push previous value if dirty
	ldxy #strings::watches_changed_line

:	; push the address
	lda @start
	pha
	lda @start+1
	pha

@getdatadone:
	; push the ID of the watch
	lda @id
	pha
	jsr text::render
	rts
.endproc

;*******************************************************************************
; MEMLINE
; Returns a line containing 8 bytes of the contents of the given address
; along with a text rendering of those 8 bytes.
; IN:
;   - .XY: the address to get the memory rendering of
; OUT:
;   - mem::spare: the rendered memory data
;   - .XY:        the rendered memory data
BYTES_TO_DISPLAY=8
.export __ui_memline
.proc __ui_memline
@src=ra
@col=rc
	stxy @src

	; initialize line to empty (all spaces)
	lda #' '
	ldx #40-1
:	sta mem::spare,x
	dex
	bpl :-

@l0:	; draw the address of this line
	lda @src+1
	jsr util::hextostr
	sty mem::spare
	stx mem::spare+1
	lda @src
	jsr util::hextostr
	sty mem::spare+2
	stx mem::spare+3
	lda #':'
	sta mem::spare+4

	ldx #$00
@l1:	stx @col

	; get a byte to display
	ldy #$00
	ldxy @src
	jsr vmem::load
	pha			; save the byte

	incw @src		; update @src to the next byte

@val2ch:
	; get the character representation of the byte
	cmp #$20
	bcc :+
	cmp #$80
	bcc @cont
:	lda #'.'	; use '.' for undisplayable chars

@cont:	ldx @col
	sta mem::spare+31,x	; write the character representation
	pla			; get the byte we're rendering
	jsr util::hextostr	; convert to hex characters
	txa			; get LSB char
	pha			; and save temporarily
	lda @col		; get col*3 (column to draw byte)
	asl
	adc @col
	tax
	pla			; restore LSB char to render
	sta mem::spare+8,x	; store to text buffer
	tya			; get MSB
	sta mem::spare+7,x	; store to text buffer
	ldx @col
	inx
	cpx #BYTES_TO_DISPLAY	; have we drawn all columns?
	bcc @l1			; repeat until we have

	ldx #<mem::spare
	ldy #>mem::spare
	rts
.endproc
