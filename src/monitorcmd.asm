;*******************************************************************************
; MONITORCMD.ASM
; This file contains the code for handling commands in the monitor (TUI)
; interface.
;*******************************************************************************

.include "asm.inc"
.include "image.inc"
.include "kernal.inc"
.macpack longbranch
.include "breakpoints.inc"
.include "ctx.inc"
.include "monitor.inc"
.include "monitorcmd.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "fp.inc"
.include "flags.inc"
.include "labels.inc"
.include "layout.inc"
.include "line.inc"
.include "macros.inc"
.include "memory.inc"
.include "memview.inc"
.include "runtime.inc"
.include "screen.inc"
.include "sim6502.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "ui.inc"
.include "util.inc"
.include "vmem.inc"
.include "watches.inc"
.include "target.inc"
.include "zeropage.inc"

.include "ram.inc"

.ifdef vic20
.include "vic20/flash.inc"
.endif

.segment "CONSOLE_VARS"

.export __dbgcmd_default_addr
__dbgcmd_default_addr: .res 3	; default start address for command

; buffer for the byte lists parsed by parse_exprs (used by f and h)
MAX_EXPR_LIST = 32
exprlist: .res MAX_EXPR_LIST

.export __dbgcmd_memory_mode
__dbgcmd_memory_mode:
memory_mode: .byte MON_MODE_VIRTUAL
addr_hi:     .byte $00	; high bytes for monitor address arguments
stop_hi:     .byte $00
target_hi:   .byte $00
count_hi:    .byte $00
access_hi:   .byte $00	; high byte for vmem_load / vmem_store
mem_limit:   .res 3	; exclusive end of the selected address space
mode_index:  .byte $00

;*******************************************************************************
; INCADDR
; Increments a monitor address consisting of a low word and a separate high byte.
.macro incaddr addr, high
.local @done
	incw addr
	bne @done
	inc high
@done:
.endmacro

;*******************************************************************************
; CMPADDR
; Compares two monitor addresses, returning the same flags as CMPW.
.macro cmpaddr addr, high, other, otherhigh
.local @done
	lda high
	cmp otherhigh
	bne @done
	ldxy addr
	cmpw other
@done:
.endmacro

BANKED_SEG "CONSOLE", FINAL_BANK_MONITOR

;*******************************************************************************
; DBGCMD RUN
; Handles the given debug command input. The given string is parsed and handled.
; This can be used to add/remove watches, breakpoints, etc.
; IN:
;  - .XY: the command to handle
; OUT:
;  - .C: set if the command wasn't understood or couldn't be executed
.export __dbgcmd_run
.proc __dbgcmd_run
@cnt=r0
	stxy zp::line

	ldy #$00
	ldx #$00
	sty @cnt

	; eat whitespace to get to the command
@l0:	lda (zp::line),y
	beq @cmdfound
	jsr is_whitespace
	beq @cmdfound

	; check if command matches one in the command list
@l1:	cmp commands,x
	bne @next
	iny
	inx
	bne @l0

@next:	; command didn't match, move .X to index of next one to check
	ldy #$00
:	lda commands,x
	inx
	cmp #$00
	bne :-

	inc @cnt
	lda @cnt
	cmp #num_commands
	bne @l0
@err:	RETURN_ERR ERR_INVALID_COMMAND

@cmdfound:
	lda commands,x	; check if end of command
	bne @next	; if not 0, we're not actually done checking commands

	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	jsr process_ws
@run:	ldx @cnt
	lda commandslo,x
	sta zp::jmpvec
	lda commandshi,x
	sta zp::jmpvec+1
	jmp (zp::jmpvec)
.endproc

;*******************************************************************************
; ADD WATCH LOAD
; wal <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; The set watch will only trigger when the watched value is read from
; IN:
;  - .XY: the parameters for the command
.proc add_watch_load
	lda #WATCH_LOAD
	bne add_watch_mode	; branch always
.endproc

;*******************************************************************************
; ADD WATCH STORE
; was <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; The set watch will only trigger when the watched value is written to
; IN:
;  - .XY: the parameters for the command
.proc add_watch_store
	lda #WATCH_STORE
	bne add_watch_mode	; branch always
.endproc

;*******************************************************************************
; ADD WATCH
; wa <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; IN:
;  - .XY: the parameters for the command
.proc add_watch
	lda #WATCH_LOAD|WATCH_STORE
	; fall through to add_watch_mode
.endproc

;*******************************************************************************
; ADD WATCH MODE
; Common handler for the watch commands
; IN:
;  - .A:  the watch mode (WATCH_LOAD and/or WATCH_STORE)
;  - .XY: the parameters for the command
.proc add_watch_mode
@addr=zp::debuggertmp
@num=zp::debuggertmp+2
	pha					; save the watch mode

	; evaluate the expression to get start address
	jsr eval
	bcs @err
	stxy @addr
	stxy r0

	jsr eat_whitespace
	ldy #$00
	lda (zp::line),y
	cmp #$00				; was there a 2nd argument?
	beq @set				; if not, continue
	cmp #','				; skip the separator (if any)
	bne :+
	incw zp::line
	jsr eat_whitespace

:	; evaluate the 2nd expression (if any) to get stop address
	jsr eval
	bcs @err
	stxy r0

@set:	lda watch::num
	sta @num
	ldxy @addr
	pla					; restore the watch mode
	CALLMAIN watch::add		; add the watch
	bcc :+
	RETURN_ERR ERR_TOO_MANY_WATCHES
:	; watch::add succeeds without adding when the exact range exists.
	lda watch::num
	cmp @num
	bne :+
	RETURN_ERR ERR_WATCH_EXISTS
:	RETURN_OK

@err:	tax					; save error code
	pla					; clean up saved watch mode
	txa					; restore error code
	sec
	rts
.endproc

;*******************************************************************************
; LIST WATCHES
; w
; Lists all active watches
.proc list_watches
@cnt=zp::tmp13
@num=zp::tmp14
	lda #$00
	sta @cnt

	CALLMAIN watch::getdata
	stx @num
	cpx #$00
	beq @done

@loop:	lda @cnt
	jsr @print
	inc @cnt
	lda @cnt
	cmp @num
	bcc @loop
@done:	RETURN_OK

@print:	CALLMAIN ui::render_watch
	jmp mon::puts
.endproc

;*******************************************************************************
; REMOVE WATCH
; wr <id>
; Deletes the watch with the given ID. The ID's can be found by listing watches
; with the w command or going to the watch viewer.
; IN:
;  - .XY: the parameters for the command
.proc remove_watch
	; get the ID
	ldxy zp::line
	CALLMAIN atoi
	bcs @done
@ok:	cpy #$00
	bne @done				; there can't be > $ff watches
	txa
	CALLMAIN watch::remove
	clc
@done:	rts
.endproc

;*******************************************************************************
; LIST BREAKPOINTS
; b
; List all breakpoints
.proc list_breakpoints
@cnt=zp::debuggertmp
@num=zp::debuggertmp+1
	lda #$00
	sta @cnt

	lda dbg::numbreakpoints
	sta @num
	cmp #$00
	beq @done

@loop:	lda @cnt
	jsr @print
	inc @cnt
	lda @cnt
	cmp @num
	bcc @loop
@done:	RETURN_OK

@print:	CALLMAIN ui::render_breakpoint
	jmp mon::puts
.endproc

;*******************************************************************************
; ADD BREAK ADDR
; ba <expr>
; Adds a breakpoint at the given address/expression
; IN:
;  - .XY: the parameters for the command
.proc add_break_addr
@addr=zp::debuggertmp
@line=zp::debuggertmp+2
	; evaluate the expression to get break address
	jsr eval
	bcs @done

	stxy @addr

	; get the line/file for the given address
	CALLMAIN dbgi::addr2line
	bcs @skip_line
	pha			; save file id
	stxy @line
	CALLMAIN dbg::setbrkatline
	bcc :+
	pla			; clean up saved file id
	RETURN_ERR ERR_TOO_MANY_BREAKPOINTS

:	lda @addr
	sta r0
	lda @addr+1
	sta r0+1
	pla			; restore file id
	ldxy @line
	CALLMAIN dbg::brksetaddr
	RETURN_OK

@skip_line:
	; no line number for the address requested
	ldxy @addr
	CALLMAIN dbg::setbrkataddr
	bcc @done				; ok
	RETURN_ERR ERR_TOO_MANY_BREAKPOINTS
@done:	rts
.endproc

;*******************************************************************************
; ADD BREAK LINE
; bl file <expr>
; Adds a breakpoint at the given line/expression
; IN:
;  - .XY: the parameters for the command
.proc add_break_line
@fileid=zp::debuggertmp
@line=zp::debuggertmp+1
	; get the filename of the file to add the breakpoint in
	skb
@err:	rts
	ldy #$ff
:	iny
	lda (zp::line),y
	beq @err		; no filename
	jsr is_whitespace
	bne :-

	lda #$00
	sta (zp::line),y
	tya
	ldxy zp::line
	sec			; +1
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	CALLMAIN dbgi::getfileid
	bcs @done				; no file found
	pha					; save file ID

	; move past the filename and whitespace
	jsr process_ws

	; evaluate the expression to get break line
	jsr eval
	stxy @line
	pla
	sta @fileid
	bcs @err		; invalid line #

	; add the breakpoint
	lda @fileid
	CALLMAIN dbg::setbrkatline

	; get the address for the given line
	ldxy @line
	lda @fileid
	CALLMAIN dbgi::line2addr
	bcs @done				; no matching line found

	stxy r0
	; map the address we looked up to the line
	ldxy @line
	lda @fileid
	CALLMAIN dbg::brksetaddr
	clc
@done:	rts
.endproc

;*******************************************************************************
; REMOVE BREAK
; br <id>
; Deletes the breakpoint with the given ID. The IDs can be found by listing
; breakpoints (bl) or going to the breakpoint viewer
; IN:
;  - .XY: the parameters for the command
.proc remove_break
	; get the ID
	ldxy zp::line
	CALLMAIN atoi
	bcs @done

@ok:	cpy #$00
	bne @done	; there can't be > $ff breakpoints

	; .X is the ID to remove
	CALLMAIN dbg::removebreakpointbyid
	clc
@done:	rts
.endproc

;*******************************************************************************
; POKE
; p <address> <value>
; Sets the given address to the provided value
; IN:
;  - .XY: the parameters for the command
.proc poke
@addr=zp::debuggertmp
	jsr eval_address
	bcs @ret
	stxy @addr
	lda expr::value+2
	sta access_hi

	jsr eat_whitespace

	; get the byte value
	jsr eval
	bcs @ret
	cmp #$02
	bcc :+
	RETURN_ERR ERR_OVERSIZED_OPERAND

:	txa
	ldxy @addr
	jsr vmem_store
@ret:	rts
.endproc

;*******************************************************************************
; FILL
; f <start> <stop> a [, b, c, ...]
; Fills the range between the two addresses/expressions with the given fill
; list.  The given list is repeated in memory from the start address until the
; stop address is reached
; IN:
;  - .XY: the parameters for the command
.proc fill
@start   = zp::debuggertmp
@stop    = zp::debuggertmp+2
@listlen = zp::debuggertmp+4
@list    = exprlist
@i       = r0
	jsr get_range
	bcs @ret

	jsr parse_exprs		; parse the list of values to fill
	bcs @ret
	lda #$00
	sta @i
	beq @chk		; branch to check if start == stop on 1st iter
@fill:	lda addr_hi
	sta access_hi
	lda mon::int
	bne @done		; SIGINT, quit
	ldx @i
	lda @list,x
	ldxy @start
	jsr vmem_store
	bcs @ret
	ldx @i
	inx
	cpx @listlen
	bcc :+
	ldx #$00
:	stx @i
	incaddr @start, addr_hi
@chk:	cmpaddr @start, addr_hi, @stop, stop_hi
	bne @fill
@done:	clc		; OK
@ret:	rts
.endproc

;*******************************************************************************
; PARSE EXPRS
; Parses as many expressions as the contents of the line contain and joins them
; into a list of bytes.
; Returns the list
; IN:
;  - zp::line: the line to parse
; OUT:
;  - .A:               the number of values extracted
;  - .C:               set if an error occurred during parsing
;  - zp::debuggertmp+4 the length of the byte array created
;  - exprlist:         the list of values that were read
.proc parse_exprs
@listlen = zp::debuggertmp+4
@list    = exprlist
	lda #$00
	sta @listlen

@l0:	lda @listlen
	cmp #MAX_EXPR_LIST-1	; room for (up to) 2 more bytes?
	bcc :+
	RETURN_ERR ERR_LINE_TOO_LONG

:	jsr eval
	bcs @ret

	cmp #$02		; was expression 2 bytes?
	tya
	ldy @listlen
	bcc :+			; if < 2 bytes, only store LSB
	sta @list+1,y		; store MSB of the expression as a fill val
	inc @listlen
:	txa			; store LSB of expression as fill val
	sta @list,y
	inc @listlen
	ldy #$00
	lda (zp::line),y	; are we at the end?
	beq @ok

	incw zp::line		; move past separator
	jsr eat_whitespace
	jmp @l0

@ok:	clc			; OK
@ret:	rts
.endproc

;*******************************************************************************
; PRINT WORD
; Prints the given 16 bit value to the console in hex
; IN:
;  - .XY: the value to print
.proc print_word
@buff=zp::debuggertmp
	tya
	pha
	txa
	jsr hextostr
	stx @buff+4
	sty @buff+3
	pla
	jsr hextostr
	stx @buff+2
	sty @buff+1
	lda #'$'
	sta @buff
	lda #$00
	sta @buff+5		; 0 terminate the buffer
	ldxy #@buff
	jmp mon::puts
.endproc

;*******************************************************************************
; ? <expression>: show a numeric value without interpreting it as an address.
; IN:
;   - zp::line: expression text
; OUT:
;   - .C: set and .A: error code on evaluation or output failure
.proc evaluate_value
	CALL FINAL_BANK_EXPR, expr::eval_wide
	bcs @ret
.if FP_SUPPORTED
	lda expr::kind
	cmp #VAL_FLOAT
	bne @integer
	CALL FINAL_BANK_EXPR, expr::float_format
	bcs @ret
	ldxy #expr::floatstr
	jmp mon::puts
.endif
@integer:
	lda expr::value+2
	ora memory_mode
	bne @wide
	ldxy expr::value
	jsr print_word
	RETURN_OK
@wide:	jmp print_value
@ret:	rts
.endproc

;*******************************************************************************
; COMPARE
; c <expr> <expr> <expr>
; Compares the given number of bytes at the two given memory addresses and
; displays any disparities.
; Example:
;  `c $100 $200 $5`
; Will show the differences between the 5 bytes in [$100, $105) and [$200, $205)
.proc compare
@block0 = zp::debuggertmp
@block1 = zp::debuggertmp+2
@num = zp::debuggertmp+4
@tmp = zp::debuggertmp+6
	; get the start of one of the blocks to compare
	jsr eval_address
	stxy @block0
	jcs @done
	lda expr::value+2
	sta addr_hi
	jsr process_ws

	; get the start of the other block to compare
	jsr eval_address
	stxy @block1
	jcs @done
	lda expr::value+2
	sta target_hi
	jsr process_ws

	; get the number of bytes to compare
	jsr eval_address
	bcs @done
	stxy @num
	lda expr::value+2
	sta count_hi
	jsr check_compare
	bcs @done
	txa
	ora @num+1
	ora count_hi
	beq @done		; if comparing 0 bytes, we're done

@l0:	lda mon::int
	bne @ok		; SIGINT, quit
	lda addr_hi
	sta access_hi
	ldxy @block0
	jsr vmem_load	; get a byte from block 0
	bcs @done
	sta @tmp
	lda target_hi
	sta access_hi
	ldxy @block1
	jsr vmem_load	; get a byte from block 1
	bcs @done
	cmp @tmp
	beq @next

	; display the address that had a mismatch
	jsr @display_item

@next:	incaddr @block0, addr_hi
	incaddr @block1, target_hi
	lda @num
	ora @num+1
	bne :+
	dec count_hi
:	decw @num
	lda @num
	ora @num+1	; decw only sets .Z for the LSB; test all 24 bits
	ora count_hi
	bne @l0
@ok:	clc
@done:	rts

;-------------------------------------------------------------------------------
@display_item:
	; push the value from the other block
	pha

	; push the value from the first block
	lda @tmp
	pha

	; push the address in the other block
	lda @block1
	pha
	lda @block1+1
	pha
	lda memory_mode
	beq :+
	lda target_hi
	pha
:

	; push the address in the first block
	lda @block0
	pha
	lda @block0+1
	pha
	lda memory_mode
	beq :+
	lda addr_hi
	pha
:

	ldxy #@compare_msg
	lda memory_mode
	beq @render

	; copy the image format to shared RAM for the text renderer
	ldx #@compare_image_end-@compare_image_msg-1
:	lda @compare_image_msg,x
	sta mem::spare,x
	dex
	bpl :-
	ldxy #mem::spare
@render:
	RENDER_STR
	jmp mon::puts

;-------------------------------------------------------------------------------
@compare_image_msg: .byte ESCAPE_BYTE, ESCAPE_VALUE, " ", ESCAPE_BYTE
                    .byte ESCAPE_VALUE, " $", ESCAPE_BYTE, " $", ESCAPE_BYTE, 0
@compare_image_end:
.PUSHSEG
.RODATA
@compare_msg: .byte ESCAPE_VALUE, " ", ESCAPE_VALUE, " $", ESCAPE_BYTE
              .byte " $", ESCAPE_BYTE, 0
.POPSEG
.endproc

;*******************************************************************************
; GOTO
; Sets the program counter to the given value (or the current PC if none is
; given)
; Example:
;  `g $1234`
.proc goto
	lda (zp::line),y
	beq :+				; if no argument given, skip setting PC

	; set the PC to the provided value
	jsr eval
	bcs @done			; if invalid address, break
	stxy sim::pc
:	CALLMAIN run::go

@done:	rts
.endproc

;*******************************************************************************
; MOVE
; m <expr> <expr> <expr>
; Moves the given range of memory to the specified destination address.
; Example:
;  `m $1000 $2000 $3000`
; Will move the memory in [$1000, $2000) to the address $3000.
.proc move
@start = zp::debuggertmp
@end = zp::debuggertmp+2
@target = zp::debuggertmp+4
	jsr get_range
	bcs @ret

	; get the target address
	jsr eval_address
	stxy @target
	bcs @ret
	lda expr::value+2
	sta target_hi
	jsr check_move
	bcs @ret

	cmpaddr @start, addr_hi, @end, stop_hi
	beq @done

	; move the data
@l0:	lda mon::int
	bne @done		; SIGINT, quit
	lda addr_hi
	sta access_hi
	ldxy @start
	jsr vmem_load
	bcs @ret
	pha
	lda target_hi
	sta access_hi
	pla
	ldxy @target
	jsr vmem_store
	bcs @ret
	incaddr @target, target_hi
	incaddr @start, addr_hi
	cmpaddr @start, addr_hi, @end, stop_hi
	bne @l0
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; HUNT
; h <addr> <val1> <val2> ...
; Hunts for the given value(s) starting at the given address.  Does not wrap
; past $FFFF.
; Example:
;  `h $1000 1 2 3`
.proc hunt
@start   = zp::debuggertmp
@i       = zp::debuggertmp+2
@listlen = zp::debuggertmp+4
@list    = exprlist
	; get the start address
	jsr eval_address
	stxy @start
	bcs @ret
	lda expr::value+2
	sta addr_hi
	jsr eat_whitespace

	jsr parse_exprs		; get the values to hunt for
	bcs @ret		; propagate parse errors

	lda #$00
	sta @i
@l0:	cmpaddr @start, addr_hi, mem_limit, mem_limit+2
	jcs @done
	lda mon::int
	bne @done		; SIGINT, quit
	lda addr_hi
	sta access_hi
	ldxy @start
	jsr vmem_load
	bcs @ret
	ldx @i
	cmp @list,x
	beq @match

	; mismatch: rewind so the search restarts one byte after the
	; position where the partial match began
	lda @start
	sec
	sbc @i
	sta @start
	lda @start+1
	sbc #$00
	sta @start+1
	lda addr_hi
	sbc #$00
	sta addr_hi
	lda #$00
	sta @i
	beq @next		; branch always

@match:	inc @i
	; if i >= listlen, we found all the values we were looking for
	lda @i
	cmp @listlen
	bcs @found

@next:	; start++
	incaddr @start, addr_hi
	jmp @l0
@ret:	rts

@found:	; subtract @listlen-1 to get HUNT address
	lda @start
	; sec
	sbc @listlen
	tax
	lda @start+1
	sbc #$00
	tay
	lda addr_hi
	sbc #$00
	sta access_hi
	inx
	bne :+
	iny
	bne :+
	inc access_hi
:	lda memory_mode
	beq @word
	lda access_hi
	jsr print_long
	jmp @done
@word:	jsr print_word	; print the address where we found the value

@done:	RETURN_OK
.endproc

;*******************************************************************************
; REGISTERS
; Displays the current contents of the registers.
.export __dbgcmd_regs
.proc __dbgcmd_regs
	ldxy #strings::debug_registers
	RENDER_STR
	jsr mon::puts

	CALLMAIN ui::regs_contents

	RENDER_STR
	jsr mon::puts

.ifdef hard8x8
	ldxy #strings::debug_registers2
	RENDER_STR
	jsr mon::puts

	CALLMAIN ui::regs_contents
	txa
	clc
	adc #17
	tax
	bcc :+
	iny
:	jsr mon::puts
.endif

	ldxy sim::pc
	lda memory_mode
	bne :+
	stxy __dbgcmd_default_addr
	lda #$00
	sta __dbgcmd_default_addr+2
:	RETURN_OK
.endproc

;*******************************************************************************
; DISASM
; Disassembles from the given expression
.proc disasm
	lda memory_mode
	beq :+
	RETURN_ERR ERR_INVALID_COMMAND
:
@addr=zp::debuggertmp
@stopaddr=zp::debuggertmp+2
@buff=mem::spare+40
	; default stop address is start address + $10
	lda #$10
	jsr get_range_or_default
	bcs @ret

@l0:	cmpaddr @addr, addr_hi, @stopaddr, stop_hi
	bcs @done
	lda mon::int
	bne @done			; SIGINT, quit
	ldxy #@buff
	stxy r0
	ldxy @addr
	lda #$00			; disassemble to string
	CALLMAIN asm::disassemble
	bcc @ok

	; if invalid, just draw a literal byte value
	jsr @drawbyte
	jmp @next

@ok:	jsr @drawline
@next:	cmpaddr @addr, addr_hi, @stopaddr, stop_hi
	bcc @l0

@done:	lda addr_hi
	beq :+
	; stop at the end of virtual memory if the instruction crossed it
	lda #$00
	sta @addr
	sta @addr+1
:	ldxy @addr
	stxy __dbgcmd_default_addr
	lda addr_hi
	sta __dbgcmd_default_addr+2
	clc
@ret:	rts

;-------------------------------------------------------------------------------
@drawbyte:
	; unknown instruction
	ldxy @addr
	CALLMAIN vmem::load	; get the byte
	pha

	; if outputting to file, don't render addresses
	lda mon::outfile
	beq @db_with_addr
	incaddr @addr, addr_hi		; move past the byte we rendered
	ldxy #@byte_msg_no_addr
	RENDER_STR
	jmp mon::puts

;-------------------------------------------------------------------------------
@db_with_addr:
	; push the address
	lda @addr
	pha
	lda @addr+1
	pha

	incaddr @addr, addr_hi
	ldxy #@byte_msg
	RENDER_STR
	jmp mon::puts

;-------------------------------------------------------------------------------
@drawline:
	tax		; save instruction size

	; if outputting to file, don't render addresses
	lda mon::outfile
	beq @with_addr

	; update the address pointer
	txa		; get size of instruction
	clc
	adc @addr
	sta @addr
	bcc :+
	inc @addr+1
	bne :+
	inc addr_hi

:	ldxy #@buff
	RENDER_STR
	jmp mon::puts	; if address rendering is off, just print

;-------------------------------------------------------------------------------
@with_addr:
	; push the disassembled string
	lda #>@buff
	pha
	lda #<@buff
	pha

	; push the address
	lda @addr
	pha
	lda @addr+1
	pha

	; update the address pointer
	txa		; get size of instruction
	clc
	adc @addr
	sta @addr
	bcc :+
	inc @addr+1
	bne :+
	inc addr_hi

:	ldxy #@disasm_msg
	RENDER_STR
	jmp mon::puts

;-------------------------------------------------------------------------------
.PUSHSEG
.RODATA
@byte_msg:
	.byte "$", ESCAPE_VALUE, " .db $", ESCAPE_BYTE, 0
@byte_msg_no_addr:
	.byte ".db $", ESCAPE_BYTE, 0
@disasm_msg:
	.byte "$", ESCAPE_VALUE, " ", ESCAPE_STRING,0	; <address> <instruction>
.POPSEG
.endproc

;*******************************************************************************
; ASSEMBLE
; Assembles the given instruction at the address of the given expression
; e.g.
;  `>A $1000, lda #$00`
; If an enquoted value is provided, it will attempt to be opened as a file
; and assembled at the target address (NOTE: any .ORG directive within will take
; precedence)
.proc assemble
	lda memory_mode
	beq :+
	RETURN_ERR ERR_INVALID_COMMAND
:
@addr=zp::debuggertmp
@line=zp::debuggertmp+2
@err=r0
	; get the address to assemble at
	jsr eval
	bcs @ret		; return if address is invalid expression
	stxy @addr
	CALLMAIN asm::setpc

	jsr process_ws
	lda (zp::line),y
	bne @getfile
	RETURN_OK		; if no instruction provided, we're done

@getfile:
	; check if a filename was provided e.g.: >A $1000 "hello.asm"
	CALLMAIN util::parse_enquoted_string
	bcs @getop		; if failed to parse, try parsing opcode

@assemble_file:
	; TODO:

@getop:	lda zp::verify
	pha
	lda zp::gendebuginfo
	pha
	lda #$00
	sta zp::verify
	sta zp::gendebuginfo

	; do first pass of assembly on the entered line
	lda #$01
	sta zp::pass
	lda #FINAL_BANK_MAIN
	ldxy zp::line
	stxy @line
@pass1:	CALLMAIN asm::tokenize	; assemble the instruction
	bcs @asmdone		; if error occurred, skip pass 2

@pass2:	; do first pass of assembly on the entered line
	lda ctx::open
	bne :+
	ldxy @addr
	CALLMAIN asm::setpc

:	inc zp::pass
	ldxy @line
	CALLMAIN asm::tokenize	; assemble the instruction (again)

@asmdone:
	sta @err
	pla
	sta zp::gendebuginfo
	pla
	sta zp::verify
	bcc @nexti

	; error
	lda @err
	CALLMAIN err::get
	jsr mon::puts				; print the error
	clc
@ret:	rts

@nexti:	; prepopulate input buffer with ".A <next address> "
	lda #$61			; 'a'
	sta mem::linebuffer+1
	lda #' '
	sta mem::linebuffer+2
	lda #'$'
	sta mem::linebuffer+3
	lda zp::asmresult+1
	jsr hextostr
	sty mem::linebuffer+4
	stx mem::linebuffer+5
	lda zp::asmresult
	jsr hextostr
	sty mem::linebuffer+6
	stx mem::linebuffer+7
	lda #' '
	sta mem::linebuffer+8
	lda #$00
	sta mem::linebuffer+9

	jsr mon::inputrow	; buffer row -> screen row
	sta zp::cury

	ldx #$09
	stx zp::curx
	ldy #$00
	CALLMAIN cur::setmin

	ldxy #mon::getch
	CALLMAIN edit::gets
	ldxy #mem::linebuffer
	jsr __monitor_puts
	ldxy #mem::linebuffer+3
	stxy zp::line
	jmp assemble
.endproc

;*******************************************************************************
; SHOWMEM
; Shows the contents of memory at the target of the given expression
; e.g.
;  `>M $1000 $1020`
.proc showmem
@addr=zp::debuggertmp
@stop=zp::debuggertmp+2

	lda #8*8		; default to 8 lines (64 bytes)
	jsr get_range_or_default
	jcs @ret

@l0:	cmpaddr @addr, addr_hi, @stop, stop_hi
	bcs @done
	lda mon::int
	bne @done		; SIGINT, quit
	ldxy @addr
	lda memory_mode
	bne @image
	CALLMAIN ui::memline
	lda #$00
	sta mem::spare+SCREEN_WIDTH	; terminate the fixed-width memory row
	jmp @print
@image:	jsr image_memline
	bcs @ret
@print:	jsr mon::puts

	; move to address for next row
	lda @addr
	clc
	ldx memory_mode
	adc row_widths,x
	sta @addr
	bcc :+
	inc @addr+1
	bne :+
	inc addr_hi
:	jmp @l0
@done:	lda memory_mode
	beq @virtual
	cmpaddr @addr, addr_hi, @stop, stop_hi
	bcc @save

	ldxy @stop
	stxy @addr
	lda stop_hi
	sta addr_hi
	jmp @save

@virtual:
	; stop at the end of virtual memory if the last row crossed it
	lda addr_hi
	beq @save
	lda #$00
	sta @addr
	sta @addr+1
@save:	ldxy @addr
	stxy __dbgcmd_default_addr
	lda addr_hi
	sta __dbgcmd_default_addr+2
	clc			; OK
@ret:	rts
.endproc

;*******************************************************************************
; QUIT
; Quits the debugger, returning to the editor
.proc quit
	lda #$00
	sta dbg::interface
	inc mon::quit		; send QUIT signal
	rts
.endproc

;*******************************************************************************
; STEP
; Steps to the next instruction while debugging
.proc step
	CALLMAIN dbg::step
	jsr mon::update_pc_view		; follow the PC in the source view
	; print the registers
	jsr __dbgcmd_regs
	jmp put_instruction
.endproc

;*******************************************************************************
; STEP_OVER
; Steps over the next instruction while debugging.  Subroutines (JSR) are
; treated as one instruction
; instruction
.proc step_over
	CALLMAIN dbg::step_over
	jsr mon::update_pc_view		; follow the PC in the source view
	jsr __dbgcmd_regs
	jmp put_instruction
@done:	rts
.endproc

;*******************************************************************************
; TRACE
; Starts TRACE'ing the program.
.proc trace
	CALLMAIN dbg::trace
	jsr __dbgcmd_regs
	jmp put_instruction
@done:	rts
.endproc

;*******************************************************************************
; GO
; Continues program execution at the current PC
.proc go
	JUMPMAIN run::go
.endproc

;*******************************************************************************
; BACKTRACE
; Prints the call (JSR) stack (with symbols if possible).
; This is based on the contents of the stack, so any data on the stack may
; result in a bad rendering.
; An optional offset from the stack pointer can be given to adjust the
; stack's start location
; .e.g.
;  `>BT 8`
.proc backtrace
@sp=zp::debuggertmp
@offset=zp::debuggertmp+2
@lbl=zp::debuggertmp+2
@addr=zp::debuggertmp+4
@namebuff=lbl::namebuffer
	; check if an offset was given
	ldy #$00
	lda (zp::line),y
	tax
	beq @cont			; no offset specified, continue

	; get the offset
	jsr eval
	bcs @done			; invalid offset expression
	cpy #$01
	bcc :+
@err:	RETURN_ERR ERR_OVERSIZED_OPERAND	; offset must be <$80
:	cpx #$80
	bcs @err

@cont:	stx @offset
	lda sim::reg_sp
	sec			; +1
	adc @offset
	bcs @ok			; start is past $01ff -> nothing to trace
	sta @sp
	lda #>$0100		; MSB of stack base
	sta @sp+1

@l0:	lda mon::int
	bne @done		; SIGINT, quit
	jsr @draw_item		; draw the stack contents for this offset
	inc @sp			; move to next procedure in the stack
	beq @ok
	inc @sp
	bne @l0
@ok:	clc
@done:	rts

;-------------------------------------------------------------------------------
@draw_item:
	; get the address of the procedure call
	ldxy @sp				; LSB of stack address
	CALLMAIN vmem::load
	sec
	sbc #$02
	php
	sta @addr
	ldxy @sp
	inx					; MSB of stack address
	CALLMAIN vmem::load
	plp
	sbc #$00
	sta @addr+1

	; if there are no symbols, we can't symbolize the frame
	iszero lbl::num
	beq @nosym

	; get the symbol name for this address (if there is one)
	ldxy @addr
	CALLMAIN lbl::by_addr
	cmpw #$ffff
	beq @nosym
	stxy @lbl		; save the id of the label

	; subtract the address we found from the address we were looking for
	CALLMAIN lbl::getaddr
	stxy r0
	ldxy @addr
	sub16 r0
	txa
	pha
	tya
	pha

@label:	lda #>@namebuff
	pha
	sta r0+1
	lda #<@namebuff
	pha
	sta r0
	ldxy @lbl
	CALLMAIN lbl::getname

@push_addr:
	; push the address of the procedure call
	lda @addr
	pha					; push LSB
	lda @addr+1
	pha

	; push the stack pointer
	lda @sp
	pha

	ldxy #@backtrace_msg
	RENDER_STR		; render the message (consumes the pushed args)
	jmp mon::puts

@nosym:	; no symbols exist; push placeholders for the name and offset
	lda #$00
	pha			; offset LSB
	pha			; offset MSB
	lda #>strings::question_marks
	pha
	lda #<strings::question_marks
	pha
	jmp @push_addr

;-------------------------------------------------------------------------------
.PUSHSEG
.RODATA
@backtrace_msg:
	; <stack address> <address> <symbol>+<offset>
	.byte "$", ESCAPE_BYTE, " $", ESCAPE_VALUE, " "
	.byte ESCAPE_STRING, "+$", ESCAPE_VALUE,0
.POPSEG
.endproc

;*******************************************************************************
; STEP OUT
; Continues execution til the current subroutine returns with an RTS
.proc step_out
	CALLMAIN dbg::step_out
	jsr mon::update_pc_view		; follow the PC in the source view
	jsr __dbgcmd_regs
	jmp put_instruction
.endproc

;*******************************************************************************
; SAVEMEM
; Saves the given memory range to the specified file
; e.g.
;  `>S $1000 $2000 FILE.PRG`
; IN:
;   - zp::line: start address, exclusive end address and filename
; OUT:
;   - .C: set on error, with the error code in .A
.proc savemem
@startaddr=zp::debuggertmp
@stopaddr=zp::debuggertmp+2
@nonempty=zp::debuggertmp+4
	jsr get_range
	bcs @done
	cmpaddr @startaddr, addr_hi, @stopaddr, stop_hi
	bcs @empty

	lda #$01
	bne @range
@empty:	lda #$00
@range:	sta @nonempty

	CALLMAIN scr::blank

	; open the output file for writing
	ldxy zp::line
	CALLMAIN file::open_w
	bcs @err
	pha

	; save the given memory range
	ldx @nonempty
	beq @close
	ldxy @startaddr
	jsr save_range
	bcs @saverr

	; close the file
@close:	pla
	CALLMAIN file::close
	jsr @err				; restore IRQ
	RETURN_OK

@saverr:
	sta @startaddr				; save error code
	pla					; get the file handle
	CALLMAIN file::close			; close the file we opened
	lda @startaddr				; restore error code

@err:	pha					; save error code
	CALLMAIN scr::unblank
	pla
	sec
@done:	rts
.endproc

;*******************************************************************************
; DUMP
; Outputs a dump of the given memory range in a format that can be assembled
; (as .db statements)
.proc dump
@addr=zp::debuggertmp
@stop=zp::debuggertmp+2
@cnt=zp::debuggertmp+4
@line=zp::debuggertmp+5
@buff=mem::spare+40
	lda #8*8			; default size of range
	jsr get_range_or_default
	bcc @l0
:	jmp @done

@l0:	cmpaddr @addr, addr_hi, @stop, stop_hi
	jcs @ok
	lda mon::int
	bne :-				; SIGINT, quit
	ldxy #@buff+4
	stxy @line

	lda #'.'
	sta @buff
	lda #'d'
	sta @buff+1
	lda #'b'
	sta @buff+2
	lda #' '
	sta @buff+3

	; get 8 bytes (1 row of .DB's)
	lda #$08
	sta @cnt

@l1:	cmpaddr @addr, addr_hi, @stop, stop_hi
	bcs @cont

	ldy #$00
	lda #'$'
	sta (@line),y

	lda addr_hi
	sta access_hi
	ldxy @addr
	jsr vmem_load
	bcs @done
	jsr hextostr
	tya
	ldy #$01
	sta (@line),y
	iny
	txa
	sta (@line),y
	iny
	lda #','
	sta (@line),y

	incaddr @addr, addr_hi		; on to the next byte

	lda @line
	clc
	adc #$04
	sta @line
	bcc :+
	inc @line+1
:	dec @cnt
	bne @l1

@cont:	decw @line	; delete the last ','
	lda #$00
	tay
	sta (@line),y	; terminate buffer

	ldxy #@buff
	jsr mon::puts

	cmpaddr @addr, addr_hi, @stop, stop_hi ; @l1 already advanced past row
	bcs :+
	jmp @l0			; next row
:
@ok:	clc			; ok
@done:	rts
.endproc

;*******************************************************************************
; NEW
; Reinitializes BASIC in "virtual" (user) memory
.proc new
	lda memory_mode
	beq :+
	RETURN_ERR ERR_INVALID_COMMAND
:
	CALLMAIN run::clr
	RETURN_OK
.endproc

;*******************************************************************************
; CLEAR
; Clears the terminal
.proc clear
	jsr mon::clear
	RETURN_OK
.endproc

;*******************************************************************************
; GET RANGE OR DEFAULT
; Evaluate one-two arguments representing an address range and stores the
; results.
; If no end to the range is provided, returns the start address + the given
; default range size
; IN:
;   - .A: the default size of the range
; OUT:
;   - .C:                set if a (valid) range was not given
;   - zp::debuggertmp:   the start of the range
;   - zp::debuggertmp+2: the end of the range
;   - addr_hi:           high byte of the start
;   - stop_hi:           high byte of the end
.proc get_range_or_default
@start = zp::debuggertmp
@stop  = zp::debuggertmp+2
@size  = zp::debuggertmp+4
	sta @size
	jsr memory_ready
	jcs @ret
	ldy #$00
	lda (zp::line),y
	bne :+

	; line is empty use default start address
	ldxy __dbgcmd_default_addr
	stxy @start
	lda __dbgcmd_default_addr+2
	sta addr_hi
	jmp @default		; jump ahead to compute default stop address

:	; get the start address
	jsr eval_address
	stxy @start
	jcs @ret
	lda expr::value+2
	sta addr_hi

	; are we at the end of the line?
	jsr eat_whitespace
	ldy #$00
	lda (zp::line),y
	bne @cont

@default:
	lda @size
	clc
	adc @start
	sta @stop
	sta __dbgcmd_default_addr
	lda @start+1
	adc #$00
	sta @stop+1
	sta __dbgcmd_default_addr+1
	lda addr_hi
	adc #$00
	sta stop_hi
	cmpaddr @stop, stop_hi, mem_limit, mem_limit+2
	bcc :+

	ldxy mem_limit
	stxy @stop
	stxy __dbgcmd_default_addr
	lda mem_limit+2
	sta stop_hi

:	lda stop_hi
	sta __dbgcmd_default_addr+2
	jsr eat_whitespace
	RETURN_OK

@cont:	; get the stop address
	jsr eat_whitespace
	jsr eval_address
	bcs @ret
	stxy @stop
	stxy __dbgcmd_default_addr

	lda expr::value+2
	sta stop_hi
	sta __dbgcmd_default_addr+2
	jsr check_range
	bcs @ret

	jsr eat_whitespace
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; GET RANGE
; Evaluate two arguments representing an address range and stores the results
; OUT:
;   - .C:                set if a (valid) range was not given
;   - zp::debuggertmp:   the start of the range
;   - zp::debuggertmp+2: the end of the range
;   - addr_hi:           high byte of the start
;   - stop_hi:           high byte of the end
.proc get_range
@start=zp::debuggertmp
@stop=zp::debuggertmp+2
	; get the start address
	jsr eval_address
	stxy @start
	bcs @ret
	lda expr::value+2
	sta addr_hi

	; get the stop address
	jsr eat_whitespace
	jsr eval_address
	stxy @stop
	bcs @ret

	lda expr::value+2
	sta stop_hi
	jsr check_range
	bcs @ret

	jsr eat_whitespace
	clc
@ret:	rts
.endproc

;*******************************************************************************
; IS_WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.export is_whitespace
.proc is_whitespace
	cmp #$0d	; newline
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #' '
:	rts
.endproc

;*******************************************************************************
; EAT_WHITESPACE
; Updates zp::line to point past any whitespace.
.proc eat_whitespace
	php
	ldy #$00
@l0:	lda (zp::line),y
	beq @done
	jsr is_whitespace
	bne @done
	incw zp::line
	bne @l0			; branch always

@done:	plp
	rts
.endproc

;*******************************************************************************
; INLINE HELPERS
inline_proc hextostr, util::hextostr

;*******************************************************************************
; PUT INSTRUCTION
; Prints the instruction at the .PC
.proc put_instruction
	; disassemble the instruction that we're at now
	ldx sim::pc
	ldy sim::pc+1
	lda #<$100
	sta r0
	lda #>$100
	sta r0+1
	lda #$00		; disassemble to string
	CALLMAIN asm::disassemble

	; print the disassembled instruction of ??? if we couldn't disassemble
	bcc :+
	ldxy #strings::question_marks
	jsr mon::puts_main
	RETURN_OK
:	ldxy #$100
	jsr mon::puts
	RETURN_OK
.endproc

;*******************************************************************************
; DEBUGGING
; Checks if the user is currently debugging a program.
; Some commands are only valid while debugging
; OUT:
;  - .C: set if the user is debugging a program
.proc debugging
	; get the debugging flag
	lda edit::debugging
	bne :+
	ldxy #@not_debugging_msg
	RENDER_STR
	jsr mon::puts
	clc		; flag NOT debugging
	rts

:	sec		; flag that we ARE debugging
	rts
.PUSHSEG
.RODATA
@not_debugging_msg: .byte "not debugging",0
.POPSEG
.endproc

;*******************************************************************************
; SHOW FILES
; Shows all files loaded in the debug info
.proc show_files
@cnt=zp::debuggertmp
	lda #$00
	sta @cnt
	cmp dbgi::numfiles
	beq @done
:	lda @cnt
	CALL FINAL_BANK_MAIN, dbgi::get_filename
	bcs @done
	jsr mon::puts
	inc @cnt
	lda @cnt
	cmp dbgi::numfiles
	bne :-
@done:	RETURN_OK
.endproc

;*******************************************************************************
; EVAL
; Calls "expr::eval" and returns
; IN:
;   - zp::line: the text for the expression to evaluate
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
;  - zp::line: updated to point beyond the parsed expression
.proc eval
	CALLMAIN expr::eval
	bcs @ret
	cmp #$03
	bcc @ret
	RETURN_ERR ERR_OVERSIZED_OPERAND
@ret:	rts
.endproc

;*******************************************************************************
; VMEM_LOAD
; Calls vmem::load or image::load for the selected mode
; IN:
;  - .XY:       low word of the address
;  - access_hi: high byte of the address
; OUT:
;  - .A: byte read, or error code on failure
;  - .C: set on error
.proc vmem_load
	lda memory_mode
	bne @image
	lda access_hi
	bne @bad
	CALLMAIN vmem::load
	RETURN_OK

@image:	stxy image::cursor
	lda access_hi
	sta image::cursor+2
	JUMP FINAL_BANK_LINKER_AUX, image::load
@bad:	RETURN_ERR ERR_FILE_TOO_BIG
.endproc

;*******************************************************************************
; VMEM STORE
; Calls vmem::store or image::store for the selected mode
; IN:
;  - .A:        byte to store
;  - .XY:       low word of the address
;  - access_hi: high byte of the address
; OUT:
;  - .A: error code on failure
;  - .C: set on error
.proc vmem_store
	pha
	lda memory_mode
	bne @image
	lda access_hi
	bne @bad
	pla
	CALLMAIN vmem::store
	RETURN_OK

@image:	stxy image::cursor
	lda access_hi
	sta image::cursor+2
	pla
	CALL FINAL_BANK_LINKER_AUX, image::store
	bcs @ret

	; include patches below the original image start in subsequent saves
	cmpaddr image::cursor, image::cursor+2, image::start, image::start+2
	bcs @ok
	ldxy image::cursor
	stxy image::start
	lda image::cursor+2
	sta image::start+2
@ok:	clc
@ret:	rts
@bad:	pla
	RETURN_ERR ERR_FILE_TOO_BIG
.endproc

;*******************************************************************************
; PROCESS_WS
; Calls "line::process_ws" in the MAIN bank
.proc process_ws
	JUMPMAIN line::process_ws
.endproc

;*******************************************************************************
; UPDATE
; Checks the disk for a Monster binary file and opens the flasher confirmation
; if found.
; OUT:
;   - .C: set if no update file was found
.ifdef vic20
.proc flash_update
	ldy #$00
	lda (zp::line),y
	beq :+

	RETURN_ERR ERR_INVALID_COMMAND
:	JUMP FINAL_BANK_FLASH, flash::launch
.endproc
.endif

;*******************************************************************************
; SELECT MODE
; Selects or reports the monitor address space. Real mode is reserved.
; IN:
;   - zp::line: mode name or empty string
; OUT:
;   - .A: error code on failure
;   - .C: set on invalid or unavailable mode
.proc select_mode
@name=r0
	ldy #$00
	lda (zp::line),y
	beq @report

	ldx #$00
@try:	stx mode_index
	ldy #$00
@char:	lda @names,x
	beq @endname
	cmp (zp::line),y
	bne @next
	inx
	iny
	bne @char

;-------------------------------------------------------------------------------
@endname:
	lda (zp::line),y
	beq @match
	cmp #' '
	bne @next
@tail:	iny
	lda (zp::line),y
	beq @match
	cmp #' '
	beq @tail
@next:	ldx mode_index
@skip:	lda @names,x
	inx
	cmp #$00
	bne @skip
	cpx #@names_end-@names
	bcc @try
@bad:	RETURN_ERR ERR_INVALID_COMMAND

;-------------------------------------------------------------------------------
@match:
	ldx mode_index
	cpx #@real_name-@names
	beq @bad
	lda #MON_MODE_VIRTUAL
	cpx #@image_name-@names
	bne @set

	lda image::mode
	cmp #IMAGE_MODE_READY
	bne @bad
	lda #MON_MODE_IMAGE
@set:	sta memory_mode
	lda #$00
	sta __dbgcmd_default_addr
	sta __dbgcmd_default_addr+1
	sta __dbgcmd_default_addr+2

@report:
	ldxy #@virtual_name
	lda memory_mode
	beq :+
	ldxy #@image_name
:	stxy @name

	; copy the mode name to shared RAM before printing from the main bank
	ldy #$00
:	lda (@name),y
	sta mem::spare,y
	iny
	cmp #$00
	bne :-
	ldxy #mem::spare
	jsr mon::puts
	RETURN_OK

;-------------------------------------------------------------------------------
@names:
@virtual_name: .byte "virtual",0
	       .byte "normal",0
@image_name:   .byte "image",0
@real_name:    .byte "real",0
@names_end:
.endproc

;*******************************************************************************
; EVAL ADDRESS
; Evaluates an address in the selected memory space.
; IN:
;   - zp::line: expression to evaluate
; OUT:
;   - .XY:           low word
;   - expr::value+2: high byte
;   - .C:            set if the expression is invalid or exceeds the space's capacity
.proc eval_address
	jsr memory_ready
	bcs @ret
	jsr eat_whitespace
	CALL FINAL_BANK_EXPR, expr::eval_wide
	bcs @ret
	lda expr::kind
	beq :+
	RETURN_ERR ERR_INVALID_EXPRESSION

:	jsr check_limit
	ldxy expr::value
@ret:	rts
.endproc

;*******************************************************************************
; MEMORY READY
; Gets the capacity of the selected memory space.
; IN:
;   - memory_mode: selected space
; OUT:
;   - mem_limit: exclusive address limit
;   - .C:        set if image mode has no completed image
.proc memory_ready
	lda #$00
	sta mem_limit
	sta mem_limit+1
	lda #$01
	sta mem_limit+2
	lda memory_mode
	beq @ok
	lda image::mode
	cmp #IMAGE_MODE_READY
	beq :+
	RETURN_ERR ERR_INVALID_COMMAND

:	lda #<image::capacity
	sta mem_limit
	lda #>image::capacity
	sta mem_limit+1
	lda #.bankbyte(image::capacity)
	sta mem_limit+2
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; CHECK LIMIT
; Checks an address or exclusive endpoint against the selected capacity.
; IN:
;   - expr::value: address
;   - mem_limit: exclusive limit
; OUT:
;   - .C: set if the address exceeds the limit
.proc check_limit
	cmpaddr mem_limit, mem_limit+2, expr::value, expr::value+2
	bcs @ok
	RETURN_ERR ERR_FILE_TOO_BIG
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; CHECK RANGE
; Checks that a memory range's end is not before its start.
; IN:
;   - zp::debuggertmp:   low word of the start
;   - addr_hi:           high byte of the start
;   - zp::debuggertmp+2: low word of the exclusive end
;   - stop_hi: high byte of the exclusive end
; OUT:
;   - .C: set if the range is reversed
.proc check_range
	cmpaddr zp::debuggertmp+2, stop_hi, zp::debuggertmp, addr_hi
	bcs @ok
	RETURN_ERR ERR_SEGMENT_OUT_OF_RANGE
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; CHECK MOVE
; Checks that a move fits at its destination before copying bytes.
; IN:
;   - zp::debuggertmp:     low word of the start
;   - addr_hi:             high byte of the start
;   - zp::debuggertmp+2:   low word of the exclusive end
;   - stop_hi:             high byte of the exclusive end
;   - zp::debuggertmp+4:   low word of the destination
;   - target_hi:           high byte of the destination
; OUT:
;   - .C: set if the destination range exceeds capacity
.proc check_move
@len=r0
	sec
	lda zp::debuggertmp+2
	sbc zp::debuggertmp
	sta @len

	lda zp::debuggertmp+3
	sbc zp::debuggertmp+1
	sta @len+1
	lda stop_hi
	sbc addr_hi
	sta @len+2
	clc
	lda zp::debuggertmp+4
	adc @len
	sta expr::value

	lda zp::debuggertmp+5
	adc @len+1
	sta expr::value+1
	lda target_hi
	adc @len+2
	sta expr::value+2
	bcc :+
	RETURN_ERR ERR_FILE_TOO_BIG
:	jmp check_limit
.endproc

;*******************************************************************************
; CHECK COMPARE
; Checks that both comparison ranges fit before reading bytes.
; IN:
;   - zp::debuggertmp:   low word of the first address
;   - addr_hi:           high byte of the first address
;   - zp::debuggertmp+2: low word of the second address
;   - target_hi:         high byte of the second address
;   - zp::debuggertmp+4: low word of the byte count
;   - count_hi:          high byte of the byte count
; OUT:
;   - .XY: low word of the count
;   - .C: set if either range exceeds capacity
.proc check_compare
	lda addr_hi
	ldx #$00
	jsr @block
	bcs @ret

	lda target_hi
	ldx #$02
	jsr @block
@ret:	ldxy zp::debuggertmp+4
	rts

@block:	pha
	clc
	lda zp::debuggertmp,x
	adc zp::debuggertmp+4
	sta expr::value
	lda zp::debuggertmp+1,x
	adc zp::debuggertmp+5
	sta expr::value+1
	pla
	adc count_hi
	sta expr::value+2
	bcc :+
	RETURN_ERR ERR_FILE_TOO_BIG
:	jmp check_limit
.endproc

;*******************************************************************************
; SAVE RANGE
; Writes bytes from the selected memory space to an open output file.
; IN:
;   - .A: file handle
;   - zp::debuggertmp:   low word of the start
;   - addr_hi:           high byte of the start
;   - zp::debuggertmp+2: low word of the exclusive end
;   - stop_hi:           high byte of the exclusive end
; OUT:
;   - .C: set on file or memory error
.proc save_range
@addr=zp::debuggertmp
@stop=zp::debuggertmp+2
	tax
	jsr krn::chkout
	bcs @err
@l0:	jsr krn::readst
	bne @err
	cmpaddr @addr, addr_hi, @stop, stop_hi
	bcs @done

	lda addr_hi
	sta access_hi
	ldxy @addr
	jsr vmem_load
	bcs @ret
	jsr krn::chrout

	incaddr @addr, addr_hi
	jmp @l0

@err:	RETURN_ERR ERR_IO_ERROR
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; PRINT LONG
; Prints the given 24-bit value to the console in hex.
; IN:
;   - .A:  high byte
;   - .XY: low word
; OUT:
;   - .C: set on output error
.proc print_long
@buff=zp::debuggertmp
	pha
	tya
	pha
	txa
	jsr hextostr
	stx @buff+6
	sty @buff+5
	pla
	jsr hextostr

	stx @buff+4
	sty @buff+3
	pla
	jsr hextostr
	stx @buff+2
	sty @buff+1

	lda #'$'
	sta @buff
	lda #$00
	sta @buff+7

	ldxy #@buff
	jmp mon::puts
.endproc

;*******************************************************************************
; PRINT VALUE
; Prints the full expression result.
; IN:
;   - expr::value: 24-bit result
; OUT:
;   - .C: set on output error
.proc print_value
	ldxy expr::value
	lda expr::value+2
	jmp print_long
.endproc

;*******************************************************************************
; IMAGE MEMLINE
; Returns a line containing image bytes along with their text rendering.
; IN:
;   - .XY:               low word of the address to render
;   - addr_hi:           high byte of the address to render
;   - zp::debuggertmp+2: low word of the exclusive end
;   - stop_hi:           high byte of the exclusive end
; OUT:
;   - .XY: rendered text in mem::spare
;   - .C:  set on memory error
.proc image_memline
@src=ra
@col=rc
@stop=zp::debuggertmp+2
	stxy @src
	lda addr_hi
	sta access_hi

	; initialize line to empty (all spaces)
	lda #' '
	ldx #IMAGE_TEXT_END-1
:	sta mem::spare,x
	dex
	bpl :-
	lda #$00
	sta mem::spare+IMAGE_TEXT_END

@l0:	; draw the address of this line
	lda access_hi
	jsr hextostr
	sty mem::spare
	stx mem::spare+1
	lda @src+1
	jsr hextostr
	sty mem::spare+2
	stx mem::spare+3
	lda @src
	jsr hextostr
	sty mem::spare+4
	stx mem::spare+5
	lda #':'
	sta mem::spare+6

	ldx #$00
@l1:	stx @col
	cmpaddr @src, access_hi, @stop, stop_hi
	bcs @done

	; get a byte to display
	ldxy @src
	jsr vmem_load
	bcs @ret
	pha			; save the byte

	incaddr @src, access_hi	; update @src to the next byte

@val2ch:
	; get the character representation of the byte
	cmp #$20
	bcc :+
	cmp #$80
	bcc @cont
:	lda #'.'		; use '.' for undisplayable chars

@cont:	ldx @col
	sta mem::spare+IMAGE_TEXT_START,x ; write the character representation
	pla			; get the byte we're rendering
	jsr hextostr		; convert to hex characters
	txa			; get LSB char
	pha			; and save temporarily
	lda @col		; get col*3 (column to draw byte)
	asl
	adc @col
	tax
	pla			; restore LSB char to render
	sta mem::spare+9,x	; store to text buffer
	tya			; get MSB
	sta mem::spare+8,x	; store to text buffer
	ldx @col
	inx
	cpx #IMAGE_ROW_BYTES	; have we drawn all columns?
	bcc @l1			; repeat until we have

@done:	ldxy #mem::spare
	clc
@ret:	rts
.endproc

;*******************************************************************************
.if .defined(vic20) .and .defined(hard8x8)
IMAGE_ROW_BYTES = $03
row_widths: .byte $04, IMAGE_ROW_BYTES
.else
IMAGE_ROW_BYTES = $04
row_widths: .byte $08, IMAGE_ROW_BYTES
.endif
IMAGE_TEXT_START = 8+IMAGE_ROW_BYTES*3
IMAGE_TEXT_END = IMAGE_TEXT_START+IMAGE_ROW_BYTES

;*******************************************************************************
; COMMANDS
commands:
.byte "mode",0
.byte "clear",0    ; clear the terminal
.byte "wa",0	   ; watch add
.byte "wal",0	   ; watch add load
.byte "was",0	   ; watch add store
.byte "wr",0	   ; watch remove
.byte "w",0	   ; list watches
.byte "b",0	   ; list breakpoints
.byte "ba",0	   ; breakpoint add by addr
.byte "bl",0	   ; breakpoint add by line
.byte "br",0	   ; breakpoint remove
.byte "f",0	   ; fill memory in the given address range with the given data
.byte "dump",0	   ; dumps the given address range
.byte "move",0	   ; move memory from the given address range to the target address
.byte "new",0	   ; reinitializes BASIC
.byte "g",0	   ; goto given expression/address
.byte "c",0	   ; compare the memory in the two given ranges
.byte "h",0	   ; hunts the given address range for the given data
.byte "r",0	   ; shows the contents of the registers (if debugging)
.byte "d",0	   ; disassembles from the given address
.byte "a",0	   ; assembles the given instruction given address
.byte "m",0	   ; show contents of memory at the given address
.byte "t",0	   ; start TRACE'ing
.byte "x",0	   ; quit the debugger
.byte "z",0	   ; step to the next instruction (if debugging)
.byte "n",0	   ; step over the next instruction (if debugging)
.byte "g",0	   ; go (continue program execution) (if debugging)
.byte "bt",0	   ; backtrace (if debugging)
.byte "zo",0	   ; step out of current subroutine (if debugging)
.byte "p",0	   ; poke a single byte to the given address
.byte "s",0	   ; save memory
.byte "files",0	   ; shows all files that are loaded in debug-info
.byte "?",0	   ; evaluate an integer or floating point expression
.ifdef vic20
.byte "update",0   ; leave Monster and run the updater
.endif

.linecont +
.define command_vectors select_mode, clear, add_watch, add_watch_load, add_watch_store, \
	remove_watch, list_watches, list_breakpoints, add_break_addr, \
	add_break_line, remove_break, fill, dump, move, new, goto, compare, \
	hunt, __dbgcmd_regs, disasm, assemble, showmem, trace, quit, step, \
	step_over, go, backtrace, step_out, poke, savemem, show_files, evaluate_value
.linecont -
commandslo: .lobytes command_vectors
.ifdef vic20
	.lobytes flash_update
.endif
commandshi: .hibytes command_vectors
.ifdef vic20
	.hibytes flash_update
.endif
num_commands=*-commandshi
