.ifndef ZEROPAGE_INC
ZEROPAGE_INC = 1

.scope zp

jmpaddr = $02		; JMP instruction
jmpvec  = $03 		; JMP vector for calling vectors

banksp      = $05	; bank stack pointer
bankval     = $06	; the value written by store_byte
bankjmpaddr = $07	; JMP in struction for banked calls
bankjmpvec  = $08	; JMP vector

memsrc      = $0a	; temporary pointer for memory operations
memdst      = $0c	; temporary pointer for memory operations

debug = $10		; $10-$28 debug info generation

debugger    = $28	; $28-$29 used by debugger
debuggertmp = $29	; $29-$30 debugger temp storage

str0 = $30		; $30-$31 string pointer 1
str1 = $31
str2 = $32		; $32-$33 string pointer 2
str3 = $33

curx   = $34		; column of the cursor on screen
cury   = $35		; row of the cursor
curtmr = $36		; timer until next cursor blink

asmresult = $37 	; pointer where code is assembled
virtualpc = $39 	; pointer to address address code will live at during
			; runtime (.RORG)
verify = $3b 		; flag for verifying (vs. assembling)
asmtmp = $3c 		; scratchpad for assembly
guitmp = $3c

disasm = $40		; base address of disassembly tmp vars
link   = $40		; base address of linker temp storage
gui    = $40		; base address for GUI routines

pass         = $4c	; the pass of assembly being done (1 or 2)
line         = $4d	; ptr to text being processed
gendebuginfo = $4f 	; flag to generate debug info

label_value     = $50	; label function arguments
label_mode      = $52	; label "mode" - zp (0) or abs (1)
label_segmentid = $53	; SEGMENT ID (or $ff for absolute) when adding label
labels          = $54	; reserved for label usage

ctx    = $70		; context util usage during assembly
macros = $7f		; macro related things during assembly

; * $90-9C are used by KERNAL I/O and should be avoided *
io_status       = $90	; status byte (READST)
io_keyrow       = $91	; row of keyscan
;io_tapetm      = $92	; time constant for tape
io_loadflag     = $93	; 0 = load, 1 = verify
io_iecwaiting   = $94	; 0 = no char waiting, !0 = char waiting
;io_cass_sync   = $96	; cassete synchronization #
;io_regsave     = $97
numfiles        = $98	; # of OPEN files
io_input_dev    = $99	; input device #
io_output_dev   = $9a	; output device #
;io_tape_parity = $9b
;io_tape_buff   = $9e	; tape pass 1 error log/character buffer
;io_tape_fix    = $9f	; tape pass 2 error log corrected

bank      = $9b 	; bank to operate on for some ram:: routines
banktmp   = bank+1	; bank temp/arg storage for ram:: routines
bankstack = banktmp+3	; small call stack for ram::call (bank bytes only)

bankoffset = $a6	; offset for relative reads/writes
bankaddr0  = $a7
bankaddr1  = $a9

; * $ab-$b6 is reserved for miscellaneous isolated routines *
; These routines should not call any of the other routines that share these
; zeropage allocations
;  - text
;  - expressions
text = $ab
expr = $ab

; * $b7-$c0 reserved for file stuff *
;namelen = $b7	; filename length (SETLFS)
lfs      = $b8	; active logical file (SETLFS)
sa       = $b9	; active secondary address (SETLFS)
		; NOTE: secondary addresses must be unique for each open file
device   = $ba	; the last used drive #
file     = $bb	; the most recently accessed file
keycnt   = $cb	; # of keys in key buffer
key      = $cb	; current keypress

keytab   = $f5	; key decode table

; * $cf-$d9 stores source *
srccur   = $cf	; left pointer of gap in active source buffer
srccur2  = $d1	; right pointer of gap in active source buffer
srcline  = $d3	; current line number in active source buffer
srclines = $d5	; current number of lines in active source buffer
srcend   = $d7	; pointer to end of buffer in active source buffer

editor 	  	= $d9
editor_mode	= $db	; editor mode (MODE_COMMAND or MODE_INSERT)
editortmp 	= $dc

; $e0-$ff temporary storage
; this is the primary scratchpad for routines.
; Be cautious when relying on this data to be in tact
; after calling a procedure as it is likely that that
; procedure uses some of these locations
tmp10 = $e0
tmp11 = $e1
tmp12 = $e2
tmp13 = $e3
tmp14 = $e4
tmp15 = $e5
tmp16 = $e6
tmp17 = $e7

; $e8-$ea is utility storage
util = $e8

; "inline" procedure storage
inline = $ec
.endscope

r0 = $f0
r1 = $f1
r2 = $f2
r3 = $f3
r4 = $f4
r5 = $f5
r6 = $f6
r7 = $f7
r8 = $f8
r9 = $f9
ra = $fa
rb = $fb
rc = $fc
rd = $fd
re = $fe
rf = $ff
.endif
