;*******************************************************************************
; STRINGS.ASM
; This file contains string constants used throughout the program.
;*******************************************************************************

.include "text.inc"

.define yes_no "(", $79, "/", $6e, ")"		; lowercase (y/n)

;*******************************************************************************
.RODATA

.export __str_machine_state
__str_machine_state:
.ifdef vic20
.byte "line cyc hpos  v1t1  v1t2  v2t1  v2t2",0
.else
.byte "line cyc hpos",0
.endif

.export __str_buffers
__str_buffers: .byte "buffers",0

.export __str_noname
__str_noname: .byte "[no name]",0

.export __str_nolabels
__str_nolabels: .byte "no labels",0

.export __str_nolog
__str_nolog: .byte "no log",0

.export __str_nomacros
__str_nomacros: .byte "no macros",0

.export __str_null
__str_null = *-1

.export __str_endrep
__str_endrep: .byte ".endrep",0

.export __str_breakpoints_title
__str_breakpoints_title: .byte "breakpoints",0

.export __str_question_marks
__str_question_marks: .byte "???",0

.export __str_debug_brk_line
__str_debug_brk_line: .byte "brk in ", ESCAPE_STRING, " line ",ESCAPE_VALUE_DEC,0

.export __str_debug_brk_addr
__str_debug_brk_addr: .byte "brk @ ", ESCAPE_VALUE, "  ", ESCAPE_STRING,0

.export __str_debug_registers
.ifdef hard8x8
__str_debug_registers: .byte " pc  a  x  y  sp",0
__str_debug_registers2: .byte "nv-bdizc addr      clk",0
.else
__str_debug_registers: .byte " pc  a  x  y  sp nv-bdizc addr      clk",0
__str_debug_registers2: .byte 0
.endif

.export __str_debug_registers2

.export __str_debug_stop_debugging
__str_debug_stop_debugging: .byte "stop debugging? y/n",0

.export __str_device_not_present
__str_device_not_present: .byte "device not present",0

.export __str_drive_error
__str_drive_error: .byte "drive error",0

.export __str_jam_detected
__str_jam_detected: .byte "jam detected",0

.export __str_illegal_detected
__str_illegal_detected: .byte "illegal detected",0

.export __str_vital_addr_clobber_detected
.ifdef hard8x8
__str_vital_addr_clobber_detected: .byte "dangerous write:$", ESCAPE_VALUE, "",0
.else
__str_vital_addr_clobber_detected: .byte "dangerous write detected ($", ESCAPE_VALUE, ")",0
.endif

.export __str_saved
__str_saved: .byte "saved",0

.export __str_saveall
__str_saveall: .byte "save all buffers? ", yes_no, 0

.export __str_watch_triggered
__str_watch_triggered:
.byte "watch @ $", ESCAPE_VALUE, "=$", ESCAPE_BYTE, " ", ESCAPE_STRING, " ", ESCAPE_STRING, 0

.export __str_invalid_command
__str_invalid_command: .byte "invalid command", 0

.export __str_done
__str_done: .byte "done",0

.export __str_edit_line_err
__str_edit_line_err: .byte ESCAPE_STRING, " l", ESCAPE_VALUE_DEC,":", ESCAPE_STRING,0

.export __str_edit_file_load_failed
__str_edit_file_load_failed: .byte "load error $",ESCAPE_BYTE,0

.export __str_edit_file_delete_failed
__str_edit_file_delete_failed: .byte "delete error $", ESCAPE_BYTE, 0

.export __str_files
__str_files: .byte "files",0

.export __str_file_open_failed
__str_file_open_failed: .byte "open error $", ESCAPE_BYTE, 0

.export __str_newl
__str_newl: .byte $0d,0

.export __str_no_file
__str_no_file: .byte "no input file specified", 0

.export __str_deleting
__str_deleting: .byte "deleting...",0

.export __str_loading
__str_loading: .byte "loading...",0

.export __str_saving
__str_saving: .byte "saving...",0

.export __str_aborted
__str_aborted: .byte "aborted",0

.export __str_assembling
__str_assembling: .byte "assembling...",0

.export __str_linking
__str_linking: .byte "linking...",0

.export __str_edit_file_save_failed
__str_edit_file_save_failed: .byte "failed to save file; error ", ESCAPE_BYTE, 0

.export __str_watches_title
__str_watches_title: .byte "watches",0

.export __str_dir
__str_dir: .byte "$",0

.export __str_dumping
__str_dumping: .byte "dumping...",0

.export __str_macros
__str_macros: .byte "macros",0

.export __str_memory
__str_memory: .byte "memory",0

.export __str_segments
__str_segments: .byte "segments",0

.export __str_load
__str_load: .byte "load",0

.export __str_link
__str_link: .byte "link",0

.export __str_run
__str_run: .byte "run",0

.export __str_watches_range_line
__str_watches_range_line: .byte ESCAPE_BYTE, ESCAPE_CHAR, " $", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0

.export __str_errors
__str_errors: .byte "errors",0

.export __str_tracing
.ifdef hard8x8
; the 22 column screen can't fit the long form of the message
__str_tracing: .byte "tracing.. restore=stop",0
.else
__str_tracing: .byte "tracing.. press [restore] to stop",0
.endif

.export __str_tracing_msg
__str_tracing_msg: .byte "tracing...",0

.export __str_tracing_stop
.ifdef hard8x8
__str_tracing_stop: .byte "restore=stop",0
.else
__str_tracing_stop: .byte "press [restore] to stop",0
.endif

.export __str_pass1
__str_pass1:
	.byte ESCAPE_SPACING
.ifdef hard8x8
	.byte 8
.else
	.byte 15
.endif
	.byte "pass 1",0

.export __str_pass2
__str_pass2:
	.byte ESCAPE_SPACING
.ifdef hard8x8
	.byte 8
.else
	.byte 15
.endif
	.byte "pass 2",0

.export __str_watches_line
;   $1000 : $10
__str_watches_line:
.byte ESCAPE_BYTE, "  $", ESCAPE_VALUE, ": ", ESCAPE_BYTE, 0

; ! $1000 : $10 > $20
.export __str_watches_changed_line
__str_watches_changed_line:
.byte ESCAPE_BYTE, "! $", ESCAPE_VALUE, ": ", ESCAPE_BYTE, CH_R_ARROW, ESCAPE_BYTE, 0

;*******************************************************************************
; These strings are modified thus are not in RODATA
.DATA
; <filename> l: <line no.> <symbol> : <addr>
.export __str_breakpoints_line
.export __str_breakpoints_line_noname
.ifdef hard8x8
__str_breakpoints_line: .byte "  ", ESCAPE_BYTE," ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, 0
__str_breakpoints_line_noname: .byte "  ", ESCAPE_BYTE," ", " $", ESCAPE_VALUE,0
.else
__str_breakpoints_line: .byte "  ", ESCAPE_BYTE," ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, " [", ESCAPE_STRING, "] $", ESCAPE_VALUE,0
__str_breakpoints_line_noname: .byte "  ", ESCAPE_BYTE," ", ESCAPE_STRING, " $", ESCAPE_VALUE,0
.endif

.export __str_watch_added
__str_watch_added:
.byte "watch added @ ", $fe, 0

; the memory viewer patches the address into its title and reads it back from
; banked code on the cart build: keep it in always-visible RAM
.segment "DATA"
.export __str_memview_title
__str_memview_title:
.byte "memory[$1000]",0
.RODATA

.export __str_symview_title
__str_symview_title: .byte "symbols",0

.export __str_monitor_title
__str_monitor_title: .byte "monitor",0
