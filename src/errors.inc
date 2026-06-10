;******************************************************************************
; stack errors
ERR_STACK_UNDERFLOW = 1
ERR_STACK_OVERFLOW  = 2

;******************************************************************************
; string errors
ERR_LINE_TOO_LONG = 3

;******************************************************************************
; expression errors
ERR_INVALID_EXPRESSION = 4

;******************************************************************************
; macro errors
ERR_INVALID_MACRO_ARGS = 5
ERR_SYNTAX_ERROR       = 6
ERR_INVALID_DIRECTIVE  = 7
ERR_LABEL_UNDEFINED    = 8
ERR_UNMATCHED_ENDIF    = 9
ERR_TOO_MANY_MACROS    = 10

;******************************************************************************
; assembly errors
ERR_UNALIGNED_LABEL      = 11
ERR_ILLEGAL_OPCODE       = 12
ERR_ILLEGAL_ADDRMODE     = 13
ERR_OVERSIZED_OPERAND    = 14
ERR_ILLEGAL_LABEL        = 15
ERR_LABEL_TOO_LONG       = 16
ERR_UNEXPECTED_CHAR      = 17
ERR_VALUE_EXPECTED       = 18
ERR_IO_ERROR             = 19
ERR_NO_MACRO_NAME        = 20
ERR_UNRESOLVABLE_LABEL   = 21
ERR_CYCLIC_INCLUDE       = 22
ERR_FAILED_OPEN_INCLUDE  = 23
ERR_OVERLAPPING_SEGMENTS = 24
ERR_MAX_FILES_EXCEEDED   = 25
ERR_PARAM_NAME_TOO_LONG  = 26
ERR_LINE_NOT_FOUND       = 27
ERR_NO_ORIGIN            = 28
ERR_BRANCH_OUT_OF_RANGE  = 29
ERR_FILE_NOT_FOUND       = 30
ERR_UNKNOWN_SEGMENT      = 31
ERR_IMPORT_UNDEFINED     = 32
ERR_PC_TARGET_UNWRITABLE = 33
ERR_CANNOT_REDUCE        = 34

;******************************************************************************
; file i/o errors
ERR_TOO_MANY_OPEN_FILES   = 35
ERR_LOGICAL_FILE_IN_USE   = 36
ERR_DRIVE_DID_NOT_RESPOND = 37
ERR_FILE_TOO_BIG          = 38

;******************************************************************************
ERR_UNNAMED_BUFFER   = 39
ERR_TOO_MANY_BUFFERS = 40
ERR_NO_FILENAME      = 41
ERR_NO_OPEN_SCOPE    = 42

ERR_LABEL_ALREADY_DEFINED = 43
ERR_TOO_MANY_LABELS       = 44
ERR_LABEL_NOT_KNOWN_PASS1 = 45

ERR_INVALID_COMMAND = 47

ERR_COPY_TOO_BIG = 48

ERR_BUFFER_NAME_EXISTS   = 49
ERR_BUFFER_FULL          = 50

;******************************************************************************
; linker errors
ERR_TOO_MANY_GLOBAL_REFS = 51
ERR_TOO_MANY_SEGMENTS    = 52
ERR_SECTION_TOO_SMALL    = 53
ERR_ADDRMODE_MISMATCH    = 55
ERR_ALREADY_EXPORTED     = 56

;******************************************************************************
; general errors
ERR_OOM                  = 57
ERR_CTX_FULL             = 58
ERR_NO_MATCHING_SCOPE    = 59

.import __err_get

.scope err
	get = __err_get
.endscope

;******************************************************************************
; RETURN_ERR
; Executes an RTS after loading .A with the given code and setting the carry flag
; - .C - set
; - .A - the error code
;
.macro RETURN_ERR code
	lda #code
	sec
	rts
.endmacro

;******************************************************************************
; RETURN_OK
; Clears the carry and returns (RTS)
.macro RETURN_OK
	clc
	rts
.endmacro
