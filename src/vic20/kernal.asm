;*******************************************************************************
; KERNAL.ASM
; This file contains procedures for calling the KERNAL functions in Commodore
; BASIC.
;*******************************************************************************

;*******************************************************************************
; CHRIN
.export __kernal_chrin
__kernal_chrin = $ffcf

;*******************************************************************************
; CHROUT
.export __kernal_chrout
__kernal_chrout = $ffd2

;*******************************************************************************
; CIOUT
.export __kernal_ciout
__kernal_ciout = $eee4

;*******************************************************************************
; READST
.export __kernal_readst
__kernal_readst = $ffb7

;*******************************************************************************
; SETNAM
.export __kernal_setnam
__kernal_setnam = $ffbd

;*******************************************************************************
; SETLFS
.export __kernal_setlfs
__kernal_setlfs = $ffba

;*******************************************************************************
; LOAD
.export __kernal_load
__kernal_load = $ffd5

;*******************************************************************************
; CHKIN
.export __kernal_chkin
__kernal_chkin = $ffc6

;*******************************************************************************
; CHKOUT
.export __kernal_chkout
__kernal_chkout = $ffc9

;*******************************************************************************
; OPEN
.export __kernal_open
__kernal_open = $ffc0

;*******************************************************************************
; CLOSE
.export __kernal_close
__kernal_close = $ffc3

;*******************************************************************************
; CLALL
.export __kernal_clall
__kernal_clall = $ffe7

;*******************************************************************************
; CLRCHN
.export __kernal_clrchn
__kernal_clrchn = $ffcc

;*******************************************************************************
; TALK
; Calls the KERNAL serial bus TALK entry point
; IN:
;   - .A: device number
.export __kernal_talk
__kernal_talk = $ffb4

;*******************************************************************************
; TKSA
; Calls the KERNAL serial bus TKSA entry point
; IN:
;   - .A: secondary address
.export __kernal_tksa
__kernal_tksa = $ff96

;*******************************************************************************
; ACPTR
; Calls the KERNAL serial bus ACPTR entry point
; OUT:
;   - .A: received byte
.export __kernal_acptr
__kernal_acptr = $ffa5

;*******************************************************************************
; UNTLK
; Calls the KERNAL serial bus UNTLK entry point
.export __kernal_untlk
__kernal_untlk = $ffab

;*******************************************************************************
; LISTEN
; Calls the KERNAL serial bus LISTEN entry point
; IN:
;   - .A: device number
.export __kernal_listen
__kernal_listen = $ffb1

;*******************************************************************************
; SECOND
; Calls the KERNAL serial bus SECOND entry point
; IN:
;   - .A: secondary address
.export __kernal_second
__kernal_second = $ff93

;*******************************************************************************
; UNLSN
; Calls the KERNAL serial bus UNLSN entry point
.export __kernal_unlsn
__kernal_unlsn = $ffae
