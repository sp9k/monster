;*******************************************************************************
; KERNAL.ASM
; This file contains procedures for calling the KERNAL functions in Commodore
; BASIC.
; If JIFFYDOS=1 uses custom JiffyDOS compatible versions of routines
;*******************************************************************************

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


.ifndef JIFFYDOS
JIFFYDOS = 0
.endif

;*******************************************************************************
; CHRIN
.export __kernal_chrin
.if JIFFYDOS
.import __jiffy_call_chrin
__kernal_chrin = __jiffy_call_chrin
.else
__kernal_chrin = $ffcf
.endif

;*******************************************************************************
; CHROUT
.export __kernal_chrout
.if JIFFYDOS
.import __jiffy_call_chrout
__kernal_chrout = __jiffy_call_chrout
.else
__kernal_chrout = $ffd2
.endif

;*******************************************************************************
; CIOUT
.export __kernal_ciout
.if JIFFYDOS
.import __jiffy_call_ciout
__kernal_ciout = __jiffy_call_ciout
.else
__kernal_ciout = $eee4
.endif

;*******************************************************************************
; CHKIN
.export __kernal_chkin
.if JIFFYDOS
.import __jiffy_call_chkin
__kernal_chkin = __jiffy_call_chkin
.else
__kernal_chkin = $ffc6
.endif

;*******************************************************************************
; CHKOUT
.export __kernal_chkout
.if JIFFYDOS
.import __jiffy_call_chkout
__kernal_chkout = __jiffy_call_chkout
.else
__kernal_chkout = $ffc9
.endif

;*******************************************************************************
; OPEN
.export __kernal_open
.if JIFFYDOS
.import __jiffy_call_open
__kernal_open = __jiffy_call_open
.else
__kernal_open = $ffc0
.endif

;*******************************************************************************
; CLOSE
.export __kernal_close
.if JIFFYDOS
.import __jiffy_call_close
__kernal_close = __jiffy_call_close
.else
__kernal_close = $ffc3
.endif

;*******************************************************************************
; CLALL
.export __kernal_clall
.if JIFFYDOS
.import __jiffy_call_clall
__kernal_clall = __jiffy_call_clall
.else
__kernal_clall = $ffe7
.endif

;*******************************************************************************
; CLRCHN
.export __kernal_clrchn
.if JIFFYDOS
.import __jiffy_call_clrchn
__kernal_clrchn = __jiffy_call_clrchn
.else
__kernal_clrchn = $ffcc
.endif

;*******************************************************************************
; TALK
.export __kernal_talk
.if JIFFYDOS
.import __jiffy_call_talk
__kernal_talk = __jiffy_call_talk
.else
__kernal_talk = $ffb4
.endif

;*******************************************************************************
; TKSA
.export __kernal_tksa
.if JIFFYDOS
.import __jiffy_call_tksa
__kernal_tksa = __jiffy_call_tksa
.else
__kernal_tksa = $ff96
.endif

;*******************************************************************************
; ACPTR
.export __kernal_acptr
.if JIFFYDOS
.import __jiffy_call_acptr
__kernal_acptr = __jiffy_call_acptr
.else
__kernal_acptr = $ffa5
.endif

;*******************************************************************************
; UNTLK
.export __kernal_untlk
.if JIFFYDOS
.import __jiffy_call_untlk
__kernal_untlk = __jiffy_call_untlk
.else
__kernal_untlk = $ffab
.endif

;*******************************************************************************
; LISTEN
.export __kernal_listen
.if JIFFYDOS
.import __jiffy_call_listen
__kernal_listen = __jiffy_call_listen
.else
__kernal_listen = $ffb1
.endif

;*******************************************************************************
; SECOND
.export __kernal_second
.if JIFFYDOS
.import __jiffy_call_second
__kernal_second = __jiffy_call_second
.else
__kernal_second = $ff93
.endif

;*******************************************************************************
; UNLSN
.export __kernal_unlsn
.if JIFFYDOS
.import __jiffy_call_unlsn
__kernal_unlsn = __jiffy_call_unlsn
.else
__kernal_unlsn = $ffae
.endif
