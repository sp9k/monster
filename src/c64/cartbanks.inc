.ifndef C64_CARTBANKS_INC
C64_CARTBANKS_INC = 1

;*******************************************************************************
; CART ROM BANKS
; The cartridge ROM bank for each banked subsystem's code
; (must match link-c64-cart.config)
CART_BANK_CONSOLE = 6
CART_BANK_LINKER  = 7
CART_BANK_LABELS  = 8
CART_BANK_DEBUG   = 9
CART_BANK_MACROS  = 10
CART_BANK_CTX     = 11
CART_BANK_DBGUI   = 12
CART_BANK_FILEDIR = 13
CART_BANK_ASM     = 14
CART_BANK_EDIT    = 15

.endif
