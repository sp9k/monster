SYM_RECORDS = 0
SYM_NODES   = 1
SYM_INDEXES = 2
SYM_NAMES   = 3

; Offset of the object-owner table after the hash nodes.
; Each byte identifies the object that owns a symbol during linking.
SYM_LINK_OWNERS = 4 + 4096*4
