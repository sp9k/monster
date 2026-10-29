; assembler control flags; zero selects normal assembly behavior
ASM_CTRL_MONITOR_DIRECTIVES  = $01	; monitor directive allowlist
ASM_CTRL_SIZE_ONLY     = $40	; advance PC without writing bytes (BIT/BVS)
ASM_CTRL_NOEXPAND_REPEAT = $80	; leave closed repeat for caller (LDA/BMI)

MODE_IMMEDIATE=$01
MODE_ZP=$02
MODE_ABS=$04
MODE_INDIRECT=$08
MODE_X_INDEXED=$10
MODE_Y_INDEXED=$20
MODE_IMPLIED=$40

; simulation side-effect flags
OP_LOAD  = $01	; constant for an operation that loads
OP_STORE = $02	; constant for an operation that stores to memory
OP_REG_A = $04	; constant for an operation that stores to the .A register
OP_REG_X = $08	; constant for an operation that stores to the .X register
OP_REG_Y = $10	; constant for an operation that stores to the .Y register
OP_FLAG  = $20	; constant for an operation that writes to .P (flag register)
OP_STACK = $40	; constant for an operation that reads/writes SP
OP_PC    = $80	; constant for an operation that modifies PC (branches)

