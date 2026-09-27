.ifndef C64_CHIPS_INC
C64_CHIPS_INC = 1
.import __chips_init, __chips_flush, __chips_sync, __chips_interrupt
.import __chips_mapped, __chips_read, __chips_write
.import __chips_synced, __chips_line, __chips_cycle
.import __chips_cia, __chips_vic
.import __chips_begin, __chips_invalidate
.import __chips_capture
.import __chips_read_cycle, __chips_write_cycle, __chips_take_interrupt
.import __chips_nmi_vector
.import __chips_end_cycle
.endif
