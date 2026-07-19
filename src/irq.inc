.import __irq_on
.import __irq_off

.ifdef vic20
.import __irq_keydecode
.endif

.scope irq
        on  = __irq_on
	off = __irq_off
.ifdef vic20
	keydecode = __irq_keydecode
.endif
.endscope

