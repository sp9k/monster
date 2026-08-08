.import __sim_affected
.import __sim_illegal
.import __sim_next_pc
.import __sim_step
.import __sim_jammed

.import __sim_op
.import __sim_operand
.import __sim_op_mode
.import __sim_register_state
.import __sim_branch_taken
.import __sim_raster
.import __sim_pc
.import __sim_prev_pc
.import __sim_reg_a
.import __sim_reg_x
.import __sim_reg_y
.import __sim_reg_sp
.import __sim_reg_p
.import __sim_effective_addr
.import __sim_effective_val
.import __sim_flush_vias
.import __sim_rti_irq
.import __sim_stopwatch
.import __sim_at_brk
.import __sim_init
.import __sim_trace
.import __sim_tracing
.import __sim_via1
.import __sim_via2
.import __sim_vital_addr_clobbered

.scope sim
	affected         = __sim_affected
	at_brk           = __sim_at_brk
	flush_vias       = __sim_flush_vias
	init             = __sim_init
	branch_taken     = __sim_branch_taken
	raster           = __sim_raster
	illegal          = __sim_illegal
	jammed           = __sim_jammed
	next_pc          = __sim_next_pc
	op               = __sim_op
	operand          = __sim_operand
	op_mode          = __sim_op_mode
	step             = __sim_step
	trace            = __sim_trace
	tracing          = __sim_tracing

	stopwatch      = __sim_stopwatch
	effective_addr = __sim_effective_addr
	effective_val  = __sim_effective_val
	rti_irq        = __sim_rti_irq
	register_state = __sim_register_state
	pc             = __sim_pc
	prev_pc        = __sim_prev_pc
	reg_a          = __sim_reg_a
	reg_x          = __sim_reg_x
	reg_y          = __sim_reg_y
	reg_sp         = __sim_reg_sp
	reg_p          = __sim_reg_p

	via1                 = __sim_via1
	via2                 = __sim_via2
	vital_addr_clobbered =__sim_vital_addr_clobbered
.endscope
