.import __link_get_segment_by_name
.import __link_init
.import __link_link
.import __link_no_debug_info
.import __link_objfiles
.import __link_objfiles_end
.import __link_parse
.import __link_fragment
.include "linkfragments.inc"
.import __link_own_symbol
.import __link_pad_fragment
.import __link_set_segment_type
.import __link_update_progress

; Shared helpers for object.asm; both modules execute in FINAL_BANK_LINKER.
.import __link_log_msg
.import __link_log_banner
.import __link_strcmp

.import __link_output_load, __link_output_store

.scope link
	init                     = __link_init
	link                     = __link_link
	log_msg                  = __link_log_msg
	log_banner               = __link_log_banner
	strcmp                   = __link_strcmp
	no_debug_info            = __link_no_debug_info
	objfiles_end             = __link_objfiles_end
	objfiles                 = __link_objfiles
	parse                    = __link_parse
	fragment                 = __link_fragment
	own_symbol               = __link_own_symbol
	pad_fragment             = __link_pad_fragment
	segid_by_name            = __link_get_segment_by_name
	set_segtype              = __link_set_segment_type
	update_progress          = __link_update_progress
	output_load              = __link_output_load
	output_store             = __link_output_store
.endscope
