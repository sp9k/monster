.import __link_get_segment_by_name
.import __link_init
.import __link_link
.import __link_objfiles
.import __link_parse
.import __link_fragment
.import __link_fragment_runlo, __link_fragment_runhi
.import __link_fragment_loadlo, __link_fragment_loadhi
.import __link_pad_fragment
.import __link_segaddr_by_id
.import __link_set_segment_type

.scope link
	init                     = __link_init
	link                     = __link_link
	objfiles                 = __link_objfiles
	parse                    = __link_parse
	fragment                 = __link_fragment
	fragment_runlo           = __link_fragment_runlo
	fragment_runhi           = __link_fragment_runhi
	fragment_loadlo          = __link_fragment_loadlo
	fragment_loadhi          = __link_fragment_loadhi
	pad_fragment             = __link_pad_fragment
	segaddr_by_id            = __link_segaddr_by_id
	segid_by_name            = __link_get_segment_by_name
	set_segtype              = __link_set_segment_type
.endscope
