.import __link_get_segment_by_name
.import __link_init
.import __link_link
.import __link_objfiles
.import __link_parse
.import __link_segaddr_by_id
.import __link_segaddr_by_name
.import __link_set_segment_type

.scope link
	init                     = __link_init
	link                     = __link_link
	objfiles                 = __link_objfiles
	parse                    = __link_parse
	segaddr_by_id            = __link_segaddr_by_id
	segaddr_by_name          = __link_segaddr_by_name
	segid_by_name            = __link_get_segment_by_name
	set_segtype              = __link_set_segment_type
.endscope
