.import __obj_add_export
.import __obj_add_reloc
.import __obj_add_section
.import __obj_close_section
.import __obj_dump
.import __obj_filename
.import __obj_get_segment_name_by_id
.import __obj_init
.import __obj_load
.import __obj_log_state
.import __obj_load_headers
.import __obj_numsections
.import __obj_sections_sizelo
.import __obj_sections_sizehi
.import __obj_segments
.import __obj_num_exports
.import __obj_num_imports
.import __obj_numsegments
.import __obj_segments_sizelo
.import __obj_segments_sizehi

.scope obj
	add_export             = __obj_add_export
	addreloc               = __obj_add_reloc
	add_section            = __obj_add_section
	close_section          = __obj_close_section
	dump                   = __obj_dump
	filename               = __obj_filename
	get_segment_name_by_id = __obj_get_segment_name_by_id
	init                   = __obj_init
	load                   = __obj_load
	load_headers           = __obj_load_headers
	log_state              = __obj_log_state
	numexports             = __obj_num_exports
	numimports             = __obj_num_imports
	numsegments            = __obj_numsegments
	numsections            = __obj_numsections
	sections_sizelo        = __obj_sections_sizelo
	sections_sizehi        = __obj_sections_sizehi
	segments               = __obj_segments
	segments_sizelo        = __obj_segments_sizelo
	segments_sizehi        = __obj_segments_sizehi
.endscope
