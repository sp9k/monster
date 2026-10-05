.ifndef BUILD_INC
BUILD_INC = 1

.import __build_options
.import __build_require_file
.import __build_open_read
.import __build_objects
.import __build_link
.import __build_find_manifest
.import __build_completed
.import __build_line

.scope build
	ONLY_OBJECTS = $08
	options = __build_options
	require_file = __build_require_file
	open_read = __build_open_read
	objects = __build_objects
	link = __build_link
	find_manifest = __build_find_manifest
	completed = __build_completed
	line = __build_line
.endscope
.endif
