.ifndef BUILD_INC
BUILD_INC = 1

.import __build_objects
.import __build_link
.import __build_completed
.import __build_line

.scope build
	objects = __build_objects
	link = __build_link
	completed = __build_completed
	line = __build_line
.endscope
.endif
