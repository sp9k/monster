.import __kernal_chrin
.import __kernal_chrout
.import __kernal_readst
.import __kernal_setnam
.import __kernal_setlfs
.import __kernal_load
.import __kernal_chkin
.import __kernal_chkout
.import __kernal_ciout
.import __kernal_open
.import __kernal_close
.import __kernal_clall
.import __kernal_clrchn

.scope krn
	chrin  = __kernal_chrin
	chrout = __kernal_chrout
	readst = __kernal_readst
	setnam = __kernal_setnam
	setlfs = __kernal_setlfs
	load   = __kernal_load
	chkin  = __kernal_chkin
	chkout = __kernal_chkout
	ciout  = __kernal_ciout
	open   = __kernal_open
	close  = __kernal_close
	clall  = __kernal_clall
	clrchn = __kernal_clrchn
.endscope
