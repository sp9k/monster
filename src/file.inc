.importzp __file_load_address
.importzp __file_save_address
.importzp __file_save_address_end
.importzp __file_load_address_end

.import __file_close
.import __file_closeall
.import __file_eof
.import __file_exists
.import __file_getline
.import __file_geterr
.import __file_init_drive
.import __file_load_bin
.import __file_load_binv
.import __file_load_src
.import __file_open
.import __file_open_r
.import __file_open_r_prg
.import __file_open_w
.import __file_readb
.import __file_save_bin
.import __file_save_src
.import __file_scratch
.import __file_write
.import __file_writeb
.import __file_write_banner

.scope file
	close            = __file_close
	closeall         = __file_closeall
	eof              = __file_eof
	exists           = __file_exists
	geterr           = __file_geterr
	getline          = __file_getline
	init_drive       = __file_init_drive
	loadbin          = __file_load_bin
	loadbinv         = __file_load_binv
	loadaddr         = __file_load_address
	load_address_end = __file_load_address_end
	loadsrc          = __file_load_src
	open_r           = __file_open_r
	open_r_prg       = __file_open_r_prg
	open_w           = __file_open_w
	open             = __file_open
	readb            = __file_readb
	save_address     = __file_save_address
	save_address_end = __file_save_address_end
	savebin          = __file_save_bin
	savesrc          = __file_save_src
	scratch          = __file_scratch
	write_banner     = __file_write_banner
.endscope
