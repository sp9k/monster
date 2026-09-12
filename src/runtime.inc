.import __run_clr
.import __run_init
.import __run_install_sigint
.import __run_install_restore
.import __run_go
.import __run_go_basic

.scope run
	clr                = __run_clr
	go                 = __run_go
	go_basic           = __run_go_basic
	init               = __run_init
	install_sigint     = __run_install_sigint
	install_restore    = __run_install_restore
.endscope
