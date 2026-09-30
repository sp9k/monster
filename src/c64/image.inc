;*******************************************************************************
; IMAGE.INC
; Dedicated REU image backing after the symbol pools ($1c0000-$20ffff).
;*******************************************************************************

.ifndef C64_IMAGE_INC
C64_IMAGE_INC = 1
IMAGE_REU_BASE = $210000
IMAGE_PAGES = $40
IMAGE_CAPACITY = IMAGE_PAGES * $2000
.assert IMAGE_REU_BASE >= $210000, error, "image overlaps symbols"
.assert IMAGE_REU_BASE + IMAGE_CAPACITY <= $f90000, error, "image overlaps runtime state"
.endif
