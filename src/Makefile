TARGET ?= vic20
REGION ?= NTSC

SCREEN_MODE ?= soft4x8
#SCREEN_MODE ?= hard8x8

# default device # (for cart image)
DEFAULT_DEVICE=8

# emulators
X64=x64sc

# default cartridge image (overridden by the target below)
CART_IMAGE=monster.bin
CART_TOOL=make-ultimem-cart.py

# name of the test suite flash image to produce
TEST_IMAGE=test.bin

PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

ASMFLAGS=--asm-define $(TARGET) --asm-define $(REGION) --asm-define $(SCREEN_MODE) --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE)

# target specific configuration
ifeq ($(TARGET),vic20)
EXPANSION ?= ultimem
ASMFLAGS+=--asm-define $(EXPANSION)
# UltiMem Profile extension (1) or stock ULTIMEM (0)
#ULTIMEM_P?=1
ULTIMEM_P?=0

ifeq ($(EXPANSION):$(ULTIMEM_P),ultimem:1)
ASMFLAGS+=--asm-define ultimem_p
endif

LINK_FILE=link-vic20-$(EXPANSION)
EXP_SRC=$(wildcard vic20/$(EXPANSION)/*.asm)
ifeq ($(EXPANSION),fe3)
CART_IMAGE=monster-fe3.bin
CART_TOOL=make-fe3-cart.py
endif
endif
ifeq ($(TARGET),c64)
LINK_FILE=link-c64
EXP_SRC=
# the c64 cart build produces a Magic Desk CRT
CART_IMAGE=monster.crt
CART_TOOL=make-magicdesk-crt.py
# the c64 boots from disk; its boot image is written to the test disk
BOOT_DEP=boot.prg
BOOT_WRITE=-write boot.prg
endif

# emulator flags for the configured REGION
ifeq ($(REGION),PAL)
EMU_REGION=-pal
endif
ifeq ($(REGION),NTSC)
EMU_REGION=-ntsc
endif

BOOTSRC=$(TARGET)/boot.asm

# platform specific source files
TARGET_SRC=$(filter-out $(BOOTSRC), $(wildcard $(TARGET)/*.asm))
MODE_SRC=$(wildcard $(TARGET)/$(SCREEN_MODE)/*.asm)

# platform independent source files
# flasher.asm is a standalone utility and must not be linked into Monster.
PI_SRC=$(filter-out flasher.asm,$(wildcard *.asm))

SRC=$(TARGET_SRC) $(PI_SRC) $(MODE_SRC) $(EXP_SRC)

UNIT=$(wildcard *.a65)
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS) tests/link

# sources for the .INC cycle/depth tests; kept off the main test disk so that
# assembling everything on it doesn't trip the deliberate errors
CYCLIC_TESTS=$(wildcard tests/cyclic/*.s)

all: test.d81

cart: $(CART_IMAGE)
ultimem: $(CART_IMAGE)

.PHONY: fe3 fe3-disk test-fe3 test-fe3-disk disk
fe3:
	$(MAKE) cart TARGET=vic20 EXPANSION=fe3

fe3-disk:
	$(MAKE) disk TARGET=vic20 EXPANSION=fe3

test-fe3:
	$(MAKE) cart TARGET=vic20 EXPANSION=fe3 CL65='$(CL65) -g'
	$(PYTHON) test_fe3.py

test-fe3-disk:
	$(MAKE) disk TARGET=vic20 EXPANSION=fe3 CL65='$(CL65) -g'
	$(PYTHON) test_fe3_disk.py

flasher: flasher.prg $(CART_IMAGE)

suite: $(TEST_IMAGE)

ifeq ($(TARGET):$(EXPANSION),vic20:fe3)
disk: monster-fe3.d64

# Use the cartridge's bank layout unchanged. Only the temporary loader moves
# to BASIC's expanded load address; all runtime areas retain their addresses.
monster-fe3-disk.prg: FORCE $(BOOTSRC) $(SRC) $(LINK_FILE)-cart.config
	$(CL65) -t $(TARGET) -o $@ $(ASMFLAGS) -C $(LINK_FILE)-cart.config -Wl -D,__FE3_BOOT_START__=4609 $(BOOTSRC) $(SRC) -Ln labels-fe3-disk.txt -m map-fe3-disk.txt

monster-fe3.d64: monster-fe3-disk.prg make-fe3-disk.py
	$(PYTHON) make-fe3-disk.py $< labels-fe3-disk.txt fe3-disk $@ --c1541 $(C1541)

else
# c64: build the boot disk
disk: boot.prg monster-disk.prg
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg
endif

installer.prg: installer/install.asm
	$(CL65) -t $(TARGET) -o $@ -C installer/link-installer.config $^

monster-cart.prg: FORCE $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $(BOOTSRC) $(SRC) -Ln labels.txt -v -m map.txt

.PHONY: FORCE
FORCE:

monster-cart-test.prg: $(BOOTSRC) $(SRC) $(UNIT)
	$(CL65) -t $(TARGET) -o $@ --asm-define TEST --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $^ -Ln labels.txt -v -m map.txt

# the linker output is already laid out bank-by-bank for the target's flash
# (see the MEMORY areas in the link config); pack it into the image format
$(CART_IMAGE): monster-cart.prg
	$(PYTHON) $(CART_TOOL) $< $@

$(TEST_IMAGE): monster-cart-test.prg
	$(PYTHON) $(CART_TOOL) $< $@

monster-disk.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ $(ASMFLAGS) -C $(LINK_FILE)-disk.config $^ -Ln labels.txt -v -m map.txt

# Runs in an unexpanded VIC-20 and streams only the populated 8 KiB banks from
# the padded 8 MiB image.  Derive the count from the linker's bank-laid output.
flasher.prg: flasher.asm monster-cart.prg
	$(CL65) -t vic20 -o $@ -C vic20-asm.cfg \
		--asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) \
		--asm-define IMAGE_BLOCKS=$$(($$(wc -c < monster-cart.prg) / 8192)) $<

# Split the image around the $$D000-$$DFFF I/O gap so the exomizer
# decruncher has free RAM to work in (see makeboot-c64.py).
boot.prg: monster-disk.prg
	$(PYTHON) makeboot-c64.py monster-disk.prg boot-lo.prg boot-hi.prg
	$(EXOMIZER) sfx basic -t64 -n -o $@ boot-lo.prg boot-hi.prg

test-disk: $(TEST_FILES)
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 $(addprefix -write , $(TEST_FILES))

test.d81: $(BOOT_DEP) $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 $(BOOT_WRITE) $(addprefix -write , $(TEST_FILES))

# manual test disk for .INC cycle detection; see tests/cyclic/README.txt
cyclic.d81: $(CYCLIC_TESTS)
	$(C1541) -format cyclic,1 d81 cyclic.d81 -attach cyclic.d81 $(addprefix -write , $(CYCLIC_TESTS))

start-disk-jiffy64: test.d81
	$(X64) -default +warp -kernal JiffyDOS_C64.bin -sound $(EMU_REGION) -drive8type 1581 -reu -reusize 16384  -moncommands moncommands -8 test.d81

start-cart-c64: monster.crt test-disk
	$(X64) -default -sound $(EMU_REGION) -drive8type 1581 -reu -reusize 16384 -cartcrt monster.crt -moncommands moncommands -8 test.d81

clean:
	rm -f *.prg *.o test.d81 cyclic.d81 monster.d81
	rm -f monster-fe3.d64 labels-fe3-disk.txt map-fe3-disk.txt fe3-disk/*.prg
	rm -f $(CART_IMAGE) $(TEST_IMAGE)
	rm -f vic20/*.o vic20/ultimem/*.o vic20/fe3/*.o vic20/hard8x8/*.o vic20/soft4x8/*.o c64/*.o
