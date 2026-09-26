TARGET ?= vic20
REGION ?= NTSC

SCREEN_MODE ?= soft4x8
#SCREEN_MODE ?= hard8x8

# default device # (for cart image)
DEFAULT_DEVICE=8

# input drive for the standalone UltiMem flasher
FLASHER_DEVICE ?= 10

# Five-digit monotonic release number, persisted in the ROM update header.
VERSION := $(strip $(shell cat VERSION))
UPDATE_IMAGE=monster$(VERSION).bin

# emulators
X64=x64sc

# name of the UltiMem flash image to produce
CART_IMAGE=monster.bin
CART_TOOL=make-ultimem-cart.py
CART_PRG=monster-cart.prg

# name of the test suite flash image to produce
TEST_IMAGE=test.bin

PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

ASMFLAGS=--asm-define $(TARGET) --asm-define $(REGION) --asm-define $(SCREEN_MODE) --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE)

# target specific configuration
ifeq ($(TARGET),vic20)
# the vic20 build always targets the UltiMem expansion
CART_FLAGS=--version $(VERSION)
ASMFLAGS+=--asm-define ultimem
# UltiMem Profile extension (1) or stock ULTIMEM (0)
#ULTIMEM_P?=1
ULTIMEM_P?=0

ifeq ($(ULTIMEM_P),1)
ASMFLAGS+=--asm-define ultimem_p
endif

LINK_FILE=link-vic20-ultimem
EXP_SRC=$(wildcard vic20/ultimem/*.asm)
endif
ifeq ($(TARGET),c64)
LINK_FILE=link-c64
EXP_SRC=
# the c64 cart build produces a Magic Desk CRT
CART_IMAGE=monster.crt
CART_PRG=monster-c64-cart.prg
CART_TOOL=make-magicdesk-crt.py
# Legacy disk build: include its boot image when explicitly building test.d81
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
# Standalone utilities must not be linked into Monster.
PI_SRC=$(filter-out flasher.asm updater.asm ramtest.asm,$(wildcard *.asm))

SRC=$(TARGET_SRC) $(PI_SRC) $(MODE_SRC) $(EXP_SRC)

UNIT=$(wildcard *.a65)
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS) tests/link

# sources for the .INC cycle/depth tests; kept off the main test disk so that
# assembling everything on it doesn't trip the deliberate errors
CYCLIC_TESTS=$(wildcard tests/cyclic/*.s)

ifeq ($(TARGET),c64)
all: $(CART_IMAGE)
else
all: test.d81
endif

cart: $(CART_IMAGE)
ultimem: $(CART_IMAGE)
ifeq ($(TARGET),vic20)
cart ultimem: $(UPDATE_IMAGE)
endif

flasher: flasher.prg $(CART_IMAGE) flasher.d81

updater: updater.prg updater.d81

ramtest: ramtest.prg ramtest.d81

suite: $(TEST_IMAGE)

# c64 only: build the boot disk
disk: boot.prg monster-disk.prg
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg

installer.prg: installer/install.asm
	$(CL65) -t $(TARGET) -o $@ -C installer/link-installer.config $^

ASM_INCLUDES=$(wildcard *.inc $(TARGET)/*.inc $(TARGET)/$(SCREEN_MODE)/*.inc $(TARGET)/ultimem/*.inc)
BUILD_OPTIONS=.build-options-$(TARGET)
.PHONY: FORCE
FORCE:

$(BUILD_OPTIONS): FORCE
	@printf '%s\n' '$(ASMFLAGS) FLASHER_DEVICE=$(FLASHER_DEVICE) VERSION=$(VERSION)' > $@.tmp
	@cmp -s $@.tmp $@ || cp $@.tmp $@
	@$(RM) $@.tmp

$(CART_PRG): $(BOOTSRC) $(SRC) $(ASM_INCLUDES) $(LINK_FILE)-cart.config Makefile $(BUILD_OPTIONS)
	$(CL65) -t $(TARGET) -o $@ --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $(BOOTSRC) $(SRC) -Ln labels.txt -v -m map.txt

monster-cart-test.prg: $(BOOTSRC) $(SRC) $(UNIT)
	$(CL65) -t $(TARGET) -o $@ --asm-define TEST --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $^ -Ln labels.txt -v -m map.txt

# the linker output is already laid out bank-by-bank for the target's flash
# (see the MEMORY areas in the link config); pack it into the image format
$(CART_IMAGE): $(CART_PRG) $(CART_TOOL) VERSION
	$(PYTHON) $(CART_TOOL) $< $@ $(CART_FLAGS)

$(TEST_IMAGE): monster-cart-test.prg
	$(PYTHON) make-ultimem-cart.py $< $@

monster-disk.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ $(ASMFLAGS) -C $(LINK_FILE)-disk.config $^ -Ln labels.txt -v -m map.txt

# Runs in an unexpanded VIC-20 and streams only the populated 8 KiB banks from
# the padded 8 MiB image.  Derive the count from the linker's bank-laid output.
flasher.prg: flasher.asm monster-cart.prg Makefile $(BUILD_OPTIONS)
	$(CL65) -t vic20 -o $@ -C vic20-asm.cfg \
		--asm-define DEFAULT_DEVICE=$(FLASHER_DEVICE) \
		--asm-define IMAGE_BLOCKS=$$(($$(wc -c < monster-cart.prg) / 8192)) $<

flasher.d81: flasher.prg $(UPDATE_IMAGE) Makefile
	$(C1541) -format "monster flash,mf" d81 $@ -attach $@ \
		-write flasher.prg flasher.prg -write $(UPDATE_IMAGE) monster.bin

# The disk update is trimmed; padded 8 MiB images remain emulator artifacts.
$(UPDATE_IMAGE): monster-cart.prg make-ultimem-cart.py VERSION
	$(PYTHON) make-ultimem-cart.py $< $@ --version $(VERSION) --trimmed

updater.prg: updater.asm updater.inc firmware.inc flasher.asm Makefile $(BUILD_OPTIONS)
	$(CL65) -t vic20 -o $@ -C vic20-asm.cfg \
		--asm-define DEFAULT_DEVICE=$(FLASHER_DEVICE) updater.asm

updater.d81: updater.prg $(UPDATE_IMAGE) Makefile
	$(C1541) -format "monster update,mu" d81 $@ -attach $@ \
		-write updater.prg updater.prg -write $(UPDATE_IMAGE) $(UPDATE_IMAGE)

# Same non-destructive bank check as the flasher, without a flash image.
ramtest.prg: ramtest.asm Makefile
	$(CL65) -t vic20 -o $@ -C vic20-asm.cfg $<

ramtest.d81: ramtest.prg Makefile
	$(C1541) -format "ultimem ram,ur" d81 $@ -attach $@ \
		-write ramtest.prg ramtest.prg

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
	rm -f *.prg *.o test.d81 cyclic.d81 monster.d81 flasher.d81 updater.d81 ramtest.d81
	rm -f $(CART_IMAGE) $(TEST_IMAGE)
	rm -f vic20/*.o vic20/ultimem/*.o vic20/hard8x8/*.o vic20/soft4x8/*.o c64/*.o
