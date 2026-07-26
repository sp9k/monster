TARGET ?= vic20
REGION ?= NTSC

SCREEN_MODE ?= soft4x8
#SCREEN_MODE ?= hard8x8

# default device # (for cart image)
DEFAULT_DEVICE=8

# emulators
X64=x64sc

# name of the UltiMem flash image to produce
CART_IMAGE=monster.bin

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
ASMFLAGS+=--asm-define ultimem
# UltiMem Profile extension (1) or stock ULTIMEM (0)
ULTIMEM_P?=1

ifeq ($(ULTIMEM_P),1)
ASMFLAGS+=--asm-define ultimem_p
endif

LINK_FILE=link-vic20-ultimem
EXP_SRC=$(wildcard vic20/ultimem/*.asm)
endif
ifeq ($(TARGET),c64)
LINK_FILE=link-c64
EXP_SRC=
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
PI_SRC=$(wildcard *.asm)

SRC=$(TARGET_SRC) $(PI_SRC) $(MODE_SRC) $(EXP_SRC)

UNIT=$(wildcard *.a65)
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS) tests/link

all: test.d81

cart: $(CART_IMAGE)
ultimem: $(CART_IMAGE)

suite: $(TEST_IMAGE)

# c64 only: build the boot disk
disk: boot.prg monster-disk.prg
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg

installer.prg: installer/install.asm
	$(CL65) -t $(TARGET) -o $@ -C installer/link-installer.config $^

monster-cart.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $^ -Ln labels.txt -v -m map.txt

monster-cart-test.prg: $(BOOTSRC) $(SRC) $(UNIT)
	$(CL65) -t $(TARGET) -o $@ --asm-define TEST --asm-define CART $(ASMFLAGS) -C $(LINK_FILE)-cart.config $^ -Ln labels.txt -v -m map.txt

# the linker output is already laid out bank-by-bank for the UltiMem's flash
# (see the MEMORY areas in the link config); pad it to the full image size
$(CART_IMAGE): monster-cart.prg
	$(PYTHON) make-ultimem-cart.py $< $@

$(TEST_IMAGE): monster-cart-test.prg
	$(PYTHON) make-ultimem-cart.py $< $@

monster-disk.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ $(ASMFLAGS) -C $(LINK_FILE)-disk.config $^ -Ln labels.txt -v -m map.txt

# Split the image around the $$D000-$$DFFF I/O gap so the exomizer
# decruncher has free RAM to work in (see makeboot-c64.py).
boot.prg: monster-disk.prg
	$(PYTHON) makeboot-c64.py monster-disk.prg boot-lo.prg boot-hi.prg
	$(EXOMIZER) sfx basic -t64 -n -o $@ boot-lo.prg boot-hi.prg

test-disk: $(TEST_FILES)
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 $(addprefix -write , $(TEST_FILES))

test.d81: $(BOOT_DEP) $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 $(BOOT_WRITE) $(addprefix -write , $(TEST_FILES))

start-disk-jiffy64: test.d81
	$(X64) -default +warp -kernal JiffyDOS_C64.bin -sound $(EMU_REGION) -drive8type 1581 -reu -reusize 16384  -moncommands moncommands -8 test.d81

clean:
	rm -f *.prg *.o test.d81 monster.d81
	rm -f $(CART_IMAGE) $(TEST_IMAGE)
	rm -f vic20/*.o vic20/ultimem/*.o vic20/hard8x8/*.o vic20/soft4x8/*.o c64/*.o
