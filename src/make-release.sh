###############################################################################
# Vic-20
###############################################################################
# make the installer
make clean; make installer.prg
cp installer.prg ../bin/vic20

# make the NTSC cart binary
make clean;TARGET=vic20 make cart REGION=NTSC
cp monster.bin ../bin/vic20/monster-ntsc.bin

# make the PAL cart binary
make clean;TARGET=vic20 make cart REGION=PAL
cp monster.bin ../bin/vic20/monster-pal.bin

# NTSC cart binary with JiffyDOS fastloader
make clean;TARGET=vic20 make cart REGION=NTSC JIFFYDOS=1
cp monster.bin ../bin/vic20/monster-ntsc-fastload.bin

# make the PAL cart binary with JiffyDOS fastloader
make clean;TARGET=vic20 make cart REGION=PAL JIFFYDOS=1
cp monster.bin ../bin/vic20/monster-pal-fastload.bin

###############################################################################
# C-64
###############################################################################
# make the universal disk image
make clean; TARGET=c64 make disk REGION=NTSC
c1541 -format monster,1 d81 monster.d81 -attach monster.d81 -write boot.prg -write masm.prg
cp monster.d81 ../bin/c64/monster.d81
