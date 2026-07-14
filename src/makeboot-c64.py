# MAKEBOOT-C64
# Splits the contiguous monster-disk.prg image into two .prg files so that the
# exomizer self-extractor does not have to decrunch the $D000-$DFFF I/O region.
#
#   low.prg  = $0801-$CFFF  (code + data)
#   (gap)    = $D000-$DFFF
#   high.prg = $E000-$FEFF  (debugger + error strings + BSS under KERNAL)
#
# usage: python3 makeboot-c64.py <infile> <lowfile> <highfile>

import sys

if len(sys.argv) != 4:
    print(f'usage: {sys.argv[0]} <infile> <lowfile> <highfile>')
    sys.exit(1)

infile, lowfile, highfile = sys.argv[1], sys.argv[2], sys.argv[3]

LOW_START  = 0x0801
GAP_START  = 0xd000
HIGH_START = 0xe000
HIGH_END   = 0xff00   # exclusive

with open(infile, 'rb') as f:
    raw = f.read()

load = raw[0] | (raw[1] << 8)
data = raw[2:]
if load != LOW_START:
    print(f'error: expected load address ${LOW_START:04x}, got ${load:04x}')
    sys.exit(1)

def slice_addr(lo, hi):   # [lo, hi) by address
    return data[lo - load:hi - load]

def write_prg(name, addr, payload):
    with open(name, 'wb') as f:
        f.write(bytes([addr & 0xff, (addr >> 8) & 0xff]))
        f.write(payload)
    print(f'  {name}: ${addr:04x}-${addr + len(payload) - 1:04x} ({len(payload)} bytes)')

write_prg(lowfile,  LOW_START,  slice_addr(LOW_START, GAP_START))
write_prg(highfile, HIGH_START, slice_addr(HIGH_START, HIGH_END))
