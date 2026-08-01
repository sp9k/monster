#!/usr/bin/env python3
# Packs the linker output (see link-c64-cart.config) into a Magic Desk CRT
# (type 19): 8 KiB banks at $8000
# Image is padded to 16 banks

import sys

BANK_SIZE = 0x2000
CRT_TYPE = 19
PAD_BANKS = 16


def main():
    if len(sys.argv) != 3:
        sys.exit(f'usage: {sys.argv[0]} <linked image> <output.crt>')

    with open(sys.argv[1], 'rb') as f:
        image = f.read()

    if len(image) % BANK_SIZE != 0:
        sys.exit(f'image size ${len(image):x} is not a multiple of ${BANK_SIZE:x}')

    nbanks = len(image) // BANK_SIZE
    if nbanks > PAD_BANKS:
        sys.exit(f'image has {nbanks} banks; increase PAD_BANKS')

    out = bytearray()

    # CRT header
    out += b'C64 CARTRIDGE   '
    out += (0x40).to_bytes(4, 'big')        # header length
    out += (0x0100).to_bytes(2, 'big')      # version
    out += CRT_TYPE.to_bytes(2, 'big')      # hardware type
    out += bytes([0])                       # EXROM line (active)
    out += bytes([1])                       # GAME line (inactive; 8K mode)
    out += bytes(6)                         # reserved
    out += b'MONSTER'.ljust(32, b'\0')      # cartridge name

    for bank in range(PAD_BANKS):
        if bank < nbanks:
            data = image[bank * BANK_SIZE:(bank + 1) * BANK_SIZE]
        else:
            data = b'\xff' * BANK_SIZE
        out += b'CHIP'
        out += (0x10 + BANK_SIZE).to_bytes(4, 'big')  # packet length
        out += (0).to_bytes(2, 'big')                 # chip type: ROM
        out += bank.to_bytes(2, 'big')                # bank number
        out += (0x8000).to_bytes(2, 'big')            # load address
        out += BANK_SIZE.to_bytes(2, 'big')           # ROM size

        out += data

    with open(sys.argv[2], 'wb') as f:
        f.write(out)

    print(f'{sys.argv[2]}: {nbanks} banks used, padded to {PAD_BANKS} '
          f'({len(out)} bytes)')


if __name__ == '__main__':
    main()
