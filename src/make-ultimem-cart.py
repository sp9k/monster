"""Stamp the ROM version and checksum; emit a padded ROM or trimmed disk image."""
import argparse
from pathlib import Path

BANK_SIZE = 8192
IMG_SIZE = 8 * 1024 * 1024
HEADER_OFFSET = 9
VERSION_OFFSET = 13
BLOCKS_OFFSET = 18
CHECKSUM_OFFSET = 20
HEADER_SIZE = 13
MAGIC = b'MUP1'


def stamp_image(data, version):
    if len(version) != 5 or not version.isascii() or not version.isdecimal():
        raise ValueError('version must be exactly five decimal digits')
    if version == '00000':
        raise ValueError('version 00000 is reserved for unversioned cartridges')
    if not data or len(data) % BANK_SIZE or len(data) > 128 * BANK_SIZE:
        raise ValueError('image must contain 1..128 complete 8 KiB banks')
    if data[4:9] != b'A0\xc3\xc2\xcd':
        raise ValueError('not a VIC-20 cartridge image')
    if data[HEADER_OFFSET:HEADER_OFFSET + HEADER_SIZE] != b'\xff' * HEADER_SIZE:
        raise ValueError('missing reserved firmware header')
    data = bytearray(data)
    data[HEADER_OFFSET:VERSION_OFFSET] = MAGIC
    data[VERSION_OFFSET:BLOCKS_OFFSET] = version.encode('ascii')
    data[BLOCKS_OFFSET:CHECKSUM_OFFSET] = (len(data) // BANK_SIZE).to_bytes(2, 'little')
    data[CHECKSUM_OFFSET:CHECKSUM_OFFSET + 2] = b'\0\0'
    checksum = sum(data) & 0xffff
    data[CHECKSUM_OFFSET:CHECKSUM_OFFSET + 2] = checksum.to_bytes(2, 'little')
    return bytes(data)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('infile', type=Path)
    parser.add_argument('imgfile', type=Path)
    parser.add_argument('--version', default=Path(__file__).with_name('VERSION').read_text().strip())
    parser.add_argument('--trimmed', action='store_true', help='emit only populated banks for disk updating')
    args = parser.parse_args()
    try:
        data = stamp_image(args.infile.read_bytes(), args.version)
    except ValueError as error:
        parser.error(str(error))
    args.imgfile.write_bytes(data if args.trimmed else data + bytes(IMG_SIZE - len(data)))


if __name__ == '__main__':
    main()
