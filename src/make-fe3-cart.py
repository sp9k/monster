"""Pack ld65's physical-bank-ordered output into a 512 KiB FE3 flash image."""
import pathlib
import sys

data = bytearray(pathlib.Path(sys.argv[1]).read_bytes())
if len(data) != 512 * 1024:
    raise SystemExit("FE3 linker output must contain exactly sixteen 32 KiB banks")
if data[0x6004:0x6009] != b"A0\xc3\xc2\xcd":
    raise SystemExit("Missing FE3 power-on cartridge header at flash offset $6000")
pathlib.Path(sys.argv[2]).write_bytes(data)
