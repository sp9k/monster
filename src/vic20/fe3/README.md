# Final Expansion 3

From `src`, build the 512 KiB flash image with:

```sh
make cart TARGET=vic20 EXPANSION=fe3 REGION=NTSC
# or REGION=PAL
```

The output is `monster-fe3.bin`. `make fe3` is a shortcut. The restored target
uses the default `soft4x8` (40-column) display. For VICE, select the complete
machine model, so its KERNAL matches the build:

```sh
xvic -default -model vic20ntsc -cartfe monster-fe3.bin
# PAL: -model vic20pal
```

Cold-restart after replacing the image: the running program is in RAM, not ROM.
Flashing this image replaces the FE3 firmware; retain a backup of your existing
flash image. This repository's UltiMem flasher is **not** an FE3 flasher.

## Disk build

To load Monster into FE3 RAM from disk, keeping your existing flash firmware:

```sh
make fe3-disk REGION=NTSC
# or: make disk TARGET=vic20 EXPANSION=fe3 REGION=PAL
```

The output is `monster-fe3.d64`. The individual PRG files are also in
`fe3-disk/`; copy **all** of them together if using another disk or device.
Use the filenames from the D64 directory: remove the host files' `.prg` suffix.
Start expanded BASIC on the FE3 (BLK1 enabled, with its bank registers still
accessible), attach the disk, and enter:

```basic
LOAD"MONSTER",8,1
RUN
```

Use your drive number in place of `8`; the loader keeps that device for the
remaining files and for Monster. A missing or incomplete file displays a load
error; correct the disk and press a key to retry that file.

The disk and cartridge builds use the same runtime memory layout and bank
assignments below. Only the temporary disk loader starts at `$1201`, so BASIC
can load and run it normally. It loads the shared initializers first, then the
banked code, using the cartridge loader's same copy table. Duplicate code banks
reuse the same disk files. Nothing is programmed into flash, and the disk is
needed again after a power cycle or reset. Launching the disk version starts a
fresh editing session.

## Memory layout

Cartridge startup first moves its loader into internal RAM at `$1100`. In START mode,
reading BLK5 locks the FE3 registers, so the loader unlocks them with a BLK5
write while executing in internal RAM. It then copies ROM code to Super RAM,
explicitly switching between ROM and RAM for each byte. It does not depend on
revision-specific Super ROM write-through behavior.

Each bank switches BLK1/2/3 together (`$2000–$7fff`, 24 KiB), plus its BLK5
code (`$a000–$bfff`). `$0400–$0fff` always aliases RAM bank zero, so the common
bank-switching routines, IRQ handlers, and shared buffers live there; bank zero's
BLK1 is otherwise unused. FE3 control registers occupy I/O3, not writable RAM.

| RAM bank | Use |
| --- | --- |
| 0 | Directory, console/monitor, error strings; shared RAM123 |
| 1 | Main editor/assembler and reset-preserved source metadata |
| 2 | Debug information |
| 3 | Expressions, floating point, help, memory configuration, graphics |
| 4 | Macros and error log |
| 5 | Linker and object metadata |
| 6–7 | Symbols and symbol names |
| 8 | Contexts and copy buffer |
| 9 | Simulator and saved machine state |
| 10 | User program |
| 11 | Assembly LOG buffer |
| 12–15 | Four independent 24 KiB source buffers |

Main BLK5 code is duplicated into source/LOG banks, and symbol code into the
symbol-name bank, so routines can continue executing during bank changes.
Startup leaves source contents and recovery metadata intact across reset.
During native GO/BASIC the debugger's RAM123 and zero page live in SIM.
A marker in SIM identifies that saved state, so cartridge startup restores it
before offering source recovery. Normal native returns clear the marker;
disk launches clear it and start a fresh session.
The source limit is four, plus the separate LOG buffer. LINK files retain their
1 KiB limit; the parser temporarily uses the unused object-relocation workspace.

## Native execution

GO/BASIC swap all shared RAM123 contents as well as internal RAM and display
state. The native NMI entry is at `$7fe0` in USER (BRK enters at `$7fe5`).
Its write to `$9c02` selects MAIN, where execution continues at `$7fef`.
Only USER and MAIN receive native transition code; SIM and the other banks
need no copies. A temporary 16-byte internal-RAM bridge at `$033c` selects
SIM for the RAM123 copy, preserving and restoring those cassette-buffer bytes
before user code or the debugger resumes.

Native vectors are installed only for GO/BASIC and replaced with the saved
debugger vectors on return. Tracing and monitor command cancellation use
separate shared signal handlers, which work across debugger bank switches.

`$7fe0–$7fff` is reserved and protected by the assembler/simulator. BLK3 must
remain enabled for the handler to be reachable, so the memory configuration
window does not allow disabling it. BASIC's contiguous memory ends at `$7f00`
when all three expansion blocks are enabled. The cassette buffer is available
to user programs again. Simulated I/O2/3 accesses cannot
change actual FE3 registers; native programs must likewise leave FE3 bank and
configuration registers alone to retain debugger access.

## Tests

Install `py65` in your chosen Python environment and run:

```sh
make test-fe3 PYTHON=myenv/bin/python
make test-fe3-disk PYTHON=myenv/bin/python
```

Set `VICE_ROM_DIR` if the VIC-20 ROMs are not in
`/opt/homebrew/share/vice/VIC20`. The tests execute the cartridge's boot loader,
banked assembler, editor assembly-completion path, source allocation/reset,
LINK parser, scrolling, tracing, native BRK/RESTORE transitions, symbol-viewer
formats/sorting, and monitor messages across bank switches. They reject
CPU JAM opcodes and start expansion RAM with a nonzero pattern.
They also check traced load flags in display RAM, VIC/color registers, and
internal-RAM code, plus cartridge reset recovery during GO/BASIC and after
normal BRK/RESTORE returns.
The disk tests run the same application regressions with erased flash and
emulated KERNAL file I/O, plus payload placement, device selection, and retries
after missing or truncated files.

The 22-column `hard8x8` build currently fails a pre-existing alert-width
assertion (also on UltiMem); it is not validated by this restoration. Hardware
testing on an actual FE3 is still needed; VICE and CPU-level tests cannot fully
validate cartridge timing and hardware revisions.

See [the bank-visibility audit](bank-audit.md) for the reviewed paths and fixes.
