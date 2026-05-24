<p align="center">
  <img width="718" alt="Screenshot 2024-09-17 at 7 52 42 PM" src="https://github.com/user-attachments/assets/8a0b14ef-3b51-4dbf-b85f-b272051a85e9">
</p>

## OVERVIEW

<img width="718" alt="Screenshot 2024-09-17 at 7 52 42 PM" src="https://github.com/user-attachments/assets/c8d3dae7-52f2-420b-95de-56edf8c89d2e">


Monster is an all-in-one development system comprised of an editor, assembler, debugger, and linker all running natively on the Commodore Vic-20.
The design philosophy is uncompromising maximalism without sacrificing efficiency or code density.

Large RAM expansions have become ubiquitous on the Vic-20 in recent years, so the philosophy of
this project is to provide a rich feature set for a generously expanded memory configuration.
Virtually any feature that I deem valuable in an assembly IDE is implemented.  Monster leverages the Ultimem
expansion to implement the following features (and many more) while running in such a way that almost the entire
flat address space is still available for the user's program.


| Features                                                  |
|-----------------------------------------------------------|
| 40 column bitmap-based editor                             |
| vi-style keybindings                                      |
| interactive source-level debugger                         |
| linker                                                    |
| breakpoint editor                                         |
| memory viewer/editor                                      |
| robust label support (including anonymous and local)      |
| directory viewer                                          |
| symbol viewer                                             |
| automatic formatting and syntax checking                  |
| powerful macro support                                    |
| virtual memory for user program isolation                 |
| user-defined character editor                             |
| many, many more...                                        |

For more information about each of the major components of Monster, refer to the documents
linked below.

 **[Assembler](docs/assembler.md)**

 **[Editor](docs/editor.md)**

 **[Debugger](docs/debugger.md)**

 **[Monitor](docs/monitor.md)**

 **[Linker](docs/linker.md)**

---

## REQUIREMENTS

Monster requires the [Ultimeme](https://www.go4retro.com/products/ultimem) to function.

Much of this RAM is used to store the multiple source code buffers (up to 8), but it is also used to store code, debug-info, and other data.

The banked memory allows the user program to execute in almost complete
isolation.  The user program state is entirely backed up in the Ultimem, which means that, although this environment consumes a vast amount of
memory itself, everything except address the IO address space ($9800-$9fff) is preserved when control moves between the
editor and the user program.  Moreso even than small monitor cartridges, the program itself is virtually unaware of the resident tooling.

## RUNNING

Prebuilt binaries can be found for all the necessary files in the `bin/` directory.
These can also be built from source with the Makefile. Build instructions can be found in the
next section of this README.

To run from _Disk_ find the BOOT.PRG and MASM.PRG files.
Write these to your disk of choice and load Monster as you would any other program on your Vic-20:

```
LOAD "BOOT.PRG",8,1
RUN
```

The cartridge binary, `monster.bin`, can be flashed to your Ultimem to boot Monster directly from ROM.
Copy this file to your IEC storage device along with the installer (`install.prg`). Then run
the following commands on your Vic-20 from the BASIC prompt:

```
LOAD "INSTALLER.PRG",8,1
RUN
```

This will flash Monster to the Final Expansion.  Reset the computer to enter Monster.

If you wish to run Monster in an emulator (VICE), ensure that VICE is installed on your
machine and run `make start-disk` or `make start-cart` from the root of the project to attach the
corresponding disk or cartridge image.
Alternatively, simply run VICE with the `-cartfe` argument:

```
xvic -cartfe monster.bin
```

## BUILD INSTRUCTIONS

### DEPENDENCIES

 - [cc65](https://github.com/cc65/cc65) (tested with version 2.19)
 - make
 - python3 (any version should do)

The build process also requires `python3` (any version should do).

### BUILD STEPS

Clone this repo `git clone https://github.com/gummyworm/monster.git`

`cd` to the directory you cloned to and run `make` for the appropriate target

Monster can be built for two targets: disk and cartridge.  The disk format
is useful if you want to test on your Final Expansion 3 without erasing its firmware.

The cartridge is a better choice for emulators or power users on real hardware.
It loads significantly faster (although JiffyDOS mostly closes the gap between the two)

To build the disk version, run
`make disk`

To build the cart version, run
`make cart`

---

### EXAMPLE PROGRAM
Here is a simple hello world program to demonstrate the basic syntax of the assembler:

```
.ORG $1400
START:
  JSR $E5B5
  LDX #$00
  LDA #' '
CLR:
  STA $1000,X
  STA $1100,X
  DEX
  BNE CLR
DISP:
  LDA MSG,X
  BEQ DONE
  JSR $FFD2
  INX
  BNE DISP
DONE:
  JMP DONE

MSG:
.DB "HELLO WORLD!",0
```
