## Linker

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.  To link a program there are a few prerequisites:

1. produce the object files you wish to link
2. produce a LINK file to describe the desired layout for the linked program
3. link the program

### Building object files
Object files are nothing more than individually assembled fragments.  Anything you assemble ({c64-keys}`C= + A`) can
be stored to disk in the object format.  This is done with the `:o` Ex command.  The linker will
specifically look for files that end in `.o` when it goes to link, so be sure to enter a filename
with that suffix: e.g. `:o hello.o`.

Your assembled program may itself specify where it should be loaded (this is what the `.org` directive does).
In these cases, the linker doesn't have much work.  It will, at least, ensure that all the linked
files don't overlap.

Its real value comes when you use the `.seg` directive instead.  The linker's job is to find all the
code and data that was defined in the same segment and to put it together into one contiguous block.

For example, say we have two object files: a.o and b.o

`a.s`
```
.seg "CODE"
    lda #$00
    sta $900f
.seg "DATA"
    .db "hello"
```

`b.s`
```
.seg "CODE"
    rol $9000
.seg "DATA"
    .db " world"
```

The linker will _concatenate_ each segment in b.o to the corresponding ones defined in a.o.
Effectively, the linked binary will correspond to something like this:

```
.seg "CODE"
    lda #$00
    sta $900f
    rol $9000
.seg "DATA"
    .db "hello"
    .db " world"
```

But what physical address will "CODE" and "DATA" actually correspond to?  Enter the `LINK` file.

### LINK file format
The LINK file is responsible for producing the desired layout for the binary program.
It contains two "blocks" of definitions for the two concepts that define how the linker performs its job of laying out
the program.
  - `MEMORY`: defines the SECTION addresses, sizes, and properties
  - `SEGMENTS`: defines how and where the SEGMENTS defined in the object code map to the memory SECTIONS.

The LINK file must always be named "LINK". Therefore, only 1 such file may exist on a given disk.
The linker loads this file before beginning the link process and uses it to initialize the layout for the final linked binary as well as define the constraints for it.

Every `MEMORY` section must define both `START` and `END`. Note that the `END` address is exclusive.
Every `SEGMENTS` entry must define `LOAD`. `RUN` is optional and defaults to the
same memory section as `LOAD`. `ALIGN` is optional and defaults to no alignment.
`FILL` is also optional and defaults to disabled.
Names must be unique within each block.

#### LOAD vs RUN

The `LOAD` property decides which SECTION the memory SEGMENT's bytes are written to in the linked
binary. The `RUN` property tells the linker where the SEGMENT's code will be executed at
runtime.  This is useful if you have code that is loaded somewhere but copied somewhere else
before execution.

When `RUN` and `LOAD` name the same section (as is usually the case) the two layouts are
identical. When they differ, the linker places the bytes at the `LOAD` address
but generates all relocation data against the `RUN` address, so the segment can
be copied to its run address by your program (the linker does nothing to perform the actual
copy) before execution.

SEGMENTs are packed into their SECTION in the order they appear in the
`SEGMENTS` block: the first segment listed for a section starts at that
section's `START`, and each subsequent one begins where the previous ended.

If a SEGMENT defines both a `LOAD` and a `RUN` SECTION, it occupies each.  That is,
the SECTIONs that it *loads* and *runs* in are both advanced by the size of the SEGMENT.

#### ALIGN

The `ALIGN` property tells the linker to begin a SEGMENT on an address boundary instead of
wherever the previous SEGMENT ended.  Its value is the size of the
boundary, (in decimal or, with a `$` prefix, in hexadecimal). Any value in the range [1,$ffff]
is allowed.

To achieve the alignment, the linker *pads* the binary (fills it with 0's) until it arrives on the
next address evenly divisible by the requested boundary.

Given the `SEGMENTS` block

```
SEGMENTS [
    CODE:
        LOAD=ROM;
    TABLE:
        LOAD=ROM
        ALIGN=$100;
]
```

and a `ROM` SECTION starting at $2000, $18 bytes of `CODE` places `TABLE` at
$2100 rather than $2018, and $2018-$20ff is padding (0) in the linked binary.

Alignment like this may be desirable for timing sensitive code, where you want
to make sure a table stays within a single page to avoid the cycle penalty for crossing one.

Note that while padding logically applies to `RUN` sections as well as `LOAD` ones, the
`LOAD` one is the only one that causes the linker to emit padding bytes (remember that `RUN`
sections just represent the execution region at runtime).


````{note}

To illustrate how the `LINK` file functions in practice, let's walk through an example.

`LINK`

```
MEMORY [
    ROM:
        START=$2000
        END=$3000;
    RAM:
        START=$0400
        END=$1000;
]

SEGMENTS [
    BOOT:
        LOAD=ROM;
    FAST:
        LOAD=ROM
        RUN=RAM;
    VARS:
        LOAD=RAM;
]
```

If `BOOT` assembles to $100 bytes, `FAST` to $80, and `VARS` to $40, the layout is:

| SEGMENT | LOAD ADDRESS (bytes written here) | RUN ADDRESS (relocated for this) |
|---------|-----------------------------------|----------------------------------|
| BOOT    | $2000 (ROM)                       | $2000 (ROM)                      |
| FAST    | $2100 (ROM)                       | $0400 (RAM)                      |
| VARS    | $0480 (RAM)                       | $0480 (RAM)                      |

`FAST` consumes $80 bytes of ROM, where its bytes are actually stored, and another
$80 bytes of RAM, where nothing is written but the address range $0400-$047f is
reserved for it.

`VARS` picks up where `FAST`'s `RUN` left off: it is placed at $0480 rather than at
RAM's `START`, because `FAST` is listed first and reserved $0400-$047f ahead of
it.

````

#### Limits

| ITEM                                                | LIMIT |
|-----------------------------------------------------|-------|
| `MEMORY` sections                                   | 8     |
| `SEGMENTS` entries / segments per object on C64     | 8     |
| `SEGMENTS` entries / segments per object on VIC-20  | 64    |
| Object files in one link                            | 16    |
| Imports per object file                             | 128   |
| Exports per object file                             | 32    |

````{note}
Below is a simple LINK file example to demonstrate its configuration format

Each item (SECTION or SEGMENT) is terminated with a `;` character. Note that this
does _not_ denote a comment as it does in assembly files; comments are not valid in
the LINK file.

```
MEMORY [
    SECTIONA:
        START=$0400
        END=$1000
        FILL=1;
    SECTIONB:
        START=$1000
        END=$1200;
]

SEGMENTS [
    SEGA:
        LOAD=SECTIONA
        RUN=SECTIONB;
]
```

````

### Section flags
In the above example, we declared the key "FILL" with the value of "1" for SECTIONA.
This is called a _section flag_.  The _FILL_ flag tells the linker how to handle unused
memory within a SECTION.  The table below describes the available flags and their names.

Note that any nonzero value for these flags will enable them while the zero value disables them.

| NAME | DESCRIPTION
|------|--------------------------------------------------------------
| FILL |  if '1' fills unused memory in the section with 0's

#### Zero page sections

A SEGMENT declared with `.SEGZP` or `.BSSZP` is an address assignment only: it
reserves zero page locations for its symbols and contributes no bytes to the
linked binary.  The SECTION such a SEGMENT loads into is therefore not part of
the program image, and the linker leaves it out when working out the program's
start and end addresses.  Without that, a single zero page byte would drag the
program's start address down into the zero page, making the saved `.PRG` load
over the stack and KERNAL workspace on its way to your code.

For the same reason `FILL` is ignored on a SECTION that any zero page SEGMENT
loads into.  There is nothing to pad -- the SECTION contributes no bytes -- and
padding it would pull the program's start address back into the zero page.

Note that this applies to the SEGMENT's *type*, not its address.  A SEGMENT
declared with plain `.SEG` that you place below $0100 is treated like any other
SEGMENT: its bytes are part of the image and the program will start there.
