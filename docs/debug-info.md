## DEBUG INFORMATION TECHNICAL DETAILS

Debug information is stored in a few tables as described below. At a high level, these are:
  - FILE TABLE: maps file IDs to their filenames
  - BLOCKS: stores ranges of addresses and lines for blocks of code
  - LINE PROGRAMS: state machine that resolves addresses and line numbers when executed

### FILE TABLE
The FILE TABLE maps filenames to an implicit ID, which is the index of a given filename in this table.


|  SIZE    | DESCRIPTION                     |
|----------|---------------------------------|
|   16     | filename 0                      |
|   16     | filename 1                      |
|   16     | ...                             |


### BLOCKS
To simplify the storage of chunks of noncontiguous addresses and multi-file programs, mappings are broken down into BLOCKS.
Each BLOCK header  defines a file id (BLOCKS will always reference one file only), and a range of lines and addresses.

The table below describes the layout of a BLOCK (header).


|  FIELD       | SIZE  | DESCRIPTION
|--------------|-------|-----------------------------------------------------------------
| base         |  2    | the (segment relative) address that this block begins at
| top address  |  2    | the (segment relative) top address represented by the block + 1
| base line    |  2    | the lowest line number represented by the block
| # of lines   |  2    | the highest line number represented by the block
| file id      |  1    | filename 0
| segment id   |  1    | segment id for the block (only used for linking)
| program      |  2    | address of the line mapping program
| program end  |  2    | address of the end of the line program


For BLOCKS that begin with a `.SEG` directive, addresses in that BLOCK are relative to a SEGMENT.
The SEGMENT table that the id references is described in more detail in the [linker](linker.md) document.

BLOCKS that begin with a `.ORG` directive, will contain the SEC\_ABS ($ff) segment id, which
means their addresses are absolute (not relative to any SEGMENT).  The linker will look up the link-time
addresses for all SEGMENTS and transform the addresses in the header to absolute ones.

The _top address_ for a BLOCK is the last address in the block + 1.
In other words, the range represented starts at the base address (inclusive)
and ends at the top address (exclusive): `[base, top)`.

Also note that a given BLOCK always represents a line/address range within a single file.
If the file changes during assembly, e.g. when a `.INC` directive is encountered, a new
BLOCK is created.

In the assembler, the pseudo-ops that force the creation of a block are:
  - including a file (`.INC`)
  - setting the address (`.ORG`)
  - creating/activating a SEGMENT (`.SEG`, `.SEGZP`)

### LINE PROGRAM
The line program facilitates compact mapping of line numbers to addresses.
It is a state machine with the following variables:
 - line number
 - address (or program counter)

Commands modify this state in order to produce the address <-> line mapping.
There are two types of instructions to handle this process: "basic" and "extended".
Each type is detailed below.

#### BASIC INSTRUCTIONS

The most basic operation, which is so common that it requires no special opcode, adds a small
offset to the current line number AND program counter.  These are both encoded into a single
byte according to the following layout:

|   FIELD     | BITS |  DESCRIPTION
|-------------|------|------------------------------------------------------
| line offset | 0-3  | number of lines to advance the "line" count
| addr offset | 4-7  | number of bytes to advance the PC or address offset

#### EXTENDED INSTRUCTIONS

For cases when a small offset to either the line or address is not enough (e.g. a macro
that expands to more than 16 bytes), or generally to handle bigger "jumps" in the line mapping, these instructions are required.
The extended instructions begin with a $00 value prefix, which is not a valid basic instruction
because at least one of the line or address states must be advanced for each entry in the line table.

Below is the list of extended commands and their effects.

| COMMAND NAME  |  OPERAND  | CODE | OPERAND SIZE| EFFECT                                            |
|---------------|-----------|------|-------------|---------------------------------------------------|
| `SET_ADDRESS` | address   |  $01 | 2           | Sets the address to the given absolute address    |
| `RESERVED`    |    -      |  $02 | x           | Reserved (unused)                                 |
| `RESERVED`    |    -      |  $03 | x           | Reserved (unused)                                 |
| `ADVANCE_LINE`| offset    |  $04 | 2           | Moves the line by the given signed offset         |
| `ADVANCE_PC`  | offset    |  $05 | 2           | Moves the address by the given signed offset      |
| `SET_PC`      | offset    |  $06 | 2           | Moves the address by the given signed offset      |
| `END`         |  -        |  $00 | 0           | Marks the end of the program (block)              |


### EXAMPLE

To illustrate how the BLOCK and LINE PROGRAM data look in memory, we will examine their
in-memory form for the following simple program:
```
.org $1000
    lda #$00
    sta $900f
loop:
    jmp loop
```

The BLOCK header sets up the file ID, line number, and (base) address.
In this program, we start with a base address of $1000 (.ORG $1000).  The first
meaningful line (line that may be executed) is line 2, so we store this in the header as well.
This example has 5 lines and occupies 8 bytes. With these two pieces of data we have enough information
to complete our header:

| FIELD                 | DATA
|-----------------------|--------------
| base address          | $00 $10
| top address           | $08 $10
| base line             | $00 02
| number of lines       | $00 05
| file ID               | $01
| line program address  | $00 $20

The state machine is initialized with values from the block header, so we enter the program with:
  - line number: 2
  - file ID: 1
  - address: $1000

Note that this corresponds to `lda #$00` in our code.  If we are looking for the address $1000 or line
number 2, we never actually execute any instructions in the line program as the initialization has
already landed us on our target.

All that's left now is to build the line program for this block.  As mentioned, "lda #$00" is implicitly
handled by the base state for the state machine, which means we only need two "basic" line-program
instructions to resolve "sta $900f" and "jmp loop".  Here's what those look like in memory:

| VALUE | DESCRIPTION                                                 |
|-------|-------------------------------------------------------------|
| $22   | move line by 2 and address by 2 (we are now at `sta $900f`) |
| $32   | move line by 2 and address by 3 (we are now at `jmp loop`)  |

### DEBUG INFO GENERATION

The flow for generating debug information is:
 1. begin block: a new BLOCK is defined for the current file, line, and address
 2. add lines: instructions are added to the line program for the active block to produce a program capable of resolving the addresses and lines for each CPU instruction.
 3. end block: the most recently defined BLOCK is closed

---

### USING DEBUG INFORMATION
Once debug information is generated, its primary function is to map lines to addresses and vice versa.

#### MAPPING ADDRESS TO LINE

To map a given address to its corresponding line number, the line program for the block that contains
its address is executed.  Once the line program reaches a PC value equal to the one that is sought,
the current line number is returned.

#### MAPPING LINE TO ADDRESS

The line to address mapping works just like address to line mapping.  The first block containing the
file/address range being sought is executed.  When the line program's line number is equal to the one
requested, the PC value for the program at that point is returned.
