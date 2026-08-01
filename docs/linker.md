## LINKER OVERVIEW

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.  To link a program there are a few prerequisites:

1. produce the object files you wish to link
2. produce a LINK file to describe the desired layout for the linked program
3. link the program

### SAVING AN OBJECT FILE
Object files are nothing more than individually assembled fragments.  Anything you assemble (C=+a) can
be stored to disk in the object format.  This is done with the `:o` Ex command.  The linker will
specifically look for files that end in `.o` when it goes to link, so be sure to enter a filename
with that suffix: e.g. `:o hello.o`.

Your assembled program may itself specify where it should be loaded (this is what the `.org` directive does).
In these cases, the linker doesn't have much work.  It will, at least, ensure that all the linked
files don't overlap.

Its real value comes when you use the `.seg` directive instead.  The linker's job is to find all the
code and data that was defined in the same segment and to put it together into one contiguous block.

For example, say we have two object files: a.o and b.o
#### a.s
```
.seg "CODE"
	lda #$00
	sta $900f
.seg "DATA"
	.byte "hello"
```

#### b.s
```
.seg "CODE"
	rol $9000
.seg "DATA"
	.byte " world"
```

The linker will _concatenate_ each segment in b.o to the corresponding ones defined in a.o.
Effectively, the linked binary will correspond to something like this:

#### c.s
```
.seg "CODE"
	lda #$00
	sta $900f
	rol $9000
.seg "DATA"
	.byte "hello"
	.byte " world"
```

But what physical address will "CODE" and "DATA" actually correspond to?  Enter the `LINK` file.

### LINK FILE FORMAT
The LINK file is responsible for producing the desired layout for the binary program.
It contains two "blocks" of definitions for the two concepts that define how the linker performs its job of laying out
the program.
  - `MEMORY`: defines the SECTION addresses, sizes, and properties
  - `SEGMENTS`: defines how and where the SEGMENTS defined in the object code map to the memory SECTIONS.

The LINK file must always be named "LINK". Therefore, only 1 such file may exist on a given disk.
The linker loads this file before beginning the link process and uses it to initialize the layout for the final linked binary as well as define the constraints for it.

#### EXAMPLE
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

### SECTION FLAGS
In the above example, we declared the key "FILL" with the value of "1" for SECTIONA.
This is called a _section flag_.  The _FILL_ flag tells the linker how to handle unused
memory within a SECTION.  The table below describes the available flags and their names.

Note that any nonzero value for these flags will enable them while the zero value disables them.

| name | description
|------|--------------------------------------------------------------
| FILL |  if '1' fills unused memory in the section with 0's

---

### TECHNICAL DETAILS
The following sections are probably only interesting the the most hardcore enthusiasts, who care how
Monster actually implements its linker.

### LINK PROCESS OVERVIEW
To link multiple object files the linker follows the following procedure:
* Parse link config file
 * Get SEGMENT base addresses (where to assemble the object code)
* Pass 1: build link context (per object file)
    * Gather SEGMENT usage from object header
    * Gather global symbols
         * Read EXPORT table in each object file
         * Add name, segment, and segment-offset for each EXPORT
* Build final layout from SEGMENT usage in each object file
* Fully resolve global symbols and build the global symbol table
* Pass 2: link objects (per object file)
    * Build object-local context
         * Get base address of each SECTION from global SEGMENT base + object's SEGMENT offset
         * Map symbol indices to their resolved addresses using global symbol table / segment base address
    * Store object code to the current address for each segment
    * Walk relocation table, for each segment, and apply the the relocations described using the global symbol table
    * Load debug info
         * for each block, add base address of SEGMENT for corresponding file
         * append line program data for each object file to get global line program data
* Validate
  * make sure segments don't overlap
* Write MAP file (if requested)

----

### OBJECT FILE FORMAT
Below is a description of the object file's components.  These are listed in the order they appear in the object file.  These are described in further depth in the rest of this document.

| field          | description
|----------------|-----------------------------------------------------------
| OBJ HEADER     | basic info (number of segments, and symbols)
| SEGMENT HEADER | names and usage of each SEGMENT
| SYMBOLS        | the symbol table (IMPORTS and EXPORTS)
| SECTIONS       | .CODE, .REL, and .DEBUGINFO, tables (per SECTION)


#### HEADER
At thet beginning of the object file is the _header_, which gives basic details about the object file.  The header simply tells us how many segments and symbols are defined in the object file.
The linker uses the header in each object file to determine the final layout in pass 1.

| field        | size |  description
|--------------|------|--------------------------------------------------
| num segments |  1   | number of SEGMENTS used
| num exports  |  1   | number of exports in object file
| num imports  |  2   | number of imports in object file


#### SEGMENT HEADER
After the header is the SEGMENT header, which describes the SEGMENT usage for the object file.  It details which SEGMENTs are used (by name) and how many bytes each one contains.

For example, given the following assembly code:
```
.seg "CODE"
    asl
.seg "DATA"
    .byte 1, 2, 3, 4, 5
.seg "CODE"
     lda #$00
```

The SEGMENT header will be:

| segment name  | size
|---------------|------------
|     CODE      |  3
|     DATA      |  5


The format of this header in the object code is as follows:

| field | size |  description
|-------|------|--------------------------------------------------
| name  |   8  | SEGMENT name (where to write SECTION to)
| size  |   2  | size in bytes

At link time, The linker sums the _size_ field for the SEGMENTs in each object file
to determine the total amount of space needed for the SEGMENT in the final binary.

The order of the definitions in this header also correspond to the order of the
SEGMENT tables written later in the object file (see "SECTIONS" below for more detail on this).

### SYMBOLS
Next is the _symbol_ table. This table contains all labels that are used in the object file.

The symbol table two parts: IMPORTS and EXPORTS, which appear in the order shown in this table:

| field          | description
|----------------|--------------------------------------------------------------------------------
| IMPORTS        | symbols that are defined in other object files but used in this one
| EXPORTS        | symbols that are defined in this object file and (potentially) used in others


Symbols are mangled before storage in this table by the assembler.
At link time their in-memory format contains a namespace, the name of the file that they are defined within.

For example, if an EXPORT symbol `LABEL` is defined within the file `FOO.O`, it will be defined as `FOO:LABEL` in the symbol table.  This namespace is _not_ defined in the object code itself.  It is generated by
the linker from the filename.

The following sections provide a more detailed overview on each block, IMPORTS and EXPORTS, in the symbol table.

#### IMPORTS
The IMPORTS block of symbols contains the names of all symbols imported by the object file, the SEGMENT index, and their segment-offset.
Since IMPORTS are external, we don't know what their index will be when we generate the object code. Instead, relocation entries reference their index
in this table, which is used to look up their resolved value by name.

Resolving names to their SEGMENT and offset occurs by reading the EXPORTS block for all object files and applying link-time adjustments, which is done after the linker's first pass (see the EXPORTS section below for more information on this process).

Validation is performed on each global (EXPORT/IMPORT) to ensure that the same symbol doesn't have conflicting definitions (e.g. two differing sizes).


|  field  | size  |  description
|---------|-------|-----------------------------------------------------------------
|  name   | 1-33  | the symbol name as a 0-terminated string
|  info   |   1   | information about the import (size)
|  index  |   2   | index of symbol in LOCALS that corresponds to this IMPORT symbol


The info field uses the following bitfield format:

|  field  | bit(s)|  description
|---------|-------|----------------------------------------------------
|  size   |   0   | 0=zeropage import ($00-$ff), 1=absolute (>= $100)


#### EXPORTS
The EXPORTS block defines symbols that are used, or may be used, in other object files.  They tell the linker where to define labels that will be used during linkage.

Before reading the object code, the linker resolves the EXPORTS to their final addresses.
To accomplish this, the linker needs the base address for the symbol's SEGMENT. Because many object files
may reference the same SEGMENT, the linker must compute the base address of the SEGMENT for each file that
references it.  This also happens in pass 1.

Suppose we have the following files where the `.CODE` segment begins at the address listed:

| filename | .CODE base
|----------|--------------------------------
|  foo     |  $1000
|  bar     |  $1020


Given a symbol `bar:LABEL`, the linker adds the link-time base address for `bar`, $1020, to the offset for `LABEL` in the EXPORT record to get the final resolved address.
Let's say `LABEL` has an offset of $20.  The resolved address is `$1020+$20 = $1040`.

Note that the address field stored in the EXPORTS table is the same as the address field in the LOCALS group (and both will always have the same value).
This duplication allows the linker to read the EXPORTS block in pass 1 and fully resolve the labels
without having to load the entire LOCALS table for each object file.


| field   | size  | description
|---------|-------|----------------------------------------------------------------------
| name    |  1-33 | the name of the symbol as a 0-terminated string
| segment |   1   | segment the symbol resides in (index in object's SEGMENT table)
| address |   2   | absolute or offset from the base of the SEGMENT in this object file

The address mode for a SYMBOL is determined by its correspondign SEGMENT.
If it SYMBOL references a zeropage SEGMENT, it will also be defined as "zeropage" (1 byte).


### SEGMENTS
After the symbols comes a list of one or more SEGMENT definitions (the exact number is defined in the OBJ HEADER).
Each SEGMENT contains a short header that tells us the size of the three sub-tables that comprise the SEGMENT followed by those sub-tables themselves: object, relocation, and a debug information.

Below is the format for the header which precedes the SEGMENT tables:

| field           | size | description
|-----------------|------|---------------------------------------------------------------------
|  info           |  1   | info byte: zeropage/absolute etc.
|  code size      |  2   | size of the object-code binary table for the SEGMENT
|  reloc size     |  2   | size of the relocation table for the SEGMENT


The _info_ bitfield for the SEGMENT uses the following format:

| field           | bit(s) | description
|-----------------|------|-------------------------------------------------------
|  size           |  0   | 0=zeropage, 1=absolute

The following sections will discuss the layout of the data tables that follow the header in each SEGMENT.

#### NOTE: SECTIONS vs. SEGMENTS
At assembly time, every time a `.SEG` directive is encountered, a new _SECTION_ is created.
This concept disappears once the object code is generated (with the exception of debug
info, which has its own version of it) when these SECTIONS are collapsed into the SEGMENTS
that they reference.

Here is an annotated example program to illustrate where new sections created:
```
.seg "CODE"   ; [section 1]
foo
    asl
.seg "DATA"   ; [section 2]
bar
    jmp $f00d
.seg "CODE"   ; [section 3]
baz
    lda foo
```
Note that although CODE is referenced twice in different ".seg" directives, each instance causes
a new SECTION to be produced.

When the object code is written, the SECTIONS are concatenated to form the SEGMENT written to
the object file. In the above example, we would have two SEGMENTS: one for "CODE" and one for "DATA".

Symbols avoid the SECTION notion altogether to remove the need to update all symbol references in the relocation table(s).
When a symbol is encountered its SEGMENT is looked up and written with its relocation entry.
This also avoids resorting to SYMBOL-relative relocation records for references to labels in noncontiguous SECTIONS that belong to the same SEGMENT.
For example, referring to the case above, `lda foo` can be handled with a SEGMENT-relative relocation because "foo" is in the same SEGMENT as the instruction referencing it.
SYMBOL-relative relocations are _only_ required for external symbols.

### RELOCATION TABLES
For each SEGMENT, the linker contains a table of _relocation info_.

This table is made up of a number of records, each describing how to relocate a byte or word within the SEGMENT.
Relocations can either be _segment-relative_ (references to object-local SEGMENT base) or _symobl-relative_ (references to external symbols).

The following table describes the relocation record format in detail.

| field             | size | description
|-------------------|------|-------------------------------------------------------------------------------------
| info              |  1   | bitfield of information about the relocation entry
| offset            |  2   | offset from SEGMENT to relocate
| symbol/segment id |  2   | the symbol index in the symbol table (for symbol-relative relocation) or segment for segment-relative
| addend MSB*       |  1   | explicit MSB for addend (only when applying post-processing)

\* see details below for when this field is included

`info` is a bitfield with the following format:

| field      | bit(s) | description
|------------|--------|---------------------------------------------------------------------
| size       |   0    | size of target value to modify 0=1 byte, 1=2 bytes
| mode       |   1    | what to relocate relative to: 0=segment relative, 1=symbol relative
| postproc   |  2-3   | post-processing to apply after adding addend (0=NONE, 1=LSB, 2=MSB)

To apply the relocation table for a SEGMENT, we walk the table, go to the address of that SEGMENT's
base + the offset for each table entry, and depending on the value of "mode" in the "info" field:
 - 0 (segment relative) -look up SEGMENT base address and add addend to it
 - 1 (symbol relative") - look up the symbol address and add addend to it

Finally, we apply post-processing (bits 2-3) in the info byte, if necessary.

The "addend" is the value stored in the object code as the operand for the instruction we are
relocating.

The addend is generally the same size as the target value to be relocated.
The one exception is relocation entries that contain post-processing.  For these,
the intermediate value may be greater than $ff, so we need to encode a full 16-bit addend for the
the 1 byte target.  For example: `LDA #<(LABEL + 500)` requires a 16-bit addend (500) to calculate
the final 8-bit target.  The LSB of this addend is stored in the instruction stream, but in
this special case the MSB is stored in an extra byte at the end of the relocation entry for that record.
Because of this, records that contain post-processing are 6 bytes instead of 5.

### DEBUG INFO
This table stores the program to evaluate line numbers and addresses within the object file as well as references to which source files were used to create the object file.  This information allows the linker to produce a single mega debug file (or .D file) that contains all the information for the linked program, which allows for source level debugging.

|       field       |     size     | description
|-------------------|--------------|----------------------------------------------------------
| file table        |      ...     | 0-terminated list of all files
| num blocks        |       1      | number of BLOCKS in the object file
| line program size |       2      | total number of bytes of line program data in all blocks
| headers           | 2\*BLOCKSIZE  | the BLOCK header data for all blocks
| line program data |      ...     | the line program data for the object code for all blocks

The debug info format itself (headers and line program data) is described in further detail in [debug-info.md](debug-info.md).
