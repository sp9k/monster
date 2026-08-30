## MONITOR OVERVIEW

The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.  The monitor and graphical debugger affect the same global debug state.
Changes made in one (e.g. creating a watch) will be reflected in the other.

Input in the monitor is buffered as with BASIC commands (but unlike the graphical
debugger). Lines are entered and when you wish to execute them, press {c64-key}`RETURN`.

---

### ACTIVATION

The monitor is activated as a window with the {c64-key}`F7` key.  In this mode the editor (or, while debugging,
the source view) remains visible above the monitor window.  The window may be resized with {c64-keys}`C= + K`
(grow) and {c64-keys}`C= + J` (shrink).  These keys work both while the monitor is active and from the editor
while the window is open.

Pressing {c64-key}`F8` ({c64-keys}`Shift + F7`) opens the monitor _maximized_ (fullscreen) instead.

The monitor can be activated from the editor both during normal editing and while debugging.
The default state of the virtual machine when you boot is the state of the Vic right after its normal
cold start procedure executes, which is what you will see in the monitor if entered without first
assembling a program.

When the monitor is quit (the `x` command), the window is left onscreen (as with other GUI windows).
Press {c64-key}`F7` or {c64-key}`F8` to re-enter it.  The monitor window is closed, just as other windows are, with the {c64-key}`Left-arrow` key.

### FILE REDIRECTION
The output from a given monitor command can be redirected to file instead of the screen by using the
redirect (`>`) operator.  When placed at the end of a command, the redirect operator writes all output from
that command to the following file.

For example:

`r > regs.txt`

Will write the contents of the simulated 6502's registers to the disk file `regs.txt`

### COMMANDS

The table below is a quick reference for the available monitor commands. See
the corresponding command section for syntax, argument details, and examples.

| COMMAND | NAME | DESCRIPTION |
|---------|------|-------------|
| `a` | `ASSEMBLE` | assembles an instruction into memory |
| `b` | `LIST BREAKPOINTS` | lists the active breakpoints |
| `ba` | `ADD BREAKPOINT AT ADDRESS` | adds a breakpoint at an address |
| `bl` | `ADD BREAKPOINT AT LINE` | adds a breakpoint at a source line |
| `br` | `REMOVE BREAKPOINT` | removes a breakpoint by ID |
| `bt` | `BACKTRACE` | displays a rendered view of the call stack |
| `c` | `COMPARE` | compares two blocks of memory |
| `clear` | `CLEAR` | clears the monitor display |
| `d` | `DISASSEMBLE` | disassembles a range of memory |
| `dump` | `DUMP MEMORY` | renders memory as assembleable `.db` directives |
| `f` | `FILL MEMORY` | fills a memory range with one or more values |
| `files` | `SHOW FILES` | lists files in the current debug information |
| `g` | `GO` | continues execution, optionally at a new address |
| `h` | `HUNT` | searches memory for a sequence of values |
| `m` | `SHOW MEMORY` | displays the contents of memory |
| `move` | `MOVE MEMORY` | copies a range of memory to a new address |
| `new` | `INITIALIZE BASIC` | re-runs the BASIC warm-start process |
| `n` | `STEP OVER` | runs the next instruction, stepping over subroutines |
| `p` | `POKE MEMORY` | writes a byte to memory |
| `r` | `REGISTERS` | displays the simulated 6502 registers |
| `s` | `SAVE MEMORY` | saves a memory range to a file |
| `t` | `TRACE` | continues execution with tracing enabled |
| `w` | `LIST WATCHES` | lists the active watches |
| `wa` | `ADD WATCH` | adds a load-and-store watch |
| `wal` | `ADD LOAD WATCH` | adds a load watch |
| `was` | `ADD STORE WATCH` | adds a store watch |
| `wr` | `REMOVE WATCH` | removes a watch by ID |
| `x` | `QUIT` | exits the monitor |
| `z` | `STEP` | runs one instruction |
| `zo` | `STEP OUT` | runs until the current subroutine returns |

Arguments shown in square brackets are optional. Most address and value
arguments may be expressions, such as `label+10`.

#### ASSEMBLE `a address instruction`

Assembles the instruction at the address given by the expression. After a
successful assembly, the monitor prepares another `a` command at the address
immediately following the new instruction.

**EXAMPLE:**

`a $1000 lda #$00`

#### LIST BREAKPOINTS `b`

Lists every active breakpoint, including the ID used by the `br` command.

**EXAMPLE:**

`b`

#### ADD BREAKPOINT AT ADDRESS `ba address`

Adds a breakpoint at the given address. If debug information maps the address
to a source line, the breakpoint is associated with that line as well.

**EXAMPLE:**

`ba main+3`

#### ADD BREAKPOINT AT LINE `bl filename line`

Adds a breakpoint at the given line in a file loaded with the current debug
information.

**EXAMPLE:**

`bl game.s 120`

#### REMOVE BREAKPOINT `br id`

Removes the breakpoint with the given ID. Use `b` to list breakpoint IDs.

**EXAMPLE:**

`br 2`

#### BACKTRACE `bt [offset]`

Displays a rendered view of the call stack, beginning just above the current
stack pointer. The optional offset adjusts the starting position and must be
less than `$80`. Stack contents are inferred, so data stored on the stack may
appear as an invalid frame.

**EXAMPLE:**

`bt 8`

#### COMPARE `c address1 address2 count`

Compares `count` bytes beginning at the two addresses and displays each pair
that differs.

**EXAMPLE:**

`c $1000 $2000 $20`

#### CLEAR `clear`

Clears the monitor and returns the cursor to the origin. The
{c64-keys}`C= + L` shortcut performs the same action.

**EXAMPLE:**

`clear`

#### DISASSEMBLE `d [start [end]]`

Disassembles memory beginning at `start-address`. If no end address is given,
the command disassembles at least `$10` bytes. If no start address is given,
disassembly continues from the monitor's current default address.

**EXAMPLE:**

`d main main+$40`

#### DUMP MEMORY `dump [start [end]]`

Renders the selected memory as assembleable `.db` directives. If no end
address is given, the command dumps `$40` bytes. If no start address is given,
the dump begins at the monitor's current default address. This command is
particularly useful with [file redirection](#file-redirection).

**EXAMPLE:**

`dump $1000 $1100 > data.s`

#### FILL MEMORY `f start end value [, value ...]`

Fills the half-open range `[start-address, end-address)` with the given values.
When more than one value is supplied, the sequence repeats until the range is
full.

**EXAMPLE:**

`f $1000 $1100 $00, $ff`

#### SHOW FILES `files`

Lists every source file loaded in the current debug information.

**EXAMPLE:**

`files`

#### GO `g [address]`

Continues execution without tracing. If an address is supplied, it becomes the
new program counter before execution begins.

**EXAMPLE:**

`g main`

#### HUNT `h start value [, value ...]`

Searches from `start-address` through `$ffff` for the first occurrence of the
given sequence and displays its address.

**EXAMPLE:**

`h $1000 $de, $ad, $be, $ef`

#### SHOW MEMORY `m [start [end]]`

Displays memory beginning at `start-address`. If no end address is given, the
command displays `$40` bytes. If no start address is given, display continues
from the monitor's current default address.

**EXAMPLE:**

`m screen screen+$100`

#### MOVE MEMORY `move start end destination`

Copies the half-open range `[start-address, end-address)` to `destination`.

**EXAMPLE:**

`move $1000 $1100 $2000`

#### INITIALIZE BASIC `new`

Reinitializes user memory by running the BASIC warm-start process.

**EXAMPLE:**

`new`

#### POKE MEMORY `p address value`

Writes the given byte value to an address.

**EXAMPLE:**

`p $900f $08`

#### REGISTERS `r`

Displays the current simulated 6502 register values. It also sets the
monitor's default address to the current program counter for subsequent `d`,
`dump`, or `m` commands.

**EXAMPLE:**

`r`

#### SAVE MEMORY `s start end filename`

Saves the half-open range `[start-address, end-address)` to the given file.

**EXAMPLE:**

`s $1000 $2000 memory.bin`

#### STEP OVER `n`

Runs the next instruction and returns to the monitor. A `JSR` and the called
subroutine are treated as a single instruction.

**EXAMPLE:**

`n`

#### TRACE `t`

Continues execution with instruction tracing enabled.

**EXAMPLE:**

`t`

#### LIST WATCHES `w`

Lists every active watch, including the ID used by the `wr` command.

**EXAMPLE:**

`w`

#### ADD WATCH `wa start [end]`

Adds a watch that triggers when the selected address or range is either read
from or written to.

**EXAMPLE:**

`wa player_x player_y`

#### ADD LOAD WATCH `wal start [end]`

Adds a watch that triggers only when the selected address or range is read.

**EXAMPLE:**

`wal $1000 $10ff`

#### ADD STORE WATCH `was start [end]`

Adds a watch that triggers only when the selected address or range is written
to.

**EXAMPLE:**

`was score score+2`

#### REMOVE WATCH `wr id`

Removes the watch with the given ID. Use `w` to list watch IDs.

**EXAMPLE:**

`wr 1`

#### QUIT `x`

Exits the monitor and returns to the editor or source view. The monitor window
remains onscreen until it is closed with {c64-key}`Left-arrow`.

**EXAMPLE:**

`x`

#### STEP `z`

Runs the next instruction and returns to the monitor, displaying the updated
registers and next instruction.

**EXAMPLE:**

`z`

#### STEP OUT `zo`

Runs until the current subroutine returns with `RTS`, then displays the updated
registers and next instruction.

**EXAMPLE:**

`zo`

### MONITOR SHORTCUTS

These keys perform monitor or window actions directly; they are not typed at
the monitor prompt.

| KEY | NAME | DESCRIPTION |
|-----|------|-------------|
| {c64-key}`F1` | `VIEW SCREEN` | toggles `$1000`-`$2000` between monitor and program memory |
| {c64-key}`F2` | `ENTER USER PROGRAM` | enters the running program, or BASIC if no program has been debugged |
| {c64-keys}`C= + L` | `CLEAR` | clears the monitor display, like the `clear` command |
| {c64-keys}`C= + K` | `GROW WINDOW` | grows the monitor window by one row in windowed mode |
| {c64-keys}`C= + J` | `SHRINK WINDOW` | shrinks the monitor window by one row in windowed mode |
