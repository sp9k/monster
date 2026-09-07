## Monitor overview

The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.  The monitor and graphical debugger affect the same global debug state.
Changes made in one (e.g. creating a watch) will be reflected in the other.

Input in the monitor is buffered as with BASIC commands (but unlike the graphical
debugger). Lines are entered and when you wish to execute them, press {c64-key}`RETURN`.

---

### Activation

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
Press {c64-key}`F7` or {c64-key}`F8` to re-enter it.  The monitor window is closed, just as other windows are, with the {c64-keys}`C= + Q` key combination, which must be pressed while the monitor window has focus.

### File redirection
The output from a given monitor command can be redirected to file instead of the screen by using the
redirect (`>`) operator.  When placed at the end of a command, the redirect operator writes all output from
that command to the following file.

For example:

`r > regs.txt`

Will write the contents of the simulated 6502's registers to the disk file `regs.txt`

### Commands

The table below is a quick reference for the available monitor commands. See
the corresponding command section for syntax, argument details, and examples.

Arguments to commands are separated by whitespace.  This means that **expression parsing behaves
differently** than it does in other parts of Monster.  For example `M 10 + 30` is not a valid
expression.  Expression parsing breaks at each whitespace, meaning the first argument is
interpreted as `10` the second as `+` (illegal, by the way), and the third as `30`. The correct
invocation would be `M 10+30`.

| COMMAND | NAME                        | DESCRIPTION                                          |
|---------|-----------------------------|------------------------------------------------------|
| `?`     | `EVAL`                      | evaluates the following expression and prints result |
| `a`     | `ASSEMBLE`                  | assembles an instruction into memory                 |
| `b`     | `LIST BREAKPOINTS`          | lists the active breakpoints                         |
| `ba`    | `ADD BREAKPOINT AT ADDRESS` | adds a breakpoint at an address                      |
| `bl`    | `ADD BREAKPOINT AT LINE`    | adds a breakpoint at a source line                   |
| `br`    | `REMOVE BREAKPOINT`         | removes a breakpoint by ID                           |
| `bt`    | `BACKTRACE`                 | displays a rendered view of the call stack           |
| `c`     | `COMPARE`                   | compares two blocks of memory                        |
| `clear` | `CLEAR`                     | clears the monitor display                           |
| `d`     | `DISASSEMBLE`               | disassembles a range of memory                       |
| `dump`  | `DUMP MEMORY`               | renders memory as assembleable `.db` directives      |
| `f`     | `FILL MEMORY`               | fills a memory range with one or more values         |
| `files` | `SHOW FILES`                | lists files in the current debug information         |
| `g`     | `GO`                        | continues execution, optionally at a new address     |
| `h`     | `HUNT`                      | searches memory for a sequence of values             |
| `m`     | `SHOW MEMORY`               | displays the contents of memory                      |
| `move`  | `MOVE MEMORY`               | copies a range of memory to a new address            |
| `new`   | `INITIALIZE BASIC`          | re-runs the BASIC warm-start process                 |
| `n`     | `STEP OVER`                 | runs the next instruction, stepping over subroutines |
| `p`     | `POKE MEMORY`               | writes a byte to memory                              |
| `r`     | `REGISTERS`                 | displays the simulated 6502 registers                |
| `s`     | `SAVE MEMORY`               | saves a memory range to a file                       |
| `t`     | `TRACE`                     | continues execution with tracing enabled             |
| `w`     | `LIST WATCHES`              | lists the active watches                             |
| `wa`    | `ADD WATCH`                 | adds a load-and-store watch                          |
| `wal`   | `ADD LOAD WATCH`            | adds a load watch                                    |
| `was`   | `ADD STORE WATCH`           | adds a store watch                                   |
| `wr`    | `REMOVE WATCH`              | removes a watch by ID                                |
| `x`     | `QUIT`                      | exits the monitor                                    |
| `z`     | `STEP`                      | runs one instruction                                 |
| `zo`    | `STEP OUT`                  | runs until the current subroutine returns            |

Arguments shown in square brackets are optional. Most address and value
arguments may be expressions, such as `label+10`.

#### Evaluate `? expression`

Evaluates the followin expression and prints the result.

```{note}
`? (2.0*SCREEN_H)`
```

#### Assemble `a address instruction`

Assembles the instruction at the address given by the expression. After a
successful assembly, the monitor prepares another `a` command at the address
immediately following the new instruction.

```{note}
`a $1000 lda #$00`
```

#### List breakpoints `b`

Lists every active breakpoint, including the ID used by the `br` command.

```{note}
`b`
```

#### Add breakpoint at address `ba address`

Adds a breakpoint at the given address. If debug information maps the address
to a source line, the breakpoint is associated with that line as well.

```{note}
`ba main+3`
```

#### Add breakpoint at line `bl filename line`

Adds a breakpoint at the given line in a file loaded with the current debug
information.

```{note}
`bl game.s 120`
```

#### Remove breakpoint `br id`

Removes the breakpoint with the given ID. Use `b` to list breakpoint IDs.

```{note}
`br 2`
```

#### Backtrace `bt [offset]`

Displays a rendered view of the call stack, beginning just above the current
stack pointer. The optional offset adjusts the starting position and must be
less than `$80`. Stack contents are inferred, so data stored on the stack may
appear as an invalid frame.

```{note}
`bt 8`
```

#### Compare `c address1 address2 count`

Compares `count` bytes beginning at the two addresses and displays each pair
that differs.

```{note}
`c $1000 $2000 $20`
```

#### Clear `clear`

Clears the monitor and returns the cursor to the origin. The
{c64-keys}`C= + L` shortcut performs the same action.

```{note}
`clear`
```

#### Disassemble `d [start [end]]`

Disassembles memory beginning at `start-address`. If no end address is given,
the command disassembles at least `$10` bytes. If no start address is given,
disassembly continues from the monitor's current default address.

```{note}
`d main main+$40`
```

#### Dump memory `dump [start [end]]`

Renders the selected memory as assembleable `.db` directives. If no end
address is given, the command dumps `$40` bytes. If no start address is given,
the dump begins at the monitor's current default address. This command is
particularly useful with [file redirection](#file-redirection).

```{note}
`dump $1000 $1100 > data.s`
```

#### Fill memory `f start end value [, value ...]`

Fills the half-open range `[start-address, end-address)` with the given values.
When more than one value is supplied, the sequence repeats until the range is
full.

```{note}
`f $1000 $1100 $00, $ff`
```

#### Show files `files`

Lists every source file loaded in the current debug information.

```{note}
`files`
```

#### Go `g [address]`

Continues execution without tracing. If an address is supplied, it becomes the
new program counter before execution begins.

```{note}
`g main`
```

#### Hunt `h start value [, value ...]`

Searches from `start-address` through `$ffff` for the first occurrence of the
given sequence and displays its address.

```{note}
`h $1000 $de, $ad, $be, $ef`
```

#### Show memory `m [start [end]]`

Displays memory beginning at `start-address`. If no end address is given, the
command displays `$40` bytes. If no start address is given, display continues
from the monitor's current default address.

```{note}
`m screen screen+$100`
```

#### Move memory `move start end destination`

Copies the half-open range `[start-address, end-address)` to `destination`.

```{note}
`move $1000 $1100 $2000`
```

#### Initialize BASIC `new`

Reinitializes user memory by running the BASIC warm-start process.

```{note}
`new`
```

#### Poke memory `p address value`

Writes the given byte value to an address.

```{note}
`p $900f $08`
```

#### Registers `r`

Displays the current simulated 6502 register values. It also sets the
monitor's default address to the current program counter for subsequent `d`,
`dump`, or `m` commands.

```{note}
`r`
```

#### Save memory `s start end filename`

Saves the half-open range `[start-address, end-address)` to the given file.

```{note}
`s $1000 $2000 memory.bin`
```

#### Step over `n`

Runs the next instruction and returns to the monitor. A `JSR` and the called
subroutine are treated as a single instruction.

```{note}
`n`
```

#### Trace `t`

Continues execution with instruction tracing enabled.

```{note}
`t`
```

#### List watches `w`

Lists every active watch, including the ID used by the `wr` command.

```{note}
`w`
```

#### Add watch `wa start [end]`

Adds a watch that triggers when the selected address or range is either read
from or written to.

```{note}
`wa player_x player_y`
```

#### Add load watch `wal start [end]`

Adds a watch that triggers only when the selected address or range is read.

```{note}
`wal $1000 $10ff`
```

#### Add store watch `was start [end]`

Adds a watch that triggers only when the selected address or range is written
to.

```{note}
`was score score+2`
```

#### Remove watch `wr id`

Removes the watch with the given ID. Use `w` to list watch IDs.

```{note}
`wr 1`
```

#### Quit `x`

Exits the monitor and returns to the editor or source view. The monitor window
remains onscreen until it is closed with {c64-keys}`C= + Q`.  Because that key
must be pressed while the window has focus, re-enter the monitor
({c64-key}`F7` or {c64-keys}`C= + W`) and press it there to close the window.

```{note}
`x`
```

#### Step `z`

Runs the next instruction and returns to the monitor, displaying the updated
registers and next instruction.

```{note}
`z`
```

#### Step out `zo`

Runs until the current subroutine returns with `RTS`, then displays the updated
registers and next instruction.

```{note}
`zo`
```

### Monitor shortcuts

These keys perform monitor or window actions directly; they are not typed at
the monitor prompt.

| KEY                | NAME                 | DESCRIPTION                                                          |
|--------------------|----------------------|----------------------------------------------------------------------|
| {c64-key}`F1`      | `VIEW SCREEN`        | toggles `$1000`-`$2000` between monitor and program memory           |
| {c64-key}`F2`      | `ENTER USER PROGRAM` | enters the running program, or BASIC if no program has been debugged |
| {c64-keys}`C= + L` | `CLEAR`              | clears the monitor display, like the `clear` command                 |
| {c64-keys}`C= + K` | `GROW WINDOW`        | grows the monitor window by one row in windowed mode                 |
| {c64-keys}`C= + J` | `SHRINK WINDOW`      | shrinks the monitor window by one row in windowed mode               |
| {c64-keys}`C= + Z` | `MAXIMIZE WINDOW`    | toggles the monitor window between maximized and its last size       |
| {c64-keys}`C= + Q` | `CLOSE WINDOW`       | closes the monitor window (windowed mode only)                       |
| {c64-keys}`C= + W` | `NEXT WINDOW`        | leaves the monitor open and cycles to the next window                |
