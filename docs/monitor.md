## Monitor

The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.  The monitor and graphical debugger affect the same global debug state.
Changes made in one (e.g. creating a watch) will be reflected in the other.

Input in the monitor is buffered as with BASIC commands (but unlike the graphical
debugger). Lines are entered and when you wish to execute them, press {c64-key}`RETURN`.

---

### Activation

Press {c64-key}`F7` to activate the monitor as a window.  In this mode the editor (or, while debugging,
the source view) remains visible above the monitor window.  The window may be resized with {c64-keys}`C= + K`
(grow) and {c64-keys}`C= + J` (shrink).  These keys work both while the monitor is active and from the editor
while the window is open.

Press {c64-keys}`C= + Z` while the monitor has focus to maximize it or restore its previous size.

The monitor can be activated from the editor both during normal editing and while debugging.
The default state of the virtual machine when you boot is the state of the Vic right after its normal
cold start procedure executes, which is what you will see in the monitor if entered without first
assembling a program.

When the monitor is quit (the `x` command), the window is left onscreen (as with other GUI windows).
Press {c64-key}`F7` to re-enter it.  Press {c64-keys}`C= + Q` while the monitor window has focus to close it, just like other windows.

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

```{warning}
The `a`, `f`, `move`, `p`, and `new` commands change simulated memory
immediately. They may clobber active program state you are debugging.
```

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

#### Evaluate

**Syntax:** `? expression`

**Behavior:** Evaluates the following expression and prints the result.

```{example}
`? (2.0*SCREEN_H)`
```

#### Assemble

**Syntax:** `a address instruction`

**Behavior:** Assembles the instruction at the address given by the expression. After a
successful assembly, the monitor prepares another `a` command at the address
immediately following the new instruction.

```{example}
`a $1000 lda #$00`
```

#### List breakpoints

**Syntax:** `b`

**Behavior:** Lists every active breakpoint, including the ID used by the `br` command.

```{example}
`b`
```

#### Add breakpoint at address

**Syntax:** `ba address`

**Behavior:** Adds a breakpoint at the given address. If debug information maps the address
to a source line, the breakpoint is associated with that line as well.

```{example}
`ba main+3`
```

#### Add breakpoint at line

**Syntax:** `bl filename line`

**Behavior:** Adds a breakpoint at the given line in a file loaded with the current debug
information.

```{example}
`bl game.s 120`
```

#### Remove breakpoint

**Syntax:** `br id`

**Behavior:** Removes the breakpoint with the given ID. Use `b` to list breakpoint IDs.

```{example}
`br 2`
```

#### Backtrace

**Syntax:** `bt [offset]`

**Behavior:** Displays a rendered view of the call stack, beginning just above the current
stack pointer. The optional offset adjusts the starting position and must be
less than `$80`. Stack contents are inferred, so data stored on the stack may
appear as an invalid frame.

```{example}
`bt 8`
```

#### Compare

**Syntax:** `c address1 address2 count`

**Behavior:** Compares `count` bytes beginning at the two addresses and displays each pair
that differs.

```{example}
`c $1000 $2000 $20`
```

#### Clear

**Syntax:** `clear`

**Behavior:** Clears the monitor and returns the cursor to the origin. The
Pressing {c64-keys}`C= + L` performs the same action.

```{example}
`clear`
```

#### Disassemble

**Syntax:** `d [start [end]]`

**Behavior:** Disassembles memory beginning at `start-address`. If no end address is given,
the command disassembles at least `$10` bytes. If no start address is given,
disassembly continues from the monitor's current default address.

```{example}
`d main main+$40`
```

#### Dump memory

**Syntax:** `dump [start [end]]`

**Behavior:** Renders the selected memory as assembleable `.db` directives. If no end
address is given, the command dumps `$40` bytes. If no start address is given,
the dump begins at the monitor's current default address. This command is
particularly useful with [file redirection](#file-redirection).

```{example}
`dump $1000 $1100 > data.s`
```

#### Fill memory

**Syntax:** `f start end value [, value ...]`

**Behavior:** Fills the half-open range `[start-address, end-address)` with the given values.
When more than one value is supplied, the sequence repeats until the range is
full.

```{example}
`f $1000 $1100 $00, $ff`
```

#### Show files

**Syntax:** `files`

**Behavior:** Lists every source file loaded in the current debug information.

```{example}
`files`
```

#### Go

**Syntax:** `g [address]`

**Behavior:** Continues execution without tracing. If an address is supplied, it becomes the
new program counter before execution begins.

```{example}
`g main`
```

#### Hunt

**Syntax:** `h start value [, value ...]`

**Behavior:** Searches from `start-address` through `$ffff` for the first occurrence of the
given sequence and displays its address.

```{example}
`h $1000 $de, $ad, $be, $ef`
```

#### Show memory

**Syntax:** `m [start [end]]`

**Behavior:** Displays memory beginning at `start-address`. If no end address is given, the
command displays `$40` bytes. If no start address is given, display continues
from the monitor's current default address.

```{example}
`m screen screen+$100`
```

#### Move memory

**Syntax:** `move start end destination`

**Behavior:** Copies the half-open range `[start-address, end-address)` to `destination`.

```{example}
`move $1000 $1100 $2000`
```

#### Initialize BASIC

**Syntax:** `new`

**Behavior:** Reinitializes user memory by running the BASIC warm-start process.

```{warning}
`new` resets the current BASIC user-memory state. Save anything you need before
running it.
```

```{example}
`new`
```

#### Poke memory

**Syntax:** `p address value`

**Behavior:** Writes the given byte value to an address.

```{example}
`p $900f $08`
```

#### Registers

**Syntax:** `r`

**Behavior:** Displays the current simulated 6502 register values. It also sets the
monitor's default address to the current program counter for subsequent `d`,
`dump`, or `m` commands.

```{example}
`r`
```

#### Save memory

**Syntax:** `s start end filename`

**Behavior:** Saves the half-open range `[start-address, end-address)` to the given file.

```{example}
`s $1000 $2000 memory.bin`
```

#### Step over

**Syntax:** `n`

**Behavior:** Runs the next instruction and returns to the monitor. A `JSR` and the called
subroutine are treated as a single instruction.

```{example}
`n`
```

#### Trace

**Syntax:** `t`

**Behavior:** Continues execution with instruction tracing enabled.

```{example}
`t`
```

#### List watches

**Syntax:** `w`

**Behavior:** Lists every active watch, including the ID used by the `wr` command.

```{example}
`w`
```

#### Add watch

**Syntax:** `wa start [end]`

**Behavior:** Adds a watch that triggers when the selected address or range is either read
from or written to.

```{example}
`wa player_x player_y`
```

#### Add load watch

**Syntax:** `wal start [end]`

**Behavior:** Adds a watch that triggers only when the selected address or range is read.

```{example}
`wal $1000 $10ff`
```

#### Add store watch

**Syntax:** `was start [end]`

**Behavior:** Adds a watch that triggers only when the selected address or range is written
to.

```{example}
`was score score+2`
```

#### Remove watch

**Syntax:** `wr id`

**Behavior:** Removes the watch with the given ID. Use `w` to list watch IDs.

```{example}
`wr 1`
```

#### Quit

**Syntax:** `x`

**Behavior:** Exits the monitor and returns to the editor or source view. The monitor window
remains onscreen until it is closed with {c64-keys}`C= + Q`.  Because that key
must be pressed while the window has focus, re-enter the monitor
({c64-key}`F7` or {c64-keys}`C= + W`) and press it there to close the window.

```{example}
`x`
```

#### Step

**Syntax:** `z`

**Behavior:** Runs the next instruction and returns to the monitor, displaying the updated
registers and next instruction.

```{example}
`z`
```

#### Step out

**Syntax:** `zo`

**Behavior:** Runs until the current subroutine returns with `RTS`, then displays the updated
registers and next instruction.

```{example}
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
