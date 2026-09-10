## Monitor

The monitor is a text-based interface for debugging programs and manipulating
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

````{example}
With `SCREEN_H` defined as 24:

```text
$? (2.0*SCREEN_H)
48
$? 2*24
$0030
```

Floating-point results are printed in decimal; integer results are printed as four hexadecimal digits.
````

#### Assemble

**Syntax:** `a address instruction`

**Behavior:** Assembles the instruction at the address given by the expression. After a
successful assembly, the monitor prepares another `a` command at the address
immediately following the new instruction.

````{example}
Enter the first instruction, then complete each prepared command with the next instruction.

```text
$a $2000 lda #$00
$a $2002 sta $900f
$a $2005 rts
$a $2006
```

The monitor begins each new line with the next address. Press RETURN without an instruction at `$2006` to finish.
````

#### List breakpoints

**Syntax:** `b`

**Behavior:** Lists every active breakpoint, including the ID used by the `br` command.

````{example}
With two enabled source breakpoints:

```text
$b
* 00 GAME.S L:120 [MAIN] $2000
* 01 GAME.S L:148 [DRAW] $2040
```

IDs are hexadecimal. Here `*` represents the enabled-breakpoint icon shown by the monitor.
````

#### Add breakpoint at address

**Syntax:** `ba address`

**Behavior:** Adds a breakpoint at the given address. If debug information maps the address
to a source line, the breakpoint is associated with that line as well.

````{example}
With no existing breakpoints, `main=$2000`, and `$2003` mapped to line 121 of `game.s`:

```text
$ba main+3
$b
* 00 GAME.S L:121 [MAIN] $2003
```

Adding the breakpoint is silent; the `b` command displays it (`*` is the enabled-breakpoint icon).
````

#### Add breakpoint at line

**Syntax:** `bl filename line`

**Behavior:** Adds a breakpoint at the given line in a file loaded with the current debug
information.

````{example}
With no existing breakpoints and line 120 of `game.s` mapped to `main` at `$2000`:

```text
$bl game.s 120
$b
* 00 GAME.S L:120 [MAIN] $2000
```

The following `b` confirms the addition
````

#### Remove breakpoint

**Syntax:** `br id`

**Behavior:** Removes the breakpoint with the given ID. Use `b` to list breakpoint IDs.

````{example}
With breakpoint 00 as the only breakpoint:

```text
$br 0
$b
$
```

Removal is silent. If no breakpoints left, `b` prints nothing
````

#### Backtrace

**Syntax:** `bt [offset]`

**Behavior:** Displays a rendered view of the call stack, beginning just above the current
stack pointer. The optional offset adjusts the starting position and must be
less than `$80`. Stack contents are inferred, so data stored on the stack may
appear as an invalid frame.

````{example}
Suppose SP is `$f3`, `draw=$2040`, and `main=$2000`. The stack contains saved return addresses `$2048` at `$01fc` and `$2007` at `$01fe`.

```text
$bt 8
$FC $2046 DRAW+$0006
$FE $2005 MAIN+$0005
```

The offset skips eight stack bytes. Each row shows the stack offset, the inferred JSR address, and its nearest symbol plus offset.
````

#### Compare

**Syntax:** `c address1 address2 count`

**Behavior:** Compares `count` bytes beginning at the two addresses and displays each pair
that differs.

````{example}
```text
$f $2000 $2020 $00
$f $2100 $2120 $00
$p $2103 $ff
$p $2110 $80
$c $2000 $2100 $20
2003 2103 $00 $FF
2010 2110 $00 $80
```

Each output row gives the two addresses followed by their differing byte values. Equal bytes produce no output.
````

#### Clear

**Syntax:** `clear`

**Behavior:** Clears the monitor and returns the cursor to the origin.
Pressing {c64-keys}`C= + L` performs the same action.

````{example}
```text
$clear
```

The display is cleared and a new `$` prompt appears at the top of the monitor.
````

#### Disassemble

**Syntax:** `d [start [end]]`

**Behavior:** Disassembles memory beginning at `start-address`. If no end address is given,
the command disassembles at least `$10` bytes. If no start address is given,
disassembly continues from the monitor's current default address.

````{example}
With bytes `$a9,$00,$8d,$0f,$90,$e8,$d0,$fd` at `$2000`:

```text
$d $2000 $2008
$2000 LDA #$00
$2002 STA $900F
$2005 INX
$2006 BNE $2005
```

Addresses are shown on screen. When disassembly is redirected to a file, the address column is omitted.
````

#### Dump memory

**Syntax:** `dump [start [end]]`

**Behavior:** Renders the selected memory as assembleable `.db` directives. If no end
address is given, the command dumps `$40` bytes. If no start address is given,
the dump begins at the monitor's current default address. This command is
particularly useful with [file redirection](#file-redirection).

````{example}
Suppose `$1000` contains the eight sprite bytes shown below.

```text
$dump $1000 $1008 > data.s
```

The command writes this line to `data.s`, rather than displaying it:

```text
.DB $18,$3C,$7E,$FF,$FF,$7E,$3C,$18
```
````

#### Fill memory

**Syntax:** `f start end value [, value ...]`

**Behavior:** Fills the half-open range `[start-address, end-address)` with the given values.
When more than one value is supplied, the sequence repeats until the range is
full.

````{example}
```text
$f $1000 $1010 $00, $ff
$m $1000 $1010
1000:  00 FF 00 FF 00 FF 00 FF ........
1008:  00 FF 00 FF 00 FF 00 FF ........
```

The fill itself is silent; `m` verifies the repeating pattern.
````

#### Show files

**Syntax:** `files`

**Behavior:** Lists every source file loaded in the current debug information.

````{example}
With debug information loaded for these two source files:

```text
$files
GAME.S
SPRITES.S
```
````

#### Go

**Syntax:** `g [address]`

**Behavior:** Continues execution without tracing. If an address is supplied, it becomes the
new program counter before execution begins.

````{example}
```text
$g main
```

Execution resumes at `main`. The command does not print a success message; press RESTORE to interrupt the running program.
````

#### Hunt

**Syntax:** `h start value [, value ...]`

**Behavior:** Searches from `start-address` through `$ffff` for the first occurrence of the
given sequence and displays its address.

````{example}
```text
$f $2000 $2020 $00
$f $2010 $2014 $de,$ad,$be,$ef
$h $2000 $de,$ad,$be,$ef
$2010
```

Only the first matching address is printed. If no match is found through `$ffff`, the command returns without printing an address.
````

#### Show memory

**Syntax:** `m [start [end]]`

**Behavior:** Displays memory beginning at `start-address`. If no end address is given, the
command displays `$40` bytes. If no start address is given, display continues
from the monitor's current default address.

````{example}
With `message=$2200` and `HELLO, MONSTER!` followed by a zero byte at that address:

```text
$m message message+$10
2200:  48 45 4C 4C 4F 2C 20 4D HELLO, M
2208:  4F 4E 53 54 45 52 21 00 ONSTER!.
```

Each row shows an address, eight hexadecimal bytes, and their character equivalents. Bytes outside `$20`–`$7f` are shown as dots.
````

#### Move memory

**Syntax:** `move start end destination`

**Behavior:** Copies the half-open range `[start-address, end-address)` to `destination`.

````{example}
```text
$f $2000 $2010 $01,$02,$03,$04
$move $2000 $2010 $2100
$m $2100 $2110
2100:  01 02 03 04 01 02 03 04 ........
2108:  01 02 03 04 01 02 03 04 ........
```

The copy is silent; here `m` displays the destination post-command.
````

#### Initialize BASIC

**Syntax:** `new`

**Behavior:** Reinitializes user memory by running the BASIC warm-start process.

```{warning}
`new` resets the current BASIC user-memory state. Save anything you need before
running it.
```

````{example}
```text
$new
```

There is no textual result. The simulated BASIC user-memory state is reinitialized.
````

#### Poke memory

**Syntax:** `p address value`

**Behavior:** Writes the given byte value to an address.

````{example}
Assuming the surrounding bytes are zero:

```text
$p $00fb $2a
$m $00f8 $0100
00F8:  00 00 00 2A 00 00 00 00 ...*....
```

The write is silent; `m` confirms that `$00fb` now contains `$2a`.
````

#### Registers

**Syntax:** `r`

**Behavior:** Displays the current simulated 6502 register values. It also sets the
monitor's default address to the current program counter for subsequent `d`,
`dump`, or `m` commands.

````{example}
For a program paused at `$2000` with A=`$08`, X=`$03`, Y=`$00`, SP=`$ff`, status=`$24`, and the cycle counter at zero:

```text
$r
 PC  A  X  Y  SP NV-BDIZC ADDR      CLK
2000 08 03 00 FF 00 00100 ----         0
```

The `addr` field is `----` when the last instruction did not access data memory. If the cycle count is invalid it is shown as `???`.
````

#### Save memory

**Syntax:** `s start end filename`

**Behavior:** Saves the half-open range `[start-address, end-address)` to the given file.

````{example}
```text
$s $1000 $2000 memory.bin
```

On success, the prompt returns without a confirmation message. The file contains the selected memory range.
````

#### Step over

**Syntax:** `n`

**Behavior:** Runs the next instruction and returns to the monitor. A `JSR` and the called
subroutine are treated as a single instruction.

````{example}
Suppose `$2000` contains `jsr $2010`, followed by `sta $900f` at `$2003`. The subroutine contains `inx` and `rts`. Initially A=`$08`, X=`$03`, Y=`$00`, SP=`$ff`, status=`$24`, and the cycle count is zero.

```text
$n
TRACING.. PRESS [RESTORE] TO STOP
 PC  A  X  Y  SP NV-BDIZC ADDR      CLK
2003 08 04 00 FF 00 00100 01FF        14
STA $900F
```

The subroutine returns before the register display. The last stack read was at `$01ff`; the final line is the next instruction, which has not yet executed.
````

#### Trace

**Syntax:** `t`

**Behavior:** Continues execution with instruction tracing enabled.

````{example}
For a loop containing `inx` at `$2000` and `jmp $2000` at `$2001`, this is one possible result after pressing RESTORE. In this example A=`$08`, Y=`$00`, SP=`$ff`, status=`$24`

```text
$t
 PC  A  X  Y  SP NV-BDIZC ADDR      CLK
2001 08 04 00 FF 00 00100 ----      ???
JMP $2000
```

The state is shown as is when the trace is interrupted.
````

#### List watches

**Syntax:** `w`

**Behavior:** Lists every active watch, including the ID used by the `wr` command.

````{example}
With an unchanged load-and-store watch at `$00fb` (value `$20`) and a load watch covering `$1000`–`$10ff`:

```text
$w
00  $00FB: 20 LOAD/STORE
01  $1000-$10FF LOAD
```

IDs and byte values are hexadecimal. The mode suffix is `load` for loads, `store` for
stores, or `load/store` for both. A watch marked as changed has `!` after its ID.
````

#### Add watch

**Syntax:** `wa start [end]`

**Behavior:** Adds a watch that triggers when the selected address or range is either read
from or written to.

````{example}
With no existing watches, `player_x=$00fb`, and `player_y=$00fc`:

```text
$wa player_x player_y
$w
00  $00FB-$00FC LOAD/STORE
```

Adding the watch is silent; `w` displays the newly watched range (note "LOAD/STORE", meaning the watch is triggered on any access
````

Only one watch can cover an identical address range, regardless of its mode.
Adding the same address or range again reports `WATCH ALREADY EXISTS` and
leaves the existing watch unchanged. To change its mode, delete the existing watch first.

````{example}
Starting with no watches and `$20` stored at `$00fb`:

```text
$wal $00fb
$was $00fb
WATCH ALREADY EXISTS
$w
00  $00FB: 20 LOAD
$wr 0
$was $00fb
$w
00  $00FB: 20 STORE
```
````

#### Add load watch

**Syntax:** `wal start [end]`

**Behavior:** Adds a watch that triggers only when the selected address or range is read.

````{example}
With no existing watches:

```text
$wal $1000 $10ff
$w
00  $1000-$10FF LOAD
```

The `load` suffix confirms that this watch triggers only on loads.
````

#### Add store watch

**Syntax:** `was start [end]`

**Behavior:** Adds a watch that triggers only when the selected address or range is written
to.

````{example}
With no existing watches and `score=$00fd`:

```text
$was score score+2
$w
00  $00FD-$00FF STORE
```

The watch covers the three score bytes, including the ending address.
````

#### Remove watch

**Syntax:** `wr id`

**Behavior:** Removes the watch with the given ID. Use `w` to list watch IDs.

Removing a watch renumbers the remaining watches. List them again before
deleting another. You can also delete a watch in the graphical watch viewer by pressing
**DEL**.

````{example}
With watch 00 as the only watch:

```text
$wr 0
$w
$
```

Removal is silent. With no watches left, `w` prints nothing and the prompt returns.
````

#### Quit

**Syntax:** `x`

**Behavior:** Exits the monitor and returns to the editor or source view. The monitor window
remains onscreen until it is closed with {c64-keys}`C= + Q`.  Because that key
must be pressed while the window has focus, re-enter the monitor
({c64-key}`F7` or {c64-keys}`C= + W`) and press it there to close the window.

````{example}
```text
$x
```

Focus returns to the editor or source view; the command prints no confirmation.
````

#### Step

**Syntax:** `z`

**Behavior:** Runs the next instruction and returns to the monitor, displaying the updated
registers and next instruction.

````{example}
Suppose `$2000` contains `inx` and `$2001` contains `sta $900f`. Initially A=`$08`, X=`$03`, Y=`$00`, SP=`$ff`, status=`$24`, and the cycle count is zero.

```text
$z
 PC  A  X  Y  SP NV-BDIZC ADDR      CLK
2001 08 04 00 FF 00 00100 ----         2
STA $900F
```

X increases to `$04`, PC advances by one byte, and the cycle count increases by two. The printed `sta` is the next instruction.
````

#### Step out

**Syntax:** `zo`

**Behavior:** Runs until the current subroutine returns with `RTS`, then displays the updated
registers and next instruction.

````{example}
Suppose execution is paused at an `inx` followed by `rts` inside a subroutine. The saved return address is `$2002`, so execution resumes at `sta $900f` at `$2003`. Initially A=`$08`, X=`$03`, Y=`$00`, SP=`$fd`, status=`$24`, and the cycle count is zero.

```text
$zo
TRACING.. PRESS [RESTORE] TO STOP
 PC  A  X  Y  SP NV-BDIZC ADDR      CLK
2003 08 04 00 FF 00 00100 01FF         8
STA $900F
```

The two instructions take eight cycles, and the return restores SP to `$ff`.
````

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
