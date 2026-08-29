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

The below table enumerates the available monitor commands and their parameters (plus optional ones).
Most parameters may be expressions (e.g. `label+10`).

| COMMAND    |  NAME                     |  REQUIRED PARAMETERS               | OPTIONAL PARAMETERS  |                      DESCRIPTION                                                                                              |
|------------|---------------------------|------------------------------------|----------------------|-------------------------------------------------------------------------------------------------------------------------------|
|  `  a     `| `ASSEMBLE              `  | instruction                        |                      | assembles the given instruction at the address of the provided expression                                                     |
|  `  b     `| `LIST BREAKPOINTS      `  |                                    |                      | lists all the breakpoints that have been added                                                                                |
|  `  ba    `| `ADD BREAKPOINT @ ADDR `  | address                            |                      | adds a breakpoint at the given address                                                                                        |
|  `  bl    `| `ADD BREAKPOINT @ LINE `  | file id, line number               |                      | adds a breakpoint at the given line number within the given file                                                              |
|  `  br    `| `REMOVE BREAKPOINT     `  | breakpoint id                      |                      | removes the given breakpoint                                                                                                  |
|  `  bt    `| `BACKTRACE             `  |                                    | offset               | displays a rendered stack view beginning at the current SP + an optional offset, up to $200 (bottom of stack)                 |
|  `  c     `| `COMPARE               `  | address 1, address 2, number       |                      | compares the given number of bytes at the two given addresses and displays any discrepancies                                  |
|  `clear   `| `CLEAR                 `  |                                    |                      | clears the monitor and sets the cursor back to the origin row/column                                                          |
|  `  d     `| `DISASSEMBLE           `  | start-address                      | end-address          | disassembles from the given start address or, if an end address is given, up to the given end address                         |
|  `dump    `| `DUMP MEMORY           `  | start-address                      | end-address          | prints a list of bytes (in valid assembleable .DB directives) for the given address range                                     |
|  `  f     `| `FILL MEMORY           `  | start-address, stop-address, value | value list           | prints all filenames loaded into the current assembly state                                                                   |
|  ` files  `| `SHOW FILES            `  |                                    |                      | fills the given address range with the given list of values, repeating when the list is exhausted                             |
|  `  g     `| `GO                    `  |                                    |                      | continues execution of the debugged program without tracing                                                                   |
|  `  h     `| `HUNT                  `  | start-address value                | value list           | hunts for the given list of values beginning at the provided address up to $ffff                                              |
|  `  m     `| `SHOW MEMORY           `  | start-address                      | end-address          | displays the contents of memory from the given start address or, if given, up to the given end address                        |
|  `  move  `| `MOVE MEMORY           `  | start-address, end-address, dest   |                      | moves the given range [start, end) to the given destination address                                                           |
|  `  new   `| `INITIALIZE BASIC      `  |                                    |                      | initializes the user memory by re-running the BASIC warm-start process                                                        |
|  `  p     `| `POKE MEMORY           `  | address value                      |                      | sets the given address to the provided value                                                                                  |
|  `  r     `| `REGISTERS             `  |                                    |                      | displays the current content of the registers in the debugged program                                                         |
|  `  s     `| `SAVE MEMORY           `  |start-address, end-address, filename|                      | saves the given memory range to the specified file                                                                            |
|  `  n     `| `STEP OVER             `  |                                    |                      | runs the next instruction, treating JSRs as a single instruction                                                              |
|  `  t     `| `TRACE                 `  |                                    |                      | continue tracing the program that is being debugged                                                                           |
|  `  w     `| `LIST WATCHES          `  |                                    |                      | lists the watches that have been added                                                                                        |
|  `  wa    `| `ADD WATCH             `  | start-address                      | end-address          | adds a watch at the given start and (optional) stop address                                                                   |
|  `  wal   `| `ADD WATCH (LOAD)      `  | start-address                      | end-address          | adds a LOAD watch at the given start and (optional) stop address                                                              |
|  `  was   `| `ADD WATCH (STORE)     `  | start-address                      | end-address          | adds a STORE watch at the given start and (optional) stop address                                                             |
|  `  wr    `| `REMOVE WATCH          `  | id                                 |                      | removes the watch with the given id                                                                                           |
|  `  x     `| `QUIT                  `  |                                    |                      | exits the monitor                                                                                                             |
|  `  z     `| `STEP                  `  |                                    |                      | runs the next instruction and returns to the monitor prompt                                                                   |
|  `  zo    `| `STEP-OUT              `  |                                    |                      | runs the program that is being debugged until the current subroutine is RTS'd from                                            |
| {c64-key}`F1` | `VIEW SCREEN           `  |                                    |                      | toggles the view of the user-memory (swaps the $1000-$2000 range monitor <-> program)                                      |
| {c64-key}`F2` | `ENTER USER PROGRAM    `  |                                    |                      | enters the actively running program (or the default KERNAL BASIC interpreter if nothing has been debugged yet)             |
| {c64-keys}`C= + L` | `CLEAR                 `  |                                    |                      | shortcut to clear the screen (equivalent to the clear command)                                                     |
| {c64-keys}`C= + K` | `GROW WINDOW           `  |                                    |                      | grows the monitor window by one row (windowed mode only)                                                           |
| {c64-keys}`C= + J` | `SHRINK WINDOW         `  |                                    |                      | shrinks the monitor window by one row (windowed mode only)                                                         |
