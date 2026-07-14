## MONITOR OVERVIEW

The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.

---

### ACTIVATION

The monitor is activated with the `F7` key. It can be activated from the editor both during
normal editing and while debugging.
If activated while debugging, a number of additional commands related to the state of the debugged program
become available.

### WINDOWED MODE

The monitor may also be activated as a window that takes up only the bottom portion of the screen
with the `F8` (`SHIFT + F7`) key.  In this mode the editor remains visible above the monitor window.
The window may be resized with `C= + +` (grow) and `C= + -` (shrink) while the monitor is active.

When the monitor is quit (the `x` command), the window is left onscreen (as with other GUI windows).
Pressing `F7` or `F8` while the window is open re-activates it in place.  The monitor window is
closed, just as other windows, are with the `<-` key.

While debugging, `F7`/`F8` open the monitor as a window over the bottom of the debug view (covering
the register/state display), leaving the source view visible above it.  The window may be resized as
usual.  When the monitor is quit, the window is dismissed and the debug view is redrawn in its place.

### FILE REDIRECTION
The output from a given monitor command can be redirected to file instead of the screen by using the
redirect (`>`) operator.  When placed at the end of a command, the redirect operator writes all output from
that command to the following file.

For example:

`r > regs.txt`

Will write the contents of the registers to the file `regs.txt`

### COMMANDS

The below table enumerates the available monitor commands and their parameters (plus optional ones).
Most parameters may be expressions (e.g. `label+10`).

| command  |  name                   |  required parameters               | optional parameters  |                      description                                                                                              |
|----------|-------------------------|------------------------------------|----------------------|-------------------------------------------------------------------------------------------------------------------------------|
|    a     | assemble                |  instruction                       |                      | assembles the given instruction at the address of the provided expression                                                     |
|    b     | list breakpoints        |                                    |                      | lists all the breakpoints that have been added                                                                                |
|    ba    | add breakpoint @ addr   |  address                           |                      | adds a breakpoint at the given address                                                                                        |
|    bl    | add breakpoint @ line   |  file id, line number              |                      | adds a breakpoint at the given line number within the given file                                                              |
|    br    | remove breakpoint       | breakpoint id                      |                      | removes the given breakpoint                                                                                                  |
|    bt    | backtrace               |                                    | offset               | displays a rendered stack view beginning at the current SP + an optioinal offset, up to $200 (bottom of stack)                |
|    c     | compare                 |  address 1, address 2, number      |                      | compares the given number of bytes at the two given addresses and displays any discrepancies                                  |
|  clear   | clear                   |                                    |                      | clears the monitor and sets the cursor back to the origin row/column                                                          |
|    d     | disassemble             |  start-address                     | end-address          | disassembles from the given start address or, if an end address is given, up to the the given end address                     |
|  dump    | dump memory             |  start-address                     | end-address          | prints a list of bytes (in valid assembleable .DB directives) for the given address range                                     |
|    f     | fill memory             |  start-address, stop-address, value| value list           | fills the given address range with the given list of values, repeating when the list is exhausted                             |
|    g     | go                      |                                    |                      | continues execution of the debugged program without tracing                                                                   |
|    h     | hunt                    |  start-address value               | value list           | hunts for the given list of values beginning at the provided address up to $ffff                                              |
|    m     | show memory             |  start-address                     | end-address          | displays the contents of memory from the given start address or, if given, up to the given end address                        |
|    move  | move memory             |  start-address, end-address, dest  |                      | moves the given range [start, end) to the given destination address                                                           |
|    new   | initialize BASIC        |                                    |                      | initializes the user memory by re-running the BASIC warm-start process                                                        |
|    p     | poke memory             |  address value                     |                      | sets the given address to the provided value                                                                                  |
|    r     | registers               |                                    |                      | displays the current content of the registers in the debugged program                                                         |
|    s     | save memory             |start-address, end-address, filename|                      | saves the given memory range to the specified file                                                                            |
|    zo    | step over               |                                    |                      | runs the next instruction, treating JSR's as a single instruction                                                             |
|    t     | trace                   |                                    |                      | continue tracing the program that is being debugged                                                                           |
|    w     | list watches            |                                    |                      | lists the watches that have been added                                                                                        |
|    wa    | add watch               | start-address                      | end-address          | adds a watch at the given start and (optional) stop address                                                                   |
|    wal   | add watch (load)        | start-address                      | end-address          | adds a LOAD watch at the given start and (optional) stop address                                                              |
|    was   | add watch (store)       | start-address                      | end-address          | adds a STORE watch at the given start and (optional) stop address                                                             |
|    wr    | remove watch            | id                                 |                      | removes the watch with the given id                                                                                           |
|    x     | quit                    |                                    |                      | exits the monitor                                                                                                             |
|    z     | step                    |                                    |                      | runs the next instrcution and returns to the the monitor prompt                                                               |
|    zo    | step-out                |                                    |                      | runs the program that is being debugged until the current subroutine is RTS'd from                                            |
|    F1    | view screen             |                                    |                      | toggles the view of the user-memory (swaps the the  $1000-$2000 range monitor <-> program                                     |
|    F2    | enter BASIC             |                                    |                      | drops into the stock KERNAL BASIC interpreter                                                                                 |
|    C= + l| clear                   |                                    |                      | shortcut to clear the screen (equivalent to the clear command)                                                                |
|    C= + +| grow window             |                                    |                      | grows the monitor window by one row (windowed mode only)                                                                      |
|    C= + -| shrink window           |                                    |                      | shrinks the monitor window by one row (windowed mode only)                                                                    |
