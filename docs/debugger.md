## DEBUGGER OVERVIEW

---

https://github.com/gummyworm/monster/assets/4626914/840f5d66-03cb-4daf-9ed2-41a4d37d4c2d

The debugger allows you to step through code, set breakpoints, and watch
data as you execute your program.

Upon entering the debugger, a view of the system state is displayed at the
current step or breakpoint.

This includes the state of the registers (A, X, Y, P, SP, and PC) as well as
any effective address that was calculated for reading/writing by the last
instruction.  Note that if the last instruction executed did not read or write to memory,
the effective address field is set to $ffff.

While debugging, most navigation commands work as normal. Breakpoints may
be set as they would in the editor prior to assembly, and they will be installed
in realtime.  Other edits are not allowed, however, while the debugger is active.

Both the debugger and the user program's RAM is saved/restored when control
transfers between the two. That is low memory ($00-$4000), the screen data ($1000-$2000), the zeropage,
I/O state (VIC and VIA's), and color RAM.  This allows the debugger and debugged program
to operate independently without worrying about writes to one affecting the other.

---

## REQUIREMENTS
In order for the debugger to coexist with your program there are a few small requirements.

#### STACK HAS 6 BYTES FREE

The stack, at its current location for a given step, must have 3 bytes free.
If your program uses an IRQ (correctly) this shouldn't be an issue because the Vic-20's
IRQ sequence also pushes 6 bytes (the registers, including status, plus the interrupt return address).

#### DON'T USE $9800-$9FFF

The memory above $9800-$9fff is reserved for the debugger.  Avoid it.
Most of this range ($9800-$9FF0) is made read-only before your program gains control, so it is unlikely you will
actually crash the system by accessing this range.  Nonetheless, your program will not work as expected if you
try to read and write to this area.  Writing to the range $9FF0-$9FFF is _especially_ dangerous though, and you
can expect a system JAM if you access it in your program.

### DON'T OVERWRITE BRK/NMI VECTORS

During free-run, the NMI vector (via RESTORE) is used to return to the debugger during
execution of your program.  If you overwrite this, the debugger will not be able to take control back when the
RESTORE NMI occurs.

The BRK vector is used to return to the debugger when a breakpoint is encountered.
If your program has its own idea of how to handle breakpoints, it may overwrite the BRK
vector, but the debugger will be unable to handle them as a result.

---

### DEBUG COMMANDS

The following commands are supported by the debugger and are accessed by their
respective Key in the table below.

|  key         | name            |   description                                                                        |
|--------------|-----------------|--------------------------------------------------------------------------------------|
|  F1          | Source View     | maximizes the screen area for viewing the source code                                |
|  F2          | Register Editor | enters the register editor                                                           |
|  F3          | Mem View        | activates the memory window, which takes control until `<-` is pressed               |
|  F5          | Break View      | displays the breakpoints that have been set and allows them to be enabled/disabled   |
|  s           | Step Over      | steps to the next instruction. If it is a JSR, continues AFTER the target subroutine |
|  y           | Step Out        | steps until the next RTS instruction                                                 |
|  z           | Step            | steps to the next instruction.                                                       |
|  t           | Trace           | like GO but the debugger takes control between each instruction                      |
|  C= + g      | Go              | begins execution at the cursor                                                       |
|  C= + p      | Jump to         | sets the PC to the address corresponding to the line the cursor is on                |
|  C= + r      | Reset Stopwatch | resets the value of the stopwatch to 0                                               |
| C= + t       | Enter monitor   | enters the text-based debug interface (see the monitor commands section for more info|
| C= + x       | Quit Debugger   | Prompts the user for confirmation then quits the debugger upon receiving it          |
|   <-         | Exit            | exits the debugger and returns to the editor                                         |
| SPACE        | Swap prog       | swaps in the internal memory for the user program (allows user to see screen state)  |
| ^ (up arrow) |  Goto Break     | navigates to the address that the debugger is currently paused at                    |

### REGISTER EDITOR (`F2`)

Pressing F2 moves the cursor to the register contents and allows the user to enter
new values for them.  Pressing `RETURN` will confirm the new register values
and update them to those values the next time the user program is invoked.
Pressing `<-` will abort this process and leave the old register values
intact.

### STOPWATCH

Next to the registers, under the CLK label, is a 24-bit counter that displays the
number of cycles executed by the instructions that have been STEP'd into.
The stopwatch can be reset to 0 with the `C= + r` key combination.

Note that the number of cylces is displayed in decimal unlike the rest of the
information in the debug view, which is displayed in hexadecimal.

---

### STEPPING THROUGH CODE

There are a variety of ways to execute the program that allow us to gather
quite a lot of information about the instructions we executed.  The debugger
also contains a 6502 simulator.  This simulato knows what registers an
instruction uses/modifies, the effective address that is read/written, and mode.

How does this help us, the user?  For example, when an instruction affects a given register,
that register is highlighted in the debugger *even if the register
value hasn't changed*. We can also activate a watch even if we don't store a new value to it.
We can even activate a watch when a value is loaded from the watched address.

The simulator also counts cycles, allowing us to keep track of how many have elapsed
since the program began or the stopwatch was reset.

#### STEP INTO (`z`)

Stepping _into_ code will return to the debugger
after the next instruction (the one currently highlighted if we have debug
information) is executed.

#### STEP OVER (`s`)

Step _over_ behaves the same as step _into_, but if the next
instruction is a subroutine call (`JSR`), execution continues until the
instruction _after_ the `JSR` (after the subroutine returns).

#### STEP OUT (`y`)

The step out command traces the program until the next RTS instruction.
The RTS instruction is run and control then returns to the debugger.

#### TRACE (`t`)

Trace executes the program as a series of STEPs until the user indicates we
should halt the trace by pressing the `RESTORE` key.

### Free Run (GO) (`C= + g`)

The `GO` command begins execution and returns to the debugger only when a
breakpoint is encountered or when RUN/STOP is pressed.  Unlike any of the step/trace
commands, Go will _not_ simulate anything.  Control is given entirely over
to the user program.  This could be dangerous, but is likely necessary in many
cases.  A nearly finished game, for example, will require the user gives over
control to the program in order to play that game.
That said, take caution when using this command and **expect to lose any unsaved state**

#### NOTES ON MEMORY SWAPPING

While the debugger and user program have isolated memory banks in the address space
above `$1fff`, the RAM _below_ this, `[$00, $2000)`, is internal to the Vic-20
and cannot be swapped out between debug steps. The debugger has no choice but to
share this address space with the user program as it is also the only RAM
that is visible to the video chip (the VIC-I).  It also contains the stack and
zeropage, which we _could_ avoid, but as long as we need to use the $10th-$20th pages
of RAM, it makes sense to handle them in the same way.

To handle this, the debugger calculates the bytes that need to be saved in
between steps and saves these values in between calls to the program.  Values
that will be used by the user program are then swapped in so that the program
behaves as if the debugger is not running.
The full internal state of the user program and debugger occupy buffers in the
debugger and are available to be swapped in/out on command with the `C= + SPACE`
key combination.  This is useful if you'd like to see what the internal RAM
state, which is the _only_ place that the screen state may live, looks like
at the current step in the program.

If we aren't stepping _into_ code in RAM (_go_, _step over_) we are unable
to calculate the addresses that will be affected when we
hand over control to the user program, we instead save the _entire_ *debugger* state of
the internal RAM and restore the _entire_ *user* state.
Although this is a rather large amount of memory, it is mitigated by being
handled by a mostly unrolled loop and therefore takes only a fraction of a second to occur.
Nonetheless, it is apparent when this is happening if you've changed the setup
of the VIC registers, or anything in the VIC's visible address range, as the
screen will briefly flash with the state of the user program.

---

## AUXILIARY VIEWS

Within the debugger, there are 3 auxiliary views that may be activated with the
function keys.  Each shows information about the machine or debug state.
Each viewer also contains an editor, which is activated with the keys enumerated
below next to their corresponding editor.

Pressing the `<-` key will return the user from the auxiliary editor to the
source code editor.  And `F1` will hide the active view to maximize the
source editor's screen size.

### MEMORY VIEWER (`F3` WHILE DEBUGGING)

The memory viewer displays the contents of RAM at a given address.  The memory
viewer is updated upon reentry to the debugger (if active).
Memory values may be updated by navigating to the value the user wishes to
change and overwriting it with a new hex value. The change occurs immediately.

In addition to hexadecimal keys to edit memory values, the following commands
are supported within the memory viewer:

| shortcut     | name      |  description                                            |
|--------------|-----------|---------------------------------------------------------|
| C= + w       | Add watch | Add watch to the highlighted address                    |
|    /         | Find Value| Seeks from current memory address for given value       |
|   <-         |  Exit     | Returns to the debugger                                 |
| ^ (up-arrow) | Set Addr  | Sets the viewer's address to the given value            |

#### SET WATCH (`C= + W`)

Watches may be placed while navigating in the memory editor.  This is done
by pressing the `C= + w` key-combination while the cursor is on the desired
byte to watch. See the _Watch Viewer_ section for more information on
watches.

#### FIND VALUE (`/`)

Prompts the user for an 8 or 16 bit value (determined by the number of
characters provided) and looks for that value in memory.
If it is found, the memory view is updated to begin at the first address
that was found containing the specified value.

Note that when seeking for a 16 bit value, the value is searched in little-endian
format.  If the input for the search is given as `$1234` the result will be
the first occurrence of the byte value `$34` followed by `$12`.

#### SET ADDRESS (`^`/`UP-ARROW`)

Moves the cursor to the address of the viewer, then prompts the user for a new
value to set the memory viewer to.  Pressing `RETURN` confirms the new address
and `<-` cancels and returns the user to the editor without changing the address

### BREAKPOINT VIEWER (`F5` WHILE DEBUGGING)

The breakpoint viewer displays all the breakpoints that have been set by the
user.  A circle is displayed next to those that are currently active.
The user simply navigates the list with the cursor keys and presses RETURN to
toggle those which he/she wishes to enable/disable.

Note that breakpoints correspond to the debug information generated with
the F4 command.  If the line numbers change after this information is generated,
breakpoints are unlikely to behave in expected ways.

### WATCH VIEWER (`F7` WHILE DEBUGGING)

The watch viewer displays all watches that have been set in the memory
viewer.  The current value of a watch is shown along with its previous
value (if it has changed since the debugger last took over).

A watched address (or range) will also be prefixed with a '!' if it was modified
during the trace or step.  This is especially important for knowing that a range
was modified as ranges do not list the previous or current values for the watch.

The following keys are supported within the watch viewer:

| shortcut     | name       |  description                                            |
|--------------|------------|---------------------------------------------------------|
| C= + w       | Add watch  | Prompt the user for expressions to watch                |
|  RETURN      | Select/Edit| Enters the memory editor at the watch's address         |
|   <-         |  Exit      | Returns to the debugger                                 |

#### ADD WATCH (`C= + W`)

While in the watch editor, the `C= + w` key combination prompts the user for an
address or address range to watch.  These are given as expressions, so you may
provide, for example `myval+3` to set a watch at the address of the label myval plus 3.
To set a watch for an address range, simply provide two expressions, separated by a comma,
at the prompt.  If the expression(s) are invalid, no watch is added.

#### EDIT WATCH (`RETURN`)

Pressing RETURN will invoke the _memory editor_ at the location of the watch
that was selected.  Returning from the memory editor will return the user
back to the watch editor.

---

## BREAKPOINTS

Breakpoints may be set/removed during both normal editing and while debugging.
Setting a breakpoint inserts a special character into the source buffer, which
tells the assembler to generate a breakpoint for the line that this character
resides on.

Because the breakpoint is represented as a character within the source code itself,
it will automatically move as lines are inserted and deleted.  The character itself
is not editable (the cursor will not move to breakpoint characters).  You may remove
it by toggling the breakpoint off _or_ by deleting the entire line.

*NOTE:* Debug information is only generated for instructions _NOT data_.  This means
that, for example, you can set a breakpoint on `LDA #$00` or a macro that expands
to such an instruction, but setting one on `.DB $00` has no effect.

### TOGGLE BREAKPOINT (`C= + B`)
During normal editing, breakpoints may be set and removed  with the `C= + b` key combination.

Pressing the same key combination (`C= + b`) will also remove a breakpoint
if it is pressed while on a line that already has one.

Breakpoints can only be added to buffers that have been named.

---

## WATCHES
Watches are set within the memory editor (`F3`). When the cursor is over the
desired byte to watch, the press `C= + w` to add a watch to the address of the
byte under the cursor.  A beep will confirm that the watch
was added.

The watch editor (`F7`) shows all active watches. This window displays the old
value of a watch and what it was changed to when it is updated.

When a value is changed the watch view is activated to alert the user to the
alteration.  If a read or write is detected while stepping _into_ the code,
the viewer is also activated.
