## EDITOR OVERVIEW

The editor provides powerful facilities for loading, saving, and modifying source code.
Text is displayed in 40 columns to provide a much higher density interface than the Vic-20's native BASIC line editor.

Navigation will be familiar to vi users.  There are also a variety of commands to handle things like assembly, disassembly, etc.

As with all work you do on your Vic-20, if you care about it, save often.

### BUFFERS

Up to 8 source buffers may be stored in memory at a time, each up to 24KB
in size. These are accessed via a key chord comprised of {c64-key}`C=` and the number
key for the corresponding buffer.  You can also navigate to the _previous_
buffer with {c64-keys}`Ctrl + H` and the _next_ buffer with {c64-keys}`Ctrl + L`.

### COMMAND SHORTCUTS

Below are the basic commands along with their associated key combinations. These
commands are available regardless of insertion mode (see the _Editor Modes_ section
below for more info on modes).

|  KEY     | NAME        |   DESCRIPTION                                                                               |
|----------|-------------|---------------------------------------------------------------------------------------------|
| {c64-keys}`C= + A`     | `ASSEMBLE      `| assembles the active program                                              |
| {c64-keys}`C= + D`     | `DEBUG         `| begins debugging at the origin of the assembled program                   |
| {c64-keys}`C= + B`     | `SET BREAKPOINT`| sets a breakpoint at the current line                                     |
| {c64-keys}`C= + C`     | `REFRESH       `| refreshes the screen by redrawing the source buffer                       |
| {c64-key}`Minus`       | `FILE VIEWER   `| list directory, shows the files on the current disk                       |
| {c64-keys}`C= + L`     | `LINK          `| links all .o files on disk using the LINK file                            |
| {c64-keys}`C= + N`     | `NEW BUFFER    `| creates a new source buffer and sets it as the active buffer              |
| {c64-keys}`C= + Q`     | `CLOSE BUFFER  `| closes the current buffer and opens the next one that is open             |
| {c64-keys}`C= + Y`     | `SHOW SYMBOLS  `| lists the symbol table for the assembled program                          |
| {c64-keys}`C= + M`     | `SHOW MACROS   `| lists the macros that are defined and allows viewing their definitions    |
| {c64-keys}`C= + E`     | `NEXT ERROR    `| if there are errors from the last assembly, navigates to the next one     |
| {c64-keys}`C= + T`     | `SHOW BUFFERS  `| displays a list of the currently open buffers                             |
| {c64-keys}`C = + L`    | `LINK          `| links the object files in the project using the LINK file on disk         |
| {c64-key}`F3`          | `MEMVIEW       `| opens the memory viewer/editor (same as while debugging; press {c64-key}`Left-arrow` to exit) |
| {c64-key}`F5`          | `BRKVIEW       `| opens the breakpoint viewer/editor (same as while debugging)              |
| {c64-key}`F6`          | `WATCHVIEW     `| opens the watch viewer/editor (same as while debugging)                   |
| {c64-keys}`C= + Plus`  | `NEXT DRIVE    `| Selects the next drive (limited to #15)                                   |
| {c64-keys}`C= + Minus` | `PREV DRIVE    `| Selects the previous drive (limited to #8)                                |
| {c64-key}`Colon`       | `EX COMMAND    `| Enters "EX" mode (see the EX COMMANDS section below for more on this)     |

#### DRIVE SELECTION

The current drive selection is displayed with a `#` prefix in the status bar.
{c64-keys}`C= + Plus` selects the _next_ available drive and {c64-keys}`C= + Minus` selects
the _previous_ available drive.  The valid device range is 8-15.

#### DIRECTORY VIEWER

Pressing the {c64-key}`Minus` key in command mode activates the directory viewer.

This tool presents a paginated view of all files on the disk.
Pressing {c64-key}`RETURN` while the cursor is on the desired file will load
that file into a new buffer and switch to that buffer.

While in the directory viewer, pressing {c64-key}`G` navigates to the last file in the directory and {c64-sequence}`GG` goes
to the first one.

#### SYMBOL VIEWER

The symbol viewer, activated with {c64-keys}`C= + Y` displays all the labels in the program
along with their corresponding address.
The up/down cursor keys navigate between pages of symbols. Press {c64-key}`RESTORE` to return to the debugger.

#### MACRO VIEWER

The macro viewer, activated with {c64-keys}`C= + M`, lists every macro
that is currently defined and allows you to inspect the body of any of them.

Macros are registered with the assembler when their definition is _assembled_
({c64-keys}`C= + A`), so the list reflects the macros from your last assembly, not
necesarrily the macros that happen to be visible in the active buffer.  If no macros have been
defined, the viewer aborts and reports `NO MACROS` in the status bar.

The viewer has two modes:

**MACRO LIST**: the initial mode, titled `MACROS`.  Each row is the name of one
defined macro, in the order of their definition.  The highlighted row is the
current selection.

**DEFINITION**: entered by pressing {c64-key}`RETURN` on the selected macro.  The
screen is cleared and the top row shows the macro's name followed by its parameter
names; the rows below it are the lines of the macro's body as defined.

| KEY                                                | MODE       | DESCRIPTION                                                          |
|----------------------------------------------------|------------|----------------------------------------------------------------------|
| {c64-key}`K`                                       | both       | moves the selection up (macro list) or scrolls up (definition)       |
| {c64-key}`J`                                       | both       | moves the selection down (macro list) or scrolls down (definition)   |
| {c64-sequence}`GG`                                 | both       | goes to the first macro (or first line of the definition)            |
| {c64-key}`G`                                       | both       | goes to the last macro (or last line of the definition)              |
| {c64-key}`RETURN`                                  | macro list | opens the definition of the selected macro                           |
| {c64-key}`RUN/STOP`                                | definition | returns to the macro list                                            |
| {c64-key}`RUN/STOP`                                | macro list | exits the viewer and restores the editor screen                      |

Lists longer than the screen scroll automatically as the selection reaches the top
or bottom row.

Note that the viewer is read-only; it is a way to confirm _what_ the assembler
actually recorded for a macro.  See the [Assembler](assembler.md) document for the
`.MAC` directive and the limits on macro count and size.

#### FUNCTION (F KEY) COMMANDS

|  KEY          | NAME               |   DESCRIPTION                                                                                |
|---------------|--------------------|----------------------------------------------------------------------------------------------|
| {c64-key}`F1` | `RUN           `   | saves Monster's state and transfers control to the last assembly (or enters BASIC if none)   |
| {c64-key}`F3` | `MEMORY VIEWER `   | activates the memory viewer                                                                  |
| {c64-key}`F4` | `LOG           `   | displays the active log file (if any)                                                        |
| {c64-key}`F5` | `BREAKPOINTS   `   | activates the breakpoint viewer                                                              |
| {c64-key}`F6` | `SHOW PROJECT  `   | displays the current project configuration                                                   |
| {c64-key}`F7` | `MONITOR       `   | opens the text-based monitor as a window (see the _Monitor_ section)                         |
| {c64-key}`F8` | `MONITOR (FULL)`   | opens the text-based monitor maximized ({c64-keys}`Shift + F7`)                              |

### EX COMMANDS

The {c64-key}`Colon` key puts the editor in _EX_ mode.  In this mode, a string is accepted from the user.
The format of this string is a _command_ (usually one or two characters) followed by zero or more
arguments.  E.g. `:s hello.s` will _write_ a file named "hello.s" to disk.

The table below details the available commands in _EX_ mode.

| COMMAND   | NAME                  |   ARGS                          | DESCRIPTION                                                                                     |
|-----------|-----------------------|---------------------------------|-------------------------------------------------------------------------------------------------|
|    `a`    | `ASSEMBLE FILE     `  | Filename                        | assembles the given filename                                                                    |
|    `B`    | `EXPORT BINARY     `  | Filename                        | exports the active assembly to a binary file (no .PRG header)                                   |
|    `D`    | `EXPORT DEBUG FILE `  | Filename                        | exports the loaded assembly, debug info, and symbol table as a debug (`.D`) file                |
|    `L`    | `LOAD DEBUG FILE   `  | Filename                        | loads the given debug (`.D`) file (symbol table, debug info, and program data)                  |
|    `e`    | `EDIT              `  | Filename                        | loads the buffer with the contents of the given file                                            |
|    `o`    | `ASSEMBLE TO OBJECT`  | Filename                        | assembles the current source buffer to an object file with the given filename                   |
|    `P`    | `EXPORT .PRG       `  | Filename                        | exports the active assembly to a .PRG file                                                      |
|    `r`    | `RENAME            `  | Name                            | renames the buffer to the given name                                                            |
|    `s`    | `SAVE              `  | Filename                        | saves the buffer to the given filename                                                          |
|    `S`    | `SAVE ALL          `  |   N/A                           | saves all modified buffers that are open currently                                              |
|    `x`    | `SCRATCH           `  | Filename                        | scratches (deletes) the given filename                                                          |


#### ASSEMBLE FILE :a [filename]

Assembles the contents of the given file. This is functionally the same as opening
the given file and assembling it with debug information ({c64-keys}`C= + A`).

Invoking the debugger will invoke it for the last assembled file (not the current
source buffer) in this scenario.  The debugger cares about the active debug
information _not_ the active file.

**EXAMPLE:**
`:a HELLO.S`

#### EXPORT BINARY :B [filename]

Exports the active assembly ({c64-keys}`C= + A`) to the given file as binary.  This means
no load address is prepended to the file.  This can be useful if you are using
Monster to create level data or other code loaded by your main program.  It
can also be used to export things like data tables for use with .INCBIN

**EXAMPLE:**
`:B DATA.B`

#### EXPORT DEBUG FILE :D [filename]

Exports the loaded assembly, debug-information, and symbol table as a debug
(`.D`) file.  You may think of these as debuggable versions of your release
binaries: a `.D` file can be loaded (`:L`) and debugged without having to
reassemble/relink it.  This command should be run after a successful assembly
or link.

**EXAMPLE:**
`:D HELLO.D`

#### LOAD DEBUG FILE :L [filename]

Loads the given debug (`.D`) file.  The symbol table, debug information, and
program data are all loaded into virtual memory so you can begin debugging,
view symbols, etc. as if you had just assembled the program.

**EXAMPLE:**
`:L HELLO.D`

#### EDIT :e [filename]

Loads the given filename to a new buffer and activates it.

**EXAMPLE:**
`:e HELLO.S`

#### ASSEMBLE TO OBJECT :o [filename]

Assembles the current source buffer to an object file with the given name.
The filename must have a `.o` (or `.O`) extension if you want the linker to
pick it up at link time.  See the [Linker](linker.md) document for more on
object files and linking.

**EXAMPLE:**
`:o HELLO.O`

#### EXPORT .PRG :P [filename]

Exports the active assembly ({c64-keys}`C= + A`) to the given file as a .PRG file.  This means
a load address is prepended to the file prior to export.  This produces a
standalone executable you can use when you are done working on your program.

**EXAMPLE:**
`:P GAME.PRG`

#### RENAME :r [buffername]

Renames the active buffer to the given name.
**EXAMPLE:**
`:r TEST2.S`

#### SAVE :s [filename]

Saves the active buffer to a file with the given name.  If no name is given,
the active buffer's name is used.

**NOTE**
Adding an `@` to this command (`s@`) will delete the file before saving. This
allows you to overwrite the existing file if it exists.

Examples:
`:s NEW.S`
`:s@ OLD.S`
`:S@` (save all)

#### SAVE ALL :S

Saves all buffers that have been modified since they were last saved.
As with the _Save_ command, adding `@` to the command (`S@`) will overwrite
existing files if they exist.

**EXAMPLE:**
`:S@`

#### SCRATCH :x [filename]

Deletes the file of the given name.
**EXAMPLE:**
`:x TEST.S`

---

## EDITOR MODES

The editor is a _modal_ editor, that is, it behaves differently depending on which _mode_ it is
in.  The modes are all accessed from the default one (called _COMMAND_ mode) and each returns
to _COMMAND_ mode when the {c64-key}`RUN/STOP` key is pressed.  Below is a list of the modes along with their function and details on how to enter them.

### COMMAND MODE ({c64-key}`RUN/STOP`)

This is the default mode.  The primary function of command mode is to navigate around the
source code and to enter other modes.
Navigation behaves similar to `vi` and many basic `vi` commands are supported.
The following keys are handled in COMMAND mode.

|  KEY         | NAME         | DESCRIPTION                                                                                   |
|--------------|--------------|-----------------------------------------------------------------------------------------------|
| {c64-key}`HOME` | `HOME       `| moves the cursor to column 0                                                               |
| {c64-key}`Colon` + _n_ | `GOTO LINE  `| at the EX prompt ({c64-key}`Colon`), enter a line number to move the cursor to it   |
| {c64-key}`C=` + {c64-key}`1`–{c64-key}`8` | `GOTO BUFFER`| opens the buffer corresponding to the number key that is pressed |
| {c64-keys}`Ctrl + H` | `PREV BUFFER`| opens the buffer before the active one (if there is one)                              |
| {c64-keys}`Ctrl + L` | `NEXT BUFFER`| opens the buffer after the active one (if there is one)                               |
| {c64-keys}`C= + I` | `JUMP UP    `| jumps forward to the next source position that was "jumped" to                          |
| {c64-keys}`C= + O` | `JUMP BACK  `| jumps back to the last source position that was "jumped" to                             |
| {c64-key}`Dollar` | `END OF LINE`| moves the cursor to the end of the current line                                          |
| {c64-sequence}`;;` | `BANNER     `| inserts a banner (full line of semicolons) below the cursor                             |
| {c64-sequence}`GG` | `TOP OF FILE`| moves the cursor to the first character in the file                                     |
| {c64-sequence}`GD` | `GOTO DEF   `| if the cursor is on a label reference, navigates to that label                          |
| {c64-keys}`Shift + G` | `END OF FILE`| moves the cursor to the last line in the file                                        |
| {c64-key}`H` | `LEFT       `| moves the cursor left                                                                         |
| {c64-key}`J` | `DOWN       `| moves the cursor down                                                                         |
| {c64-key}`K` | `UP         `| moves the cursor up                                                                           |
| {c64-key}`L` | `RIGHT      `| moves the cursor right                                                                        |
| {c64-keys}`Shift + H` | `HOME       `| moves the cursor to the top left of the screen                                       |
| {c64-keys}`Shift + L` | `LAST       `| moves the cursor to the bottom left of the screen                                    |
| {c64-sequence}`D0` | `DELETE TO  `| deletes everything on the line before the cursor                                        |
| {c64-keys}`Shift + D` / {c64-sequence}`D$` | `DELETE REST`| deletes the contents of the line after the cursor's position    |
| {c64-sequence}`DD` | `DELETE LINE`| deletes the next line                                                                   |
| {c64-sequence}`DW` | `DELETE WORD`| deletes the next word                                                                   |
| {c64-keys}`Shift + J` | `JOIN LINES `| moves the contents of the next line to the end of the current one                    |
| {c64-key}`0` | `COLUMN 0   `| moves the cursor to the first column of the current line                                      |
| {c64-key}`A` | `APPEND CHAR`| enters insert mode and moves to the next character                                            |
| {c64-keys}`Shift + A` | `APPEND LINE`| enters insert mode and moves to the last character in the current line               |
| {c64-keys}`Shift + C` | `CHANGE LINE`| deletes from the cursor to the end of the line and enters insert                     |
| {c64-key}`O` | `OPEN LINE  `| opens a new line below the cursor and moves to it                                             |
| {c64-keys}`Shift + O` | `OPEN LINE ^`| opens a new line above the cursor and moves to it                                    |
| {c64-key}`S` | `SUB CHAR   `| deletes the character under the cursor and enters insert mode                                 |
| {c64-keys}`Shift + S` | `SUB LINE   `| deletes the line under the cursor and enters insert mode                             |
| {c64-key}`P` | `PASTE BELOW`| pastes the contents of the copy-buffer to the line below the cursor                           |
| {c64-keys}`Shift + P` | `PASTE ABOVE`| pastes the contents of the copy-buffer to the line above the cursor                  |
| {c64-keys}`Shift + I` | `INSERT LINE`| enters insert mode and moves to the first character in the current line              |
| {c64-key}`Left-bracket` | `PREV BLOCK `| moves to the previous empty line or start of file if there isn't one               |
| {c64-key}`Right-bracket` | `NEXT BLOCK `| moves to the next empty line or end of file if there isn't one                    |

### INSERT MODE
Entering insert mode allows the user to enter text at the cursor location.  Keystrokes are
interpreted as their corresponding ASCII character value in this mode, so there are no special
commands accessed via them.

There are various keys that enter INSERT mode from COMMAND: {c64-key}`I`, {c64-key}`A`,
{c64-keys}`Shift + A`, etc.

### VISUAL MODE
In _VISUAL_ mode (accessed via {c64-key}`V` in _COMMAND_ mode), the user can select
a block of text which may then be deleted or copied.  Below is the table of supported commands
while in visual mode. The {c64-key}`Left-arrow` key will return the user to _COMMAND_ mode.

|  KEY         | NAME      | DESCRIPTION                                                            |
|--------------|-----------|------------------------------------------------------------------------|
| {c64-key}`D` | `DELETE`  | deletes the selected text _and_ copies it to the copy buffer           |
| {c64-key}`Y` | `YANK  `  | copies the selected text (in VISUAL mode) to the copy buffer           |

### VISUAL LINE MODE
_VISUAL LINE_, which is entered with the {c64-keys}`Shift + V` key combination from _COMMAND_ mode is similar to _VISUAL_ mode,
but selections include only entire lines.  Upon entering _VISUAL LINE_ mode, the current row is selected.
Navigating to rows above or below will select additional lines.  The delete and yank keys behave the same as they do
in _VISUAL_ mode.

---

### COPY BUFFER
When text is deleted (delete line, delete word) or _yanked_, it is stored to a buffer where
it may be recalled by the paste commands ({c64-key}`P`, paste below and {c64-keys}`Shift + P` paste above).
When the paste command is executed, the buffer is cleared.

The copy buffer is stored in a dedicated memory bank, so a selection may be as big as a
source buffer (24KB).

Because the editor is limited to 40 columns in width, the first and last lines are handled
specially.  If the first or last line will not fit, the paste is aborted.  This is similar to
how the BACKSPACE and JOIN LINE commands behave, which will error with a beep if the resulting
line would not fit on screen.

### LINE ENDINGS

Files are stored with $0d line endings, but files saved with UNIX-style
line endings ($0a) will be automatically converted when the file is loaded.

### JUMP LISTS
When the user "jumps" to a different position in the source (`gg`, `G`, `goto line`,
`find`, `[`, and `]`) the editor saves the old position.  To recall the positions
that were "jumped" from are two commands: _jump-forward_ ({c64-keys}`C= + I`) and _jump-backward_ ({c64-keys}`C= + O`).

### SYNTAX CHECKING
Lines are checked and formatted according to their contents each time they
are completed ({c64-key}`RETURN` is pressed).
While this should reduce the number of errors you encounter when assembling,
it does not guarantee it.  The following permissions are granted in order to
provide a smoother editing experience for common cases that are invalid at
assembly time:
    - labels may not be defined
    - origin may not be set
This means that lines using undefined labels are treated as valid.  If
the label does not exist at assembly time, of course this will result in an
error.
Macros, however, are expected to be defined.

Although labels aren't _required_ to be defined, they are internally tracked
while editing.  Because their addresses aren't valid til assembly, you cannot
access them (e.g. in the symbol viewer) until then.

### UDG EDITOR
The UDG (user defined graphics) editor is entered with the {c64-keys}`C= + U` key combination.
This editor allows you to visually create simple graphics for your programs.  Navigation
is done with the same vi-like commands used in the main editor and graphics are created using the
following commands:

| COMMAND NAME    |   KEY     |  BEHAVIOR
|-----------------|-----------|------------------------------------------------------------------------------------------------------|
| ` PLOT COLOR 1` | {c64-key}`1` | Sets the selected position to the background color                                               |
| ` PLOT COLOR 2` | {c64-key}`2` | Sets the selected position to the character color (hires mode) or the border color (multicolor mode) |
| ` PLOT COLOR 3` | {c64-key}`3` | Multicolor mode only. Sets the selected position to the character color                          |
| ` PLOT COLOR 4` | {c64-key}`4` | Multicolor mode only. Sets the selected position to the auxiliary color                          |
| ` CLEAR       ` | {c64-keys}`Shift + CLR` | Sets all pixels in the UDG to the background color                                  |
| ` DONE        ` | {c64-key}`RETURN` | Exits the editor and enters (or updates) the .db commands to create the graphic in the editor |
| ` QUIT        ` | {c64-key}`RUN/STOP` | Exits the editor without creating/updating the graphic contained in the editor              |
| `TOGGLE MODE  ` | {c64-key}`M` | If in hires mode, switches to multicolor mode or vice versa                                     |

Entering the editor while on a line with an 8-byte ".db" definition (e.g. `.db $ff,$00,$ff,$00,$ff,$00,$ff,$00`) will pre-populate the
UDG editor with the character defined by these directives.

```{figure} screenshots/editor-udg-1.png
:alt: The UDG editor
:align: center
:width: 75%
:class: screenshot

The UDG editor activated on a row of .db directives
```
