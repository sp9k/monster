## EDITOR OVERVIEW

The editor provides powerful facilities for loading, saving, and modifying source code.
Text is displayed in 40 columns to provide a much higher density interface than the Vic-20's native BASIC line editor.

Navigation will be familiar to vi users.  There are also a variety of commands to handle things like assembly, disassembly, etc.

As with all work you do on your Vic-20, if you care about it, save often.

### BUFFERS

Up to 8 source buffers may be stored in memory at a time, each up to 24KB
in size. These are accessed via a key chord comprised of `C=` and the number
key for the corresponding buffer.  You can also navigate to the _previous_
buffer with `Ctrl + h` and the _next_ buffer with `Ctrl + l`.

### COMMAND SHORTCUTS

Below are the basic commands along with their associated key combinations. These
commands are available regardless of insertion mode (see the _Editor Modes_ section
below for more info on modes).

|  KEY     | NAME            |   DESCRIPTION                                                          |
|----------|-----------------|------------------------------------------------------------------------|
| `C= + a` | `ASSEMBLE      `| assembles the active program                                           |
| `C= + d` | `DEBUG         `| begins debugging at the origin of the assembled program                |
| `C= + b` | `SET BREAKPOINT`| sets a breakpoint at the current line                                  |
| `C= + c` | `REFRESH       `| refreshes the screen by redrawing the source buffer                    |
| `   -  ` | `FILE VIEWER   `| list directory, shows the files on the current disk                    |
| `C= + l` | `LINK          `| links all .o files on disk using the LINK file                         |
| `C= + n` | `NEW BUFFER    `| creates a new source buffer and sets it as the active buffer           |
| `C= + q` | `CLOSE BUFFER  `| closes the current buffer and opens the next one that is open          |
| `C= + y` | `SHOW SYMBOLS  `| lists the symbol table for the assembled program                       |
| `C= + e` | `NEXT ERROR    `| if there are errors from the last assembly, navigates to the next one  |
| `C= + t` | `SHOW BUFFERS  `| displays a list of the currently open buffers                          |
| `  F4  ` | `LINK          `| links the object files in the project using the LINK file on disk      |
| `  F3  ` | `MEMVIEW       `| opens the memory viewer/editor (same as while debugging; press <- to exit)|
| `  F5  ` | `BRKVIEW       `| opens the breakpoint viewer/editor (same as while debugging)           |
| `  F6  ` | `WATCHVIEW     `| opens the watch viewer/editor (same as while debugging)                |
| `C= + +` | `NEXT DRIVE    `| Selects the next drive (limited to #15)                                |
| `C= + -` | `PREV DRIVE    `| Selects the previous drive (limited to #8)                             |
| `   :  ` | `EX COMMAND    `| Enters "EX" mode (see the EX COMMANDS section below for more on this)  |

#### DRIVE SELECTION

The current drive selection is displayed with a `#` prefix in the status bar.
`C= + + (PLUS)` selects the _next_ available drive and `C= + - (MINUS)` selects
the _previous_ available drive.  The valid device range is 8-15.

#### DIRECTORY VIEWER

Pressing the `-` key in command mode activates the directory viewer.

This tool presents a paginated view of all files on the disk.
Pressing `RETURN` while the cursor is on the desired file will load
that file into a new buffer and switch to that buffer.

While in the directory viewer, pressing 'G' navigates to the last file in the directory and 'gg' goes
to the first one.

#### SYMBOL VIEWER

The symbol viewer, activated with the `C= + Y` key combination displays all the labels in the program
along with their corresponding address.
The up/down cursor keys navigate between pages of symbols. Press RESTORE to return to the debugger.

#### FUNCTION (f KEY) COMMANDS

|  KEY     | NAME             |   DESCRIPTION                                                                                |
|----------|------------------|----------------------------------------------------------------------------------------------|
|   `f1`   | `RUN           `   | saves Monster's state and transfers control to the last assembly (or enters BASIC if none)   |
|   `f3`   | `MEMORY VIEWER `   | activates the memory viewer                                                                  |
|   `f4`   | `LOG           `   | displays the active log file (if any)                                                         |
|   `f5`   | `SHOW BUFFERS  `   | displays a list of the currently open buffers                                                |
|   `f6`   | `SHOW PROJECT  `   | displays the current project configuration                                                   |
|   `f7`   | `MONITOR       `   | opens the text-based monitor as a window (see the _Monitor_ section)                          |
|   `f8`   | `MONITOR (FULL)`   | opens the text-based monitor maximized (`SHIFT + f7`)                                         |

### EX COMMANDS

The `:` key puts the editor in _EX_ mode.  In this mode, a string is accepted from the user.
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
the given file and assembling it with debug information (`C= + a`).

Invoking the debugger will invoke it for the last assembled file (not the current
source buffer) in this scenario.  The debugger cares about the active debug
information _not_ the active file.

Example:
`:a HELLO.S`

#### EXPORT BINARY :B [filename]

Exports the active assembly (`C= + a`) to the given file as binary.  This means
no load address is prepended to the file.  This can be useful if you are using
Monster to create level data or other code loaded by your main program.  It
can also be used to export things like data tables for use with .INCBIN

Example:
`:B DATA.B`

#### EXPORT DEBUG FILE :D [filename]

Exports the loaded assembly, debug-information, and symbol table as a debug
(`.D`) file.  You may think of these as debuggable versions of your release
binaries: a `.D` file can be loaded (`:L`) and debugged without having to
reassemble/relink it.  This command should be run after a successful assembly
or link.

Example:
`:D HELLO.D`

#### LOAD DEBUG FILE :L [filename]

Loads the given debug (`.D`) file.  The symbol table, debug information, and
program data are all loaded into virtual memory so you can begin debugging,
view symbols, etc. as if you had just assembled the program.

Example:
`:L HELLO.D`

#### EDIT :e [filename]

Loads the given filename to a new buffer and activates it.

Example:
`:e HELLO.S`

#### ASSEMBLE TO OBJECT :o [filename]

Assembles the current source buffer to an object file with the given name.
The filename must have a `.o` (or `.O`) extension if you want the linker to
pick it up at link time.  See the [Linker](linker.md) document for more on
object files and linking.

Example:
`:o HELLO.O`

#### EXPORT .PRG :P [filename]

Exports the active assembly (`C= + a`) to the given file as a .PRG file.  This means
a load address is prepended to the file prior to export.  This produces a
standalone executable you can use when you are done working on your program.

Example:
`:P GAME.PRG`

#### RENAME :r [buffername]

Renames the active buffer to the given name.
Example:
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

Example:
`:S@`

#### SCRATCH :x [filename]

Deletes the file of the given name.
Example:
`:x TEST.S`

---

## EDITOR MODES

The editor is a _modal_ editor, that is, it behaves differently depending on which _mode_ it is
in.  The modes are all accessed from the default mode (called _COMMAND_ mode) and each mode returns
to _COMMAND_ mode when the `<-` key is pressed.  Below is a list of the modes along with
details on how to enter them and how the editor behaves while in that mode.

### COMMAND MODE (<-)

Command mode is the default mode.  The primary function of command mode is to navigate around the
source code and to enter other modes.
Navigation behaves similar to `vi` and many basic `vi` commands are supported.
The following keys are handled in COMMAND mode.

|  KEY         | NAME         | DESCRIPTION                                                            |
|--------------|--------------|------------------------------------------------------------------------|
| `HOME      ` | `HOME       `| moves the cursor to column 0                                           |
| `:_n_      ` | `GOTO LINE  `| at the EX prompt (`:`), enter a line number to move the cursor to it   |
| `C= + [1-8]` | `GOTO BUFFER`| opens the buffer corresponding to the number key that is pressed       |
| `Ctrl + h  ` | `PREV BUFFER`| opens the buffer before the active one (if there is one)               |
| `Ctrl + l  ` | `NEXT BUFFER`| opens the buffer after the active one (if there is one)                |
| `C= + i    ` | `JUMP UP    `| jumps forward to the next source position that was "jumped" to         |
| `C= + o    ` | `JUMP BACK  `| jumps back to the last source position that was "jumped" to            |
| `   $      ` | `END OF LINE`| moves the cursor to the end of the current line                        |
| `   ;;     ` | `BANNER     `| inserts a banner (full line of semicolons) below the cursor            |
| `   gg     ` | `TOP OF FILE`| moves the cursor to the first character in the file                    |
| `   gd     ` | `GOTO DEF   `| if the cursor is on a label reference, navigates to that label         |
| `   G      ` | `END OF FILE`| moves the cursor to the last line in the file                          |
| `   h      ` | `LEFT       `| moves the cursor left                                                  |
| `   j      ` | `DOWN       `| moves the cursor down                                                  |
| `   k      ` | `UP         `| moves the cursor up                                                    |
| `   l      ` | `RIGHT      `| moves the cursor right                                                 |
| `   H      ` | `HOME       `| moves the cursor to the top left of the screen                         |
| `   L      ` | `LAST       `| moves the cursor to the bottom left of the screen                      |
| `   d0     ` | `DELETE TO  `| deletes everything on the line before the cursor                       |
| `   D/d$   ` | `DELETE REST`| deletes the contents of the line after the cursor's position           |
| `   dd     ` | `DELETE LINE`| deletes the next line                                                  |
| `   dw     ` | `DELETE WORD`| deletes the next word                                                  |
| `   J      ` | `JOIN LINES `| moves the contents of the next line to the end of the current one      |
| `   0      ` | `COLUMN 0   `| moves the cursor to the first column of the current line               |
| `   a      ` | `APPEND CHAR`| enters insert mode and moves to the next character                     |
| `   A      ` | `APPEND LINE`| enters insert mode and moves to the last character in the current line |
| `   C      ` | `CHANGE LINE`| deletes from the cursor to the end of the line and enters insert       |
| `   o      ` | `OPEN LINE  `| opens a new line below the cursor and moves to it                      |
| `   O      ` | `OPEN LINE ^`| opens a new line above the cursor and moves to it                      |
| `   s      ` | `SUB CHAR   `| deletes the character under the cursor and enters insert mode          |
| `   S      ` | `SUB LINE   `| deletes the line under the cursor and enters insert mode               |
| `   p      ` | `PASTE BELOW`| pastes the contents of the copy-buffer to the line below the cursor    |
| `   P      ` | `PASTE ABOVE`| pastes the contents of the copy-buffer to the line above the cursor    |
| `   I      ` | `INSERT LINE`| enters insert mode and moves to the first character in the current line|
| `   [      ` | `PREV BLOCK `| moves to the previous empty line or start of file if there isn't one   |
| `   ]      ` | `NEXT BLOCK `| moves to the next empty line or end of file if there isn't one         |

### INSERT MODE
Entering insert mode allows the user to enter text at the cursor location.  Keystrokes are
interpreted as their corresponding ASCII character value in this mode, so there are no special
commands accessed via them.

There are various keys that enter INSERT mode from COMMAND: i, a, A, etc.

### VISUAL MODE
In _VISUAL_ mode (accessed via `v` in _COMMAND_ mode), the user can select
a block of text which may then be deleted or copied.  Below is the table of supported commands
while in visual mode. The `<-` key will return the user to _COMMAND_ mode.

|  KEY         | NAME      | DESCRIPTION                                                            |
|--------------|-----------|------------------------------------------------------------------------|
|    `d`       | `DELETE`  | deletes the selected text _and_ copies it to the copy buffer           |
|    `y`       | `YANK  `  | copies the selected text (in VISUAL mode) to the copy buffer           |

### VISUAL LINE MODE
_VISUAL LINE_, which is entered with the `SHIFT - v` key combination from _COMMAND_ mode is similar to _VISUAL_ mode,
but selections include only entire lines.  Upon entering _VISUAL LINE_ mode, the current row is selected.
Navigating to rows above or below will select additional lines.  The delete and yank keys behave the same as they do
in _VISUAL_ mode.

---

### COPY BUFFER
When text is deleted (delete line, delete word) or _yanked_, it is stored to a buffer where
it may be recalled by the paste commands (`p`, paste below and `P` paste above).
When the paste command is executed, the buffer is cleared.

The copy buffer is stored in a separate bank, so a selection may be as big as a
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
that were "jumped" from are two commands: _jump-forward_ (`C= + i`) and _jump-backward_ (`C= + o`).

### SYNTAX CHECKING
Lines are checked and formatted according to their contents each time they
are completed (RETURN is pressed).
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
The UDG (user defined graphics) editor is entered with the `C= + u` key combination.
This editor allows you to visually create simple graphics for your programs.  Navigation
is done with the same vi-like commands used in the main editor and graphics are created using the
following commands:

| COMMAND NAME    |   KEY     |  BEHAVIOR
|-----------------|-----------|------------------------------------------------------------------------------------------------------|
| ` PLOT COLOR 1` |`    1    `| Sets the selected position to the background color                                                   |
| ` PLOT COLOR 2` |`    2    `| Sets the selected position to the character color (hires mode) or the border color (multicolor mode) |
| ` PLOT COLOR 3` |`    3    `| Multicolor mode only. Sets the selected position to the character color                              |
| ` PLOT COLOR 4` |`    4    `| Multicolor mode only. Sets the selected position to the auxiliary color                              |
| ` CLEAR       ` |`SHIFT+CLR`| Sets all pixels in the UDG to the background color                                                   |
| ` DONE        ` |` RETURN  `| Exits the editor and enters (or updates) the .db commands to create the graphic in the editor        |
| ` QUIT        ` |` STOP    `| Exits the editor without creating/updating the graphic contained in the editor                       |
| `TOGGLE MODE  ` |`   M     `| If in hires mode, switches to multicolor mode or vice versa                                          |

Entering the editor while on a line with an 8-byte ".db" definition (e.g. `.db $ff,$00,$ff,$00,$ff,$00,$ff,$00`) will pre-populate the
UDG editor with the character defined by these directives.

<insert screenshot>
