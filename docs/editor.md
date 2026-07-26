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

|  key   | name          |   description                                                          |
|--------|---------------|------------------------------------------------------------------------|
| C= + b | Set Breakpoint    | sets a breakpoint at the current line (or removes it if one exists)     |
| C= + c | Refresh           | refreshes the screen by redrawing the source buffer                    |
|    -   | File Viewer       | list directory, shows the files on the current disk                    |
| C= + n | New buffer        | creates a new source buffer and sets it as the active buffer           |
| C= + q | Close buffer      | closes the current buffer and opens the next one that is open          |
| C= + y | Show Symbols      | lists the symbol table for the assembled program                       |
| C= + m | Macro Viewer      | lists the macros defined in the assembled program                      |
| C= + e | Next Error        | if there are errors from the last assembly, navigates to the next one  |
| C= + w | Swap Window       | swaps to an open "window" (e.g. error window) or back to the editor    |
| C= + l | Open Log          | opens (or closes) the log of the last assembly/link                    |
| C= + f | Toggle Autoformat | toggles auto-formatting on/off                                         |
| C= + v | Visible Whitespace| toggles display of TAB characters (rendered as a small dot)            |
| C= + + | Next Drive        | Selects the next drive (limited to #15)                                |
| C= + - | Prev Drive        | Selects the previous drive (limited to #8)                             |
|    :   | Ex Command        | enters "EX" mode (see the EX COMMANDS section below for more on this)  |

#### DRIVE SELECTION

The current drive selection is displayed with a `#` prefix in the status bar.
`C= + + (plus)` selects the _next_ available drive and `C= + - (minus)` selects
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

|  key   | name             |   description                                                                                |
|--------|------------------|----------------------------------------------------------------------------------------------|
|   f1   | Run              | saves Monster's state and transfers control to the last assembly (or enters BASIC if none)   |
|   f3   | Assemble         | assembles the current source buffer to memory                                                |
|   f4   | Link project     | using the LINK file on disk, links all object files (`*.o`) on the disk                       |
|   f5   | Show buffers     | displays a list of the currently open buffers                                                |
|   f6   | Show project     | displays the current project configuration                                                   |
|   f7   | Monitor          | opens the text-based monitor as a window (see the _Monitor_ section)                          |
|   f8   | Monitor (full)   | opens the text-based monitor maximized (`SHIFT + f7`)                                         |

While debugging, the f-keys have different functionality, as described in the _Debug Commands_ section.

### EX COMMANDS

The `:` key puts the editor in _EX_ mode.  In this mode, a string is accepted from the user.
The format of this string is a _command_ (usually one or two characters) followed by zero or more
argments.  E.g. `:w hello.s` will _write_ a file named "hello.s" to disk.

The table below details the available commands in _EX_ mode.

| command | name                         |   args                          | description                                                                                     |
|---------|------------------------------|---------------------------------|-------------------------------------------------------------------------------------------------|
|    a    | Assemble File                | Filename                        | assembles the given filename                                                                    |
|    B    | export Binary                | Filename                        | exports the active assembly to a binary file (no .PRG header)                                   |
|    d    | Start Debugger               | Symbol to debug at (optional)   | begins debugging at the given label                                                             |
|    db   | Start Debugger (with init)   | Symbol to debug at (optional)   | begins debugging at the given label. Initializes target state with the BASIC cold start handler |
|    D    | export Debug file            | Filename                        | exports the loaded assembly, debug info, and symbol table as a debug (`.D`) file                |
|    L    | Load Debug file              | Filename                        | loads the given debug (`.D`) file (symbol table, debug info, and program data)                  |
|    e    | Edit                         | Filename                        | loads the buffer with the contents of the given file                                            |
|    g    | Goto                         | Symbol to run at (optional)     | executes the program at the address of the given symbol                                         |
|    o    | assemble to Object           | Filename                        | assembles the current source buffer to an object file with the given filename                   |
|    P    | export .PRG                  | Filename                        | exports the active assembly to a .PRG file                                                      |
|    r    | Rename                       | Name                            | renames the buffer to the given name                                                            |
|    s    | Save                         | Filename                        | saves the buffer to the given filename                                                          |
|    S    | Save All                     |   N/A                           | saves all modified buffers that are open currently                                              |
|    x    | Scratch                      | Filename                        | scratches (deletes) the given filename                                                          |


#### ASSEMBLE FILE :a [filename]

Assembles the contents of the given file. This is functionally the same as opening
the given file and assembling it with debug information (F4).

Invoking the debugger will invoke it for the last assembled file (not the current
source buffer) in this scenario.  The debugger cares about the active debug
information _not_ the active file.

Example:
`:a HELLO.S`

#### EXPORT BINARY :B [filename]

Exports the active assembly (F3/F4) to the given file as binary.  This means
no load address is prepended to the file.  This can be useful if you are using
Monster to create level data or other code loaded by your main program.  It
can also be used to export things like data tables for use with .INCBIN

Example:
`:B DATA.B`

#### Start Debugger :d [symbol]
Begins debugging at the given symbol using the active debug information.

If no symbol is given, the program will
begin and the debugger invoked at the _lowest_ defined origin (.ORG) in the
program. See [Debugger](https://github.com/gummyworm/monster#debugger) for more
details on debugging.

Example:
`:d START`

#### START DEBUGGER (with init) :db [symbol]

Begins debugging at the given symbol using the active debug information, initializing the
system with the BASIC COLD start handler first.

This is useful if your program expects the system to be initialized with the default
VIC-registers, zeropage values , etc.

If no symbol is given, the program will
begin and the debugger invoked at the _lowest_ defined origin (.ORG) in the
program. See [Debugger](https://github.com/gummyworm/monster#debugger) for more
details on debugging.

Example:
`:db START`

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

Exports the active assembly (F3/F4) to the given file as a .PRG file.  This means
a load address is prepended to the file prior to export.  This produces a
standalone executable you can use when you are done working on your program.

Example:
`:B GAME.PRG`

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

|  key       | name       | description                                                            |
|------------|------------|------------------------------------------------------------------------|
| HOME       | Home       | moves the cursor to column 0                                           |
| :_n_       | Goto line  | at the EX prompt (`:`), enter a line number to move the cursor to it   |
| C= + [1-8] | Goto Buffer| opens the buffer corresponding to the number key that is pressed       |
| Ctrl + h   | Prev Buffer| opens the buffer before the active one (if there is one)               |
| Ctrl + l   | Next Buffer| opens the buffer after the active one (if there is one)                |
| C= + i     | Jump up    | jumps forward to the next source position that was "jumped" to         |
| C= + o     | Jump back  | jumps back to the last source position that was "jumped" to            |
|    $       | End of Line| moves the cursor to the end of the current line                        |
|    ;;      | Banner     | inserts a banner (full line of semicolons) below the cursor            |
|    gg      | Top of File| moves the cursor to the first character in the file                    |
|    gd      | Goto Def   | if the cursor is on a label reference, navigates to that label         |
|    G       | End of File| moves the cursor to the last line in the file                          |
|    h       | Left       | moves the cursor left                                                  |
|    j       | Down       | moves the cursor down                                                  |
|    k       | Up         | moves the cursor up                                                    |
|    l       | Right      | moves the cursor right                                                 |
|    H       | Home       | moves the cursor to the top left of the screen                         |
|    L       | Last       | moves the cursor to the bottom left of the screen                      |
|    d0      | Delete To  | deletes everything on the line before the cursor                       |
|    D/d$    | Delete Rest| deletes the contents of the line after the cursor's position           |
|    dd      | Delete Line| deletes the next line                                                  |
|    dw      | Delete Word| deletes the next word                                                  |
|    J       | Join lines | moves the contents of the next line to the end of the current one      |
|    0       | Column 0   | moves the cursor to the first column of the current line               |
|    a       | append char| enters insert mode and moves to the next character                     |
|    A       | append line| enters insert mode and moves to the last character in the current line |
|    C       | change line| deletes the from the cursor to the end of the line and enters insert   |
|    o       | open line  | opens a new line below the cursror and moves to it                     |
|    O       | open line ^| opens a new line above the cursor and moves to it                      |
|    s       | sub char   | deletes the character under the cursor and enters insert mode          |
|    S       | sub line   | deletes the line under the cursor and enters insert mode               |
|    p       | paste below| pastes the contents of the copy-buffer to the line below the cursor    |
|    P       | paste above| pastes the contents of the copy-buffer to the line above the cursor    |
|    I       | Insert line| enters insert mode and moves to the first character in the current line|
|    [       | Prev Block | moves to the previous empty line or start of file if there isn't one   |
|    ]       | Next Block | moves to the next empty line or end of file if there isn't one         |

### INSERT MODE
Entering insert mode allows the user to enter text at the cursor location.  Keystrokes are
interpreted as their corresponding ASCII character value in this mode, so there are no special
commands accessed via them.

There are various keys that enter INSERT mode from COMMAND: i, a, A, etc.

### VISUAL MODE
In _VISUAL_ mode (accessed via `v` in _COMMAND_ mode), the user can select
a block of text which may then be deleted or copied.  Below is the table of supported commands
while in visual mode. The `<-` key will return the user to to _COMMAND_ mode.

|  key       | name       | description                                                            |
|------------|------------|------------------------------------------------------------------------|
|    d       | delete     | deletes the selected text _and_ copies it to the copy buffer           |
|    y       | yank       | copies the selected text (in VISUAL mode) to the copy buffer           |

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

Files are stored with $0d line endings, but file saved with UNIX
style line-endings ($0a), will automatically converted when the file is loaded.

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

| command Name  |   key   |  behavior
|---------------|---------|------------------------------------------------------------------------------------------------------|
|  Plot Color 1 |    1    | Sets the selected position to the background color                                                   |
|  Plot Color 2 |    2    | Sets the selected position to the character color (hires mode) or the border color (multicolor mode) |
|  Plot Color 3 |    3    | Multicolor mode only. Sets the selected position to the character color                              |
|  Plot Color 4 |    4    | Multicolor mode only. Sets the selected position to the auxiliary color                              |
|  Clear        |SHIFT+CLR| Sets all pixels in the UDG to the background color                                                   |
|  Done         | RETURN  | Exits the editor and enters (or updates) the .db commands to create the graphic in the editor        |
|  Quit         | STOP    | Exits the editor without creating/updating the graphic contained in the editor                       |
| Toggle Mode   |   M     | If in hires mode, switches to multicolor mode or vise-versa                                          |

Entering the editor while on a line with an 8-byte ".db" definition (e.g. `.db $ff,$00,$ff,$00,$ff,$00,$ff,$00`) will pre-populate the
UDG editor with the character defined by these directives.
