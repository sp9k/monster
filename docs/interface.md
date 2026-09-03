## INTERFACE

### WINDOWS

Within the editor, you can launch several different "windows", interfaces dedicated to a particular task.
Some of these are opened automatically, such as the error log which appears when assembly fails, while others
are user activated, like the text-based monitor, breakpoint viewer, etc.

Navigation to and from these windows as well as cycling between active ones can be performed with the following
commands.

| COMMAND    |  KEY               |  DESCRIPTION
|------------|--------------------|----------------------------------------------------------------------------------------------------
| `NEXT WIN` | {c64-keys}`C= + W` | Cycles to the next available window (if already in the editor, enters the active window)
| `EXIT    ` | {c64-key}`RUN/STOP`| If a window is active, returns focus to the editor.  The window is left open, its rows still onscreen
| `CLOSE   ` | {c64-keys}`C= + Q` | Closes the active window.  Focus moves to the next open window, or back to the editor if it was the last one
| `HIDE    ` | {c64-keys}`C= + H` | Toggles the display of windows entirely

Note that {c64-key}`RUN/STOP` only gives focus back to the editor; the window remains open above the
status row.  To restore the fullscreen editor, either close the window with {c64-keys}`C= + Q` or hide the window
area entirely with {c64-keys}`C= + H`.

{c64-keys}`C= + Q` closes whatever has focus: it closes the active window when a window is focused,
and the current source buffer when the editor is focused (see the _EDITOR_ section).

Some windows are also closed for you: the error log is closed by the next assembly (which clears the
old errors), and entering or quitting the debugger closes every open window.

Some windows, in particular the memory viewer/editor and the text-based monitor, are resizable; this is done with
the following keys.

| COMMAND    |  KEY               |  DESCRIPTION
|------------|--------------------|-----------------------------------------------------------------------------------------------------
| `GROW    ` | {c64-keys}`C= + K` | Enlarges the active window
| `SHRINK  ` | {c64-keys}`C= + J` | Shrinks the active window
| `MAXIMIZE` | {c64-keys}`C= + Z` | Toggles between full screen (if currently not maximized) or its last un-maximized size (if maximized)

#### FULLSCREEN VIEWERS

The directory viewer ({c64-key}`Minus`), symbol viewer ({c64-keys}`C= + Y`) and macro viewer
({c64-keys}`C= + M`) are _not_ windows.  Each takes over the whole screen while open,
and they cannot be resized, hidden, or cycled through with the window keys above.

They can still be cloed with {c64-keys}`C= + Q`. {c64-key}`RUN/STOP` also quits these viewers in most cases.
The one excepiton being the macro viewer.  In it, {c64-key}`RUN/STOP` first returns from a macro definition to
the macro list, while {c64-keys}`C= + Q` closes the viewer outright from either mode.
