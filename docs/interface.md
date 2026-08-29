## INTERFACE

### WINDOWS

Within the editor, you can launch several different "windows", interfaces dedicated to a particular task.
Some of these are opened automatically, such as the error log which appears when assembly fails, while others
are user activated, like the text-based monitor, breakpoint viewer, etc.

Navigation to and from these windows as well as cycling between active ones can be performed with the following
commands.

| COMMAND    |  KEY       |  DESCRIPTION
|------------|------------|----------------------------------------------------------------------------------------------------
| `NEXT WIN` | {c64-keys}`C= + W` | Cycles to the next available window (if already in the editor, enters the active window)
| `EXIT    ` | {c64-key}`RUN/STOP` | If window is active, exits it, returning to editor
| `HIDE    ` | {c64-keys}`C= + H` | Toggles the display of windows entirely

Some windows, in particular the memory viewer/editor and the text-based monitor, are resizable; this is done with
the following keys.

| COMMAND    |  KEY       |  DESCRIPTION
|------------|------------|-----------------------------------------------------------------------------------------------------
| `GROW    ` | {c64-keys}`C= + K` | Enlarges the active window
| `SHRINK  ` | {c64-keys}`C= + J` | Shrinks the active window
| `MAXIMIZE` | {c64-keys}`C= + Z` | Toggles between full screen (if currently not maximized) or its last un-maximized size (if maximized)
