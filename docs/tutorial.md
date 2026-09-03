### TUTORIAL

I hope you're feeling excited and inspired by our adventure writing "Hello World" because
we will now walk through a much more substantial project.  The goal is to build something
that familiarizes you with the multitude of powerful features Monster provides.

By the end of this tutorial we will have a smoothly moving character that can run from side to side
and jump under joystick control.

#### MAIN

This project will span multiple files, but when assembling directly into memory, Monster begins with the
active source file.  For us, that will be a `main.s` file.  All other files will be _included_ from
this one (more on that when we get to it).

If you still have buffers open from your past work, close them with {c64-keys}`C= + Q` until only one remains.  Press {c64-keys}`C= + T` to
enter the **BUFFERS VIEWER**.  This will pop open a _window_ which allows you to view all open buffers and
select one to navigate to.  Confirm in this view that we have only one buffer open.

Once confirmed, with the BUFFERS VIEWER, press {c64-keys}`C= + Q` to close the BUFFERS VIEWER.
You can also press ({c64-key}`RUN/STOP` to re-enter the editor, but leave the viewer onscreen.
We'll touch more on the concept of these "windows" when we start debugging.

Now rename the buffer by entering EX COMMAND mode ({c64-key}`:`) and typing `r main.s` at the prompt.

Let's set the origin of this program to `$1000`.

```
	.org $1000
```

Since this program will be a bit more substantial, we will want to leverage Monster's macro
capabilities a bit.  A good organizational practice for this is to have a single "macros" file
that you include at the top of your "main" assembly file (`main.s` for us).

To create a new buffer, press {c64-keys}`C= + N`.  This will open a new unnamed buffer.  Press
{c64-keys}`C= + T` and you should see there are now two buffers: `main.s` and our new unnamed one.

#### MACROS

Let's call this new file `macros.inc`.  Rename it using the `r` EX COMMAND.
The `.inc` suffix tells us this is an _include_ file.
Monster doesn't care what suffix you use in most cases, but avoid `.o`, which is
reserved for use by the linker.

Macro use is very much a matter of personal taste.  I avoid heavy macro use as it can obscure
potential optimizations, which is half the fun of writing assembly by hand, but there are some
simple ones that make life a little bit easier without hiding much from the user.

For this project, we'll define two macros to treat the index registers `X` and `Y` like a single
16-bit value:


```{figure} screenshots/tutorial-macros.png
:alt: Macros
:align: center
:width: 75%
:class: screenshot

macros.inc
```

By now, hopefully, you're getting a sense of Monster's autoformatting and syntax checking.
If you had an error when entering any of the above text, Monster reports it and leaves you on
the line containing the error so that you can correct it.  If you made no errors (yet), try
editing `.endmac` to `.endmacc` and pressing {c64-key}`RETURN` to witness this behavior.

Of course, there are some classes of errors that cannot be checked immediately without
assembling.  You may find it useful to incrementally test your files as you are working on them.
You can do this even with files like this which emit no real bytes.  In fact, in the case
of macros, it's often a good idea to do so.

If you left the `macros.inc` buffer, return to it and press {c64-keys}`C= + A` to assemble it.  You
should see a simple "DONE" message.  But what actually happened?  Press {c64-keys}`C= + M` to open
the **MACRO VIEWER**.  Here you will see all the macros that Monster has registered from our
assembly.

Why might you want to do this?  Consider a macro like this:

```
.mac lsr2
	lsr
	lsr
.endmac
```

and an invocation like this:

```
	lsr2
```

How does Monster know if this is a label or a macro invocation?  The answer: unless we've
assembled the definition already, it doesn't.  Monster will format this as a label for lack of
information.

This is why it's a good idea to start your session by assembling your macros file and to
include it at the top of your "main" entrypoint file.

#### CUSTOM CHARACTERS

Many sizeable programs will contain a relatively large chunk of data.  Logically it makes sense
to store this in its own file.

A character set is one popular use case, and this is exactly what we'll be defining.
Defining an entire character set is quite a lot of work, so we're going to base ours on the
VIC-20's own character set.

To do this we will dip our toes into one of Monster's powerful utilities: the **MONITOR**.
Activate the monitor with the {c64-key}`F7` key.  A window will appear in which text commands
are entered.  The character set on which we wish to base our design lives at address `$8000` in
the VIC-20's ROM.  Run the following command to take a peek at the memory there:

```
m $8000
```

Pretty neat, but not too helpful in producing a usable character set.  A couple of modifications
to our command will change that.  First, we must understand the `>` operator available
in the monitor.  When appended to a command, the output from the command will be _redirected_
to whatever filename follows.

The other thing to understand is that commands like `dump` take an optional second parameter.
In this case, it defines the address at which to stop dumping memory.  This ending address is
exclusive, so `$8400` includes all bytes through `$83ff`.  With these things in mind, we can
save the whole range from `$8000`-`$83ff` (one of the VIC-20's character sets) to a file for
our own repurposing.

```
dump $8000 $8400 > chars.s
```

Exit the monitor now by running the `x` command:

```
x
```

This returns you to the editor.  Now open the directory viewer and you should see the file we wrote: `chars.s`.
Navigate to it and press {c64-key}`RETURN`.  Once it loads you should see a wall of `.db`
directives.  Remember from our "Hello World" example that these define a list of raw byte values.

Now, move the cursor to any `.db` row and press {c64-keys}`C= + U` to bring up the **UDG EDITOR**.
This will show you an 8×8 representation of the VIC's interpretation of the character data
represented by the row you activated the editor on.

Feel free to play around with all the other characters in the set.  You can always regenerate
the whole set with the same command we used to get the character set in the first place.  To do
so, close the `chars.s` buffer, scratch the existing file with `:x chars.s`, and run the `dump`
command again.

That is enough for now.  We will return to the character set once our program is ready to use
it—and once we are feeling sufficiently inspired.

#### BUFFER SWITCHING

At this point we have at least three buffers open (perhaps more if you got curious).  There are
several ways to move between them and this will be a frequent part of our workflow, so it's
worth taking a moment to get a handle on them.

{c64-keys}`CTRL + H` navigates to the _previous_ buffer and {c64-keys}`CTRL + L` navigates to the
_next_ one.  Go back and forth between your buffers with these keys to get a feel for this.

You may have noticed a number to the left of your buffers' names.  This is the buffer's "ID" but,
more importantly, it is a handle for quick navigation to it.  If your `main.s` buffer has ID `1`,
for example, you can jump straight to it, no matter which buffer you're currently on, by
pressing {c64-keys}`CTRL + 1`.

The last way is one we've already seen: the buffer viewer ({c64-keys}`C= + T`).  This is
the most general way to select the buffer you want by name.  If you haven't noticed by now,
the `H`, `J`, `K`, and `L` keys are almost always usable in addition to the cursor keys.  This is
true in the buffer viewer as well as the UDG editor and others we've yet to explore.

#### IMPLEMENTATION LOGIC

Okay, time for the exciting stuff: let's work on writing the logic that ties everything together.
Navigate to the `main.s` buffer.

First things first, we need to setup the display. The VIC registers at $9000 retain their
"cold start" defaults in Monster's virtual memory upon boot, but those are not fit for our
purposes.  Configuring the display can be thought of in two parts: the geometry/attributes, and the
matrix.

Let's begin with geometry and attributes.  This is configured by writing to the VIC registers
to achieve the desired number of rows/columns, colors, etc.  For our program, we will use
a matrix that is 12x20 with double height characters.  This arrangement allows us to create a large
"bitmap" which only uses a single page of memory for the screen matrix (each matrix position representing
16 bytes thanks to the double height characters).  This setup is commonly referred to as MINIGRAFIK.

We will first configure the screen's width and height.  Note that in `$9003`, bit 0 sets double-height
characters and bits 1-6 sets the number of character rows.  While we're here, we might as well set the
color of the border and background too (`$900f`).  Note that bit 3 must be _set_  for non-reverse
colors.

```
    lda #20        ; # columns
    sta $9002

    lda #(12*2)+1  ; (# rows << 1) | 1
    sta $9003

    lda #$08       ; black/black (no rvs)
    sta $900f
```

Great, now we need to configure the screen matrix.
As we alluded to earlier, we want to set up a sort of virtual bitmap, where each column represents one continuous
row of bytes.  With this arrangement, we can easily address a given pixel by loading a zeropage
variable with the address of the "sprite"'s x-position and then using indirect, y-indexed addressing
to specify its y-position, e.g.

```
    ldy spritey
    sta (@col),y
```

To accomplish this, we must arrange the screen matrix, which is organized row-by-row, so that the
values in each row sequentially align with the ones on the row above, e.g.

```
0 3 6
1 4 7
2 5 8
```

We will accomplish this with a nested loop that initializes the screen matrix column-by-column.
We don't explicitly track the row counter, but when we have fully initialized the screen the
`@addr` update will leave the most significant byte of that pointer at `$20`, which we can use
as our signal to stop.

```
init
    .eq @addr $f0

    ; set @addr to matrix origin ($1000)
    ldxy #$1000
    stxy @addr

@l0 ldx #0        ; row counter
:   txa
    sta (@addr),y
    inx
    cpx #20
    bne -

    ; next column
    lda @addr
    clc
    adc #$c0
    sta @addr
    bcc +
    inc @addr+1
:   lda @addr+1
    cmp #$20
    bne @l0
```

The matrix should now be established.  It lives at address `$1000` and references a custom
character set from `$1100-$1fff` (our "bitmap").  At this point, the contents of the bitmap
are yet uninitialized.  If we ran the program now, we'd see garbage strewn throughout the
display.  Let's fix that by clearing the bitmap:

```
clr
.eq @bm $f0
    ldxy #$1100
    stxy @bm

    ldy #$00
    ldx #$20-$11    ; # of pages to clear
:   sta (@bm),y
    iny
    bne -
    inc @bm+1
    dex
    bne -
```

We've already built a few logical chunks of code.  It's always a good idea to test as you
go so that you're not left trying to hunt down a bug in hundreds of lines of untested code.
Let's take a pause here and familiarize ourselves with the environment a bit more.  There's
plenty more of our program to write, but it will help if we can iteratively build up to
the final product.

#### SAVING

Before we even think about beginning debugging, we should make sure our progress is
safely stored on disk.

You should have a rough handle on saving buffers already (and the importance of doing so).
It is always a good idea to save your work before assembling.  If you have any dirty buffers,
you will be asked if you want to do so with a prompt.

You can also use the EX COMMAND `:S` to save all buffers.  The `@` suffix can be applied
to all save commands (`s` and `S`) to overwrite files that already share the buffers' names.
In most cases this will be the desired command (save everything and overwrite) and it is
also what will effectively be executed if you confirm "yes" to the prompt you're given
upon assembly:

```
:S@
```

#### ASSEMBLY

As mentioned earlier, assembly will typically take place from the top-level unit from which
all others are included (`main.s` in our case).  Navigate to that buffer and press
{c64-keys}`C= + A` to assemble it.

#### ERRORS

There's a good chance your first assembly will generate one or more errors.
If it does, they are displayed in a menu,
which is focused to allow you to select one for inspection.  Press {c64-key}`RETURN` to
navigate to the error you wish to address.  This will jump the cursor to the
file/line of the error so that you can fix it.

When you're satisfied you've fixed the error, press {c64-keys}`C= + E` to navigate to the
next error or press {c64-keys}`C= + W` to return to the error menu (this is how you re-enter any "window"
generally).  Repeat as needed until you think your program will assemble successfully.  And
then repeat as needed until it actually does.

Errors often have a cascading effect, so it's usually best to address the errors that
occurred first during assembly.

#### LOG

In addition to the error window, the log provides a chronological record of what happened
during assembly.  It will show you the order in which files were processed, errors
as they occurred, etc.  When your program is successfully assembled, it will also give you
details about the final result.

#### DEBUGGING

This debug session will be more involved than the "Hello World" one. We will cover breakpoints,
watches, and the monitor interface (which we've already touched on a bit).

As before, press {c64-keys}`C= + D` to begin the debug session.

Our program begins by configuring the screen layout. To sanity check that this looks as expected,
let's just step through all of that.  Press {c64-key}`Z` several times until the cursor is
past all the VIC writes (stores to `$90xx`).  Then press {c64-key}`SPACE` to observe the new state of
the screen post-setup.

Alternatively, you can set a breakpoint after all the setup code and TRACE ({c64-key}`T`) the program.

So far so good?  If not, you may want to enter the monitor ({c64-key}`F7`) to make sure the VIC registers are configured
as expected:

```
m $9000 $9010
```

Our setup code is very simple, so if any correction is required it ought to be a simple exercise from here.

Remember, if you need to make changes to your program at any point during the debug cycle, you must first
stop debugging ({c64-keys}`C= + X`) to return to edit mode.  When you are done with your changes, reassemble
the program ({c64-keys}`C= + A`) and try again.


Okay, however circuitous your path to get there, we are contniue our debug session post-VIC initialization.
This is where the code gets a bit more interesting.  For starters, we have control flow to initialize the screen.
And it's quite a lot of iterations this time.  Repeated stepping would be tedious here, so let's instead set a
breakpoint after the screen initialization loop and see if the outcome is as we expect.

The easiest way to inspect the output here is a tool we've yet to invoke: the MEMORY VIEWER (activated
with {c64-key}`F3`).  The memory viewer is similar to the monitor's `m` command, but it allows us to easily
scroll around through memory as we please using the usual motion keys (h/j/k/l).

Once activated, set the address to our screen matrix by pressing {c64-key}`Up-Arrow` and then entering `1000` and
{c64-key}`RETURN`.  The viewer will refresh with the contents at address `$1000` and _hopefully_ you will
see a steadily increasing (by `$0c`) array of values: `01`, `0c`, `18`, ...

If you don't, then try to see what is wrong with the pattern, hunt for any bugs in the initializatoin loop, and
fix using the usual flow.

#### WINDOW MANAGEMENT

We introduced the concept of windows earlier with the BUFFER VIEWER. The MEMORY VIEWER is another one.
A WINDOW is an interactive widget that can be invoked to allow you to do things like
view breakpoints ({c64-key}`F5`), watches ({c64-key}`F6`), enter the monitor ({c64-key}`F7`), etc.

While these behave totally differently than the BUFFER VIEWER, they all share some common functionality.
To control the window's geometry you can use the {c64-keys}`C= + J`/{c64-keys}`C= + K` keys to resize (shrink/grow),
or {c64-keys}`C= + Z` to _maximize_/_unmaximize_
{c64-keys}`C= + Q` closes the active window, and {c64-key}`RUN/STOP` leaves the selected window (without
closing it) and refocuses the editor.

Note that multiple windows may be open at once.  If the MEMORY VIEWER is active, you may still invoke
the BREAKPOINT VIEWER without closing it.  If multiple windows are active, you can cycle through them
with {c64-keys}`C= + W` (also re-enters the visible window if the editor is in focus).

Finally, all active windows can be hidden with {c64-keys}`C= + H`.  The same key-combination also unhides
them if they are already hidden.

#### EDITOR TIPS

Before we finish up our program, let's take a moment to hone our editing skills.
The `main.s` buffer is still small, but it's getting big enough that navigation by individual cursor
motion may be feeling a little cumbersome.  Fortunately Monster has many options for zipping around your
code more efficiently.  We will touch on only a few here.

To go to the _top_ of the buffer, press {c64-sequence}`GG`.  Note that this command (and some others)
waits for a second keypress (the second `G` in this case).  You can see the buffered input in
the status bar when another key is expected.

To go to the _bottom_ of the buffer, press {c64-keys}`SHIFT + G`.

Press {c64-key}`/` to open a FIND prompt.  At the prompt, enter the string to look for, then
press {c64-key}`RETURN`.  Press {c64-key}`N` to navigate to the next occurrence of the string
(assuming one is found) or {c64-keys}`SHIFT + N` to navigate to the _previous_ one.

The {c64-key}`[` and {c64-key}`]` keys navigate to the previous and next empty lines, respectively.
Empty lines therefore make useful logical divisions in your source.

Finally, a common practice will be inserting new lines above or below the current line.
From COMMAND mode you can do this by pressing {c64-key}`O` (to insert a line _below_) or {c64-keys}`SHIFT + O`
to insert one _above_ the current line.  Both commands will also enter INSERT mode so that you can
immediately begin writing your new line.

This should get you started.  See the **EDITOR** chapter for the other navigation commands if
you still find yourself frustrated at your editing/navigation speed.

#### FINISHING THE PROGRAM

We're not quite done with initialization just yet. Remember that we wish to use joystick input
to move the player sprite around the screen.  To do this we need to configure the VIAs (the Vic-20's
chips responsible for handling keyboard/joystick input, among other duties) to read the joystick.
This is as simple as our VIC initialization was.

```
```

There's a few remaining items to finish up the program.

1. read input from the joystick
2. apply the input to the "player" sprite position
3. redraw the sprite at its new position


#### SYMBOL VIEWER

It is often useful to examine the symbols defined once your program is assembled.  This is
a great way to get a sense of the program's final layout and make sure things look
as you expect.  It's also useful if you can't remember the name of one of your symbols
and need a quick refresher.  To make inspecting this state easier, Monster has a **SYMBOL VIEWER**
(activated with {c64-keys}`C= + Y`).  This viewer displays a list of all symbols defined in the
last assembly along with their addresses.  {c64-key}`F1` toggles between name and address
sorting in this view.  Press {c64-key}`RETURN` on a symbol to navigate to its
definition.

