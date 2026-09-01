### TUTORIAL

I hope you're feeling excited and inspired from our adventure writing "Hello World" because
we will now walk through a much more substantial project.  The goal of this is to build
something that really familiarizes you with the multitude of powerful features Monster provides.

By the end of this tutorial we will have a smoothly moving character that can run from side to side
and jump under joystick control.

#### MAIN

This project will span multiple files, but in IMMEDIATE operation, Monster always assembles from the
active source file.  For us, that will be a `main.s` file.  All other files will be _included_ from
this one (more on that when we get to it).

If you still have buffers open from your past work, close all of them with {c64-keys}`C= + X`.  You should only have one buffer left once all the others are closed.  Press {c64-keys}`C= + T` to see the
active buffers (you should only see our lone, yet unnamed, buffer).  Press {c64-key}`RUN/STOP` to leave the buffers view.

Now rename the buffer by entering EX COMMAND mode ({c64-key}`:`) and typing `r main.s` at the prompt.

Let's set the origin of this program to `$1000`.

```
	.org $1000
```

Since this program will be a bit more substantial, we will want to leverage Monster's macro
capabilities a bit.  A good organizational practice for this is to have a single "macros" file
that you include at the top of your "main" assembly file (`main.s` for us).

To create a new buffer, press {c64-keys}`C= + N`.  This will open a a new unnamed buffer.  Press
{c64-key}`F5` and you should see there are now two buffers: `main.s` and our new unnamed one.

#### MACROS

Let's call this new file `macros.inc`, rename it to this using the `r` EX COMMAND.
The `.inc` suffix tells us this is an _include_ file.
Monster doesn't care what suffix you use in most cases, but avoid `.o`, which is
reserved for use by the linker.

Macro use is very much a matter of personal taste.  I avoid heavy macro use as it can obscure
potential optimizations, which is half the fun of handwriting assembly, but there are some
simple ones that make life a little bit easier without hiding much from the user.

For this project, we'll define two macros to treat the index registers `X` and `Y` like a single
16 bit value:

```
.mac ldxy_i val
	ldx #<ldx
	ldy #>ldy
.endmac

.mac stxy addr
	stx addr
	sty addr+1
.endmac
```

By now hopefully you're getting a sense of Monster's autoformatting and syntax checking.
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
Vic-20's own character set.

To do this we will dip our toes in one of Monster's powerful utilities: the **MONITOR**.
Activate the monitor with the {c64-key}`f7` key.  A window will appear in which text commands
be input.  The character set we wish to base our design off of lives at address `$800` in
the Vic-20's ROM.  Run the following command to take a peek at the memory there:

```
$m $8000
```

Pretty neat, but not too helpful in producing a usable character set.  A couple modifications
to our command will change that.  First, we must understand the `>` operator available
in the monitor.  When appended to a command, the output from the command will be _redirected_
to whatever filename follows.

The other thing to understand is that commands like `m` (the VIEW MEMORY command) take an
optional second parameter.  In this case, it defines the address to stop displaying memory
at.  With these two things in mind, we can save off the whole range from `$8000`-`$83ff` (one
of the Vic-20's character sets) to a file for our own repurposing.

```
$m $8000 $83ff > chars.s
```

Exit the monitor now by running the `x` command:

```
$x
```

to return to the editor. Now pop open the directory viewer and you should see the file we wrote: `chars.s`.
Navigate to it and press {c64-key}`RETURN`.  Once it loads you should see a wall of `.db`
directives.  Remember from our "Hello World" example that these define a list of raw byte values.

Now, move the cursor to any row of `.db`'s and press {c64-keys}`C= + U` to bring up the **UDG EDITOR**.
This will show you an 8x8 representation of the VIC's interpretation of the character data
represented by the row you activated the editor on.

Feel free to play around with all the other characters in the set.  You can always regenerate
the whole set with the same command we used to get the character set in the first place.

This is enough here for now.  We will return when our code is ready to make use of the custom
character set and when we're feeling sufficiently inspired.

#### BUFFER SWITCHING

At this point we have at least 3 buffers open (perhaps more if you got curious).  There are
several ways to move between them and this will be a frequent part of our workflow, so it's
worth taking a moment to get a handle on them.

{c64-keys}`CTRL + H` navigates to the _previous_ buffer and {c64-keys}`CTRL + L` navigates to the
_next_ one.  Go back and forth between your buffers with these keys to get a feel for this.

You may have noticed a number to the left of your buffers' names.  This is the buffer's "ID" but,
more importantly, it is a handle for quick navigation to it.  If your `main.s` buffer has ID `1`,
for example, you can jump straight to it, no matter which buffer you're currently on, by
pressing {c64-keys}`C= + 1`.

The last way is one we've already seen: the buffer viewer ({c64-keys}`C= + T`).  This is
the most general way to select the buffer you want by name.  If you haven't noticed by now,
the h/j/k/l keys are almost always usable in addition to the cursor keys.  This is true in the
buffer viewer as well as the UDG editor and others we've yet to explore.

#### IMPLEMENTATION LOGIC

Okay, time for the exciting stuff: let's work on writing the logic that ties everything together.
Navigate to the `main.s` buffer.

#### BUFFER NAVIGATION

Our buffer is getting big enough that navigation by individual cursor motion is probably
feeling a little cumbersome.  Fortunately Monster has many options for zipping around your
code more efficiently.  We will touch on only a few here.

To go to the _top_ of the buffer, press {c64-key}`G`{c64-key}`G`. Note that this command (and some others)
waits for second key (the second 'g' in this case).  You can see the buffered input in
the status bar when another key is expected.

To go to the _bottom_ of the buffer, press {c64-keys}`SHIFT + G`.

Press {c64-key}`/` to open a FIND prompt.  At the prompt, enter the string to look for then
press {c64-key}`RETURN`.  Press {c64-key}`N` to navigate to the next occurrence of the string
(assuming one is found) or {c64-key}`SHIFT + N` to navigate to the _previous_ one.

The {c64-key}`[`/{c64-key}`]` keys navigate to the previous/next (respectively) empty line.
This is useful for jumping to logical breaks in the code.  Leave empty lines where such breaks
make logical sense to make effective use of this.

This should get you started, but the **EDITOR** portion of this manual, you can see the
many other navigation options available.

#### ASSEMBLY

Everything is in place, it's time to start the assembly/debug/test loop.  Realistically, this
is where you will spend most of your time.

As mentioned before, assembly will typically take place from the top level unit from which
all others are included (`main.s` in our case).  Navigate to that buffer and press
{c64-keys}`C= + A` to assemble it.

More than likely you will have _some_ error(s).  If you do, they are displayed in a menu,
which is focused to allow you to select one for inspection.  Press {c64-key}`RETURN` to
navigate to the error you wish to address.  This will jump the cursor to the
file/line of the error so that you can fix it.

When you're satisfied you've fixed the error press {c64-keys}`C= + E` to navigate to the
next error or press {c64-keys}`C= + W` to return to the error menu (this is how you re-enter any "window"
generally).  Repeat as needed until you think your program will assemble successfully.  And
then repeat as needed until it actually does.

Errors often have a cascading affect, so it's usually best to address the errors that
occurred first during assembly.

#### SYMBOL VIEWER

It is often useful to examine the symbols defined once your program is assembled.  This is
a great way to get a sense of the program's final layout and make sure things look
as you expect.  It's also useful if you can't remember the name of one or your symbols
and need a quick refresher.  To make inspecting this state easier, Monster has a **SYMBOL VIEWER**
(activated with {c64-keys}`C= + Y`).  This viewer displays a list of all symbols defined in the
last assembly along with their addresses.A  {c64-key}`F1` toggles between alphanumeric
and by-address sorting in this view.  Press {c64-key}`RETURN` on a symbol to navigate to its
definition.

####
