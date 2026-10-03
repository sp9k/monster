# Assembler

## Assembler overview

### Syntax
The assembler syntax is very similar to any other major assembler.  For basic
instructions, the canonical 6502 assembly syntax is supported.  That means '$'
denotes a hex value, '#' an immediate operand, parentheses an indirect address,
etc.

### Format
The structure of a single assembly line is divided into 3 logical parts:

```
LABEL   INSTRUCTIONS   COMMENT
```

**LABEL** - when present, defines a symbol whose value is the address of the
program at the start of this line.  Labels are described in greater detail later
in this section.

**INSTRUCTIONS** - may be literal 6502 instructions (e.g. `LDA #$00`), assembler
directives (like `.DB $00`), or macros (e.g. `LDXY #$0000`).  Instructions are
separated by colons.  Operands (or arguments, in the case of macros) may be
literal values or expressions, which are described in the next section.

**COMMENT** - a semicolon outside a quoted string or character literal starts
a comment. Everything after it is for the coder's reference only. Punctuation
inside literals keeps its literal meaning, including parentheses, colons, and
semicolons.

Below are some examples of valid lines:

````{example}
```
lda #$00
LOOP   inc BUFFER,X  ; INCREMENT BUFFER+X
lda #$00:ldx #$80:ldy #$10
lda #$00:ldxy #$ffff
```
````

### Expressions

Operands are evaluated as expressions.  An expression may be a simple value,
such as `10` or `$1234`, or a label, in which case it resolves to that value or
the address of the label respectively.  They may also be more complex and
involve several operations performed on a mixture of labels and literal values,
for example: `SCREEN+(NUM_ROWS*2)`.

The table below shows the supported operators along with their precedence.
Operators with a _higher_ precedence are evaluated before those with a lower
one.  For example, `1+2*3` will evaluate `2*3` (6) before adding `1+6` to
produce the final result of 7.

| OPERATOR | DESCRIPTION                                                            | PRECEDENCE |
|----------|------------------------------------------------------------------------|------------|
|   `+`    | binary operator to add two values                                      |     1      |
|   `-`    | binary operator to subtract one value from another (also unary negate) |     1      |
|   `*`    | binary operator to multiply two values                                 |     2      |
|   `/`    | binary operator to divide one value by another                         |     2      |
|   `&`    | binary operator: bitwise AND of two values                             |     3      |
|   `^`    | binary operator: exclusive OR (EOR) of two values                      |     4      |
|   `\|`   | binary operator: bitwise OR of two values                              |     5      |
|   `<`    | unary operator: least significant byte of the value                    |     3      |
|   `>`    | unary operator: most significant byte of the value                     |     3      |
|   `==`   | binary operator: 1 if the two values are equal, else 0                 |     0      |
|   `!=`   | binary operator: 1 if the two values are not equal, else 0             |     0      |
|   `<`    | binary operator: 1 if the left value is less than the right, else 0    |     0      |
|   `<=`   | binary operator: 1 if the left value is not greater, else 0            |     0      |
|   `>`    | binary operator: 1 if the left value is greater than the right, else 0 |     0      |
|   `>=`   | binary operator: 1 if the left value is not less, else 0               |     0      |

Note that `<` and `>` are byte-select post-processing operators if value is expected (`LDA #<LABEL`) and
comparisons where an operator is expected (`LDA #LABEL<$100`).

Integer comparisons compare unsigned 16-bit values and produce the integer 1 (true)
or 0 (false), so they may be used anywhere a value may be.  Their precedence of
0 is lower than every other operator, meaning arithmetic on either side is always evaluated first.
For example:

````{example}
```
lda #1+1==2      ; (1+1) == 2
lda #LEVEL>=3    ; 1 if the constant LEVEL is 3 or more
.if LEVEL>=3     ; conditional assembly on constant
```
````

Expressions may also contain parentheses, which are evaluated as you would expect,
but note that if the entire expression is enclosed in parentheses, the
assembler will interpret this as indirect addressing. For example:

````{example}
```
jmp (1+3)   ; jump-indirect to the address in memory address (4)
jmp 1+3     ; jump-absolute to address 4
```
````

Immediate addressing and indirect addressing are mutually exclusive, so the assembler
will allow you to enclose the whole expression in parentheses for immediate expressions
prefixed with a '#' (e.g. `LDA #(2+4)`)

Labels are supported in expressions and will evaluate to their address when assembled.

When assembling to object files, the difference of two local labels, imported symbols, or one of each,
can be used.  Differences that cannot be resolved during assembly are deferred for the linker.
For example:

```
.import Sprites
.import EndOfSprites
.seg "CODE"
    ldx #(EndOfSprites-Sprites)
    .dw EndOfSprites-Sprites+4
```

The deferred difference may also have a constant offset (+ or -) and a byte-selector (`<` or `>`).
An error will occur if the linker expects a byte operand, but the resolved value is >255 (e.g. `lda #(sprites_end-sprites)`)

````{example}
```
lda #<LABEL1
```
````

Hexadecimal and decimal numbers are supported.  Hexadecimal numbers must be prefixed
with a '$'.

````{example}
```
lda #(10+$20)
```
````

Character literals are also supported. These are represented as a character enclosed within
single quotes.

`LDA #'x'`

Character literals must contain exactly one character and always resolve to
a 1 byte value.

Quoted strings and character literals preserve case: `'a'` is `$61`, while
`'A'` is `$41`.

## Formatting

Spacing is not important, but instructions are auto-formatted so that they are TAB indented.
Labels and directives are, by convention, not indented. The formatter will also take care of this.

## Labels

Labels begin with either an alpha-character or, in the case of _local_
labels, a '@' character.  They are limited to 16 characters, but it is advisable to keep them shorter (8 characters or less).
Long labels are harder to squeeze onto a line.

They are case-insensitive (`a` and `A` refer to the same label)
and their definitions may end with a colon (':') but are not required to (`A:` and `A` are both valid label definitions)

### Nested scopes

`.SCOPE name` and `.PROC name` open a new namespace.  All labels defined
inside the current scope are prioritized during the search for a matching label.
If none is found, enclosing scopes are searched before, finally, the root scope.
To end a scope use the `.ENDSCOPE` and `.ENDPROC` directives (depending on which
directive opened the scope).

To reference a label that is scoped, use the "dot" syntax: `screen.clear`.
When referencing symbols this way, the full path from root must be specified.

```
.seg "CODE"
.scope screen
    .eq width 40
    .proc clear
        ldx #width
        lda #$20
    @loop:
        sta $1e00-1,x
        dex
        bne @loop
        rts
    .endproc
.endscope

jsr screen.clear
```

Inside an explicit scope, `@` labels stay in that scope even when another
ordinary label is defined.

Up to 16 scopes may be open, counting procedures. Scope names must be single,
non-local identifiers.  Fully qualified names must be 255 charactersor less.

### Local labels

Local labels are defined by prefixing the label with an `@` symbol. Outside an
explicit scope, they are valid until the next non-local label, as shown in
the following example.

````{example}
```
PROC0:
@L0:
    dex
    bne @L0
    rts
PROC1:
@L0:
    dey
    bne @L0
    rts
```
````

Note that the scope of the `@L0` defined under `PROC0` is valid until the next
non-local label (`PROC1`) at which point the name is recycled and may be used
again.

Because of the way local labels are implemented they are not totally
inaccessible. They _can_ be accessed by
prepending the global label that encapsulates them.  This can be used to
emulate structural data types e.g.

````{example}
```
PLAYER
@X: .db 0
@Y: .db 0

GAME:
    lda PLAYER@X
```
````

### Anonymous labels

Anonymous labels can be declared with ':'.
Anonymous labels are useful when you need to do a short branch where
a descriptive label name isn't necessary.

A + or - character is used to reference these labels.  Pluses (+) refer
to the next _forward_ anonymous label and minuses (-) refer to the
previous _backward_ anonymous label.

for example
````{example}
```
    .org $1000
:   jmp +       ; JMP $1003
:   jmp -       ; JMP $1003
:   jmp --      ; JMP $1003
```
````

Using multiple +'s or -'s will count the same number of references before landing
on the corresponding anonymous label.
for example:
````{example}
```
    jmp +++
:   nop
:   nop
:   nop         ; will jump here
```
````

## Directives

Directives begin with a `.` character and instead of being directly assembled,
as with an instruction, tell the assembler to generate some special code or data
based on the operands.

Some directives (`.MAC` and `.REP`) generate a variable amount of code or data based on the value
of their operands.
For these directives, the expressions used as arguments must be resolvable
in pass 1 of the assembler.  This means any labels used in the expression
must be declared before the directive.

The following example illustrates why this is necessary:

````{example}
```
.rep NUM, I
    asl
.endrep
.eq NUM 5
```
````

Note that `NUM` is not declared until after the `.REP` directive. Because of this
the assembler does not know how many times to repeat the `ASL`. We could assume
the label is an arbitrary 16-bit value as we do with labels that are undefined
in pass 1, but any subsequent labels would have the wrong address if we guessed
any number other than 5.

---

### Directives list

Below is a list of all available directives along with their usage and
examples of how to use them.

#### .ALIGN

**Syntax:** `.ALIGN boundary [, fill]`

**Behavior:** Pads with 0's (or optionally a provided value) until the PC is aligned (divisible) by that
value.

````{example}
```
.align $100
CHARS

.align $1000, $ff
HIRAM
```
````

#### .BSS

**Syntax:** `.BSS "name"`

**Behavior:** Activates an absolute "BSS" segment with the given name.  All labels declared are defined as
absolute and treated as part of this segment.  For more details on segments, refer to the
linker section of the manual.

**Constraint:** BSS segments must contain only zero-value bytes.

````{example}
```
.bss "DATA"
curx    .db 0
cury    .db 0
```
````

#### .BSSZP

**Syntax:** `.BSSZP "name"`

**Behavior:** Activates a zero page "BSS" segment with the given name.  All labels declared after are defined
as zero page and treated as part of this segment.  For more details on segments, refer to the
linker section of the manual.

**Constraint:** BSS segments must contain only zero-value bytes.

````{example}
```
.bsszp "ZPCODE"
curx    .db 0
cury    .db 0
```
````

#### .DB

**Syntax:** `.DB expression [, expression ...]`

**Behavior:** Defines a sequence of bytes from the comma-separated list that follows.

````{example}
```
.db $00, $01, $02 ; $00 $01 $02
.db "HI",0        ; $48 $49 $00
.db "Hello!",0    ; $48 $65 $6c $6c $6f $21 $00
```
````

#### .DF

**Syntax:** `.DF expression [, expression ...]`

**Behavior:** Defines a sequence of five-byte CBM floating-point values.
See [CBM floating-point support](floating-point.md) for details and examples.

#### .DW

**Syntax:** `.DW expression [, expression ...]`

**Behavior:** Defines a sequence of words from the comma-separated list that follows.

````{example}
```
.dw $00, $01, $02 ; $00 $00 $01 $00 $02 $00
```
````

#### .ELSE

**Syntax:** `.ELSE`

**Behavior:** Declares an "else" clause for the open "if" one.  If the "if" condition evaluated to false, the
contents of the "else" block are assembled.

**Related:** [.IF](#if)

````{example}
```
.if NTSC
    .eq LINES 261
.else
    .eq LINES 312
.endif
```
````

#### .ENDIF

**Syntax:** `.ENDIF`

**Behavior:** Ends an `.IF` block.

**Related:** [.IF](#if)

#### .ENDMAC

**Syntax:** `.ENDMAC`

**Behavior:** Closes a macro definition.

````{example}
```
.mac ldxy A
    ldx <A
    ldy >A
.endmac
```
````

#### .ENDREP

**Syntax:** `.ENDREP`

**Behavior:** Closes a repeat block.

````{example}
```
.rep 10
    asl
.endrep
```
````

#### .EQ

**Syntax:** `.EQ name expression`

**Behavior:** Defines a constant that may be used in expressions.

````{example}
```
.eq BITMAP $1100
    lda #$00
    sta BITMAP+20
```
````

#### .EXPORT

**Syntax:** `.EXPORT name`

**Behavior:** Exports a label for use (import) by another module.  See the linker section of this
manual for more details.

An object file can export up to 64 labels.


````{example}
```
.export blit
blit
    ...
```
````

#### .IF

**Syntax:** `.IF expression`

**Behavior:** Evaluates the expression and conditionally assembles the lines
between this directive and its matching `.ENDIF`.

````{example}
```
.if NTSC
.eq CYCLES_PER_LINE 65
.eq LINES 261
.else
.eq CYCLES_PER_LINE 71
.eq LINES 312
.endif
```
````

#### .IFDEF

**Syntax:** `.IFDEF label`

**Behavior:** Evaluates to TRUE if _label_ is defined.  This is different from .IF because
_label_ may be defined to be 0 and this will still evaluate to TRUE.
This can be useful inside macros to determine if a parameter was provided or not.

#### .IMPORT

**Syntax:** `.IMPORT name`

**Behavior:** Imports a label defined (exported) by another module.  See the linker section of this
manual for more details.

Up to 512 distinct imports can be referenced by an object's emitted
code or data. Symbols that are imported but unused in the assembly code are
omitted from the object code (and thus do not count toward this limit).

````{example}
```
.import blit

    ldx #10
    ldy #20
    jsr blit
```
````

#### .IMPORTZP

**Syntax:** `.IMPORTZP name`

**Behavior:** Imports a zero page label defined (exported) by another module.  See the linker section of this
manual for more details.


````{example}
```
.importzp curx
    ldx curx
    ldy #$00
    jsr blit
```
````

#### .INC

**Syntax:** `.INC "filename"`

**Behavior:** Includes a file at the line of the directive. The file is loaded line-by-line
from disk and assembled as if the code was copy/pasted in place of the include directive.

Filenames preserve case unlike normal assembly syntax. Including a file named `Data.inc` must use the
same exact casing (`.inc "Data.inc"`).  The same is true with `.INCBIN` filenames.

````{example}
```
.inc "KERNAL.INC"
    lda #$00
    jsr CHROUT
```
````

#### .INCBIN

**Syntax:** `.INCBIN "filename"`

**Behavior:** Includes the contents of a binary file at the current assembly
target location.

````{example}
```
.eq BITMAP $1100
    ldx #$07
L0:
    lda SPRITES,X
    sta BITMAP,X
    dex
    bpl L0

SPRITES:
.incbin "SPRITES.BIN"
```
````

#### .MAC

**Syntax:** `.MAC name [parameter, ...]`

**Behavior:** Defines a macro.

````{example}
```
.mac ldxy VAL
    ldx #<VAL
    ldy #>VAL
.endmac

    ldxy $1234
```
````

Will generate the following code:

````{example}
```
    ldx #$34
    ldy #$12
```
````

Macro definitions begin with the `.MAC` directive followed by the name of the
macro and a comma-separated list of the parameters for the macro.

Macros are invoked with the name of the macro followed by a comma-separated
list of the parameters.

#### .ORG

**Syntax:** `.ORG expression`

**Behavior:** Sets the address at which subsequent code is assembled.

````{example}
```
.org $1000
; start up code

.org $2000
; main code
```
````

#### .RES

**Syntax:** `.RES count[, fill]`

**Behavior:** Reserves `count` bytes, initialized to `fill` (defaults to zero when omitted).

````{example}
```
    .res SCREEN_W * SCREEN_H
    .res $10, $ff
```
````

#### .RORG

**Syntax:** `.RORG expression`

**Behavior:** Sets the address the code will run at when executed.
This is useful for code that will be relocated prior to execution.

````{example}
```
.org $1000
.rorg $00
    ; some tight loop
    lda #$01
    sta *+3
    lda #$00
    sta $900F
```
````

**Constraint:** `.RORG` must follow `.ORG`; `.ORG` sets the virtual PC to the
same location as the physical PC.

#### .REP

**Syntax:** `.REP count [, iterator]`

**Behavior:** Assembles the code between this directive and `.ENDREP` for the given number of
times.

````{example}
```
.rep 3
    asl
.endrep
```
````

Becomes

````{example}
```
    asl
    asl
    asl
```
````

An optional parameter can be given that will be assigned the value of
the current iteration of repetition during assembly.

Iterator names are matched as complete tokens. Quoted strings and character
literals are left intact: with iterator `I`, `"I"` and `'I'` remain literals,
and names such as `INDEX` and `object.I` are separate names. Nested repeats
capture iterator values as binary integers, so inserting a larger value does
not lengthen the stored source line.

````{example}
```
.rep 5,I
    inc $F0+I
.endrep
```
````

Becomes

````{example}
```
    inc $F0
    inc $F1
    inc $F2
    inc $F3
    inc $F4
```
````

Nested `.REP` directives are also supported:

````{example}
```
.rep 2,I
.rep 5,J
        inc $F0+I*5+J
@skip:
.endrep
    asl
.endrep
```
````

Becomes:

````{example}
```
    inc $F0
    inc $F1
    inc $F2
    inc $F3
    inc $F4
    asl
    inc $F5
    inc $F6
    inc $F7
    inc $F8
    inc $F9
    asl
```
````

#### .POPSEG

**Syntax:** `.POPSEG`

`.POPSEG` restores the most recently "pushed" (see `PUSHSEG`) segment, picking
up where it left off.

#### .PUSHSEG

**Syntax:** `.PUSHSEG`

`.PUSHSEG` saves the current segment for later use (see `POPSEG`).

```
.seg "CODE"
    lda #$01
.pushseg
.seg "TABLES"
    .db $10,$20,$30
.popseg
    rts
```

#### .SEG

**Syntax:** `.SEG "name"`

**Behavior:** Activates an absolute segment with the given name.  All labels defined are treated as
absolute and considered to be part of this segment.  For more details on segments, refer to the
linker section of the manual.

````{example}
```
.seg "CODE"
    lda #$00
    sta $900f
```
````

#### .SEGZP

**Syntax:** `.SEGZP "name"`

**Behavior:** Activates a zero page segment with the given name.  All labels defined are treated as
zero page and considered to be part of this segment.  For more details on segments, refer to the
linker section of the manual.

````{example}
```
.segzp "ZPCODE"
:   asl
    asl
    bcc :-
```
````

---

### Macros

Macros offer a convenient way to abstract patterns that you find yourself
frequently writing.

Macros may invoke other macros, as in this example:

````{example}
```
.mac ldxy VAL
    ldx VAL
    ldy VAL+1
.endmac

.mac stxy ADDR
    stx ADDR
    sty ADDR+1
.endmac

.mac SET DST, SRC
    ldxy SRC
    stxy DST
.endmac
```
````


```
.mac LDXY ARG
.if .isimm(ARG)
    ldx #<(.value(ARG))
    ldy #>(.value(ARG))
.else
    ldx ARG
    ldy ARG+1
.endif
.endmac

LDXY #message
LDXY pointer
```

These operations are available in macro bodies:

| Operation       | Result                                                                              |
|-----------------|-------------------------------------------------------------------------------------|
| `.isimm(ARG)`   | 1 when the argument begins with `#`, otherwise 0                                    |
| `.present(ARG)` | 1 if a nonblank argument is given; 0 if not                                         |
| `.value(ARG)`   | The argument expression without its immediate marker (if any)                       |
| `.ident(ARG)`   | The original argument spelling (for generated labels/procedure names)               |
| `.text(ARG)`    | The argument spelling as a quoted string (if already quoted does nothing)           |

You may omit trailing arguments or leave some parameters empty (e.g. `PLOT x,,z`).
Referencing an omitted argument will generate an `INVALID MACRO ARGS` error.

```asm
.mac SAVEBYTES A, B, C
.if .present(A)
    lda A
    pha
.endif
.if .present(B)
    lda B
    pha
.endif
.if .present(C)
    lda C
    pha
.endif
.endmac

SAVEBYTES #0,,address
```

Passing a parameter as a complete argument to another macro preserves its
presence, immediate marker, original spelling, and caller binding. For example,
`INNER A,B` forwards both arguments even when `B` was omitted. Expressions such
as `INNER .value(A)+1` create a new argument.

Local symbols work the same way they do during normal assembly in macro
definitions: a `@` prefix defines a private label unique to each invocation.

You can also declare symbols with the `.local` directive for any labels that should also be
kept local to the macro definition. This directive must precede the symbol's first use.

```
.mac INCW ADDR
.local skip
    inc ADDR
    bne skip
    inc ADDR+1
skip:
.endmac
```

Use `.ident` to use the argument as a name for the expansion.
These strings can also be concatenated with `+` in `.inc` and `.incbin`.

```asm
.mac INLINE_PROC NAME
.proc .ident(NAME)
.inc "i/"+.text(NAME)+".s"
.endproc
.endmac
```

The resulting filename still obeys the existing file/debug-name limit (15
characters). A self-assembly build must stage long host paths under suitable
short disk names. Concatenation does not change directory or device handling.

#### Mutable assembly constants

`.set NAME expression` defines or updates an integer assembly-time constant.
Unlike `.eq`, these assignments occur on both passes. The expression must already be
resolved and absolute. Immutable constants and address labels cannot be overwritten.

```
.set CUR_BANK 1
.mac CALL BANK, TARGET
.if BANK==CUR_BANK
    jsr TARGET
.else
    jsr trampoline
    .db BANK
    .dw TARGET
.endif
.endmac

.set CUR_BANK 2
...
```

### Macro limitations

There are at most 128 macros and four parameters per macro. Definitions share
`$4E60` bytes. Each macro and `.REP` body must fit in a `$1000`-byte context,
including its header and parameters. This limit applies to encoded tokens
rather than source characters; comments are discarded.


### Other limitations/guidelines

#### Memory usage

The user program may use all available memory from $00 to $7fff. Addresses in the I/O range ($9800-$9fff)
are reserved for the debugger.  The I/O range is read-only while debugging.

#### Use anonymous labels

Anonymous labels take up no space for the label names, only address.  Using
them is much more efficient than labels, and so this should be done for short
branches that don't require much description.  Using too many labels, in the
extreme case, can push your program over the symbol limit.

```{toctree}
:maxdepth: 2

floating-point
```
