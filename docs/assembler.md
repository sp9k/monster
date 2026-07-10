## ASSEMBLER OVERVIEW

### SYNTAX
The assembler syntax is very similar to any other major assembler.  For basic
instructions, the canonical 6502 assembly syntax is supported.  That means '$'
denotes a hex value, '#' and immediate operand, parentheses an indirect address,
etc.

### EXPRESSIONS
Operands, in addition to basic values and labels, may contain an expression,
which is evaluated at assemble-time to a value.

The supproted operators are: '+', '-', '*', and '/'. Multiplication and division
have higher precedence than addition and subtraction.

Expressions may also contain parentheses, which are evaluated as you would expect,
but note that if the entire expression is enclosed in parentheses, the
assembler will interpret this as indirect addressing. For example:

```
JMP (1+3)	; jump-indirect to the address in memory address (4)
JMP 1+3 	; jump-absolute to address 4
```

Immediate addressing and indirect addressing are mutually exclusive, so the assembler
will allow you to enclose the whole expression in parentheses for immediate expressions
prefixed with a '#' (e.g. `LDA #(2+4)`)

Labels are supported in expressions and will evaluate to their address when assembled.

```
LDA #<LABEL1
```

Hexadecimal and decimal numbers are supported.  Hexadecimal numbers must be prefixed
with a '$'.

```
LDA #(10+$20)
```

Character literals are also supported. These are represented as a character enquoted within
single quotes.

```LDA #'x'```

Character literals must contain exactly one character and always resolve to
a 1 byte value.

## FORMATTING

Spacing is not important, but instructions are auto-formatted so that they are TAB indented.
Labels and directives are, by convention, not indented. The formatter will also take care of this.

## LABELS

Labels begin with either an alpha-character or, in the case of _local_
labels, a '@' character.  They are limited to 16 characters, but it is advisable to keep them shorter (8 characters or less).
Long labels are harder to squeeze onto a line.

They are case-insensitive (`a` and `A` refer to the same label)
and their definitions may end with a colon (':') but are not required to (`A:` and `A` are both valid label definitions)

#### LOCAL LABELS

Local labels are defined by prefixing the label with a '@' symbol.  This _does_
count toward the 16 character label limit.
Local labels are valid until the next non-local label is defined as shown in
the following example.

```
PROC0:
@L0:
    DEX
    BNE L0
    RTS
PROC1:
@L0:
    DEY
    BNE L0
    RTS
```

Note that the scope of the `@L0` defined under `PROC0` is valid until the next
non-local label (`PROC1`) at which point the name is recylced and may be used
again.

Because of the way local labels are implemented they are not totally
inaccessible. They _can_ be accessed by
prepending the global label that encapsulates them.  This can be used to
emulate structural data types e.g.

```
PLAYER
@X: .db 0
@Y: .db 0

GAME:
	LDA PLAYER@X
```

#### ANONYMOUS LABELS

Anonymous labels can be declared with ':'.
Anonymous labels are useful when you need to do a short branch where
a descriptive label name isn't necessary.

A + or - character is used to reference these labels.  Pluses (+) refer
to the next _forward_ anonymous label and minuses (-) refer to the
previous _backward_ anonymous label.

for example
```
    .ORG $1000
:   JMP +       ; JMP $1003
:   JMP -       ; JMP $1003
:   JMP --      ; JMP $1003
```

Using multiple +'s or -'s will count the same number of references before landing
on the corresponding anonymous label.
for example:
```
    JMP +++
:   nop
:   nop
:   nop         ; will jump here
```

## DIRECTIVES

Directives begin with a `.` character and instead of being directly assembled,
as with an instruction, tell the assembler to generate some special code or data
based on the operands.

Some directives (`.MAC` and `.REP`) generate a variable amount of code or data based on the value
of their operands.
For these directives, the expressions used as arguments must be resolvable
in pass 1 of the assembler.  This means any labels used in the expression
must be declared before the directive.

The following example illustrates why this is necessary:

```
.REP NUM, I
  ASL
.ENDREP
.EQU NUM 5
```

Note that `NUM` is not declared until after the `.REP` directive. Because of this
the assembler does not know how many times to repeat the `ASL`. We could assume
the label is an arbitrary 16-bit value as we do with labels that are undefined
in pass 1, but any subsequent labels would have the wrong address if we guessed
any number other than 5.

---

### DIRECTIVES LIST

Below is a list of all available directives along with their usage and
examples of how to use them.


#### .DB _expression_, ..., _expression_
Defines a sequence of bytes from the comma-separated list that follows.

Examples:
 |       code        |    generated binary  |
 |-------------------|----------------------|
 | .DB $00, $01, $02 | $00 $01 $02          |
 | .DB "HI",0	     | $48 $49 $00          |

#### .DW _expression_, ..., _expression_
deines a sequence of words from the comma-separated list that      |

Examples:
 |       code        |    generated binary     |
 |-------------------|-------------------------|
 | .DW $00, $01, $02 | $00 $00 $01 $00 $02 $00 |

#### .ENDIF
Ends a .IF block

See [.IF](#if-expression)

#### .EQU _name_ _expression_

Defines a constant which may be used in expressions

```
.EQU BITMAP $1100
  LDA #$00
  STA BITMAP+20
```

#### .IF _expression_

Evaluates the expression
Conditionally assembles the lines between this directive and its matching
`.ENDIF`.

```
.IF NTSC
.EQU CYCLES_PER_LINE 65
.EQU LINES 261
.ELSE
.EQU CYCLES_PER_LINE 71
.EQU LINES 312
.ENDIF
```

#### .IFDEF _label_

Evaluates to TRUE if _label_ is defined.  This is different from .IF because
_label_ may be defined to be 0 and this will still evaluate to TRUE.
This can be useful inside macros to determine if a paramter was provided or not.

#### .INC _filename_

Includes a file at the line of the directive. The file is loaded line-by-line
from disk and assembled as if the code was copy/pasted in place of the include directive.

```
.INC "KERNAL.INC"
  LDA #$00
  JSR CHROUT
```

#### .INCBIN _filename_

Includes the binary file. The binary contents are stored at the current location
of the assembly target when this directive is encountered

```
.EQ BITMAP $1100
  LDX #$07
L0:
  LDA SPRITES,X
  STA BITMAP,X
  DEX
  BPL L0

SPRITES:
.INCBIN "SPRITES.BIN"
```

#### .MAC _name_ _param 1_, ..., _param n_

Defines a macro

```
.MAC LDXY VAL
  LDX #<VAL
  LDY #>VAL
.ENDMAC

  LDXY $1234
```

Will generate the following code:

```
  LDX #$34
  LDY #$12
```

Macro definitions begin with the `.MAC` directive followed by the name of the
macro and a comma-separated list of the parameters for the macro.

Macros are invoked with the name of the macro followed by a comma-separated
list of the parameters.

#### .ORG _expression_

Sets the address to assemble code to

```
.ORG $1000
; start up code

.ORG $2000
; main code
```

#### .RORG _expression_

Sets the address the code will run at when executed.
This is useful for code that will be relocated prior to execution.

```
.ORG $1000
.RORG $00
  ; some tight loop
  LDA #$01
  STA *+3
  LDA #$00
  STA $900F
```

Note that the `.RORG` directive must follow the `.ORG` directive in order to
avoid the virtual PC being overwritten.
`.ORG` will set the virtual PC to the same location as the physical PC.

#### .REP _expression_ [, _iterator name_]

Assembles the code between this directive and `.ENDREP` for the given number of
times.

```
.REP 3
  ASL
.ENDREP
```

Becomes

```
  ASL
  ASL
  ASL
```

An optional parameter can be given that will be assigned the value of
the current iteration of repetition during assembly.

```
.REP 5,I
   INC $F0+I
.ENDREP
```

Becomes

```
  INC $F0
  INC $F1
  INC $F2
  INC $F3
  INC $F4
```

---

### MACROS

Macros offer a conveinient way to abstract patterns that you find yourself
frequently writing.

They may be recursive as in this example:

```
.MAC LDXY VAL
	LDX VAL
	LDY VAL+1
.ENDMAC

.MAC STXY ADDR
	STX ADDR
	STY ADDR+1
.ENDMAC

.MAC SET DST, SRC
	LDXY SRC
	STXY DST
.ENDMAC
```

You may omit arguments to a macro if your macro knows how to deal with
less than the maximum number it expects as in this example:

```
.MAC SAVEBYTES A, B, C
.IFDEF A
	LDA A
	PHA
.ENDIF
.IFDEF B
	LDA B
	PHA
.ENDIF
.IFDEF C
	LDA C
	PHA
.ENDIF
.ENDMAC
```

### MACRO LIMITATIONS

There are some limitations on the number of macros and overall size of the
macros per assembly.  The source for all macros must be less than $1400 bytes.
There is also a 128 macro limit.

Each macro can be at most 16 lines or 512 bytes, whichever is lower. This restriction also applies to .REP.

Comments are excluded from the internal context buffer, so using them will not count toward the byte limit.

### OTHER LIMITATOINS/GUIDELINES

#### MEMORY USAGE

The user program may use all available memory from $00 to $7fef. The $11 bytes above $7fef are reserved
for the debugger.  If your program needs these, you can still assemble to them, but you will not be
able to use the debugger.

#### USE ANONYMOUS LABELS

Anonymous labels take up no space for the label names, only address.  Using
them is much more efficient than labels, and so this should be done for short
branches that don't require much description.  Using too many labels, in the
extreme case, can push your program over the symbol limit.

