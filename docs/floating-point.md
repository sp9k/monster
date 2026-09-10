# CBM floating-point support

The assembler evaluates CBM five-byte floating-point values using the BASIC/KERNAL ROM math routines.

## Expressions and output

```
.eq HALF .5
.eq NEG -1.5
.eq THIRD FLOAT(1)/3

.df HALF, NEG, THIRD      ; three five-byte CBM floats
.dw INT(HALF*1000)        ; 500
.db ROUND(HALF*255)       ; 128

.if HALF                 ; zero is false; every nonzero float is true
    .db 1
.endif

.if HALF >= .5
    .db 2
.endif
```

Literals accept decimal fractions as well as scientific notation: `1.5`, `.5`, `1e3`,
`1.5e-2`. Unary `+` and `-` work on values and parenthesized expressions.
Float arithmetic supports `+`, `-`, `*`, and `/`. Mixed integer/float operations
promote the integer to a float and evaluate to a float result.

The `.EQ` directive preserves the result's type. `.DF`, on the other hand, promotes
its final result if necessary and _always_ emits a 5-byte CBM float value.

Integer only operations otherwise always produce an integer result type.

```{note}
- `1/3` is zero, including in `.DF 1/3`. Use `FLOAT(1)/3` or `1.0/3`.
- `-1` wraps to 65535. Use `-1.0` or `-FLOAT(1)` for a negative float.
- `^` remains bitwise XOR, not exponentiation.
- `.` remains bitwise OR where it cannot be a decimal point. Write `3 . 14`
  for OR; `3.14` is a float. A trailing decimal point alone is not a float marker.
```

Byte/word output, addresses, counts, and bitwise operations all require a float to be
integral and within the range [0, 65535].  Fractions and negative or out-of-range values
generate an error.

## Functions

Function names are case-insensitive to the assembler.
They must be immediately followed by their opening parenthesis.  If they are not,
they will be treated as symbols (`sin` is a valid symbol name).

Functions may also be nested.

| Function   | Result                                                                 |
|------------|------------------------------------------------------------------------|
| `ABS(x)`   | Absolute value as a float                                              |
| `CEIL(x)`  | Integral float, rounded toward positive infinity                       |
| `COS(x)`  | Cosine (x is in radians)                                               |
| `EXP(x)`   | Natural exponent                                                       |
| `FLOAT(x)` | Promotes an unsigned integer or preserves a float                      |
| `FLOOR(x)` | Integral float, rounded toward negative infinity                       |
| `LOG(x)`   | Natural logarithm                                                      |
| `INT(x)`   | Unsigned integer, only if x is exactly integral and in 0..65535        |
| `SIN(x)`  | Sine (x is in radians)                                                 |
| `SQRT(x)`  | Square root                                                            |
| `ROUND(x)` | Integral float, nearest integer; halfway cases away from zero          |
| `TRUNC(x)` | Integral float, rounded toward zero                                    |

NOTE: `INT` is a custom conversion, **not** Commodore BASIC's flooring `INT` function;
use `FLOOR` for that behavior.

In general, do not depend on floats cleanly producing an integer value.
For example, use `ROUND(SQRT(9.0))`; do not depend on an approximate result being exactly integral.
