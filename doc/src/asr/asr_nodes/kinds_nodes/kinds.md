# kinds

The `kind` member of an intrinsic type.

## Declaration

### Syntax

```text
Integer(int kind)
UnsignedInteger(int kind)
Real(int kind)
Complex(int kind)
Logical(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind of the type, as a number of bytes. |

### Return values

None. A type is not evaluated.

## Description

The kind is part of the type, so every value in ASR has a width that is known
where it appears: there is no integer whose size is implied by context, and an
operation between two kinds is an explicit
[Cast](../expression_nodes/Cast.md) rather than a promotion a backend performs.

LFortran supports these kinds:

| Type | Kinds | Default |
|------|-------|---------|
| [Integer](../type_nodes/Integer.md) | 1 (i8), 2 (i16), 4 (i32), 8 (i64) | 4 |
| [UnsignedInteger](../type_nodes/UnsignedInteger.md) | 1, 2, 4, 8 | 4 |
| [Real](../type_nodes/Real.md) | 4 (f32), 8 (f64) | 4 |
| [Complex](../type_nodes/Complex.md) | 4 (c32), 8 (c64) | 4 |
| [String](../type_nodes/String.md) | 1 (a byte) | 1 |
| [Logical](../type_nodes/Logical.md) | 1, 2, 4 | 4 |

A [Complex](../type_nodes/Complex.md) of kind 8 holds two `real(8)` values: the
kind is the kind of each part, not the size of the pair.

The default logical kind is 4, the same as the default integer kind. That
follows both languages LFortran serves: in Fortran the "default logical kind
has the same storage size as the default integer", and in Python "Booleans are
implemented as a subclass of integers".

A kind is written in Fortran as a literal suffix, `1.0_dp`, or as a type
parameter, `real(kind=dp)`. Either way the frontend resolves it to a number
before ASR is built, so a kind in ASR is always a plain integer and never an
expression.

## Examples

A `real(8)` variable and a `real(8)` constant:

```clojure
(RealConstant
  :r 2.5
  :type (Real
    :kind 8
  )
)
```

The complete translation unit:

```{literalinclude} ../../examples/real_expr.asr
:language: clojure
```

## See Also

[ttype](../type_nodes/ttype.md), [Integer](../type_nodes/Integer.md),
[Real](../type_nodes/Real.md), [Cast](../expression_nodes/Cast.md),
[TypeInquiry](../expression_nodes/TypeInquiry.md)
