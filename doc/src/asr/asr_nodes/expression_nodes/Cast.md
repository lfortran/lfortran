# Cast

Converts a value to another type.

## Declaration

### Syntax

```text
Cast(expr arg, cast_kind kind, ttype type, expr? value, expr? dest)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the value being converted. |
| `kind` | which conversion this is; see [cast_kind](../cast_kind_nodes/cast_kind.md). |
| `type` | the type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `dest` | the destination the converted value is written into, when the conversion needs one; `nil` otherwise. |

### Return values

The value of the expression.

## Description

**Cast** changes the bits of `arg`, unlike
[ArrayPhysicalCast](ArrayPhysicalCast.md) and
[StringPhysicalCast](StringPhysicalCast.md), which only change how the same
bits are described.

Every implicit conversion Fortran performs is an explicit **Cast** in ASR. The
frontend inserts it, so a backend never has to decide whether an operand needs
converting: it lowers what it is given. If a backend appears to need a
conversion of its own, the bug is upstream in the semantics.

## Examples

```clojure
(Cast
  :arg (Var
    :v (SymbolRef 1 "i")
  )
  :kind :IntegerToReal
  :type (Real
    :kind 8
  )
  :value nil
  :dest nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cast_expr.asr
:language: clojure
```

## See Also

[cast_kind](../cast_kind_nodes/cast_kind.md), [ArrayPhysicalCast](ArrayPhysicalCast.md), [StringPhysicalCast](StringPhysicalCast.md)
