# UnsignedIntegerConstant

An unsigned integer literal.

## Declaration

### Syntax

```text
UnsignedIntegerConstant(int n, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `n` | the value. |
| `type` | the unsigned integer type. |

### Return values

The value of the expression.

## Description

Unsigned integers are an LFortran extension, not standard Fortran. They exist
as a separate type rather than as a flag on
[Integer](../type_nodes/Integer.md) so that every operation on them is a
distinct node and no pass can silently treat an unsigned value as signed.

## Examples

```clojure
(UnsignedIntegerConstant
  :n 7
  :type (UnsignedInteger
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/unsigned_expr.asr
:language: clojure
```

## See Also

[IntegerConstant](IntegerConstant.md), [UnsignedInteger](../type_nodes/UnsignedInteger.md), [UnsignedIntegerBinOp](UnsignedIntegerBinOp.md)
