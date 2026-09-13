# SizeOfType

The size in bytes of a type.

## Declaration

### Syntax

```text
SizeOfType(ttype arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the type being measured. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`c_sizeof(x)`, and the size an allocation needs. The operand is a type, not an
expression, because nothing is read: the answer depends only on the type.

## Examples

```clojure
(SizeOfType
  :arg (Integer
    :kind 4
  )
  :type (Integer
    :kind 8
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[TypeInquiry](TypeInquiry.md), [IntegerBitLen](IntegerBitLen.md), [Allocate](../statement_nodes/Allocate.md)
