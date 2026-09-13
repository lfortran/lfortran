# IntegerBitLen

The number of bits in the integer type of a value.

## Declaration

### Syntax

```text
IntegerBitLen(expr a, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the value whose type is measured. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

This is `bit_size(i)`. The operand is not read: only its type matters, so the
result is a compile time constant.

## Examples

```clojure
(IntegerBitLen
  :a (Var
    :v (SymbolRef 1 "i")
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/integer_expr.asr
:language: clojure
```

## See Also

[IntegerBitNot](IntegerBitNot.md), [SizeOfType](SizeOfType.md), [TypeInquiry](TypeInquiry.md)
