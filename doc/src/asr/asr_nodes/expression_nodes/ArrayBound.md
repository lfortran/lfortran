# ArrayBound

The lower or upper bound of a dimension of an array.

## Declaration

### Syntax

```text
ArrayBound(expr v, expr? dim, ttype type, arraybound bound,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the array. |
| `dim` | the dimension, counting from one, or `nil` for the whole shape. |
| `type` | the type of the expression. |
| `bound` | `LBound` or `UBound`; see [arraybound](../enum_nodes/arraybound.md). |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`lbound(a, d)` and `ubound(a, d)` are one node distinguished by `bound`,
because they take the same operands and differ only in which end they report.

For an array with explicit bounds the frontend folds the result. For an
allocatable or a pointer the bounds are read from the descriptor at run time.

## Examples

```clojure
(ArrayBound
  :v (Var
    :v (SymbolRef 1 "a")
  )
  :dim (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
    :kind 4
  )
  :bound :UBound
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[arraybound](../enum_nodes/arraybound.md), [ArraySize](ArraySize.md), [dimension](../helper_nodes/dimension.md)
