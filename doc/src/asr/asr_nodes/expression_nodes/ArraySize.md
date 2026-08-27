# ArraySize

The number of elements of an array, or the extent of one dimension.

## Declaration

### Syntax

```text
ArraySize(expr v, expr? dim, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the array. |
| `dim` | the dimension to measure, counting from one, or `nil` for the total number of elements. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`size(a)` and `size(a, d)`. For an array whose shape is known at compile time
the frontend folds the result into `value`.

## Examples

```clojure
(ArraySize
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
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArrayBound](ArrayBound.md), [ArrayRank](ArrayRank.md), [Array](../type_nodes/Array.md)
