# dimension

One dimension of an array type.

## Declaration

### Syntax

```text
dimension = (expr? start, expr? length)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `start` | the lower bound, or `nil` when it is not known where the type is written. |
| `length` | the extent, the number of elements, or `nil`. |

### Return values

None.

## Description

A dimension stores the lower bound and the *extent*, not the lower and upper
bounds. The upper bound is `start + length - 1`, and
[ArrayBound](../expression_nodes/ArrayBound.md) is what computes it, so that
an array whose bounds are not one-based needs no special case anywhere else.

Both members are `nil` for a deferred shape, as in an allocatable or a pointer
declared `a(:)`, and for an assumed shape dummy argument. The shape then comes
from the descriptor at run time.

## Examples

```clojure
(dimension
  :start (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :length (IntegerConstant
    :n 3
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[Array](../type_nodes/Array.md), [ArrayBound](../expression_nodes/ArrayBound.md), [ArraySize](../expression_nodes/ArraySize.md), [alloc_arg](alloc_arg.md)
