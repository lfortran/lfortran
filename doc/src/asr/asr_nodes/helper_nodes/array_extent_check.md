# array_extent_check

One comparison, made at run time by a
[DebugCheckArrayBounds](../statement_nodes/DebugCheckArrayBounds.md), of the
extents of two of its arrays in one dimension.

## Declaration

### Syntax

```text
array_extent_check = (int left, int right, int dim, expr left_extent, expr right_extent)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the first array: 0 for the `target` of the check, i for the i-th element of its `components`. |
| `right` | the second array, an element of `components` numbered the same way. |
| `dim` | the dimension compared, starting at 1. |
| `left_extent` | the extent of `left` in dimension `dim`, a scalar `integer(4)`. |
| `right_extent` | the extent of `right` in dimension `dim`, a scalar `integer(4)`. |

### Return values

None.

## Description

`left` is less than `right`, and `dim` is a dimension of both arrays. The
arrays are named by their positions so that a diagnostic can name and point at
them, while the extents are expressions of their own: an extent is
`ArraySize` of the array in that dimension, or, for an array constructor, the
size its type gives, since a constructor is not evaluated for its shape.

The check fails when the two extents differ. A comparison of `target` with an
operand reports a shape mismatch in the assignment, a comparison of two
operands one in a binary operation.

## Examples

```clojure
(array_extent_check
  :left 0
  :right 1
  :dim 1
  :left_extent (ArraySize
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
  :right_extent (ArraySize
    :v (Var
      :v (SymbolRef 1 "b")
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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/debugcheckarraybounds_stmt.asr
:language: clojure
```

## See Also

[DebugCheckArrayBounds](../statement_nodes/DebugCheckArrayBounds.md), [ArraySize](../expression_nodes/ArraySize.md)
