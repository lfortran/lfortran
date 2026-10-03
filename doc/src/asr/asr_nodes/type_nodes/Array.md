# Array

An array type.

## Declaration

### Syntax

```text
Array(ttype type, dimension* dims, array_physical_type physical_type,
    memory_space memory_space)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the element type. It is never itself an array: a rank two array is one **Array** with two dimensions. |
| `dims` | one [dimension](../helper_nodes/dimension.md) per rank, giving the lower bound and the extent. Both may be `nil` when the shape is not known where the type is written. |
| `physical_type` | how the array is represented; see [array_physical_type](../enum_nodes/array_physical_type.md). |
| `memory_space` | which memory the storage lives in; see [memory_space](../enum_nodes/memory_space.md). |

### Return values

None. A type is not evaluated.

## Description

The shape is part of the type, so a procedure's dummy argument and the actual
argument passed to it may have the same element type and rank but different
`physical_type`, and the frontend inserts an
[ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md) between them.

An assumed-rank array has no dimensions at all: its `dims` is empty and its
physical type is `AssumedRankArray`. Its rank is only known at the call, and
[SelectRank](../statement_nodes/SelectRank.md) is what recovers it.

`memory_space` says which memory the elements live in. Everything on the host
is `Global`, and so is a buffer a kernel is given; the other spaces appear
only inside GPU code, where a variable may instead be private to a thread or
shared by a threadgroup. See [memory_space](../enum_nodes/memory_space.md).

## Examples

```clojure
(Array
  :type (Integer
    :kind 4
  )
  :dims [
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
  ]
  :physical_type :FixedSizeArray
  :memory_space :Global
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[dimension](../helper_nodes/dimension.md), [array_physical_type](../enum_nodes/array_physical_type.md), [memory_space](../enum_nodes/memory_space.md), [ArrayItem](../expression_nodes/ArrayItem.md), [ArrayPhysicalCast](../expression_nodes/ArrayPhysicalCast.md), [ArrayConstant](../expression_nodes/ArrayConstant.md)
