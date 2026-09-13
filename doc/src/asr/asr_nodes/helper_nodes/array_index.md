# array_index

One subscript or one range of a subscript list.

## Declaration

### Syntax

```text
array_index = (expr? left, expr? right, expr? step)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the first index of a range. |
| `right` | the last index of a range, or the subscript itself when `left` and `step` are `nil`. |
| `step` | the stride of a range. |

### Return values

None.

## Description

The same product serves an element and a section. In an
[ArrayItem](../expression_nodes/ArrayItem.md) only `right` is set, and it is
the subscript. In an [ArraySection](../expression_nodes/ArraySection.md) a
dimension with `left`, `right` and `step` is a range, while a dimension with
only `right` fixes that subscript and drops the dimension from the result.

## Examples

```clojure
(array_index
  :left nil
  :right (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :step nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArrayItem](../expression_nodes/ArrayItem.md), [ArraySection](../expression_nodes/ArraySection.md), [ListSection](../expression_nodes/ListSection.md), [dimension](dimension.md)
