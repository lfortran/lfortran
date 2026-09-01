# coarray_index

One coindex of a coarray reference.

## Declaration

### Syntax

```text
coarray_index = (expr? index, codimension_type star)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `index` | the image index. |
| `star` | whether the coindex was written as `*`; see [codimension_type](../enum_nodes/codimension_type.md). |

### Return values

None.

## Description

The coindices in `a[2]` select the image, and are kept apart from the
subscripts that select an element within one image's copy.

## Examples

```clojure
(coarray_index
  :index (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :star :CodimensionExpr
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarrayref.asr
:language: clojure
```

## See Also

[CoarrayRef](../expression_nodes/CoarrayRef.md), [codimension](codimension.md), [codimension_type](../enum_nodes/codimension_type.md)
