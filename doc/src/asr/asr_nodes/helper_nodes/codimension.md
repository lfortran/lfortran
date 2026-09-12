# codimension

One codimension of a coarray.

## Declaration

### Syntax

```text
codimension = (expr? start, expr? end, codimension_type end_star)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `start` | the lower cobound. |
| `end` | the upper cobound, when `end_star` is `CodimensionExpr`. |
| `end_star` | whether the upper cobound is an expression or `*`; see [codimension_type](../enum_nodes/codimension_type.md). |

### Return values

None.

## Description

Unlike a [dimension](dimension.md) this holds the two bounds rather than a
bound and an extent, because the last codimension is always `*`: its extent
depends on the number of images and is only known at run time.

Codimensions live on the [Variable](../symbol_nodes/Variable.md) rather than
on the type, since they describe how copies of the object are distributed
across images and not the shape of one copy.

## Examples

```clojure
(codimension
  :start (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :end nil
  :end_star :CodimensionExpr
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarrayref.asr
:language: clojure
```

## See Also

[codimension_type](../enum_nodes/codimension_type.md), [coarray_index](coarray_index.md), [CoarrayRef](../expression_nodes/CoarrayRef.md), [Variable](../symbol_nodes/Variable.md)
