# CoarrayRef

A reference to a coarray on another image.

## Declaration

### Syntax

```text
CoarrayRef(expr var, coarray_index* coindices, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `var` | the coarray being referenced. |
| `coindices` | one [coarray_index](../helper_nodes/coarray_index.md) per codimension, selecting the image. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a[2]` reads `a` on image two. The coindices select the image, and are written
in square brackets to keep them distinct from the subscripts that select an
element within one image's copy.

A reference with no coindices is not a **CoarrayRef**: it is an ordinary
[Var](Var.md) or [ArrayItem](ArrayItem.md) reading this image's copy.

## Examples

```clojure
(CoarrayRef
  :var (Var
    :v (SymbolRef 1 "a")
  )
  :coindices [
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
  ]
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarrayref.asr
:language: clojure
```

## See Also

[coarray_index](../helper_nodes/coarray_index.md), [codimension](../helper_nodes/codimension.md), [SyncAll](../statement_nodes/SyncAll.md), [Variable](../symbol_nodes/Variable.md)
