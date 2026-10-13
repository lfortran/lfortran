# common_association

The place of a variable that EQUIVALENCE puts in a COMMON block.

## Declaration

### Syntax

```text
common_association = (symbol variable, expr designator, expr anchor)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `variable` | the variable that EQUIVALENCE puts in the block. Its `common_block` is the [CommonBlock](../symbol_nodes/CommonBlock.md) view that holds this association. |
| `designator` | a designator of `variable`: the variable itself, an element of it, or a substring of either. |
| `anchor` | a designator of a member of the view, or of the `variable` of an earlier association of the view. |

### Return values

None.

## Description

The first storage unit of `designator` is the first storage unit of `anchor`.
This places all of `variable` in the block, at the position that follows from
where `anchor` is; an association whose `variable` extends past the last
member makes the block larger.

The subscripts and substring bounds of both designators are constant, as they
are in the EQUIVALENCE statement they come from, so the ASR records how every
variable of a view is placed relative to the members. The byte offsets that
follow depend on the sizes of the types on the target, and are computed when a
backend lowers the view. Fortran does not allow an EQUIVALENCE to extend a
block before its first storage unit.

## Examples

`equivalence (e(1), n)`, with `n` the first member of the block:

```clojure
(common_association
  :variable (SymbolRef 1 "e")
  :designator (ArrayItem
    :v (Var
      :v (SymbolRef 1 "e")
    )
    :args [
      (array_index
        :left nil
        :right (IntegerConstant
          :n 1
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
        :step nil
      )
    ]
    :type (Integer
      :kind 4
    )
    :storage_format :ColMajor
    :value nil
  )
  :anchor (Var
    :v (SymbolRef 1 "n")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/commonblock.asr
:language: clojure
```

## See Also

[CommonBlock](../symbol_nodes/CommonBlock.md), [Variable](../symbol_nodes/Variable.md)
