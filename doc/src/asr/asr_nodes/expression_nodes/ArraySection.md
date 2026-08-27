# ArraySection

A part of an array, with the same rank or a lower one.

## Declaration

### Syntax

```text
ArraySection(expr v, array_index* args, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the array. |
| `args` | one [array_index](../helper_nodes/array_index.md) per dimension. A dimension with `left`, `right` and `step` is a range; a dimension with only `right` is a single subscript and does not appear in the result. |
| `type` | the array type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a(1:3)` or `a(2, :)`. The result is an array, and a section that fixes a
subscript in one dimension has a lower rank than the array it came from.

A section is not necessarily contiguous, so its type usually has a descriptor
physical type: the stride has to be carried along with the data.

## Examples

```clojure
(ArraySection
  :v (Var
    :v (SymbolRef 1 "a")
  )
  :args [
    (array_index
      :left (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :right (IntegerConstant
        :n 3
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :step (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
    )
  ]
  :type (Array
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
    :physical_type :DescriptorArray
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArrayItem](ArrayItem.md), [array_index](../helper_nodes/array_index.md), [ArrayIsContiguous](ArrayIsContiguous.md), [ArrayPhysicalCast](ArrayPhysicalCast.md)
