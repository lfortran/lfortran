# ArrayItem

One element of an array.

## Declaration

### Syntax

```text
ArrayItem(expr v, array_index* args, ttype type,
    arraystorage storage_format, expr? value, bool in_bounds)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the array. |
| `args` | one [array_index](../helper_nodes/array_index.md) per dimension, each with only `right` set to the subscript. |
| `type` | the type of the element. |
| `storage_format` | the element order of the array; see [arraystorage](../enum_nodes/arraystorage.md). |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `in_bounds` | `true` when every subscript is known to be within the bounds of `v`, so a backend need not check it at runtime; `false` otherwise. |

### Return values

The value of the expression.

## Description

`a(i, j)`. The result is a scalar of the element type, which is what
distinguishes it from [ArraySection](ArraySection.md): a section keeps the
array type.

Subscripts are as written, so an array declared `a(2:5)` is indexed from two.
A backend subtracts the lower bound itself, using the `dims` of the array
type.

With runtime bounds checking, a backend checks every subscript of an element
reference against the bounds of `v`. A pass that creates the reference can
set `in_bounds` when the subscript cannot be out of bounds, for example in a
loop it creates over the bounds of `v` itself, and the check is then omitted.
The frontend always sets `false`.

## Examples

```clojure
(ArrayItem
  :v (Var
    :v (SymbolRef 1 "a")
  )
  :args [
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
  ]
  :type (Integer
    :kind 4
  )
  :storage_format :ColMajor
  :value nil
  :in_bounds false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArraySection](ArraySection.md), [array_index](../helper_nodes/array_index.md), [ArrayBound](ArrayBound.md), [Array](../type_nodes/Array.md)
