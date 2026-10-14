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
set `in_bounds` when the subscript cannot be out of bounds, and the check is
then omitted. The frontend sets it when every subscript is a compile-time
constant and the array, neither allocatable nor a pointer, has compile-time
constant bounds in every dimension, as for `a(2, 3)` with `a(8, 4)`; a
constant subscript outside such bounds is a compile-time error. A reference
into an assumed-shape, assumed-size, allocatable or pointer array, or with a
subscript that is not a constant, is still checked. The `array_op` pass sets it in
the loops it creates for an array assignment: for the operand whose bounds
the loops run over, and for every operand whose shape a preceding
[DebugCheckArrayBounds](../statement_nodes/DebugCheckArrayBounds.md) compares
with the target, or whose extents are compile-time constants equal to those
of the target (the check is then omitted). With a runtime check the claim
holds only because that check stops the program on a mismatch, so a backend
that honors `in_bounds` must also implement `DebugCheckArrayBounds`.

A section target such as `a(:, i)` is assigned through its base array, with
the loops running over its subscript triplets. Its element references are
marked when the end of every triplet is known to lie within the bounds of
`a`, for example a bound of `a` itself, and the subscripts of the first
element, the starts of the triplets and the scalar subscripts, are known to
as well. With bounds checking, `array_op` checks the latter once instead,
by referencing that element before the loops if the section is not empty.

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
