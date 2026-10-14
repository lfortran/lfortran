# ArrayBlockCopy

Assigns a named constant array to consecutive elements of an array, as a
single block copy.

## Declaration

### Syntax

```text
ArrayBlockCopy(expr target, expr value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | an [ArrayItem](../expression_nodes/ArrayItem.md), the first element assigned. |
| `value` | a [Var](../expression_nodes/Var.md) of a named constant (parameter) array of constant size. |

### Return values

None.

## Description

**ArrayBlockCopy** assigns the elements of `value`, in array element order, to
the element `target` and to the elements that follow it in the array element
order of its array. Its array must store its elements contiguously: it is not
a pointer, and it is an array of constant size, an explicit-shape or
assumed-size array, or an allocatable array. The elements are of the same
intrinsic numeric or logical type as `target`, so the assignment copies their
storage without conversion. As `value` is a named constant, it cannot overlap
the elements assigned.

This node is inserted by the `array_op` pass, and it is never produced by a
frontend. It replaces the loop that would assign a named constant to an array
section such as `a(:, i)` or `a(2:9, i)`, whose only subscript triplet is its
first subscript, with a unit step and constant bounds, and which has as many
elements as `value`. `array_op` inserts it only when the backend lowers it,
which the LLVM backend does with a single `memcpy`; a function with many such
assignments then compiles to straight-line code instead of one loop each.
The passes run for `--show-asr` and `--dump-all-passes` keep the loop.

With bounds checking, a backend checks the subscripts of `target` unless it is
marked [in_bounds](../expression_nodes/ArrayItem.md), and `array_op` assigns
by a block copy only when the last element assigned is known at compile time
to lie within the bounds of the array.

## Examples

```clojure
(ArrayBlockCopy
  :target (ArrayItem
    :v (Var
      :v (SymbolRef 1 "a")
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
    :type (Real
      :kind 4
    )
    :storage_format :ColMajor
    :value nil
    :in_bounds true
  )
  :value (Var
    :v (SymbolRef 1 "c")
  )
)
```

It comes from this complete ASR text document, where it assigns `c` to
`a(:, 2)`:

```{literalinclude} ../../examples/arrayblockcopy_stmt.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [ArrayItem](../expression_nodes/ArrayItem.md), [ArraySection](../expression_nodes/ArraySection.md)
