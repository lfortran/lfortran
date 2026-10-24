# DebugCheckArrayBounds

Checks at run time that the shapes in an array assignment conform.

## Declaration

### Syntax

```text
DebugCheckArrayBounds(expr target, expr* components,
    array_extent_check* checks, bool move_allocation)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the array being assigned to. |
| `components` | the array operands of the right hand side, whose shapes must match `target`. |
| `checks` | the extents compared at run time, in order (see [array_extent_check](../helper_nodes/array_extent_check.md)). |
| `move_allocation` | `true` when the assignment it guards moves an allocation instead of copying. |

### Return values

None.

## Description

This node is inserted by the `array_op` pass ahead of an array assignment when
bounds checking is enabled, and it is never produced by a frontend. It fails
at run time with a diagnostic naming the mismatching extents rather than
writing out of bounds. It is omitted when the extents of the target and of
all operands are compile-time constants and equal, since it cannot fail then.
The extents of a section are compile-time constants when the start, end and
stride of each of its subscript triplets are, as for `a(:, i)` with `a` of
constant bounds.

After it, every operand has the extent of the target in every dimension. The
element references of the loop that implements the assignment rely on that
and are marked [in_bounds](../expression_nodes/ArrayItem.md).

Each element of `components` must itself be an array: the check compares the
extents of the target against the extents of the operands the assignment
reads.

The pass that inserts the node also decides what it compares, and records it
in `checks`, so that a code generator only evaluates and compares the
extents it is given. Each operand is compared with the next one, when both
have the same rank, and then the first operand with `target`, in every
dimension the two arrays have. A dimension in which both extents are
compile-time constants and equal is left out, since its comparison cannot
fail. A comparison of two operands reports a mismatch in a binary operation,
one with `target` a mismatch in the assignment. When `target` is an
allocatable array that is not allocated, its unallocated state is reported
instead of its extents.

## Examples

```clojure
(DebugCheckArrayBounds
  :target (Var
    :v (SymbolRef 1 "a")
  )
  :components [
    (Var
      :v (SymbolRef 1 "b")
    )
  ]
  :checks [
    (array_extent_check
      :left 0
      :right 1
      :dim 1
      :left_extent (ArraySize
        :v (Var
          :v (SymbolRef 1 "a")
        )
        :dim (IntegerConstant
          :n 1
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
        :type (Integer
          :kind 4
        )
        :value nil
      )
      :right_extent (ArraySize
        :v (Var
          :v (SymbolRef 1 "b")
        )
        :dim (IntegerConstant
          :n 1
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
        :type (Integer
          :kind 4
        )
        :value nil
      )
    )
  ]
  :move_allocation false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/debugcheckarraybounds_stmt.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [array_extent_check](../helper_nodes/array_extent_check.md), [ArraySize](../expression_nodes/ArraySize.md), [ArrayBound](../expression_nodes/ArrayBound.md)
