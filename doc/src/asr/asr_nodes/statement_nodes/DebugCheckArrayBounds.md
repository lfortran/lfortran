# DebugCheckArrayBounds

Checks at run time that the shapes in an array assignment conform.

## Declaration

### Syntax

```text
DebugCheckArrayBounds(expr target, expr* components,
    bool move_allocation)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the array being assigned to. |
| `components` | the array operands of the right hand side, whose shapes must match `target`. |
| `move_allocation` | `true` when the assignment it guards moves an allocation instead of copying. |

### Return values

None.

## Description

This node is inserted by the `array_op` pass ahead of an array assignment when
bounds checking is enabled, and it is never produced by a frontend. It fails
at run time with a diagnostic naming the mismatching extents rather than
writing out of bounds.

Each element of `components` must itself be an array: the check compares the
extents of the target against the extents of the operands the assignment
reads.

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
  :move_allocation false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/debugcheckarraybounds_stmt.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [ArraySize](../expression_nodes/ArraySize.md), [ArrayBound](../expression_nodes/ArrayBound.md)
