# Associate

Binds a name to a target rather than copying a value.

## Declaration

### Syntax

```text
Associate(expr target, expr value, pointer_bounds_form form)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the pointer or associate name being bound. |
| `value` | what it is bound to. |
| `form` | whether `target` gives the bounds of a pointer, and how; see [pointer_bounds_form](../enum_nodes/pointer_bounds_form.md). |

### Return values

None.

## Description

**Associate** is pointer assignment, `p => t`, and it is what gives the names
of an [AssociateBlock](../symbol_nodes/AssociateBlock.md) their meaning. It
makes the target refer to the same storage as the value; it does not copy
anything, which is what distinguishes it from
[Assignment](Assignment.md).

`Associate` with a
[PointerNullConstant](../expression_nodes/PointerNullConstant.md) value is how
`p => null()` is represented, and [Nullify](Nullify.md) is the equivalent
statement form.

A pointer assignment with a bounds-spec-list, `p(lb:) => a`, or a
bounds-remapping-list, `p(lb:ub) => a`, has as target an
[ArraySection](../expression_nodes/ArraySection.md) of the pointer with the
lower and the upper bound of every dimension, and `form` says which of the two
it is. For a bounds-spec semantics computes the upper bounds from the extents
of the value, so the backend only lowers the bounds it is given. Every other
association has `form` `NoPointerBounds`. The verifier checks that the form is
set exactly when the target is a section of a pointer, that the bounds have no
stride, that the value is not `null()`, that the value of a bounds-spec has the
rank of the pointer and that the value of a bounds-remapping-list has rank one
or is simply contiguous (F2018 9.5.4, 10.2.2.3).

## Examples

```clojure
(Associate
  :target (Var
    :v (SymbolRef 1 "p")
  )
  :value (GetPointer
    :arg (Var
      :v (SymbolRef 1 "target")
    )
    :type (Pointer
      :type (Integer
        :kind 4
      )
    )
    :value nil
  )
  :form :NoPointerBounds
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [AssociateBlock](../symbol_nodes/AssociateBlock.md), [Nullify](Nullify.md), [GetPointer](../expression_nodes/GetPointer.md), [pointer_bounds_form](../enum_nodes/pointer_bounds_form.md)
