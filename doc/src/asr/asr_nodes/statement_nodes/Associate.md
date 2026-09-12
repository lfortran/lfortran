# Associate

Binds a name to a target rather than copying a value.

## Declaration

### Syntax

```text
Associate(expr target, expr value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the pointer or associate name being bound. |
| `value` | what it is bound to. |

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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [AssociateBlock](../symbol_nodes/AssociateBlock.md), [Nullify](Nullify.md), [GetPointer](../expression_nodes/GetPointer.md)
