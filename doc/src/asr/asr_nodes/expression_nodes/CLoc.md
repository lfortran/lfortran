# CLoc

The C address of a target.

## Declaration

### Syntax

```text
CLoc(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the target whose address is taken. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`c_loc(x)`. The result is a [CPtr](../type_nodes/CPtr.md), which carries no
type and no shape, so nothing can be read through it until
[CPtrToPointer](../statement_nodes/CPtrToPointer.md) gives it both.

## Examples

```clojure
(CLoc
  :arg (Var
    :v (SymbolRef 1 "target")
  )
  :type (CPtr)
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[CPtr](../type_nodes/CPtr.md), [CPtrToPointer](../statement_nodes/CPtrToPointer.md), [PointerToCPtr](PointerToCPtr.md), [GetPointer](GetPointer.md)
