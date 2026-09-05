# PointerToCPtr

A Fortran pointer converted to a C pointer.

## Declaration

### Syntax

```text
PointerToCPtr(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the pointer to convert. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The descriptor is dropped and only the address survives, which is why the
opposite direction needs a shape supplied explicitly.

## Examples

```clojure
(PointerToCPtr
  :arg (Var
    :v (SymbolRef 1 "p")
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

[CPtrToPointer](../statement_nodes/CPtrToPointer.md), [CLoc](CLoc.md), [CPtr](../type_nodes/CPtr.md)
