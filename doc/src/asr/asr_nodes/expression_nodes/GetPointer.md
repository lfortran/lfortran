# GetPointer

A pointer to a target.

## Declaration

### Syntax

```text
GetPointer(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the target. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

This is what the right hand side of `p => t` becomes: it produces a pointer
value referring to the storage of `t`, which
[Associate](../statement_nodes/Associate.md) then stores in `p`.

Unlike [CLoc](CLoc.md) the result keeps the type and the shape of the target.

## Examples

```clojure
(GetPointer
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
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[Associate](../statement_nodes/Associate.md), [CLoc](CLoc.md), [PointerAssociated](PointerAssociated.md), [Pointer](../type_nodes/Pointer.md)
