# SetClear

Removes every element of a set.

## Declaration

### Syntax

```text
SetClear(expr a)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the set. |

### Return values

None.

## Description

The set is empty afterwards.

## Examples

```clojure
(SetClear
  :a (Var
    :v (SymbolRef 1 "s")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[SetInsert](SetInsert.md), [SetRemove](SetRemove.md), [Set](../type_nodes/Set.md)
