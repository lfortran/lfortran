# ListClear

Removes every element of a list.

## Declaration

### Syntax

```text
ListClear(expr a)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |

### Return values

None.

## Description

The list is empty afterwards; its storage may be released or kept.

## Examples

```clojure
(ListClear
  :a (Var
    :v (SymbolRef 1 "a")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[ListAppend](ListAppend.md), [ListRemove](ListRemove.md), [List](../type_nodes/List.md)
