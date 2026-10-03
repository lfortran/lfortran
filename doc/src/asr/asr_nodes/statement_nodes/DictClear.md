# DictClear

Removes every entry of a dictionary.

## Declaration

### Syntax

```text
DictClear(expr a)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the dictionary. |

### Return values

None.

## Description

The dictionary is empty afterwards.

## Examples

```clojure
(DictClear
  :a (Var
    :v (SymbolRef 1 "d")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/dict_expr.asr
:language: clojure
```

## See Also

[DictInsert](DictInsert.md), [DictPop](../expression_nodes/DictPop.md), [Dict](../type_nodes/Dict.md)
