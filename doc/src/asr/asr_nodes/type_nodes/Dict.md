# Dict

A mapping from keys of one type to values of another.

## Declaration

### Syntax

```text
Dict(ttype key_type, ttype value_type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `key_type` | the type of the keys. |
| `value_type` | the type of the values. |

### Return values

None. A type is not evaluated.

## Description

LPython's `dict`. Every key is distinct, and lookup by key does not depend on
the number of entries.

## Examples

```clojure
(Dict
  :key_type (Integer
    :kind 4
  )
  :value_type (Real
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/dict_expr.asr
:language: clojure
```

## See Also

[Set](Set.md), [List](List.md), [DictItem](../expression_nodes/DictItem.md), [DictConstant](../expression_nodes/DictConstant.md)
