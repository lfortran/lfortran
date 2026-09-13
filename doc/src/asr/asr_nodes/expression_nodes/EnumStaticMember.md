# EnumStaticMember

An enumerator, reached through the enumeration.

## Declaration

### Syntax

```text
EnumStaticMember(expr v, symbol m, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the expression the enumeration is taken from. |
| `m` | the enumerator's [Variable](../symbol_nodes/Variable.md) symbol. |
| `type` | the type of the enumerator. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`Color.red`: the enumerator belongs to the enumeration and not to any value of
it, so nothing is read from `v`.

## Examples

```clojure
(EnumStaticMember
  :v (Var
    :v (SymbolRef 3 "c")
  )
  :m (SymbolRef 2 "red")
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/enum_expr.asr
:language: clojure
```

## See Also

[Enum](../symbol_nodes/Enum.md), [EnumValue](EnumValue.md), [EnumName](EnumName.md), [StructStaticMember](StructStaticMember.md)
