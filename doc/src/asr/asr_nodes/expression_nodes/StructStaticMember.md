# StructStaticMember

A component reached through the type rather than through an object.

## Declaration

### Syntax

```text
StructStaticMember(expr v, symbol m, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the expression the type is taken from. |
| `m` | the component's symbol. |
| `type` | the type of the component. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

LPython's class variables are shared by every instance, so reading one does
not read the object. Fortran has no such component, and the Fortran frontend
uses [StructInstanceMember](StructInstanceMember.md) throughout.

## Examples

```clojure
(StructStaticMember
  :v (Var
    :v (SymbolRef 3 "p")
  )
  :m (SymbolRef 2 "x")
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/structstaticmember.asr
:language: clojure
```

## See Also

[StructInstanceMember](StructInstanceMember.md), [Struct](../symbol_nodes/Struct.md), [EnumStaticMember](EnumStaticMember.md)
