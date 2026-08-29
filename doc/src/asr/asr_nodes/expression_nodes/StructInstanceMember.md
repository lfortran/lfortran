# StructInstanceMember

A component of a derived type object.

## Declaration

### Syntax

```text
StructInstanceMember(expr v, symbol m, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the object. |
| `m` | the component's [Variable](../symbol_nodes/Variable.md) symbol, owned by the type's symbol table. |
| `type` | the type of the component. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`p%x`. The component is named by its symbol rather than by a string, so a
rename or a shadowed name cannot make the reference ambiguous.

Nested components are nested nodes: `a%b%c` is a **StructInstanceMember** of a
**StructInstanceMember**.

## Examples

```clojure
(StructInstanceMember
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

```{literalinclude} ../../examples/struct_expr.asr
:language: clojure
```

## See Also

[StructStaticMember](StructStaticMember.md), [Struct](../symbol_nodes/Struct.md), [Var](Var.md), [UnionInstanceMember](UnionInstanceMember.md)
