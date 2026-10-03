# UnionInstanceMember

A member of a union object.

## Declaration

### Syntax

```text
UnionInstanceMember(expr v, symbol m, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the union object. |
| `m` | the member's [Variable](../symbol_nodes/Variable.md) symbol. |
| `type` | the type of the member. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Every member of a union starts at the same address, so this reads the object's
storage as the member's type. Reading a member other than the one last written
is not diagnosed: the node says what to read, not whether it is meaningful.

## Examples

```clojure
(UnionInstanceMember
  :v (Var
    :v (SymbolRef 3 "w")
  )
  :m (SymbolRef 2 "i")
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/union_expr.asr
:language: clojure
```

## See Also

[Union](../symbol_nodes/Union.md), [UnionConstructor](UnionConstructor.md), [StructInstanceMember](StructInstanceMember.md)
