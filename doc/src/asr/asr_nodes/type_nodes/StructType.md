# StructType

The type of a derived type object.

## Declaration

### Syntax

```text
StructType(ttype* data_member_types, ttype* member_function_types,
    bool is_cstruct, bool is_unlimited_polymorphic)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `data_member_types` | the types of the data components, in the order the [Struct](../symbol_nodes/Struct.md) lists them in `members`. |
| `member_function_types` | the types of the type-bound procedures. |
| `is_cstruct` | `true` for a concrete derived type (`type(t)`); `false` for a polymorphic class wrapper (`class(t)`). C interoperability is recorded by the defining `Struct`'s ABI. |
| `is_unlimited_polymorphic` | `true` for `class(*)`, which may hold a value of any type. |

### Return values

None. A type is not evaluated.

## Description

The definition of a derived type is the [Struct](../symbol_nodes/Struct.md)
symbol; **StructType** is what a *variable* of that type has. It carries the
component types so that most of what a backend needs is available without
following the symbol, and the
`type_declaration` member of the [Variable](../symbol_nodes/Variable.md) is
what names the definition when it is needed.

A polymorphic variable, `class(t)`, has the **StructType** of `t`: the dynamic
type is a run time property, and
[SelectType](../statement_nodes/SelectType.md) is what examines it.
Its `is_cstruct` flag is false, whereas the defining concrete `Struct`'s
signature has the flag true. A self-referential `class(t)` component must
not make the enclosing concrete signature polymorphic.

## Examples

The following example is a polymorphic type.

```clojure
(StructType
  :data_member_types [
    (Integer
      :kind 4
    )
    (Real
      :kind 4
    )
  ]
  :member_function_types []
  :is_cstruct false
  :is_unlimited_polymorphic false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/struct_expr.asr
:language: clojure
```

## See Also

[Struct](../symbol_nodes/Struct.md), [StructInstanceMember](../expression_nodes/StructInstanceMember.md), [StructConstructor](../expression_nodes/StructConstructor.md), [UnionType](UnionType.md), [SelectType](../statement_nodes/SelectType.md)
