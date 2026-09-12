# UnionType

The type of a union object.

## Declaration

### Syntax

```text
UnionType(ttype* data_member_types)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `data_member_types` | the types of the members, in the order the [Union](../symbol_nodes/Union.md) lists them. |

### Return values

None. A type is not evaluated.

## Description

Every member starts at the same address, so the size of the type is the size
of its largest member.

## Examples

```clojure
(UnionType
  :data_member_types [
    (Integer
      :kind 4
    )
    (Real
      :kind 4
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/union_expr.asr
:language: clojure
```

## See Also

[Union](../symbol_nodes/Union.md), [UnionInstanceMember](../expression_nodes/UnionInstanceMember.md), [StructType](StructType.md)
