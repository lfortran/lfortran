# enumtype

How the values of an enumeration are distributed.

## Declaration

### Syntax

```text
enumtype
    = IntegerConsecutiveFromZero
    | IntegerUnique
    | IntegerNotUnique
    | NonInteger
```

### Values

| Value | Meaning |
|----------|-------------|
| `IntegerConsecutiveFromZero` | the values are 0, 1, 2, ... in order. A backend may use one directly as an index. |
| `IntegerUnique` | integer values, all different, but not consecutive. |
| `IntegerNotUnique` | integer values, with repeats. |
| `NonInteger` | the values are not integers, so arithmetic on them is not meaningful. |

### Return values

None. An enumeration value is not evaluated.

## Description

The frontend works this out once, when it builds the
[Enum](../symbol_nodes/Enum.md), so that a later pass can tell whether a jump
table is possible without examining every enumerator.

## See Also

[Enum](../symbol_nodes/Enum.md), [EnumType](../type_nodes/EnumType.md), [EnumValue](../expression_nodes/EnumValue.md)
