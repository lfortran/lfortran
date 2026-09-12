# string_length_kind

How the length of a string is determined.

## Declaration

### Syntax

```text
string_length_kind
    = AssumedLength
    | DeferredLength
    | ExpressionLength
    | ImplicitLength
```

### Values

| Value | Meaning |
|----------|-------------|
| `AssumedLength` | `character(len=*)`: a dummy argument takes the length of the actual argument. |
| `DeferredLength` | `character(len=:)`: the length is fixed when the object is allocated or assigned. |
| `ExpressionLength` | the length is the expression in the `len` member, which may be a constant or computed at run time. |
| `ImplicitLength` | the length follows from the operation that produced the value rather than from a declaration. It is what the result of a [StringPhysicalCast](../expression_nodes/StringPhysicalCast.md) must use. |

### Return values

None. An enumeration value is not evaluated.

## Description

`len` and `len_kind` go together: `ExpressionLength` requires a `len`, and the
other three leave it `nil`.

## See Also

[String](../type_nodes/String.md), [StringPhysicalCast](../expression_nodes/StringPhysicalCast.md), [StringLen](../expression_nodes/StringLen.md)
