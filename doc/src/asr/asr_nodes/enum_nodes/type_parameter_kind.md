# type_parameter_kind

The form of a LEN parameter actual.

## Declaration

### Syntax

```text
type_parameter_kind = Explicit | Assumed | Deferred
```

### Values

| Value | Meaning |
|----------|-------------|
| `Explicit` | an integer actual expression is supplied. |
| `Assumed` | an assumed LEN parameter, written `*`. |
| `Deferred` | a deferred LEN parameter, written `:`. |

## See Also

[type_parameter](../helper_nodes/type_parameter.md)
