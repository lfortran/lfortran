# type_parameter

A LEN parameter actual attached to a derived type instance.

## Declaration

### Syntax

```text
type_parameter = (expr? value, type_parameter_kind kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `value` | the integer actual expression for an explicit parameter, or `nil`. |
| `kind` | whether the parameter is explicit, assumed or deferred. |

## Description

[StructType](../type_nodes/StructType.md) stores these entries in the order of the definition's LEN parameters. The value is converted in semantics to the parameter's declared integer kind.
