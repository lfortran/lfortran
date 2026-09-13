# access

Whether a symbol is visible outside the module that declares it.

## Declaration

### Syntax

```text
access = Public | Private
```

### Values

| Value | Meaning |
|----------|-------------|
| `Public` | visible to anything that uses the module. |
| `Private` | visible only inside the module. |

### Return values

None. An enumeration value is not evaluated.

## Description

This is the `public` and `private` attribute. It applies to the symbol, not to
the type: a private component of a public type is a
[Variable](../symbol_nodes/Variable.md) with `access=Private` inside a
[Struct](../symbol_nodes/Struct.md) with `access=Public`.

## See Also

[Variable](../symbol_nodes/Variable.md), [Function](../symbol_nodes/Function.md), [Module](../symbol_nodes/Module.md), [Struct](../symbol_nodes/Struct.md)
