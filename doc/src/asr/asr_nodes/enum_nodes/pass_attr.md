# pass_attr

Whether the object is passed to a procedure pointer component.

## Declaration

### Syntax

```text
pass_attr = NotMethod | Pass | NoPass
```

### Values

| Value | Meaning |
|----------|-------------|
| `NotMethod` | the symbol is not a procedure pointer component, so the question does not apply. |
| `Pass` | the object is passed as an argument, `pass`. |
| `NoPass` | the object is not passed, `nopass`. |

### Return values

None. An enumeration value is not evaluated.

## Description

For a `Pass` component, the `self_argument` of the
[Variable](../symbol_nodes/Variable.md) names which dummy argument the object
is passed as; with no name it is the first.

## See Also

[Variable](../symbol_nodes/Variable.md), [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md), [Struct](../symbol_nodes/Struct.md)
