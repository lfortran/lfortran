# pass_attr

Whether the object is passed to a procedure pointer component, or which dummy
of a trait message is its receiver.

## Declaration

### Syntax

```text
pass_attr = NotMethod | Pass | NoPass
```

### Values

| Value | Meaning |
|----------|-------------|
| `NotMethod` | the symbol is not a procedure pointer component or a trait message receiver, so the question does not apply. |
| `Pass` | the object is passed as an argument, `pass`; for a dummy of a trait message, it is the message's receiver. |
| `NoPass` | the object is not passed, `nopass`. |

### Return values

None. An enumeration value is not evaluated.

## Description

For a `Pass` component, the `self_argument` of the
[Variable](../symbol_nodes/Variable.md) names which dummy argument the object
is passed as; with no name it is the first.

In a trait message, the dummy declared `type(itself), pass :: name` has
`pass_attr` `Pass` and no `self_argument`: it is the receiver, at any
position and with any name, and keeps its declared `intent`, including none.
A message has at most one such dummy; a message without one has no receiver
and is implemented by a `nopass` binding. A constraint's requirement keeps
the marker on the receiver it moves first, while the private view or
dictionary argument of runtime slots and adapters is `NotMethod`.

## See Also

[Variable](../symbol_nodes/Variable.md), [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md), [Struct](../symbol_nodes/Struct.md)
