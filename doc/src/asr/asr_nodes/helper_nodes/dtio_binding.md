# dtio_binding

The defined output procedure that formats the items of one derived type in a
[StringFormat](../expression_nodes/StringFormat.md).

## Declaration

### Syntax

```text
dtio_binding = (symbol type_declaration, bool polymorphic, symbol procedure,
    symbol? callback)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type_declaration` | the declared type of the items, a [Struct](../symbol_nodes/Struct.md) (possibly through an [ExternalSymbol](../symbol_nodes/ExternalSymbol.md)). |
| `polymorphic` | whether the items are polymorphic (`class(T)`) rather than of type `T`. |
| `procedure` | the specific procedure a call for these items would name: the [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md) of a type-bound `write(formatted)` binding, so the call dispatches on the dynamic type, or the [Function](../symbol_nodes/Function.md) of an `interface write(formatted)`. It has the six dummy arguments of a defined output procedure. |
| `callback` | a subroutine with seven dummy arguments that calls `procedure` and that the runtime formatter calls, or `nil` until a pass creates it. |

### Return values

None.

## Description

A [StringFormat](../expression_nodes/StringFormat.md) has one
**dtio_binding** for each declared type and polymorphism among its
derived-type items that has a defined output procedure, and no two with the
same `type_declaration` and `polymorphic`. Generic resolution of a defined
output procedure depends only on the declared type of the item, so this pair
determines `procedure` for every item of the statement, including array
elements, implied-do values and components of other derived-type items.

`procedure` counts as a call in the dependencies of the enclosing procedure,
and `procedure` and `callback` must both be visible from the scope of the
expression.

## See Also

[StringFormat](../expression_nodes/StringFormat.md), [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md), [Function](../symbol_nodes/Function.md)
