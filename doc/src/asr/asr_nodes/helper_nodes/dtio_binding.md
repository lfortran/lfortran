# dtio_binding

The defined output procedure that formats the items of one derived type in a
[StringFormat](../expression_nodes/StringFormat.md).

## Declaration

### Syntax

```text
dtio_binding = (symbol type_declaration, bool polymorphic, symbol procedure,
    expr? callback)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type_declaration` | the declared type of the items, a [Struct](../symbol_nodes/Struct.md) (possibly through an [ExternalSymbol](../symbol_nodes/ExternalSymbol.md)). |
| `polymorphic` | whether the items are polymorphic (`class(T)`) rather than of type `T`. |
| `procedure` | the specific procedure a call for these items would name: the [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md) of a type-bound `write(formatted)` binding, so the call dispatches on the dynamic type, or the [Function](../symbol_nodes/Function.md) of an `interface write(formatted)`. It has the six dummy arguments of a defined output procedure. |
| `callback` | a procedure-value expression whose interface has seven explicit dummy arguments and no result, or `nil` until a pass creates it. Hidden string lengths and any bound environment are not explicit dummies. |

### Return values

None.

## Description

A [StringFormat](../expression_nodes/StringFormat.md) has one
**dtio_binding** for each declared type and polymorphism among its
derived-type items that has a defined output procedure, and no two with the
same `type_declaration` and `polymorphic`. Generic resolution of a defined
output procedure depends only on the declared type of the item, so this pair
determines `procedure` for every item of the statement, including array
elements and implied-do values. A component of a derived-type item that has no
defined output procedure of its own is not bound, even when the type of the
component has one.

`procedure` counts as a call in the dependencies of the enclosing procedure,
and `procedure` and `callback` must both be visible from the scope of the
expression.

The runtime receives an activation-local code/environment record for each
callback. A capturing callback explicitly forwards its environment rather
than exporting a raw external C function pointer.

## See Also

[StringFormat](../expression_nodes/StringFormat.md), [StructMethodDeclaration](../symbol_nodes/StructMethodDeclaration.md), [Function](../symbol_nodes/Function.md)
