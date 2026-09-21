# deferred_type_attr

Whether a deferred type may be extended, and whether it is abstract.

## Declaration

### Syntax

```text
deferred_type_attr = NonExtensible | Extensible | Abstract
```

### Values

| Value | Meaning |
|----------|-------------|
| `NonExtensible` | the type is neither extensible nor abstract. An instantiation argument for it may be any non-extensible type, and only `TYPE(t)` can declare an entity of it. |
| `Extensible` | the type is extensible: it may be extended, so `CLASS(t)` can declare a polymorphic entity of it. An instantiation argument for it shall be an extensible, non-abstract type. |
| `Abstract` | the type is abstract, and therefore also extensible. `TYPE(t)` cannot declare an entity of it, only `CLASS(t)` can. An instantiation argument for it shall be an abstract type. |

### Return values

None. An enumeration value is not evaluated.

## Description

A deferred type is declared by a `DEFERRED TYPE` statement, whose
`deferred-type-attr-list` (R1617 of the Fortran 2028 working draft) carries at
most one of `ABSTRACT` and `EXTENSIBLE`. `NonExtensible` is the state of a
declaration that names neither.

The three values are an enumeration rather than two independent flags because
an abstract type implicitly has the EXTENSIBLE attribute (16.4.1.2), and
C1614 forbids a declaration from specifying both. A type that is abstract but
not extensible, and a type that is both, are therefore states that cannot
arise — and with a single enumeration they cannot be represented either, so
C1614 needs no check once the attribute list has been read.

The attribute lives on the [TypeParameter](../type_nodes/TypeParameter.md)
type rather than on the [Variable](../symbol_nodes/Variable.md) that declares
it, so that it survives a module file: an `INSTANTIATE` of a template that
another module defines can still check its instantiation arguments against it.

## See Also

[TypeParameter](../type_nodes/TypeParameter.md), [Template](../symbol_nodes/Template.md), [Requirement](../symbol_nodes/Requirement.md)
