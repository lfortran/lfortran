# Requirement

A named set of type parameters and the operations required of them.

## Declaration

### Syntax

```text
Requirement(symbol_table symtab, identifier name, identifier* args,
    require_instantiation* requires)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the requirement: the type parameters, as variables of type [TypeParameter](../type_nodes/TypeParameter.md), and the signatures required of them, as bodyless [Function](Function.md) symbols. |
| `name` | the name of the requirement. |
| `args` | the names of the parameters of the requirement, in order. Every name must be declared in `symtab`. |
| `requires` | the requirements this one builds on, each a `Require` naming a requirement and the arguments to instantiate it with. |

### Return values

None.

## Description

A **Requirement** states what a generic procedure needs from its type
parameters: which types it is generic over, and which operations must exist on
them. It plays the role of a type class in Haskell or a trait in Rust.

A requirement declares nothing concrete. Its functions have no body, and its
types are [TypeParameter](../type_nodes/TypeParameter.md) placeholders. A
[Template](Template.md) is what refers to a requirement, and instantiation is
what replaces the parameters with real types.

`requires` lets one requirement reuse another: the names it passes need not be
declared locally, since the `Require` binds them to the parameters of the
requirement being reused.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/requirement.asr
:language: clojure
```

## See Also

[Template](Template.md), [TypeParameter](../type_nodes/TypeParameter.md), [Function](Function.md)
