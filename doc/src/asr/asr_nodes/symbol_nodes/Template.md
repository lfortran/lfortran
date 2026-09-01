# Template

A generic program unit, parameterised by types and operations.

## Declaration

### Syntax

```text
Template(symbol_table symtab, identifier name, identifier* args,
    require_instantiation* requires)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the template: its type parameters, the operations it requires, and the generic procedures it defines. |
| `name` | the name of the template. |
| `args` | the names of the parameters of the template, in order. |
| `requires` | the requirements the parameters must satisfy, each a `Require`. |

### Return values

None.

## Description

A **Template** holds procedures written against type parameters instead of
concrete types. It is not code: nothing in a template can be called or
compiled directly, because the types are not known yet.

`instantiate` produces the code. Instantiating a template substitutes concrete
types and procedures for `args`, checks them against `requires`, and copies the
procedures of the template into the instantiating scope with the substitution
applied. Those copies are ordinary [Function](Function.md) symbols, so
everything downstream of instantiation sees ordinary ASR.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/template.asr
:language: clojure
```

## See Also

[Requirement](Requirement.md), [TypeParameter](../type_nodes/TypeParameter.md), [Function](Function.md)
