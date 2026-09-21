# TypeParameter

A placeholder for a type inside a template.

## Declaration

### Syntax

```text
TypeParameter(identifier param, deferred_type_attr deferred_attr, bool is_class)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `param` | the name of the parameter. |
| `deferred_attr` | whether the parameter's declaration specified `ABSTRACT`, `EXTENSIBLE`, or neither. See [deferred_type_attr](../enum_nodes/deferred_type_attr.md). |
| `is_class` | whether the entity was declared `CLASS(t)` rather than `TYPE(t)`. Instantiation gives such an entity a polymorphic type. |

### Return values

None. A type is not evaluated.

## Description

Inside a [Requirement](../symbol_nodes/Requirement.md) or a
[Template](../symbol_nodes/Template.md) the concrete types are not known yet,
and a **TypeParameter** stands in for one. Instantiation substitutes real
types for the parameters, so nothing outside a template ever holds this type.

`deferred_attr` records what the `DEFERRED TYPE` statement declared about the
parameter, and it constrains both ends of the substitution: which types may be
passed as an instantiation argument for the parameter, and how the parameter
may be used inside the template. A non-extensible parameter can only be named
by `TYPE(t)`; an abstract one only by `CLASS(t)`; an extensible one by either.

`is_class` is a property of the entity's declaration rather than of the
parameter, and it is what makes `CLASS(t)` work once `t` is substituted.
Without it the entity would become a non-polymorphic variable of what may be
an abstract type, which the verifier rejects; the instantiation pass reads the
flag and makes the substituted [StructType](StructType.md) polymorphic exactly
when it is set.

## Examples

```clojure
(TypeParameter
  :param "t"
  :deferred_attr :NonExtensible
  :is_class false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/requirement.asr
:language: clojure
```

## See Also

[Template](../symbol_nodes/Template.md), [Requirement](../symbol_nodes/Requirement.md), [Function](../symbol_nodes/Function.md), [deferred_type_attr](../enum_nodes/deferred_type_attr.md)
