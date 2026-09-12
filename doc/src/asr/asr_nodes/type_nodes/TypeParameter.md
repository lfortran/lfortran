# TypeParameter

A placeholder for a type inside a template.

## Declaration

### Syntax

```text
TypeParameter(identifier param)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `param` | the name of the parameter. |

### Return values

None. A type is not evaluated.

## Description

Inside a [Requirement](../symbol_nodes/Requirement.md) or a
[Template](../symbol_nodes/Template.md) the concrete types are not known yet,
and a **TypeParameter** stands in for one. Instantiation substitutes real
types for the parameters, so nothing outside a template ever holds this type.

## Examples

```clojure
(TypeParameter
  :param "t"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/requirement.asr
:language: clojure
```

## See Also

[Template](../symbol_nodes/Template.md), [Requirement](../symbol_nodes/Requirement.md), [Function](../symbol_nodes/Function.md)
