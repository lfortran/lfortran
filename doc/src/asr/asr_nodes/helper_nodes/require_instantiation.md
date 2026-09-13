# require_instantiation

A requirement a template or another requirement builds on.

## Declaration

### Syntax

```text
require_instantiation = Require(identifier name, identifier* args)
```

### Arguments

None.

### Return values

None.

## Description

There is one constructor, **Require**:

| Argument | Description |
|----------|-------------|
| `name` | the [Requirement](../symbol_nodes/Requirement.md) being reused. |
| `args` | the names passed to it, in the requirement's parameter order. |

The names need not be declared where the `Require` appears: binding them to
the parameters of the requirement is what gives them their types and the
operations required of them.

## Examples

```clojure
(Require
  :name "r"
  :args [
    "t"
    "op"
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/template.asr
:language: clojure
```

## See Also

[Requirement](../symbol_nodes/Requirement.md), [Template](../symbol_nodes/Template.md), [TypeParameter](../type_nodes/TypeParameter.md)
