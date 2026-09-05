# attribute

A named attribute with arguments.

## Declaration

### Syntax

```text
attribute = Attribute(identifier name, attribute_arg *args)
attribute_arg = (identifier arg)
```

### Arguments

None.

### Return values

None.

## Description

There is one constructor, **Attribute**:

| Argument | Description |
|----------|-------------|
| `name` | the name of the attribute. |
| `args` | its arguments, each an **attribute_arg** holding one identifier. |

Nothing in ASR refers to this type today: the attributes a declaration carries
are members of [Variable](../symbol_nodes/Variable.md) and of the other
symbols, so that a pass reads a named member rather than searching a list. It
is kept for attributes that have no member of their own yet.

## See Also

[Variable](../symbol_nodes/Variable.md), [Function](../symbol_nodes/Function.md)
