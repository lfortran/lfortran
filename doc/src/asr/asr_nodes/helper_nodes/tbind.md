# tbind

A binding label for another language.

## Declaration

### Syntax

```text
tbind = Bind(string lang, string name)
```

### Arguments

None.

### Return values

None.

## Description

There is one constructor, **Bind**:

| Argument | Description |
|----------|-------------|
| `lang` | the language being bound to, such as `c`. |
| `name` | the name the symbol has in that language. |

Nothing in ASR refers to this type today: `bind(c, name=...)` is recorded in
the `bindc_name` member of
[Variable](../symbol_nodes/Variable.md) and
[FunctionType](../type_nodes/FunctionType.md), and the ABI in their `abi`
member.

## See Also

[abi](../enum_nodes/abi.md), [FunctionType](../type_nodes/FunctionType.md), [Variable](../symbol_nodes/Variable.md)
