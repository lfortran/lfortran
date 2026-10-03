# storage_type

How a variable's storage behaves.

## Declaration

### Syntax

```text
storage_type = Default | Save | Parameter
```

### Values

| Value | Meaning |
|----------|-------------|
| `Default` | storage lasting as long as the scope it is declared in. |
| `Save` | the `save` attribute: the variable keeps its value between calls, so it is allocated statically. |
| `Parameter` | a named constant. Its `value` is required and is substituted wherever the name is used, so it needs no storage at all. |

### Return values

None. An enumeration value is not evaluated.

## Description

A `Parameter` is the only value that constrains the rest of the symbol: the
[Variable](../symbol_nodes/Variable.md) must have a folded `value`, since a
named constant with no value could not be used in a constant expression.

## See Also

[Variable](../symbol_nodes/Variable.md), [IntegerConstant](../expression_nodes/IntegerConstant.md)
