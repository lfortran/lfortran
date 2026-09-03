# LogicalConstant

A logical literal.

## Declaration

### Syntax

```text
LogicalConstant(bool value, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `value` | `true` or `false`. |
| `type` | the logical type, which fixes the kind and therefore the storage size. |

### Return values

The value of the expression.

## Description

The default logical kind is 4, matching the default integer kind, so a logical
occupies the same storage as an integer unless another kind is asked for.

## Examples

```clojure
(LogicalConstant
  :value true
  :type (Logical
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/logical_expr.asr
:language: clojure
```

## See Also

[IntegerConstant](IntegerConstant.md), [Logical](../type_nodes/Logical.md), [LogicalBinOp](LogicalBinOp.md)
