# OverloadedStringConcat

Concatenation overloaded for a user-defined type.

## Declaration

### Syntax

```text
OverloadedStringConcat(expr left, expr right, ttype type, expr? value,
    expr overloaded)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand, as written. |
| `right` | the right operand, as written. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `overloaded` | the [FunctionCall](FunctionCall.md) that implements the operator for this type. |

### Return values

The value of the expression.

## Description

`a // b` where at least one operand is not a string, with the call that
implements it in `overloaded`.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/overloaded_expr.asr
:language: clojure
```

## See Also

[CustomOperator](../symbol_nodes/CustomOperator.md), [StringConcat](StringConcat.md), [OverloadedBinOp](OverloadedBinOp.md)
