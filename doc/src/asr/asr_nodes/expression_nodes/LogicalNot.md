# LogicalNot

Logical negation.

## Declaration

### Syntax

```text
LogicalNot(expr arg, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the operand. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`.not. p`. This is not a bitwise operation:
[IntegerBitNot](IntegerBitNot.md) is what flips bits.

## Examples

```clojure
(LogicalNot
  :arg (Var
    :v (SymbolRef 1 "p")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/logical_expr.asr
:language: clojure
```

## See Also

[LogicalBinOp](LogicalBinOp.md), [IntegerBitNot](IntegerBitNot.md)
