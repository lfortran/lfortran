# UnsignedIntegerCompare

A comparison of two unsigned integers.

## Declaration

### Syntax

```text
UnsignedIntegerCompare(expr left, cmpop op, expr right, ttype type,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the comparison; see [cmpop](../enum_nodes/cmpop.md). |
| `right` | the right operand. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The comparison is unsigned: a value with its top bit set is greater than one
without, which is the opposite of the signed
[IntegerCompare](IntegerCompare.md).

## Examples

```clojure
(UnsignedIntegerCompare
  :left (Var
    :v (SymbolRef 1 "u")
  )
  :op :Gt
  :right (Var
    :v (SymbolRef 1 "v")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/unsigned_expr.asr
:language: clojure
```

## See Also

[IntegerCompare](IntegerCompare.md), [UnsignedIntegerBinOp](UnsignedIntegerBinOp.md)
