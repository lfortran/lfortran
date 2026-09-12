# SymbolicCompare

A comparison of two symbolic expressions.

## Declaration

### Syntax

```text
SymbolicCompare(expr left, cmpop op, expr right, ttype type,
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

Symbolic expressions are LPython's interface to a computer algebra system, and
comparing two of them asks whether the expressions are equal as expressions,
not whether they have equal numeric values.

## Examples

```clojure
(SymbolicCompare
  :left (Var
    :v (SymbolRef 1 "s")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 1 "s")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/symbolic_expr.asr
:language: clojure
```

## See Also

[SymbolicExpression](../type_nodes/SymbolicExpression.md), [IntegerCompare](IntegerCompare.md)
