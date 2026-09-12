# NamedExpr

An assignment that is also an expression.

## Declaration

### Syntax

```text
NamedExpr(expr target, expr value, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | the variable assigned to. |
| `value` | the value assigned, which is also the value of the expression. |
| `type` | the type of the expression. |

### Return values

The value of the expression.

## Description

This is Python's `x := v`. It has no Fortran spelling. The value is stored in
`target` and is also the result of the expression, so it can be used where a
value is expected.

## Examples

```clojure
(NamedExpr
  :target (Var
    :v (SymbolRef 1 "i")
  )
  :value (IntegerConstant
    :n 5
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/namedexpr.asr
:language: clojure
```

## See Also

[Assignment](../statement_nodes/Assignment.md), [Expr](../statement_nodes/Expr.md)
