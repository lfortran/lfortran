# IfExp

A conditional expression.

## Declaration

### Syntax

```text
IfExp(expr test, expr body, expr orelse, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `test` | the condition, of a logical type. |
| `body` | the value when the condition is true. |
| `orelse` | the value when it is false. |
| `type` | the type of the expression. Both branches have it. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The two branches have the same type, and the type of the whole expression is
theirs. Fortran has no conditional expression before Fortran 2023;
**IfExp** is LPython's `a if c else b` and Fortran 2023's `merge`-like
conditional expression, and it is also convenient for compiler-generated code.

Unlike the [If](../statement_nodes/If.md) statement, both branches must
produce a value.

## Examples

```clojure
(IfExp
  :test (Var
    :v (SymbolRef 1 "b")
  )
  :body (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :orelse (IntegerConstant
    :n 0
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/ifexp.asr
:language: clojure
```

## See Also

[If](../statement_nodes/If.md), [Select](../statement_nodes/Select.md)
