# ImpliedDoLoop

A loop that contributes several values to an array constructor or I/O list.

## Declaration

### Syntax

```text
ImpliedDoLoop(expr* values, expr var, expr start, expr end,
    expr? increment, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `values` | the expressions produced for each iteration. |
| `var` | the loop variable. |
| `start` | the first value of the loop variable. |
| `end` | the last value. |
| `increment` | the step, or `nil` for one. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`(i, i=1,3)` inside an array constructor, or inside the input or output list
of an I/O statement. It is an expression rather than a statement because it
appears where values are expected and produces a sequence of them.

The loop variable is local to the construct: it exists only while the values
are being produced.

## Examples

```clojure
(ImpliedDoLoop
  :values [
    (Var
      :v (SymbolRef 1 "i")
    )
  ]
  :var (Var
    :v (SymbolRef 1 "i")
  )
  :start (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :end (IntegerConstant
    :n 3
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :increment nil
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/implieddoloop.asr
:language: clojure
```

## See Also

[ArrayConstructor](ArrayConstructor.md), [FileWrite](../statement_nodes/FileWrite.md), [DoLoop](../statement_nodes/DoLoop.md)
