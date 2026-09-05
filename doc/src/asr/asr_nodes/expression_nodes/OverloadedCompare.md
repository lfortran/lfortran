# OverloadedCompare

A comparison operator overloaded for a user-defined type.

## Declaration

### Syntax

```text
OverloadedCompare(expr left, cmpop op, expr right, ttype type,
    expr? value, expr overloaded)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand, as written. |
| `op` | the comparison as written; see [cmpop](../enum_nodes/cmpop.md). |
| `right` | the right operand, as written. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `overloaded` | the [FunctionCall](FunctionCall.md) that implements the operator for this type. |

### Return values

The value of the expression.

## Description

The comparison as written, together with the call that implements it. The
result type is whatever the procedure returns, which is normally logical but
is not required to be.

## Examples

```clojure
(OverloadedCompare
  :left (Var
    :v (SymbolRef 8 "p")
  )
  :op :Eq
  :right (Var
    :v (SymbolRef 8 "q")
  )
  :type (Logical
    :kind 4
  )
  :value nil
  :overloaded (FunctionCall
    :name (SymbolRef 8 "vec_eq")
    :original_name nil
    :args [
      (call_arg
        :value (Var
          :v (SymbolRef 8 "p")
        )
      )
      (call_arg
        :value (Var
          :v (SymbolRef 8 "q")
        )
      )
    ]
    :type (Logical
      :kind 4
    )
    :value nil
    :dt nil
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/overloaded_expr.asr
:language: clojure
```

## See Also

[CustomOperator](../symbol_nodes/CustomOperator.md), [OverloadedBinOp](OverloadedBinOp.md), [IntegerCompare](IntegerCompare.md)
