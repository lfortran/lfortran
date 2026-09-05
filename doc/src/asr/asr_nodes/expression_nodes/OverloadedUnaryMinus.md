# OverloadedUnaryMinus

Unary minus overloaded for a user-defined type.

## Declaration

### Syntax

```text
OverloadedUnaryMinus(expr arg, ttype type, expr? value,
    expr overloaded)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the operand, as written. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `overloaded` | the [FunctionCall](FunctionCall.md) that implements the operator for this type. |

### Return values

The value of the expression.

## Description

`-a` where `a` is of a derived type, with the call that implements it in
`overloaded`.

## Examples

```clojure
(OverloadedUnaryMinus
  :arg (Var
    :v (SymbolRef 8 "p")
  )
  :type (StructType
    :data_member_types [
      (Real
        :kind 4
      )
    ]
    :member_function_types []
    :is_cstruct false
    :is_unlimited_polymorphic false
  )
  :value nil
  :overloaded (FunctionCall
    :name (SymbolRef 8 "vec_neg")
    :original_name nil
    :args [
      (call_arg
        :value (Var
          :v (SymbolRef 8 "p")
        )
      )
    ]
    :type (StructType
      :data_member_types [
        (Real
          :kind 4
        )
      ]
      :member_function_types []
      :is_cstruct false
      :is_unlimited_polymorphic false
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

[CustomOperator](../symbol_nodes/CustomOperator.md), [IntegerUnaryMinus](IntegerUnaryMinus.md), [OverloadedBinOp](OverloadedBinOp.md)
