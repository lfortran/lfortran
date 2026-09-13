# OverloadedBoolOp

A logical operator overloaded for a user-defined type.

## Declaration

### Syntax

```text
OverloadedBoolOp(expr left, logicalbinop op, expr right, ttype type,
    expr? value, expr overloaded)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand, as written. |
| `op` | the operator as written; see [logicalbinop](../enum_nodes/logicalbinop.md). |
| `right` | the right operand, as written. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `overloaded` | the [FunctionCall](FunctionCall.md) that implements the operator for this type. |

### Return values

The value of the expression.

## Description

`a .and. b` where the operands are not logical. Fortran allows the logical
operators to be overloaded like any other; the node keeps the spelling and the
call that implements it.

## Examples

```clojure
(OverloadedBoolOp
  :left (Var
    :v (SymbolRef 8 "p")
  )
  :op :And
  :right (Var
    :v (SymbolRef 8 "q")
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
    :name (SymbolRef 8 "vec_and")
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

[CustomOperator](../symbol_nodes/CustomOperator.md), [LogicalBinOp](LogicalBinOp.md), [OverloadedBinOp](OverloadedBinOp.md)
