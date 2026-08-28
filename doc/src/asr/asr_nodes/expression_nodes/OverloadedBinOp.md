# OverloadedBinOp

A binary operator overloaded for a user-defined type.

## Declaration

### Syntax

```text
OverloadedBinOp(expr left, binop op, expr right, ttype type,
    expr? value, expr overloaded)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand, as written. |
| `op` | the operator as written; see [binop](../enum_nodes/binop.md). |
| `right` | the right operand, as written. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `overloaded` | the [FunctionCall](FunctionCall.md) that implements the operator for this type. |

### Return values

The value of the expression.

## Description

`a + b` where the operands are of a derived type is a call to the procedure
declared in `interface operator(+)`. The call is in `overloaded`, and a
backend lowers that member and ignores the rest.

The operands and the operator are kept so that the node still says what was
written. A pass that understands the operator can then work with it directly,
and diagnostics can name the operator rather than the procedure.

## Examples

```clojure
(OverloadedBinOp
  :left (Var
    :v (SymbolRef 8 "p")
  )
  :op :Add
  :right (Var
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
    :name (SymbolRef 8 "vec_add")
    :original_name nil
    :args [
      (call_arg
        :value (Var
          :v (SymbolRef 8 "p")
        )
      )
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

[CustomOperator](../symbol_nodes/CustomOperator.md), [OverloadedCompare](OverloadedCompare.md), [IntegerBinOp](IntegerBinOp.md), [FunctionCall](FunctionCall.md)
