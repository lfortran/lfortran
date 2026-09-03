# StructConstructor

A derived type value built from its components.

## Declaration

### Syntax

```text
StructConstructor(symbol dt_sym, call_arg* args, ttype type,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `dt_sym` | the [Struct](../symbol_nodes/Struct.md) being constructed. |
| `args` | the component values, in the order of the type's `members`, each a [call_arg](../helper_nodes/call_arg.md). |
| `type` | the [StructType](../type_nodes/StructType.md) of the result. |
| `value` | the folded [StructConstant](StructConstant.md), when every component is constant; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`point(1, 2.0)`. The arguments are positional and in `members` order, so the
frontend has already reordered any component keywords and filled in any
default initialisers.

## Examples

```clojure
(StructConstructor
  :dt_sym (SymbolRef 3 "point")
  :args [
    (call_arg
      :value (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
    )
    (call_arg
      :value (RealConstant
        :r 2.0
        :type (Real
          :kind 4
        )
      )
    )
  ]
  :type (StructType
    :data_member_types [
      (Integer
        :kind 4
      )
      (Real
        :kind 4
      )
    ]
    :member_function_types []
    :is_cstruct false
    :is_unlimited_polymorphic false
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/struct_expr.asr
:language: clojure
```

## See Also

[StructConstant](StructConstant.md), [Struct](../symbol_nodes/Struct.md), [StructInstanceMember](StructInstanceMember.md), [StructType](../type_nodes/StructType.md)
