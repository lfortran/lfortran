# StructConstant

A derived type constant.

## Declaration

### Syntax

```text
StructConstant(symbol dt_sym, call_arg* args, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `dt_sym` | the [Struct](../symbol_nodes/Struct.md) the constant belongs to. |
| `args` | the component values, all of them constants. |
| `type` | the [StructType](../type_nodes/StructType.md) of the value. |

### Return values

The value of the expression.

## Description

The folded form of a [StructConstructor](StructConstructor.md), used as the
`value` of a named constant of derived type and as a default initialiser. It
has no `value` member of its own, because it is one.

## Examples

```clojure
(StructConstant
  :dt_sym (SymbolRef 3 "point")
  :args [
    (call_arg
      :value (IntegerConstant
        :n 0
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
    )
    (call_arg
      :value (RealConstant
        :r 0.0
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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/structconstant.asr
:language: clojure
```

## See Also

[StructConstructor](StructConstructor.md), [Struct](../symbol_nodes/Struct.md), [Variable](../symbol_nodes/Variable.md)
