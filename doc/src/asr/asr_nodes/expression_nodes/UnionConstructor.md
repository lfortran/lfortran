# UnionConstructor

A union value built from one of its members.

## Declaration

### Syntax

```text
UnionConstructor(symbol dt_sym, expr* args, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `dt_sym` | the [Union](../symbol_nodes/Union.md) being constructed. |
| `args` | the value of the member being set. |
| `type` | the [UnionType](../type_nodes/UnionType.md) of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Only one member of a union holds a value at a time, so a constructor takes one
argument: the member being set.

## Examples

```clojure
(UnionConstructor
  :dt_sym (SymbolRef 3 "word")
  :args [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  ]
  :type (UnionType
    :data_member_types [
      (Integer
        :kind 4
      )
      (Real
        :kind 4
      )
    ]
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/union_expr.asr
:language: clojure
```

## See Also

[Union](../symbol_nodes/Union.md), [UnionInstanceMember](UnionInstanceMember.md), [UnionType](../type_nodes/UnionType.md)
