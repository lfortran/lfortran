# ArrayConstructor

An array value built from a list of expressions.

## Declaration

### Syntax

```text
ArrayConstructor(expr* args, ttype type, expr? value,
    arraystorage storage_format, expr? struct_var)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `args` | the elements, in order. An element may be an [ImpliedDoLoop](ImpliedDoLoop.md), which contributes several values. |
| `type` | the array type of the result. |
| `value` | the folded [ArrayConstant](ArrayConstant.md), when every element is constant; `nil` otherwise. |
| `storage_format` | the order the elements are given in; see [arraystorage](../enum_nodes/arraystorage.md). |
| `struct_var` | for a constructor of an array of derived type, the object the components belong to; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`[1, i, 3]`. When the frontend can fold every element it also stores the
resulting [ArrayConstant](ArrayConstant.md) in `value`, so a backend can use
the folded form and ignore the expressions.

## Examples

```clojure
(ArrayConstructor
  :args [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    (Var
      :v (SymbolRef 1 "i")
    )
    (IntegerConstant
      :n 3
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  ]
  :type (Array
    :type (Integer
      :kind 4
    )
    :dims [
      (dimension
        :start (IntegerConstant
          :n 1
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
        :length (IntegerConstant
          :n 3
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
      )
    ]
    :physical_type :FixedSizeArray
    :memory_space :Global
  )
  :value nil
  :storage_format :ColMajor
  :struct_var nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arrayconstructor.asr
:language: clojure
```

## See Also

[ArrayConstant](ArrayConstant.md), [ImpliedDoLoop](ImpliedDoLoop.md), [Array](../type_nodes/Array.md)
