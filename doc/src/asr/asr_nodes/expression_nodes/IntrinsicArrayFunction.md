# IntrinsicArrayFunction

A call to an intrinsic function that operates on a whole array.

## Declaration

### Syntax

```text
IntrinsicArrayFunction(int arr_intrinsic_id, expr* args,
    int overload_id, ttype? type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arr_intrinsic_id` | which intrinsic this is, as the integer id of the array intrinsic registry. |
| `args` | the actual arguments. |
| `overload_id` | which signature was selected. |
| `type` | the type of the result; `nil` before it is resolved. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`sum`, `matmul`, `maxval` and the other intrinsics that take an array as a
whole rather than element by element. They have a node of their own because
their lowering is different: an array intrinsic becomes a loop nest, and the
`intrinsic_array_function` pass is what writes it.

## Examples

```clojure
(IntrinsicArrayFunction
  :arr_intrinsic_id 14
  :args [
    (ArrayPhysicalCast
      :arg (Var
        :v (SymbolRef 1 "a")
      )
      :old :FixedSizeArray
      :new :DescriptorArray
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
        :physical_type :DescriptorArray
        :memory_space :Global
      )
      :value nil
    )
  ]
  :overload_id 0
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/intrinsic_expr.asr
:language: clojure
```

## See Also

[IntrinsicElementalFunction](IntrinsicElementalFunction.md), [ArrayPack](ArrayPack.md), [ArrayTranspose](ArrayTranspose.md)
