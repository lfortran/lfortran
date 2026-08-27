# ArrayTranspose

The transpose of a rank two array.

## Declaration

### Syntax

```text
ArrayTranspose(expr matrix, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `matrix` | the array to transpose. |
| `type` | the array type of the result, with the extents exchanged. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`transpose(a)`. It is a node of its own rather than an intrinsic call so that
passes can recognise it: transposing an operand of a matrix multiplication,
for instance, can often be folded into the multiplication.

## Examples

```clojure
(ArrayTranspose
  :matrix (Var
    :v (SymbolRef 1 "a")
  )
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
      (dimension
        :start (IntegerConstant
          :n 1
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
        :length (IntegerConstant
          :n 2
          :type (Integer
            :kind 4
          )
          :intboz_type :Decimal
        )
      )
    ]
    :physical_type :FixedSizeArray
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arraytranspose.asr
:language: clojure
```

## See Also

[ArrayReshape](ArrayReshape.md), [ArrayPack](ArrayPack.md), [IntrinsicArrayFunction](IntrinsicArrayFunction.md)
