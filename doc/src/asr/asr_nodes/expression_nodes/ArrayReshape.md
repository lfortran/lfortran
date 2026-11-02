# ArrayReshape

An array with the same elements in a different shape.

## Declaration

### Syntax

```text
ArrayReshape(expr array, expr shape, expr? pad, expr? order,
    ttype type, expr? value, expr? result)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `array` | the array whose elements are taken. |
| `shape` | a rank one integer array giving the new extents. |
| `pad` | values used to fill the result when `array` has too few elements; `nil` when it does not. |
| `order` | the permutation of the dimensions to fill in; `nil` for the natural order. |
| `type` | the array type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `result` | optional allocatable descriptor temporary supplying independent scratch storage for a runtime numeric or logical result without `pad`. |

### Return values

The value of the expression.

## Description

`reshape(a, shape)`. The elements are taken in array element order, so the
result holds the same values in the same sequence with different extents.

The temporary pass can supply `result`, with explicit allocation before
evaluation and deallocation before reuse. Scope cleanup releases its last
allocation. The backend fills this storage without allocating another data
buffer; it does not transfer ownership to the assignment target. This keeps
repeated evaluation bounded while preserving independent assigned values.

## Examples

```clojure
(ArrayReshape
  :array (Var
    :v (SymbolRef 1 "a")
  )
  :shape (Var
    :v (SymbolRef 1 "shape")
  )
  :pad nil
  :order nil
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
          :n 2
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
  :result nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arrayreshape.asr
:language: clojure
```

## See Also

[ArrayTranspose](ArrayTranspose.md), [ArrayBroadcast](ArrayBroadcast.md), [ArraySize](ArraySize.md)
