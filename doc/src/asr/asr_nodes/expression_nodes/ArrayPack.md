# ArrayPack

The elements of an array selected by a mask.

## Declaration

### Syntax

```text
ArrayPack(expr array, expr mask, expr? vector, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `array` | the array to select from. |
| `mask` | a logical array of the same shape, or a scalar. |
| `vector` | values to pad the result with when it must be longer than the number of selected elements; `nil` otherwise. |
| `type` | the rank one array type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`pack(a, mask)`. The result is always rank one, whatever the rank of the
input, and its length is the number of true elements of the mask unless
`vector` makes it longer.

## Examples

```clojure
(ArrayPack
  :array (Var
    :v (SymbolRef 1 "a")
  )
  :mask (Var
    :v (SymbolRef 1 "mask")
  )
  :vector nil
  :type (Array
    :type (Integer
      :kind 4
    )
    :dims [
      (dimension
        :start nil
        :length nil
      )
    ]
    :physical_type :DescriptorArray
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arraypack.asr
:language: clojure
```

## See Also

[Where](../statement_nodes/Where.md), [ArrayReshape](ArrayReshape.md), [IntrinsicArrayFunction](IntrinsicArrayFunction.md)
