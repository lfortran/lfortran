# ArrayBroadcast

A scalar or smaller array expanded to a shape.

## Declaration

### Syntax

```text
ArrayBroadcast(expr array, expr shape, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `array` | the value being expanded. |
| `shape` | the shape to expand it to. |
| `type` | the array type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a = 0` where `a` is an array assigns the scalar to every element. The
frontend makes that explicit by wrapping the scalar in an **ArrayBroadcast**,
so the assignment has an array on both sides and the `array_op` pass has one
shape to work with.

## Examples

```clojure
(ArrayBroadcast
  :array (Var
    :v (SymbolRef 1 "i")
  )
  :shape (Var
    :v (SymbolRef 1 "shape")
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
    ]
    :physical_type :FixedSizeArray
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arraybroadcast.asr
:language: clojure
```

## See Also

[ArrayReshape](ArrayReshape.md), [Assignment](../statement_nodes/Assignment.md), [Where](../statement_nodes/Where.md)
