# ArrayConstant

An array constant, stored as raw data.

## Declaration

### Syntax

```text
ArrayConstant(int n_data, void data, ttype type,
    arraystorage storage_format)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `n_data` | the size of `data` in bytes. |
| `data` | the elements, in the order given by `storage_format`, as the raw bytes of the element type. |
| `type` | the array type, which gives the element type and the shape. |
| `storage_format` | `RowMajor` or `ColMajor`; see [arraystorage](../enum_nodes/arraystorage.md). |

### Return values

The value of the expression.

## Description

An array whose elements are all compile time constants is stored as bytes
rather than as a list of expression nodes. A large data statement then costs
its data and nothing else, and a backend can emit it as an initialised object
directly.

[ArrayConstructor](ArrayConstructor.md) is the node for an array whose
elements are not all constant. ASR text writes the bytes with the
`#asr/bytes` tag, and never truncates them.

## Examples

```clojure
(ArrayConstant
  :n_data 12
  :data #asr/bytes "010000000200000003000000"
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
  :storage_format :ColMajor
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArrayConstructor](ArrayConstructor.md), [Array](../type_nodes/Array.md), [arraystorage](../enum_nodes/arraystorage.md)
