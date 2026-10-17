# ArrayPhysicalCast

Changes how an array is represented, not what it holds.

## Declaration

### Syntax

```text
ArrayPhysicalCast(expr arg, array_physical_type old,
    array_physical_type new, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the array expression to re-describe. |
| `old` | its current representation; see [array_physical_type](../enum_nodes/array_physical_type.md). |
| `new` | the representation to produce. |
| `type` | the type of the result, whose `physical_type` is `new`. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The logical type does not change here: the element type, the rank and the
extents are the same on both sides, and only the physical representation
differs. That is what separates this node from [Cast](Cast.md), which changes
the value.

Passing a fixed size array to a procedure that takes an assumed shape
argument, for instance, needs a descriptor to be built around it, and the cast
is where that happens. The new physical type appears both in `new` and in the
`physical_type` of `type`.

There is one exception, the rank(0) view of an assumed-rank array. Inside a
`rank (0)` block of `select rank` the selector is a scalar, so the cast has
`old = AssumedRankArray`, `new = DescriptorArray` and a scalar `type`, the
element type of the array. A scalar has no `physical_type`; the value is read
through the data pointer of the assumed-rank descriptor. The verifier
recognizes this case by `old = AssumedRankArray` and a `type` that is not an
array, and only then does not compare `new` with `type`.

## Examples

```clojure
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
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/arrayphysicalcast.asr
:language: clojure
```

## See Also

[array_physical_type](../enum_nodes/array_physical_type.md), [Cast](Cast.md), [StringPhysicalCast](StringPhysicalCast.md), [Array](../type_nodes/Array.md)
