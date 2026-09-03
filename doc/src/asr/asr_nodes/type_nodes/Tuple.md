# Tuple

A fixed length sequence whose elements may have different types.

## Declaration

### Syntax

```text
Tuple(ttype* type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the types of the elements, in order. Its length is the length of the tuple. |

### Return values

None. A type is not evaluated.

## Description

The length and the element types are part of the type, so
[TupleItem](../expression_nodes/TupleItem.md) needs a constant index and
[TupleLen](../expression_nodes/TupleLen.md) is always known at compile time.
That is what separates a tuple from a [List](List.md).

## Examples

```clojure
(Tuple
  :type [
    (Integer
      :kind 4
    )
    (Real
      :kind 4
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/tuple_expr.asr
:language: clojure
```

## See Also

[List](List.md), [TupleConstant](../expression_nodes/TupleConstant.md), [TupleItem](../expression_nodes/TupleItem.md)
