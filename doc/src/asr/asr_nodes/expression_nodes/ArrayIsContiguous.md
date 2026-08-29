# ArrayIsContiguous

Whether an array occupies contiguous storage.

## Declaration

### Syntax

```text
ArrayIsContiguous(expr array, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `array` | the array to test. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`is_contiguous(a)`. A section with a stride, or one that fixes a subscript in
the wrong dimension, is not contiguous, and a procedure that requires
contiguity has to copy it. The test lets generated code choose between a fast
path and a copy.

## Examples

```clojure
(ArrayIsContiguous
  :array (Var
    :v (SymbolRef 1 "a")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/array_expr.asr
:language: clojure
```

## See Also

[ArraySection](ArraySection.md), [ArrayPhysicalCast](ArrayPhysicalCast.md), [array_physical_type](../enum_nodes/array_physical_type.md)
