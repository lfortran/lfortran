# Complex

A complex type.

## Declaration

### Syntax

```text
Complex(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind, which fixes the storage size and the range or precision; see [kinds](../kinds_nodes/kinds.md). |

### Return values

None. A type is not evaluated.

## Description

A complex value is a pair of reals of the same kind, so the kind here is the
kind of each part: `complex(8)` holds two `real(8)` values.

## Examples

```clojure
(Complex
  :kind 4
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/complex_expr.asr
:language: clojure
```

## See Also

[Real](Real.md), [ComplexConstant](../expression_nodes/ComplexConstant.md), [ComplexRe](../expression_nodes/ComplexRe.md), kinds
