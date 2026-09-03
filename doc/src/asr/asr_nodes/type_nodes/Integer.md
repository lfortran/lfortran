# Integer

A signed integer type.

## Declaration

### Syntax

```text
Integer(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind, which fixes the storage size and the range or precision; see [kinds](../kinds_nodes/kinds.md). |

### Return values

None. A type is not evaluated.

## Description

The supported kinds are 1, 2, 4 and 8 bytes, and the default is 4. The kind is
part of the type, so ASR never has an integer whose width is implied: an
operation between different kinds is an explicit [Cast](../expression_nodes/Cast.md).

## Examples

```clojure
(Integer
  :kind 4
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/integer_expr.asr
:language: clojure
```

## See Also

[UnsignedInteger](UnsignedInteger.md), [Real](Real.md), kinds, [IntegerConstant](../expression_nodes/IntegerConstant.md)
