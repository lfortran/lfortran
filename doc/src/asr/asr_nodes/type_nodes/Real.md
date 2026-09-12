# Real

A floating point type.

## Declaration

### Syntax

```text
Real(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind, which fixes the storage size and the range or precision; see [kinds](../kinds_nodes/kinds.md). |

### Return values

None. A type is not evaluated.

## Description

The supported kinds are 4 and 8 bytes; kind 16 is accepted where the target
provides it, and ASR text writes such constants with the `#asr/real128` tag.

## Examples

```clojure
(Real
  :kind 8
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/real_expr.asr
:language: clojure
```

## See Also

[Integer](Integer.md), [Complex](Complex.md), kinds, [RealConstant](../expression_nodes/RealConstant.md)
