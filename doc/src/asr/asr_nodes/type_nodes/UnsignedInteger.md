# UnsignedInteger

An unsigned integer type.

## Declaration

### Syntax

```text
UnsignedInteger(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind, which fixes the storage size and the range or precision; see [kinds](../kinds_nodes/kinds.md). |

### Return values

None. A type is not evaluated.

## Description

An LFortran extension. It is a separate type rather than an attribute of
[Integer](Integer.md) so that every operation on unsigned values is a distinct
node and no pass can treat one as signed by accident.

## Examples

```clojure
(UnsignedInteger
  :kind 4
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/unsigned_expr.asr
:language: clojure
```

## See Also

[Integer](Integer.md), [UnsignedIntegerConstant](../expression_nodes/UnsignedIntegerConstant.md), kinds
