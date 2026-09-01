# Logical

A logical type.

## Declaration

### Syntax

```text
Logical(int kind)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `kind` | the kind, which fixes the storage size and the range or precision; see [kinds](../kinds_nodes/kinds.md). |

### Return values

None. A type is not evaluated.

## Description

The supported kinds are 1, 2 and 4 bytes and the default is 4, matching the
default integer kind, because Fortran gives the default logical the same
storage size as the default integer.

## Examples

```clojure
(Logical
  :kind 4
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/logical_expr.asr
:language: clojure
```

## See Also

[Integer](Integer.md), [LogicalConstant](../expression_nodes/LogicalConstant.md), kinds
