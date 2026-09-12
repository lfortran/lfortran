# IntegerConstant

An integer literal.

## Declaration

### Syntax

```text
IntegerConstant(int n, ttype type, integerboz intboz_type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `n` | the value. |
| `type` | the integer type, which fixes the kind and therefore the range of the constant. |
| `intboz_type` | how the constant was written: `Decimal`, or `Binary`, `Octal` or `Hex` for a BOZ literal; see [integerboz](../enum_nodes/integerboz.md). |

### Return values

The value of the expression.

## Description

The value is stored as a number, not as text, so a BOZ literal such as
`z'ff'` is an **IntegerConstant** with `n` equal to 255. `intboz_type` records
only how it was spelled, which matters for diagnostics and for unparsing but
not for code generation.

An **IntegerConstant** has no `value` member because it is its own value.

## Examples

```clojure
(IntegerConstant
  :n 255
  :type (Integer
    :kind 4
  )
  :intboz_type :Hex
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/integer_expr.asr
:language: clojure
```

## See Also

[RealConstant](RealConstant.md), [LogicalConstant](LogicalConstant.md), [integerboz](../enum_nodes/integerboz.md), [Integer](../type_nodes/Integer.md)
