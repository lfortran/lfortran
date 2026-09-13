# integerboz

How an integer constant was written.

## Declaration

### Syntax

```text
integerboz = Binary | Hex | Octal | Decimal
```

### Values

| Value | Meaning |
|----------|-------------|
| `Binary` | a `b'...'` literal. |
| `Hex` | a `z'...'` literal. |
| `Octal` | an `o'...'` literal. |
| `Decimal` | an ordinary decimal literal. |

### Return values

None. An enumeration value is not evaluated.

## Description

The value of an [IntegerConstant](../expression_nodes/IntegerConstant.md) is
stored as a number, so this member only records the spelling. It matters for
diagnostics and for unparsing, and because a BOZ literal is restricted in
where the standard allows it to appear.

## See Also

[IntegerConstant](../expression_nodes/IntegerConstant.md), [Integer](../type_nodes/Integer.md)
