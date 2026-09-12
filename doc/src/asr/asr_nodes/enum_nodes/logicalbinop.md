# logicalbinop

The logical binary operators.

## Declaration

### Syntax

```text
logicalbinop = And | Or | Xor | NEqv | Eqv
```

### Values

| Value | Meaning |
|----------|-------------|
| `And` | `.and.` |
| `Or` | `.or.` |
| `Xor` | exclusive or. |
| `NEqv` | `.neqv.`, which is exclusive or. |
| `Eqv` | `.eqv.`, which is equivalence. |

### Return values

None. An enumeration value is not evaluated.

## Description

`Xor` and `NEqv` compute the same result: they are separate values so that
unparsed ASR reproduces the operator that was written.

## See Also

[LogicalBinOp](../expression_nodes/LogicalBinOp.md), [OverloadedBoolOp](../expression_nodes/OverloadedBoolOp.md), [binop](binop.md)
