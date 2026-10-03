# IfArithmetic

The obsolescent arithmetic `if`: a three-way branch on the sign of an expression.

## Declaration

### Syntax

```text
IfArithmetic(expr test, int lt_label, int eq_label, int gt_label)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `test` | the numeric expression whose sign selects the branch. |
| `lt_label` | the label to jump to when `test` is negative. |
| `eq_label` | the label to jump to when `test` is zero. |
| `gt_label` | the label to jump to when `test` is positive. |

### Return values

None.

## Description

`if (x) 10, 20, 30` jumps to one of three labels according to the sign of `x`.
The feature was declared obsolescent in Fortran 90 and is supported for legacy
code.

The three members are label ids, matching the `id` of a
[GoToTarget](GoToTarget.md), exactly like [GoTo](GoTo.md).

## Examples

```clojure
(IfArithmetic
  :test (Var
    :v (SymbolRef 1 "i")
  )
  :lt_label 100
  :eq_label 100
  :gt_label 100
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/goto_stmt.asr
:language: clojure
```

## See Also

[If](If.md), [GoTo](GoTo.md), [GoToTarget](GoToTarget.md)
