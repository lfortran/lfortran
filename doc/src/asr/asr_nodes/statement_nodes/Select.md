# Select

A `select case` construct.

## Declaration

### Syntax

```text
Select(identifier? name, expr test, case_stmt* body, stmt* default,
    bool enable_fall_through)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the construct name, or `nil`. |
| `test` | the expression the cases are matched against. |
| `body` | the cases, each a [case_stmt](../helper_nodes/case_stmt.md). |
| `default` | the statements of `case default`. |
| `enable_fall_through` | `true` when a case may fall through to the next one. Fortran cases never fall through; the member exists for languages whose `switch` does. |

### Return values

None.

## Description

The cases are checked in order and at most one runs. A case is either a
[CaseStmt](../helper_nodes/case_stmt.md), holding a list of values to match,
or a `CaseStmt_Range`, holding a range.

`test` must be integer, character or logical; a real selector is not allowed,
which is what makes the construct implementable as a jump table.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/select_stmt.asr
:language: clojure
```

## See Also

[case_stmt](../helper_nodes/case_stmt.md), [If](If.md), [SelectType](SelectType.md), [SelectRank](SelectRank.md)
