# case_stmt

One branch of a `select case` construct.

## Declaration

### Syntax

```text
case_stmt
    = CaseStmt(expr* test, stmt* body, bool fall_through)
    | CaseStmt_Range(expr? start, expr? end, stmt* body)
```

### Arguments

None.

### Return values

None.

## Description

A branch is one of two constructors.

**CaseStmt** matches a list of values:

| Argument | Description |
|----------|-------------|
| `test` | the values this branch matches. |
| `body` | the statements of the branch. |
| `fall_through` | `true` when control continues into the next branch. Fortran cases never fall through; the member exists for languages whose `switch` does. |

**CaseStmt_Range** matches a range, `case (2:5)`:

| Argument | Description |
|----------|-------------|
| `start` | the first value of the range, or `nil` for `case (:n)`. |
| `end` | the last value, or `nil` for `case (n:)`. |
| `body` | the statements of the branch. |

The branches of a [Select](../statement_nodes/Select.md) are checked in order
and at most one of them runs.

## Examples

```clojure
(CaseStmt
  :test [
    (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  ]
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "j")
      )
      :value (IntegerConstant
        :n 0
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
  :fall_through false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/select_stmt.asr
:language: clojure
```

## See Also

[Select](../statement_nodes/Select.md), [type_stmt](type_stmt.md), [rank_stmt](rank_stmt.md)
