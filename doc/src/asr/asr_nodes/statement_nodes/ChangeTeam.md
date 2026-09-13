# ChangeTeam

Runs a block with a team as the current team.

## Declaration

### Syntax

```text
ChangeTeam(expr team, expr? stat, expr? errmsg, stmt* body,
    expr? end_stat, expr? end_errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `team` | the team to switch to. |
| `stat` | a variable receiving the status on entry. |
| `errmsg` | a character variable receiving the error message on entry. |
| `body` | the statements executed with that team current. |
| `end_stat` | a variable receiving the status on exit. |
| `end_errmsg` | a character variable receiving the error message on exit. |

### Return values

None.

## Description

Inside the block, image indices and coarray coindices are relative to the new
team, and leaving the block synchronises its images. The construct has a
status on entry and another on exit, because both the switch and the implied
barrier at the end can fail.

## Examples

```clojure
(ChangeTeam
  :team (Var
    :v (SymbolRef 1 "team")
  )
  :stat nil
  :errmsg nil
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "n")
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
  :end_stat nil
  :end_errmsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarray_stmt.asr
:language: clojure
```

## See Also

[FormTeam](FormTeam.md), [SyncTeam](SyncTeam.md)
