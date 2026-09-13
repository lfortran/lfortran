# FormTeam

Divides the current team into new teams.

## Declaration

### Syntax

```text
FormTeam(expr team_number, expr team, expr? new_index, expr? stat,
    expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `team_number` | which new team this image joins. |
| `team` | the team variable that describes the new team. |
| `new_index` | the index this image takes in the new team. |
| `stat` | a variable receiving the status. |
| `errmsg` | a character variable receiving the error message. |

### Return values

None.

## Description

`form team` is executed by every image of the current team: each one names the
team it joins, and the images naming the same number end up in the same team.
The resulting team variable is what [ChangeTeam](ChangeTeam.md) and
[SyncTeam](SyncTeam.md) take.

## Examples

```clojure
(FormTeam
  :team_number (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :team (Var
    :v (SymbolRef 1 "team")
  )
  :new_index (Var
    :v (SymbolRef 1 "n")
  )
  :stat nil
  :errmsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarray_stmt.asr
:language: clojure
```

## See Also

[ChangeTeam](ChangeTeam.md), [SyncTeam](SyncTeam.md), [SyncAll](SyncAll.md)
