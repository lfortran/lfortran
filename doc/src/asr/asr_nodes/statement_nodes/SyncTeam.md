# SyncTeam

Waits until every image of a team reaches this point.

## Declaration

### Syntax

```text
SyncTeam(expr team, expr? stat, expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `team` | the team to synchronise. |
| `stat` | a variable receiving the status. |
| `errmsg` | a character variable receiving the error message. |

### Return values

None.

## Description

`sync team` is [SyncAll](SyncAll.md) restricted to the images of one team, so
teams formed by [FormTeam](FormTeam.md) can synchronise independently of each
other.

## Examples

```clojure
(SyncTeam
  :team (Var
    :v (SymbolRef 1 "team")
  )
  :stat (Var
    :v (SymbolRef 1 "stat")
  )
  :errmsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarray_stmt.asr
:language: clojure
```

## See Also

[FormTeam](FormTeam.md), [ChangeTeam](ChangeTeam.md), [SyncAll](SyncAll.md)
