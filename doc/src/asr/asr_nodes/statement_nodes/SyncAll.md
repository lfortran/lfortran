# SyncAll

Waits until every image reaches this point.

## Declaration

### Syntax

```text
SyncAll(expr? stat, expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `stat` | a variable receiving the status of the synchronisation. |
| `errmsg` | a character variable receiving the error message. |

### Return values

None.

## Description

`sync all` is a barrier across all images of a coarray program: no image
continues until all of them have arrived, and every coarray write made before
the barrier is visible to every image after it.

## Examples

```clojure
(SyncAll
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

[SyncImages](SyncImages.md), [SyncMemory](SyncMemory.md), [SyncTeam](SyncTeam.md), [CoarrayRef](../expression_nodes/CoarrayRef.md)
