# SyncMemory

Orders memory operations without waiting for other images.

## Declaration

### Syntax

```text
SyncMemory(expr? stat, expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `stat` | a variable receiving the status. |
| `errmsg` | a character variable receiving the error message. |

### Return values

None.

## Description

`sync memory` ends one segment and starts the next on this image alone. It
does not block: it is the building block for user-written synchronisation
built out of atomic operations, where the ordering matters but the waiting is
done by other means.

## Examples

```clojure
(SyncMemory
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

[SyncAll](SyncAll.md), [SyncImages](SyncImages.md)
