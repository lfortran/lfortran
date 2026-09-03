# FileClose

Disconnects a unit.

## Declaration

### Syntax

```text
FileClose(int label, expr? unit, expr? iostat, expr? iomsg, expr? err,
    expr? status)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `err` | the label to branch to when the operation fails. |
| `status` | `keep` or `delete`: whether the file survives the close. |

### Return values

None.

## Description

Closing flushes pending output. A unit that is never closed is closed when the
program terminates normally.

## Examples

```clojure
(FileClose
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :iostat nil
  :iomsg nil
  :err nil
  :status nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileOpen](FileOpen.md), [Flush](Flush.md)
