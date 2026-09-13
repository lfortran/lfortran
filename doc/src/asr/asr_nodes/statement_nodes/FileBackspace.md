# FileBackspace

Moves a unit back one record.

## Declaration

### Syntax

```text
FileBackspace(int label, expr? unit, expr? iostat, expr? iomsg,
    expr? err)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `err` | the label to branch to when the operation fails. |

### Return values

None.

## Description

`backspace` positions a sequential file before the record it was after, so the
record just read can be read again. It has no effect at the start of a file.

## Examples

```clojure
(FileBackspace
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :iostat nil
  :iomsg nil
  :err nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileRewind](FileRewind.md), [FileRead](FileRead.md), [FileEndfile](FileEndfile.md)
