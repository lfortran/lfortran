# FileEndfile

Writes an end of file record.

## Declaration

### Syntax

```text
FileEndfile(int label, expr? unit, expr? iostat, expr? iomsg,
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

`endfile` marks the current position as the end of the file and truncates
anything after it, so the file can be read back without seeing stale records.

## Examples

```clojure
(FileEndfile
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

[FileRewind](FileRewind.md), [FileWrite](FileWrite.md)
