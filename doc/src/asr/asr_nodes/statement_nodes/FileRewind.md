# FileRewind

Positions a unit at the start of its file.

## Declaration

### Syntax

```text
FileRewind(int label, expr? unit, expr? iostat, expr? err, expr? iomsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `err` | the label to branch to when the operation fails. |
| `iomsg` | a variable receiving the error message when the operation fails. |

### Return values

None.

## Description

After `rewind` the next read returns the first record of the file.

## Examples

```clojure
(FileRewind
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :iostat nil
  :err nil
  :iomsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileBackspace](FileBackspace.md), [FileRead](FileRead.md)
