# Flush

Writes pending output of a unit to the file.

## Declaration

### Syntax

```text
Flush(int label, expr unit, expr? err, expr? iomsg, expr? iostat)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `err` | the label to branch to when the operation fails. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |

### Return values

None.

## Description

`flush` makes data written so far visible to other processes reading the file.
It does not disconnect the unit, which is what [FileClose](FileClose.md) does.

## Examples

```clojure
(Flush
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :err nil
  :iomsg nil
  :iostat nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileWrite](FileWrite.md), [FileClose](FileClose.md)
