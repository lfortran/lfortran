# FileRewind

Position a file at its beginning.

## Declaration

### Syntax

```fortran
FileRewind(int label, expr? unit, expr? iostat, expr? err)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `iostat` | `expr?` | Integer variable for status. |
| `err` | `expr?` | Statement label for error branching. |

### Return values

None.

## Description

The `FileRewind` node represents the Fortran `REWIND` statement, which positions the specified file at its initial point.

## Examples

```fortran
rewind(10)
```

ASR:
```
(FileRewind
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
)
```

## See Also

[FileBackspace](filebackspace.md), [FileEndfile](fileendfile.md)
