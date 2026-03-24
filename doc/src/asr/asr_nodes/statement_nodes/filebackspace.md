# FileBackspace

Position a file before the preceding record.

## Declaration

### Syntax

```fortran
FileBackspace(int label, expr? unit, expr? iostat, expr? err)
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

The `FileBackspace` node represents the Fortran `BACKSPACE` statement, which positions the specified file before the preceding record.

## Examples

```fortran
backspace(10)
```

ASR:
```
(FileBackspace
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
)
```

## See Also

[FileRewind](filerewind.md), [FileEndfile](fileendfile.md)
