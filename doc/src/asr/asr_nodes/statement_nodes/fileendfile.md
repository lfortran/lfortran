# FileEndfile

Write an end-of-file record.

## Declaration

### Syntax

```fortran
FileEndfile(int label, expr? unit, expr? iostat, expr? err)
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

The `FileEndfile` node represents the Fortran `ENDFILE` statement, which writes an end-of-file record to the specified file.

## Examples

```fortran
endfile(10)
```

ASR:
```
(FileEndfile
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
)
```

## See Also

[FileRewind](filerewind.md), [FileBackspace](filebackspace.md)
