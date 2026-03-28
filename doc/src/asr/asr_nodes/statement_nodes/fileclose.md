# FileClose

Close a file or disconnect a unit.

## Declaration

### Syntax

```fortran
FileClose(int label, expr? unit, expr? iostat, expr? iomsg, expr? err, expr? status)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number to be closed. |
| `iostat` | `expr?` | Integer variable that will receive the status of the I/O operation. |
| `iomsg` | `expr?` | Character variable that will receive an error message if an error occurs. |
| `err` | `expr?` | Statement label to branch to if an error occurs. |
| `status` | `expr?` | Character expression specifying the status of the file after closing (`'KEEP'` or `'DELETE'`). |

### Return values

None.

## Description

The `FileClose` node represents the Fortran `CLOSE` statement, which is used to disconnect an external file from a unit.

## Examples

```fortran
close(10)
```

ASR:
```
(FileClose
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
    ()
    ()
)
```

## See Also

[FileOpen](fileopen.md)
