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
| `iostat` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `iomsg` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `err` | `expr?` | Statement label to branch to if an error occurs. |
| `status` | `expr?` | Character expression specifying the status of the file after closing (`'KEEP'` or `'DELETE'`). |

### Return values

None.

## Description

The `FileClose` node represents the Fortran `CLOSE` statement, which is used to disconnect an external file from a unit.

### Specifier behavior (brief)

- `unit`:
    - Identifies the connected unit to close.

- `status`:
    - `KEEP`: keep the file after closing.
    - `DELETE`: delete the file when closing.

    ```fortran
    close(unit=10, status='delete')
    ```

- `err`:
    - Statement label to branch to if an error occurs during close.

    ```fortran
    integer :: ios
    close(unit=10, iostat=ios, err=100)
100 continue
    ```

- `iostat` and `iomsg`:
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`).

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
