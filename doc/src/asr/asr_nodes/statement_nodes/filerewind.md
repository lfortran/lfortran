# FileRewind

Position a file at its beginning.

## Declaration

### Syntax

```fortran
FileRewind(int label, expr? unit, expr? iostat, expr? err, expr? iomsg)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `iostat` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `err` | `expr?` | Statement label for error branching. |
| `iomsg` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |

### Return values

None.

## Description

The `FileRewind` node represents the Fortran `REWIND` statement, which positions the specified file at its initial point.

### Specifier behavior (brief)

- `unit`:
    - Identifies the connected unit to rewind.

- `iostat` and `iomsg`:
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`).

- `err`:
    - Statement label to branch to if an error occurs during rewind.

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
    ()
)
```

## See Also

[FileBackspace](filebackspace.md), [FileEndfile](fileendfile.md)
