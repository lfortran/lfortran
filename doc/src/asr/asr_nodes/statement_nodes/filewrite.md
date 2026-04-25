# FileWrite

Write data to a file.

## Declaration

### Syntax

```fortran
FileWrite(int label, expr? unit, expr? iomsg, expr? iostat, expr? id, expr* values, expr? separator, expr? end, stmt? overloaded, bool is_formatted, symbol? nml, expr? rec, expr? pos)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `iomsg` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `iostat` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `id` | `expr?` | Integer expression for asynchronous I/O. |
| `values` | `expr*` | List of expressions to write. |
| `separator` | `expr?` | Separator expression. |
| `end` | `expr?` | End of line expression. |
| `overloaded` | `stmt?` | Overloaded statement (if any). |
| `is_formatted` | `bool` | Whether the write is formatted. |
| `nml` | `symbol?` | Namelist symbol (if using namelist I/O). |
| `rec` | `expr?` | Record number for direct access I/O. |
| `pos` | `expr?` | Integer expression for file position (stream I/O). |

### Return values

None.

## Description

The `FileWrite` node represents the Fortran `WRITE` statement, which is used to transfer data from the items in the output list or a namelist group to an external file or an internal file.

### Specifier behavior (brief)

- `unit`:
    - Selects the external unit (or internal file expression) used as the write target.

- `values`:
    - Output list of expressions/items written by the statement.

- `id`:
    - Associates the statement with an asynchronous transfer identifier.

- `rec`:
    - Record number for direct-access I/O writes.

    ```fortran
    write(unit=20, rec=5) row
    ```

- `pos`:
    - Sets/uses stream file position for stream access writes.

- `nml`:
    - Namelist group symbol when the write is namelist-driven.

    ```fortran
    write(unit=30, nml=config)
    ```

- `separator` and `end`:
    - Optional output formatting controls used by lowered/internal write forms.

- `is_formatted`:
    - Internal ASR flag indicating whether this write is treated as formatted transfer.

- `overloaded`:
    - Internal lowered statement used when WRITE resolves through an overloaded procedure.

- `iostat` and `iomsg`:
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`).

## Examples

```fortran
write(10, *) a, b
```

ASR:
```
(FileWrite
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
    ()
    [(Var 2 a) (Var 2 b)]
    ()
    ()
    ()
    .true.
    ()
    ()
    ()
)
```

## See Also

[FileRead](fileread.md), [FileOpen](fileopen.md)
