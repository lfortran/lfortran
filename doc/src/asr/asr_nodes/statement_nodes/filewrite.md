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
| `iomsg` | `expr?` | Character variable for error message. |
| `iostat` | `expr?` | Integer variable for status. |
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
