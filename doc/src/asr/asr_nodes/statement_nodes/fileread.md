# FileRead

Read data from a file.

## Declaration

### Syntax

```fortran
FileRead(int label, expr? unit, expr? fmt, expr? iomsg, expr? iostat, expr? advance, expr? size, expr? id, expr? pos, expr* values, stmt? overloaded, bool is_formatted, symbol? nml, expr? rec)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `fmt` | `expr?` | Format specification. |
| `iomsg` | `expr?` | Character variable for error message. |
| `iostat` | `expr?` | Integer variable for status. |
| `advance` | `expr?` | Character expression (`'YES'` or `'NO'`) for non-advancing I/O. |
| `size` | `expr?` | Integer variable to receive the number of characters read. |
| `id` | `expr?` | Integer expression for asynchronous I/O. |
| `pos` | `expr?` | Integer expression for file position (stream I/O). |
| `values` | `expr*` | List of variables to read into. |
| `overloaded` | `stmt?` | Overloaded statement (if any). |
| `is_formatted` | `bool` | Whether the read is formatted. |
| `nml` | `symbol?` | Namelist symbol (if using namelist I/O). |
| `rec` | `expr?` | Record number for direct access I/O. |

### Return values

None.

## Description

The `FileRead` node represents the Fortran `READ` statement, which is used to transfer data from an external file or an internal file to the items in the input list or a namelist group.

## Examples

```fortran
read(10, *) a, b
```

ASR:
```
(FileRead
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    [(Var 2 a) (Var 2 b)]
    ()
    .true.
    ()
    ()
)
```

## See Also

[FileWrite](filewrite.md), [FileOpen](fileopen.md)
