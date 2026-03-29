# FileInquire

Inquire about file properties.

## Declaration

### Syntax

```fortran
FileInquire(int label, expr? unit, expr? file, expr? iostat, expr? err, expr? exist, expr? opened, expr? number, expr? named, expr? name, expr? access, expr? sequential, expr? direct, expr? form, expr? formatted, expr? unformatted, expr? recl, expr? nextrec, expr? blank, expr? position, expr? action, expr? read, expr? write, expr? readwrite, expr? delim, expr? pad, expr? flen, expr? blocksize, expr? convert, expr? carriagecontrol, expr? size, expr? pos, expr? iolength, expr* iolength_vars, expr? decimal, expr? sign)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `file` | `expr?` | Character expression specifying the file name. |
| `iostat` | `expr?` | Integer variable for status. |
| `err` | `expr?` | Statement label for error branching. |
| `exist` | `expr?` | Logical variable to receive existence state. |
| `opened` | `expr?` | Logical variable to receive connection state. |
| `number` | `expr?` | Integer variable to receive unit number. |
| `named` | `expr?` | Logical variable to receive whether file has a name. |
| `name` | `expr?` | Character variable to receive file name. |
| `access` | `expr?` | Character variable to receive access method. |
| `sequential` | `expr?` | Character variable to receive whether sequential access is allowed. |
| `direct` | `expr?` | Character variable to receive whether direct access is allowed. |
| `form` | `expr?` | Character variable to receive connection form (`'FORMATTED'` or `'UNFORMATTED'`). |
| `formatted` | `expr?` | Character variable to receive whether formatted I/O is allowed. |
| `unformatted` | `expr?` | Character variable to receive whether unformatted I/O is allowed. |
| `recl` | `expr?` | Integer variable to receive record length. |
| `nextrec` | `expr?` | Integer variable to receive next record number. |
| `blank` | `expr?` | Character variable to receive blank interpretation mode. |
| `position` | `expr?` | Character variable to receive file position. |
| `action` | `expr?` | Character variable to receive allowed actions. |
| `read` | `expr?` | Character variable to receive whether reading is allowed. |
| `write` | `expr?` | Character variable to receive whether writing is allowed. |
| `readwrite` | `expr?` | Character variable to receive whether reading/writing is allowed. |
| `delim` | `expr?` | Character variable to receive delimiter mode. |
| `pad` | `expr?` | Character variable to receive padding mode. |
| `flen` | `expr?` | Integer variable to receive file length. |
| `blocksize` | `expr?` | Integer variable to receive block size. |
| `convert` | `expr?` | Character variable to receive conversion mode. |
| `carriagecontrol`| `expr?` | Character variable to receive carriage control mode. |
| `size` | `expr?` | Integer variable to receive file size. |
| `pos` | `expr?` | Integer variable to receive file position. |
| `iolength` | `expr?` | Integer variable to receive I/O list length. |
| `iolength_vars` | `expr*` | List of variables for IOLENGTH inquiry. |
| `decimal` | `expr?` | Character variable to receive decimal mode. |
| `sign` | `expr?` | Character variable to receive sign mode. |

### Return values

None.

## Description

The `FileInquire` node represents the Fortran `INQUIRE` statement, which is used to inquire about the properties of a particular file or of the connection to a particular unit.

## Examples

```fortran
inquire(unit=10, opened=is_open)
```

ASR:
```
(FileInquire
    0
    (IntegerConstant 10 (Integer 4 []))
    ()
    ()
    ()
    ()
    (Var 2 is_open)
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    ()
    []
    ()
    ()
)
```

## See Also

[FileOpen](fileopen.md)
