# FileOpen

Open a file or connect a unit to a file.

## Declaration

### Syntax

```fortran
FileOpen(int label, expr? newunit, expr? filename, expr? status, expr? form, expr? access, expr? iostat, expr? iomsg, expr? action, expr? delim, expr? recl, expr? position, expr? blank, expr? encoding, expr? sign, expr? decimal)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `newunit` | `expr?` | Integer variable that will receive the unit number assigned by the system. |
| `filename` | `expr?` | Character expression specifying the name of the file to be opened. |
| `status` | `expr?` | Character expression specifying the status of the file (`'OLD'`, `'NEW'`, `'SCRATCH'`, `'REPLACE'`, or `'UNKNOWN'`). |
| `form` | `expr?` | Character expression specifying whether the file is connected for `'FORMATTED'` or `'UNFORMATTED'` I/O. |
| `access` | `expr?` | Character expression specifying the access method (`'SEQUENTIAL'`, `'DIRECT'`, or `'STREAM'`). |
| `iostat` | `expr?` | Integer variable that will receive the status of the I/O operation. |
| `iomsg` | `expr?` | Character variable that will receive an error message if an error occurs. |
| `action` | `expr?` | Character expression specifying the allowed actions (`'READ'`, `'WRITE'`, or `'READWRITE'`). |
| `delim` | `expr?` | Character expression specifying the delimiter used for list-directed or namelist output (`'APOSTROPHE'`, `'QUOTE'`, or `'NONE'`). |
| `recl` | `expr?` | Integer expression specifying the record length for the file. |
| `position` | `expr?` | Character expression specifying the file position (`'ASIS'`, `'REWIND'`, or `'APPEND'`). |
| `blank` | `expr?` | Character expression specifying how blanks in numeric fields are interpreted (`'NULL'` or `'ZERO'`). |
| `encoding` | `expr?` | Character expression specifying the encoding of the file (`'UTF-8'` or `'DEFAULT'`). |
| `sign` | `expr?` | Character expression specifying the sign mode for formatted output (`'PLUS'`, `'SUPPRESS'`, or `'PROCESSOR_DEFINED'`). |
| `decimal` | `expr?` | Character expression specifying the decimal mode for formatted I/O (`'COMMA'` or `'POINT'`). |

### Return values

None.

## Description

The `FileOpen` node represents the Fortran `OPEN` statement, which is used to connect an external file to a unit, create a new file and connect it to a unit, or change certain specifiers of a connection between a file and a unit.

## Examples

```fortran
open(unit=10, file="test.txt", status="old")
```

ASR:
```
(FileOpen
    0
    ()
    (StringConstant "test.txt")
    (StringConstant "old")
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
)
```

## See Also

[FileClose](fileclose.md), [FileInquire](fileinquire.md)
