# FileInquire

Inquire about file properties.

## Declaration

### Syntax

```fortran
FileInquire(int label, expr? unit, expr? file, expr? iostat, expr? err, expr? exist, expr? opened, expr? number, expr? named, expr? name, expr? access, expr? sequential, expr? direct, expr? form, expr? formatted, expr? unformatted, expr? recl, expr? nextrec, expr? blank, expr? position, expr? action, expr? read, expr? write, expr? readwrite, expr? delim, expr? pad, expr? flen, expr? blocksize, expr? convert, expr? carriagecontrol, expr? size, expr? pos, expr? iolength, expr* iolength_vars, expr? decimal, expr? sign, expr? encoding, expr? stream, expr? iomsg, expr? round, expr? pending, expr? asynchronous)
```

### Arguments

For details on the arguments, see [FileOpen](fileopen.md#arguments). The `FileInquire` node shares many of the same arguments as `FileOpen`, but they are all optional and used to receive information about the file or unit rather than to specify properties for opening a file.

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. See [FileOpen](fileopen.md#arguments). |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `file` | `expr?` | Character expression specifying the file name. |
| `iostat` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `err` | `expr?` | Statement label for error branching. |
| `exist` | `expr?` | Logical variable to receive existence state. |
| `opened` | `expr?` | Logical variable to receive connection state. |
| `number` | `expr?` | Integer variable to receive unit number. |
| `named` | `expr?` | Logical variable to receive whether file has a name. |
| `name` | `expr?` | Character variable to receive file name. |
| `access` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`access`). |
| `sequential` | `expr?` | Character variable to receive whether sequential access is allowed. |
| `direct` | `expr?` | Character variable to receive whether direct access is allowed. |
| `form` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`form`). |
| `formatted` | `expr?` | Character variable to receive whether formatted I/O is allowed. |
| `unformatted` | `expr?` | Character variable to receive whether unformatted I/O is allowed. |
| `recl` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`recl`). |
| `nextrec` | `expr?` | Integer variable to receive next record number. |
| `blank` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`blank`). |
| `position` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`position`). |
| `action` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`action`). |
| `read` | `expr?` | Character variable to receive whether reading is allowed. |
| `write` | `expr?` | Character variable to receive whether writing is allowed. |
| `readwrite` | `expr?` | Character variable to receive whether reading/writing is allowed. |
| `delim` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`delim`). |
| `pad` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`pad`). |
| `flen` | `expr?` | Integer variable to receive file length. |
| `blocksize` | `expr?` | Integer variable to receive block size. |
| `convert` | `expr?` | Character variable to receive conversion mode. |
| `carriagecontrol`| `expr?` | Character variable to receive carriage control mode. |
| `size` | `expr?` | Integer variable to receive file size. |
| `pos` | `expr?` | Integer variable to receive file position. |
| `iolength` | `expr?` | Integer variable to receive I/O list length. |
| `iolength_vars` | `expr*` | List of variables for IOLENGTH inquiry. |
| `decimal` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`decimal`). |
| `sign` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`sign`). |
| `encoding` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`encoding`). |
| `stream` | `expr?` | Character variable to receive whether stream access is allowed. |
| `iomsg` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `round` | `expr?` | See [FileOpen arguments](fileopen.md#arguments) (`round`). |
| `pending` | `expr?` | Logical variable to receive whether an asynchronous transfer is pending for the unit. |
| `asynchronous` | `expr?` | Character variable to receive asynchronous capability/mode information for the connection. |

### Return values

None.

## Description

The `FileInquire` node represents the Fortran `INQUIRE` statement, which is used to inquire about the properties of a particular file or of the connection to a particular unit.

### Specifier behavior (brief)

- `unit` and `file`:
    - Select inquiry target by connected unit (`unit=`) or by file path (`file=`).
    - In practice, one target selector is typically used per inquiry.

- `iostat` and `iomsg`:
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`).

- `access`, `form`, `recl`, `blank`, `position`, `action`, `delim`, `pad`, `decimal`, `sign`, `encoding`, `round`:
    - These report connection modes using the same value families documented for OPEN.
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief).

- `exist`:
    - Returns whether the file exists.
    - Typical values: `.true.` / `.false.`.

    ```fortran
    logical :: ex
    inquire(file="data.txt", exist=ex)
    ```

- `opened`:
    - Returns whether the file or unit is currently connected.

    ```fortran
    logical :: is_open
    inquire(unit=10, opened=is_open)
    ```

- `number`:
    - Returns the connected unit number (if one exists for the file inquiry).

- `named` and `name`:
    - `named` indicates whether the connected file has a name.
    - `name` returns that file name when available.

    ```fortran
    logical :: has_name
    character(len=256) :: fname
    inquire(unit=10, named=has_name, name=fname)
    ```

- `sequential`, `direct`, `stream`:
    - Return whether each access form is allowed for the connection.
    - Common processor values are character results like `"YES"`, `"NO"`, or `"UNKNOWN"`.

- `formatted`, `unformatted`:
    - Return whether formatted or unformatted transfer is allowed.
    - Common processor values are character results like `"YES"`, `"NO"`, or `"UNKNOWN"`.

- `read`, `write`, `readwrite`:
    - Return whether corresponding data transfer modes are permitted.
    - Common processor values are character results like `"YES"`, `"NO"`, or `"UNKNOWN"`.

- `pending` and `asynchronous`:
    - `pending` reports whether an asynchronous I/O operation is currently pending.
    - `asynchronous` reports asynchronous I/O mode/capability for the connection (for example values similar to `"YES"`, `"NO"`, or `"UNKNOWN"`).

- `nextrec`:
    - For direct-access connections, returns the next record number for sequential processing.

- `size` and `pos`:
    - `size` returns file size in file storage units (processor dependent).
    - `pos` returns current file position for stream access.

- `flen` and `blocksize`:
    - Return processor-dependent file length and block size information when available.

- `convert`:
    - Returns processor/runtime data conversion mode for the connection.

- `carriagecontrol`:
    - Returns carriage control mode for formatted records, when supported by the processor.

- `iolength` and `iolength_vars`:
    - `iolength` requests inquiry-by-I/O-list mode.
    - `iolength_vars` is the I/O list used to compute transfer length, and no other INQUIRE specifiers are allowed at the same time.

    ```fortran
    integer :: n, i
    real :: x
    inquire(iolength=n) i, x
    ```

- `err`:
    - Label to branch to if an error occurs during the inquiry.

    ```fortran
    integer :: ios
    inquire(unit=10, iostat=ios, err=100, opened=is_open)
100 continue
    ```

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
    ()
    ()
    ()
    ()
    ()
    ()
)
```

## See Also

[FileOpen](fileopen.md)
