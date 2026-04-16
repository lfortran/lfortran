# FileOpen

Open a file or connect a unit to a file.

## Declaration

### Syntax

```fortran
FileOpen(int label, expr? newunit, expr? filename, expr? status, expr? form, expr? access, expr? iostat, expr? iomsg, expr? action, expr? delim, expr? recl, expr? position, expr? blank, expr? encoding, expr? sign, expr? decimal, expr? round, expr? pad)
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
| `round` | `expr?` | Character expression specifying the rounding mode for formatted output (`'UP'`, `'DOWN'`, `'ZERO'`, `'NEAREST'`, `'COMPATIBLE'`, or `'PROCESSOR_DEFINED'`). |
| `pad` | `expr?` | Character expression specifying the padding mode for formatted input (`'YES'` or `'NO'`). |
### Return values

None.

## Description

The `FileOpen` node represents the Fortran `OPEN` statement, which is used to connect an external file to a unit, create a new file and connect it to a unit, or change certain specifiers of a connection between a file and a unit.

### Mode behavior (brief)

- `newunit`:
    - If `newunit` is present, the system will select an available unit number and return it in `newunit`.
    - If `newunit` is not present, the unit number must be specified in the `filename` argument.

- `label`:
    - Statement label for branching on error.
    
- `iostat` and `iomsg`:
    - `iostat` receives a processor-dependent status code for the I/O statement.
    - A value of `0` indicates success; nonzero values indicate error or end conditions.
    - `iomsg` receives a processor-provided diagnostic message when a nonzero `iostat` condition is reported.

    ```fortran
    integer :: ios
    character(len=256) :: msg
    open(unit=10, file="data.txt", status="old", iostat=ios, iomsg=msg)
    ```

- `status`:
    - `OLD`: file must already exist.
    - `NEW`: file must not exist.
    - `SCRATCH`: connect to a temporary unnamed file (typically deleted on close).
    - `REPLACE`: replace if exists, otherwise create.
    - `UNKNOWN`: processor/runtime chooses behavior when file existence is unknown.

    ```fortran
    open(unit=10, file="out.txt", status="replace")
    ```

- `form`:
    - `FORMATTED`: text I/O.
    - `UNFORMATTED`: binary-like record I/O.

    ```fortran
    open(unit=20, file="data.bin", form="unformatted")
    ```

- `access`:
    - `SEQUENTIAL`: read/write in order.
    - `DIRECT`: fixed-length records (`recl=` needed).
    - `STREAM`: byte stream (no record boundaries).

    ```fortran
    open(unit=30, file="tbl.dat", access="direct", recl=128)
    ```

- `action`:
    - `READ`, `WRITE`, `READWRITE` control permitted operations.

    ```fortran
    open(unit=40, file="in.txt", action="read")
    ```

- `delim` (list-directed/namelist output):
    - `QUOTE`: uses `"text"`.
    - `APOSTROPHE`: uses `'text'`.
    - `NONE`: no delimiters around character values.

    ```fortran
    open(unit=41, file="out.txt", delim="quote")
    ```

- `position`:
    - `REWIND`: start of file.
    - `APPEND`: end of file.
    - `ASIS`: keep current position.

    ```fortran
    open(unit=50, file="log.txt", position="append")
    ```

- `blank` (formatted numeric input):
    - `NULL`: ignore embedded blanks in numeric input fields.
    - `ZERO`: interpret embedded blanks as zeros.

    ```fortran
    open(unit=51, file="nums.txt", blank="zero")
    ```

- `encoding`:
    - `UTF-8`: UTF-8 encoded text stream.
    - `DEFAULT`: processor default encoding.

    ```fortran
    open(unit=52, file="utf8.txt", encoding="utf-8")
    ```

- `sign` (formatted numeric output):
    - `PLUS`: always print sign for positive and negative values.
    - Example effect: `+3`, `-3`
    - `SUPPRESS`: do not print `+` for positive values; still print `-` for negatives.
    - Example effect: `3`, `-3`
    - `PROCESSOR_DEFINED`: use compiler/runtime default sign behavior.

    ```fortran
    open(unit=53, file="nums.txt", sign="plus")
    ! write(53,'(F5.1)') 3.0   -> " +3.0"
    ! write(53,'(F5.1)') -3.0  -> " -3.0"
    ```

- `decimal` (formatted I/O):
    - `POINT`: decimal point, e.g., `3.14`.
    - `COMMA`: decimal comma, e.g., `3,14`.

    ```fortran
    open(unit=54, file="nums.txt", decimal="comma")
    ```

- `round` (formatted output rounding mode):
    - `UP`: round away from zero at halfway and above.
    - Example effect (`1.26` to one decimal): `1.3`
    - `DOWN`: round toward zero.
    - Example effect (`1.26` to one decimal): `1.2`
    - `ZERO`: round toward zero (same direction idea as `DOWN` for many cases).
    - Example effect (`-1.26` to one decimal): `-1.2`
    - `NEAREST`: round to nearest representable result.
    - `COMPATIBLE`: round in a processor-compatible mode.
    - `PROCESSOR_DEFINED`: compiler/runtime default rounding mode.

    ```fortran
    open(unit=55, file="nums.txt", round="down")
    ! write(55,'(F4.1)') 1.26   -> "1.2"
    ```

- `pad` (formatted input only):
    - `YES`: if a read needs more characters than remain in the record, missing characters are treated as blanks.
    - `NO`: no blank padding; attempting to read past record end can raise an end-of-record condition.

    ```fortran
    ! File line is only: "12"
    open(unit=60, file="nums.txt", form="formatted", pad="yes")
    read(60, '(A4)') s   ! s becomes "12  " (padded with blanks)
    ```

    ```fortran
    ! Same file line: "12"
    open(unit=61, file="nums.txt", form="formatted", pad="no")
    read(61, '(A4)') s   ! no padding; read can hit end-of-record
    ```

- `recl`:
    - For `DIRECT` access, specifies the record length in file storage units (processor dependent).
    - Required when `access="DIRECT"`.

    ```fortran
    open(unit=70, file="data.dat", access="direct", recl=256)
    ```

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
    ()
    ()
)
```

## See Also

[FileClose](fileclose.md), [FileInquire](fileinquire.md)
