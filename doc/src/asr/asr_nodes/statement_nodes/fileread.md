# FileRead

Read data from a file.

## Declaration

### Syntax

```fortran
FileRead(int label, expr? unit, expr? fmt, expr? iomsg, expr? iostat, expr? advance, expr? size, expr? id, expr? pos, expr* values, stmt? overloaded, bool is_formatted, symbol? nml, expr? rec, expr? pad)
```

### Arguments

| Argument | Type | Description |
| :--- | :--- | :--- |
| `label` | `int` | Statement label. |
| `unit` | `expr?` | Integer expression specifying the unit number. |
| `fmt` | `expr?` | Format specification. |
| `iomsg` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `iostat` | `expr?` | See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`). |
| `advance` | `expr?` | Character expression (`'YES'` or `'NO'`) for non-advancing I/O. |
| `size` | `expr?` | Integer variable to receive the number of characters read. |
| `id` | `expr?` | Integer expression for asynchronous I/O. |
| `pos` | `expr?` | Integer expression for file position (stream I/O). |
| `values` | `expr*` | List of variables to read into. |
| `overloaded` | `stmt?` | Overloaded statement (if any). |
| `is_formatted` | `bool` | Whether the read is formatted. |
| `nml` | `symbol?` | Namelist symbol (if using namelist I/O). |
| `rec` | `expr?` | Record number for direct access I/O. |
| `pad` | `expr?` | Character expression for padding mode. For details, see [FileOpen](fileopen.md#arguments). |

### Return values

None.

## Description

The `FileRead` node represents the Fortran `READ` statement, which is used to transfer data from an external file or an internal file to the items in the input list or a namelist group.

### Specifier behavior (brief)

- `unit`:
    - Selects the external unit (or internal file expression) used as the read source.

- `fmt`:
    - Selects formatted input interpretation.
    - Typical forms include explicit format strings and list-directed input (`*`).

    ```fortran
    read(10, '(I5,1X,F8.3)') i, x
    read(10, *) i, x
    ```

- `advance` (formatted sequential I/O):
    - `YES`: normal advancing read (default behavior).
    - `NO`: non-advancing read; the file position stays on the current record.

    ```fortran
    read(10, '(A)', advance='no', size=n) chunk
    ```

- `size`:
    - Receives the number of characters transferred in a non-advancing formatted read.

- `id`:
    - Associates the statement with an asynchronous transfer identifier.

- `pos`:
    - Sets/uses stream file position for stream access reads.

- `rec`:
    - Record number for direct-access I/O.

    ```fortran
    read(unit=20, rec=5) row
    ```

- `values`:
    - Input list of variables/items populated by the read.

- `is_formatted`:
    - Internal ASR flag indicating whether this read is treated as formatted transfer.

- `overloaded`:
    - Internal lowered statement used when READ resolves through an overloaded procedure.

- `nml`:
    - Namelist group symbol when the read is namelist-driven.

    ```fortran
    read(unit=30, nml=config)
    ```

- `pad`:
    - Uses OPEN-compatible padding mode semantics for formatted input.
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief).

- `iostat` and `iomsg`:
    - See [FileOpen mode behavior](fileopen.md#mode-behavior-brief) (`iostat` and `iomsg`).

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
    ()
)
```

## See Also

[FileWrite](filewrite.md), [FileOpen](fileopen.md)
