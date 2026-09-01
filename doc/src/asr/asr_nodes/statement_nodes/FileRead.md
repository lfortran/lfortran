# FileRead

Transfers data from a unit into variables.

## Declaration

### Syntax

```text
FileRead(int label, expr? unit, expr? fmt, expr? iomsg, expr? iostat,
    expr? advance, expr? size, expr? id, expr? pos, expr* values,
    stmt? overloaded, bool is_formatted, symbol? nml, expr? rec,
    expr? pad)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `fmt` | the format, or `nil` for list-directed input. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `advance` | `yes` or `no`: whether the file position moves to the next record after the transfer. |
| `size` | a variable receiving the number of characters transferred in a non-advancing read. |
| `id` | the identifier of an asynchronous transfer. |
| `pos` | the file position for a stream access read. |
| `values` | the input list: the variables the data is read into. |
| `overloaded` | the call implementing a user-defined derived type input procedure. |
| `is_formatted` | `false` for an unformatted read. |
| `nml` | the [Namelist](../symbol_nodes/Namelist.md) group for a namelist read. |
| `rec` | the record number for a direct access read. |
| `pad` | `yes` or `no`: whether a short record is padded with blanks. |

### Return values

None.

## Description

The input list is a list of expressions rather than a list of variables,
because an item may be an array section, a component or an
[ImpliedDoLoop](../expression_nodes/ImpliedDoLoop.md). Every one of them must
be writable.

With `nml`, `values` is empty: a namelist read transfers the variables of the
group, matching them by name in the file.

## Examples

```clojure
(FileRead
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :fmt nil
  :iomsg nil
  :iostat (Var
    :v (SymbolRef 1 "iostat")
  )
  :advance nil
  :size nil
  :id nil
  :pos nil
  :values [
    (Var
      :v (SymbolRef 1 "n")
    )
  ]
  :overloaded nil
  :is_formatted true
  :nml nil
  :rec nil
  :pad nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileWrite](FileWrite.md), [FileOpen](FileOpen.md), [Namelist](../symbol_nodes/Namelist.md)
