# FileWrite

Transfers data from expressions to a unit.

## Declaration

### Syntax

```text
FileWrite(int label, expr? unit, expr? iomsg, expr? iostat, expr? id,
    expr* values, expr? separator, expr? end, stmt? overloaded,
    bool is_formatted, symbol? nml, expr? rec, expr? pos,
    expr? asynchronous)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit number to act on. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `id` | the identifier of an asynchronous transfer. |
| `values` | the output list. |
| `separator` | the separator written between items. |
| `end` | the string written at the end of the record, for `advance='no'` style output. |
| `overloaded` | the call implementing a user-defined derived type output procedure. |
| `is_formatted` | `false` for an unformatted write. |
| `nml` | the [Namelist](../symbol_nodes/Namelist.md) group for a namelist write. |
| `rec` | the record number for a direct access write. |
| `pos` | the file position for a stream access write. |
| `asynchronous` | `yes` for an asynchronous transfer. |

### Return values

None.

## Description

`write` and [Print](Print.md) are different nodes because `write` names a
unit and carries the full set of I/O specifiers, while `print` always writes a
single formatted record to standard output.

With `nml`, `values` is empty and the variables of the group are written with
their names.

## Examples

```clojure
(FileWrite
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :iomsg nil
  :iostat nil
  :id nil
  :values [
    (Var
      :v (SymbolRef 1 "n")
    )
  ]
  :separator nil
  :end nil
  :overloaded nil
  :is_formatted true
  :nml nil
  :rec nil
  :pos nil
  :asynchronous nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileRead](FileRead.md), [Print](Print.md), [Namelist](../symbol_nodes/Namelist.md), [StringFormat](../expression_nodes/StringFormat.md)
