# StringFormat

A formatted string built from a format and a list of values.

## Declaration

### Syntax

```text
StringFormat(expr? fmt, expr* args, string_format_kind kind,
    dtio_binding* dtio_bindings, expr? dtio_unit, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `fmt` | the format, or `nil` for list-directed formatting. |
| `args` | the values to format. |
| `kind` | which formatting language the format is written in; see [string_format_kind](../enum_nodes/string_format_kind.md). |
| `dtio_bindings` | the defined output procedures of the derived-type items, one per declared type and polymorphism; see [dtio_binding](../helper_nodes/dtio_binding.md). Empty when no item has one. |
| `dtio_unit` | the unit passed to a defined output procedure: an `IntegerConstant` or a `Var`, or `nil` for an internal file. Always `nil` when `dtio_bindings` is empty. |
| `type` | the string type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Formatting is an expression producing a string, not a property of the I/O
statements. [Print](../statement_nodes/Print.md) and
[FileWrite](../statement_nodes/FileWrite.md) therefore carry a
**StringFormat** rather than a format and an output list of their own, and a
backend that can build the string has already implemented both.

`kind` distinguishes Fortran edit descriptors from C's `printf` and from
Python's several formatting languages, so one node serves every frontend.

Fortran formats support the Fortran 2023 `LZP`, `LZS`, and `LZ` controls:
print, suppress, or restore processor-defined optional leading zeros in
`F`, `E`, `D`, and real `G` output. The mode lasts for the current formatting
operation, including format reversion; it does not affect `EN`, `ES`, `EX`,
or input conversion. The related `OPEN`/`WRITE` `LEADING_ZERO=` specifiers
are not implemented by these FORMAT controls. Embedded blanks in these
controls are insignificant, including on input, where no target is consumed.
Leading-zero selection does not change the significant digits or rounding
of real `G` output, in either fixed or exponential form. The regression
`integration_tests/format_leading_zero.f90` currently runs only with LLVM:
GFortran 16 rejects these valid Fortran 2023 descriptors, so independent
reference validation remains unavailable.

A derived-type item with a defined output procedure is formatted by calling
that procedure when its edit descriptor is reached. `dtio_bindings` says which
procedure each such item calls, and `dtio_unit` which unit it is given, so the
formatting stays one expression.

## Examples

```clojure
(StringFormat
  :fmt nil
  :args [
    (Var
      :v (SymbolRef 1 "n")
    )
  ]
  :kind :FormatFortran
  :dtio_bindings []
  :dtio_unit nil
  :type (String
    :kind 1
    :len nil
    :len_kind :DeferredLength
    :physical_type :DescriptorString
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/stringformat.asr
:language: clojure
```

## See Also

[string_format_kind](../enum_nodes/string_format_kind.md), [Print](../statement_nodes/Print.md), [FileWrite](../statement_nodes/FileWrite.md), [StringConstant](StringConstant.md)
