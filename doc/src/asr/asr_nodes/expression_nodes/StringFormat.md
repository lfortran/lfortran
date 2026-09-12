# StringFormat

A formatted string built from a format and a list of values.

## Declaration

### Syntax

```text
StringFormat(expr? fmt, expr* args, string_format_kind kind,
    ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `fmt` | the format, or `nil` for list-directed formatting. |
| `args` | the values to format. |
| `kind` | which formatting language the format is written in; see [string_format_kind](../enum_nodes/string_format_kind.md). |
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
