# string_format_kind

Which formatting language a format string is written in.

## Declaration

### Syntax

```text
string_format_kind
    = FormatFortran
    | FormatFortranLeadingBlank
    | FormatC
    | FormatPythonPercent
    | FormatPythonFString
    | FormatPythonFormat
```

### Values

| Value | Meaning |
|----------|-------------|
| `FormatFortran` | Fortran edit descriptors, `"(f8.3,i4)"`. |
| `FormatFortranLeadingBlank` | Fortran list-directed formatting (`fmt` is `nil`) that starts with a blank when at least one value is written; with `FormatFortran` it starts with the first value. |
| `FormatC` | C `printf` conversions, `"%f: %d"`. |
| `FormatPythonPercent` | Python's `%` operator, `"%f: %d" % (a, b)`. |
| `FormatPythonFString` | an f-string, `f"{a}: {b}"`. |
| `FormatPythonFormat` | `str.format`, `"{}: {}".format(a, b)`. |

### Return values

None. An enumeration value is not evaluated.

## Description

[StringFormat](../expression_nodes/StringFormat.md) carries this so that one
node serves every frontend: the format string is interpreted according to the
language it was written in.

## See Also

[StringFormat](../expression_nodes/StringFormat.md), [Print](../statement_nodes/Print.md), [FileWrite](../statement_nodes/FileWrite.md)
