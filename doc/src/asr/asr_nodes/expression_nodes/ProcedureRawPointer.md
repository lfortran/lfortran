# ProcedureRawPointer

Checked export of a Fortran procedure value to a raw native pointer.

## Declaration

### Syntax

```text
ProcedureRawPointer(expr procedure, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `procedure` | a closure-represented procedure value. |
| `type` | `ProcedureType(RawPointer)` with the same logical signature. |

## Description

A known capture is diagnosed; a dynamic value requires a located runtime
check that its environment is null. Capturing external C callbacks are not
supported. Noncapturing code and null pointers remain exportable.
The printer uses internal `procedure_raw_pointer(...)` pseudo-Fortran.
