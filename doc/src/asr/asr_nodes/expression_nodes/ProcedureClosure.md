# ProcedureClosure

Bind a procedure value to its own host activation.

## Declaration

### Syntax

```text
ProcedureClosure(expr procedure, symbol? interface,
    symbol? environment_parameter, expr? environment, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `procedure` | native code, or the named entry when capturing. |
| `interface` | the logical interface declaration, when available. |
| `environment_parameter` | the exact entry dummy that receives the frame. |
| `environment` | an addressable frame designator; absent for native code. |
| `type` | logical `FunctionType` before lowering, `ProcedureType(Closure)` afterwards. |

## Description

The environment and its parameter must either both be present or both be
absent. Entry arguments other than that parameter, and the result, must
match the logical interface's representations. Forwarding entries may
erase bounds but cannot change kinds, ranks or physical array forms.
An explicit `FunctionPointerCast` expresses permitted interface adaptation.

The environment is borrowed; the host must remain active during invocation.
The printer uses internal `procedure_closure(...)` pseudo-Fortran.
