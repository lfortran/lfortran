# SubroutineCall

Calls a subroutine.

## Declaration

### Syntax

```text
SubroutineCall(symbol name, symbol? original_name, call_arg* args,
    expr? dt, bool strict_bounds_checking)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the subroutine actually called, after generic resolution. |
| `original_name` | the symbol as written, when it differs: a [GenericProcedure](../symbol_nodes/GenericProcedure.md) or an [ExternalSymbol](../symbol_nodes/ExternalSymbol.md). `nil` when the call named the procedure directly. |
| `args` | the actual arguments, each a [call_arg](../helper_nodes/call_arg.md). An absent optional argument is a `call_arg` with a `nil` value. |
| `dt` | for a call through a type-bound procedure, the object it was reached through; `nil` otherwise. |
| `strict_bounds_checking` | `true` when the actual and dummy shapes must be checked to agree at run time. |

### Return values

None.

## Description

Resolution has already happened when a **SubroutineCall** exists: `name` is
the one procedure that runs. `original_name` keeps what the user wrote so that
diagnostics and unparsing can show it.

An actual argument for an `intent(out)` or `intent(inout)` dummy must be
writable: a variable, or a cast wrapper around one. Actual arguments are
positional; keyword arguments are reordered by the frontend.

## Examples

```clojure
(SubroutineCall
  :name (SymbolRef 3 "reset")
  :original_name nil
  :args [
    (call_arg
      :value (Var
        :v (SymbolRef 3 "x")
      )
    )
  ]
  :dt nil
  :strict_bounds_checking false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/externalsymbol.asr
:language: clojure
```

## See Also

[FunctionCall](../expression_nodes/FunctionCall.md), [GenericProcedure](../symbol_nodes/GenericProcedure.md), [call_arg](../helper_nodes/call_arg.md), [Function](../symbol_nodes/Function.md)
