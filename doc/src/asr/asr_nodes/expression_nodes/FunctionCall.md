# FunctionCall

Calls a function and uses its result.

## Declaration

### Syntax

```text
FunctionCall(symbol name, symbol? original_name, call_arg* args,
    ttype type, expr? value, expr? dt)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the function actually called, after generic resolution. |
| `original_name` | the symbol as written, when it differs; `nil` otherwise. |
| `args` | the actual arguments, each a [call_arg](../helper_nodes/call_arg.md). |
| `type` | the type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |
| `dt` | for a call through a type-bound procedure, the object it was reached through; `nil` otherwise. |

### Return values

The value of the expression.

## Description

A **FunctionCall** is an expression, so it appears where a value is wanted;
[SubroutineCall](../statement_nodes/SubroutineCall.md) is the statement form.
Both store the resolved procedure in `name` and what was written in
`original_name`.

An actual argument for an `intent(out)` or `intent(inout)` dummy must be
writable. `value` is set when the frontend could evaluate the call at compile
time, which it can do for some intrinsics.

## Examples

```clojure
(FunctionCall
  :name (SymbolRef 3 "square")
  :original_name nil
  :args [
    (call_arg
      :value (IntegerConstant
        :n 5
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
    )
  ]
  :type (Integer
    :kind 4
  )
  :value nil
  :dt nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/functioncall.asr
:language: clojure
```

## See Also

[SubroutineCall](../statement_nodes/SubroutineCall.md), [Function](../symbol_nodes/Function.md), [call_arg](../helper_nodes/call_arg.md), [GenericProcedure](../symbol_nodes/GenericProcedure.md)
