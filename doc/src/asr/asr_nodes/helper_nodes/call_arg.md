# call_arg

One actual argument of a call.

## Declaration

### Syntax

```text
call_arg = (expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `value` | the argument, or `nil` when an optional argument is absent. |

### Return values

None.

## Description

An argument list always has one **call_arg** per dummy argument, in the
procedure's order: keyword arguments are reordered by the frontend and an
absent optional argument is a `nil` value. A backend therefore never has to
match actual arguments to dummies itself.

## Examples

```clojure
(call_arg
  :value (IntegerConstant
    :n 5
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/functioncall.asr
:language: clojure
```

## See Also

[FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md), [presence](../enum_nodes/presence.md), [StructConstructor](../expression_nodes/StructConstructor.md)
