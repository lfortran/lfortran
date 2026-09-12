# intent

How a procedure uses a dummy argument, and what role a variable plays.

## Declaration

### Syntax

```text
intent = Local | In | Out | InOut | ReturnVar | Unspecified
```

### Values

| Value | Meaning |
|----------|-------------|
| `Local` | a local variable, not an argument. |
| `In` | `intent(in)`: the argument is read and must not be written. |
| `Out` | `intent(out)`: the argument is written before it is read. Its value on entry is undefined. |
| `InOut` | `intent(inout)`: the argument is both read and written. |
| `ReturnVar` | the result variable of a function. |
| `Unspecified` | a dummy argument with no `intent` attribute. Whether the procedure writes through it is not known here. |

### Return values

None. An enumeration value is not evaluated.

## Description

The intent is a contract the call site has to satisfy: an actual argument for
an `Out` or `InOut` dummy must be writable, which means a variable expression
or a cast wrapper around one. For `In` and `Unspecified` any expression is
allowed.

`Local` and `ReturnVar` are not argument intents at all: they identify what
the variable is. Every [Variable](../symbol_nodes/Variable.md) carries one of
these six values, so the role of a symbol is never inferred from where it
appears.

## See Also

[Variable](../symbol_nodes/Variable.md), [Function](../symbol_nodes/Function.md), [SubroutineCall](../statement_nodes/SubroutineCall.md), [FunctionCall](../expression_nodes/FunctionCall.md)
