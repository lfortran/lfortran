# presence

Whether a dummy argument must be supplied.

## Declaration

### Syntax

```text
presence = Required | Optional
```

### Values

| Value | Meaning |
|----------|-------------|
| `Required` | the argument must be present at every call. |
| `Optional` | the `optional` attribute: the argument may be absent. |

### Return values

None. An enumeration value is not evaluated.

## Description

An absent actual argument is a
[call_arg](../helper_nodes/call_arg.md) whose `value` is `nil`, so a call
always has as many arguments as the procedure has dummies and a backend never
has to match them up itself.

## See Also

[Variable](../symbol_nodes/Variable.md), [call_arg](../helper_nodes/call_arg.md), [FunctionCall](../expression_nodes/FunctionCall.md)
