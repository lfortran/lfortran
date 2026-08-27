# deftype

Whether a procedure has a body here.

## Declaration

### Syntax

```text
deftype = Implementation | Interface
```

### Values

| Value | Meaning |
|----------|-------------|
| `Implementation` | the body is present in this ASR. |
| `Interface` | only the signature is present. |

### Return values

None. An enumeration value is not evaluated.

## Description

An interface block, an external procedure and a procedure read from a module
file as interface ASR are all `Interface`. The distinction is not the same as
the [abi](abi.md): a procedure may have a body and still use a foreign ABI.

## See Also

[FunctionType](../type_nodes/FunctionType.md), [Function](../symbol_nodes/Function.md), [abi](abi.md)
