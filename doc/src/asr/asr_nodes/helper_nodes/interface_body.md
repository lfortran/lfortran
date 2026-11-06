# interface_body

Facts about a [Function](../symbol_nodes/Function.md) that is an interface
body (`deftype=Interface`), owned by its `interface_body` field.

## Declaration

### Syntax

```text
interface_body = (bool abstract)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `abstract` | `true` when the body is in an abstract interface block. |

### Return values

None.

## Description

An interface body in an ordinary interface block names a procedure that exists
elsewhere, such as an external procedure, so it has a link name and calls may
target it. An interface body in an abstract interface block names no
procedure: its name is not a global identifier. It only gives the
characteristics of procedure pointers, dummy procedures and deferred bindings
declared with it, has no link name and is never a call target. A code
generator therefore declares no external symbol for it, and an external
procedure or a `bind(c)` procedure with the same name is unrelated to it.

An absent `interface_body` means the interface body names an existing
procedure. ASR verification requires the field to appear only on an
interface.

## See Also

[Function](../symbol_nodes/Function.md), [FunctionType](../type_nodes/FunctionType.md)
