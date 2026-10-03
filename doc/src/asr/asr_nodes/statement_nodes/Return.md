# Return

Returns from a procedure.

## Declaration

### Syntax

```text
Return()
```

### Arguments

None.

### Return values

None.

## Description

**Return** leaves the procedure immediately. A function returns whatever its
result variable holds at that point, so there is nothing to carry here: the
value is in the `return_var` of the
[Function](../symbol_nodes/Function.md).

Falling off the end of a procedure returns as well, so a **Return** is only
needed for an early exit.

## Examples

```clojure
(Return)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/return_stmt.asr
:language: clojure
```

## See Also

[Function](../symbol_nodes/Function.md), [Stop](Stop.md)
