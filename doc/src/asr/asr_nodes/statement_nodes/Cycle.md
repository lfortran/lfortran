# Cycle

Starts the next iteration of a loop.

## Declaration

### Syntax

```text
Cycle(identifier? stmt_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `stmt_name` | the construct name of the loop to continue, or `nil` for the innermost enclosing loop. |

### Return values

None.

## Description

**Cycle** skips the rest of the loop body. With a construct name it applies to
the named loop, which is how `cycle outer` leaves an inner loop as well.

The name refers to the `name` member of the enclosing
[DoLoop](DoLoop.md) or [WhileLoop](WhileLoop.md), not to a symbol.

## Examples

```clojure
(Cycle
  :stmt_name "loop"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[Exit](Exit.md), [DoLoop](DoLoop.md), [WhileLoop](WhileLoop.md)
