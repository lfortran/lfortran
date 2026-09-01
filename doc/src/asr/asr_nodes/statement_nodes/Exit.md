# Exit

Leaves a loop or a named construct.

## Declaration

### Syntax

```text
Exit(identifier? stmt_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `stmt_name` | the construct name to leave, or `nil` for the innermost enclosing loop. |

### Return values

None.

## Description

Execution continues after the construct that was left. With a name, **Exit**
leaves that construct, so `exit outer` from an inner loop leaves both.

## Examples

```clojure
(Exit
  :stmt_name "loop"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[Cycle](Cycle.md), [DoLoop](DoLoop.md), [WhileLoop](WhileLoop.md)
