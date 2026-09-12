# GoTo

Jumps to a labelled statement in the same procedure.

## Declaration

### Syntax

```text
GoTo(int target_id, identifier name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target_id` | the `id` of the [GoToTarget](GoToTarget.md) to jump to. |
| `name` | the label as it was written, for diagnostics. |

### Return values

None.

## Description

The jump is linked by the integer `target_id` rather than by a pointer to the
target node. A pointer would have to be fixed up every time ASR is serialised
into a module file and read back; an integer survives that untouched.

`target_id` is unique within a procedure only, and a `go to` may not leave the
procedure it appears in.

## Examples

```clojure
(GoTo
  :target_id 100
  :name "100"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/goto_stmt.asr
:language: clojure
```

## See Also

[GoToTarget](GoToTarget.md), [IfArithmetic](IfArithmetic.md), [Assign](Assign.md)
