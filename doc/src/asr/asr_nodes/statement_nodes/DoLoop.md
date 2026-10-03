# DoLoop

A counted `do` loop.

## Declaration

### Syntax

```text
DoLoop(identifier? name, do_loop_head head, stmt* body, stmt* orelse)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the construct name, or `nil`. [Exit](Exit.md) and [Cycle](Cycle.md) refer to it. |
| `head` | the [do_loop_head](../helper_nodes/do_loop_head.md) with the index variable, the start and end values and the step. |
| `body` | the statements of the loop. |
| `orelse` | statements to run when the loop finishes without an `exit`. Fortran has no such clause; it is there for Python's `for ... else`. |

### Return values

None.

## Description

The trip count is computed from `head` before the first iteration, so changing
the bounds inside the body does not change the number of iterations. An
increment of `nil` means one.

A `do` loop with no head at all is an infinite loop, `do ... end do`, and is
represented by a head whose members are all `nil`.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[WhileLoop](WhileLoop.md), [DoConcurrentLoop](DoConcurrentLoop.md), [Exit](Exit.md), [Cycle](Cycle.md), [do_loop_head](../helper_nodes/do_loop_head.md)
