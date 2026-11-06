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
the bounds inside the body does not change the number of iterations. The head
is always in the [normalized form](../helper_nodes/do_loop_head.md):
`start`, `end` and `increment` are present and have the type of the index
variable.

A `do` loop with no head at all is an infinite loop, `do ... end do`, and is
represented by a head whose members are all `nil`.

### Lowering

Every backend lowers this node itself; no pass rewrites it into a
[WhileLoop](WhileLoop.md). For `do i = a, b, c`:

* `a`, `b` and `c` are evaluated once, before the first iteration, and `i`
  is set to `a`.
* The C-family backends (C, C++, CUDA, Metal) write a C `for` loop, copying
  `b` and `c` into constants first when the body can change them.
* The other backends (LLVM, WASM, x86, MLIR) test whether the loop runs at
  all (`a <= b` for a positive step, `a >= b` for a negative one). After
  each iteration they add `c` to `i` and continue while `i` had not reached
  `b` (a step of `1` or `-1`), or while iterations remain of the unsigned
  count `|b - a| / |c|` (any other step), which is right even where `b - a`
  overflows the kind of `i`. A step of zero is a run-time error.
* [Cycle](Cycle.md) goes to the increment of `i`, and [Exit](Exit.md) past
  the loop and its `orelse`.

After the loop, `i` has the value the standard specifies: one step past its
last value, or `a` for a loop that does not run.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[WhileLoop](WhileLoop.md), [DoConcurrentLoop](DoConcurrentLoop.md), [Exit](Exit.md), [Cycle](Cycle.md), [do_loop_head](../helper_nodes/do_loop_head.md)
