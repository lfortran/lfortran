# DoConcurrentLoop

A loop whose iterations may run in any order.

## Declaration

### Syntax

```text
DoConcurrentLoop(do_loop_head* head, expr* shared, expr* local,
    reduction_expr* reduction, stmt* body, exec_target exec_target)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `head` | one [do_loop_head](../helper_nodes/do_loop_head.md) per index, giving the index variable and its range. Several heads mean a nest of concurrent indices. |
| `shared` | the variables shared by all iterations. |
| `local` | the variables each iteration has its own copy of. |
| `reduction` | the reductions, each a [reduction_expr](../helper_nodes/reduction_expr.md) pairing an operator with the variable it accumulates into. |
| `body` | the statements of one iteration. |
| `exec_target` | the [exec_target](../enum_nodes/exec_target.md) that says who runs the iterations. |

### Return values

None.

## Description

`do concurrent` asserts that the iterations do not depend on each other, so a
backend may run them in any order or in parallel. ASR records the assertion
and the data environment; it does not check it.

The locality lists say what each iteration sees. A variable in `local` is
private to an iteration, a variable in `shared` is not, and a variable in
`reduction` is combined across iterations with the named operator.

Every parallel loop is said one way before anything decides how to lower it,
and that way is an **OMPRegion**: the `parallel_canonicalize` pass rewrites a
`do concurrent` loop, an `!$omp target` region and, when the compiler is asked
to offload them, an `!$omp parallel do` into one canonical region carrying
[OMPIndependent](../omp_nodes/OMPIndependent.md) -- the assertion this loop
makes. `exec_target` on that region then carries the lowering decision for
that one loop, so a program may mix serial, host-threaded and device loops.

Only the outermost loop of a nest becomes a region, so a concurrent loop left
in the body keeps this node and runs its iterations in order inside the thread
that reached it. Passes that run after the parallel pipeline, such as `forall`
and `array_struct_temporary`, also build this node; `do_loops` lowers all of
them into an ordinary loop nest.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/doconcurrentloop_stmt.asr
:language: clojure
```

## See Also

[DoLoop](DoLoop.md), [OMPRegion](OMPRegion.md), [do_loop_head](../helper_nodes/do_loop_head.md), [reduction_expr](../helper_nodes/reduction_expr.md), [exec_target](../enum_nodes/exec_target.md)
