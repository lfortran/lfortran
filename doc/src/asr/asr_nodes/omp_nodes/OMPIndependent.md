# OMPIndependent

The iterations of this region are independent of one another.

## Declaration

### Syntax

```text
OMPIndependent()
```

### Arguments

None.

### Return values

None.

## Description

The clause records an assertion the compiler is allowed to rely on: no
iteration of the region depends on another, so they may run in any order, on
any number of threads, or on a device, without a dependence analysis.

Fortran's `do concurrent` carries the assertion in the language. An OpenMP
`parallel do` does not — its iterations may synchronize with each other
through `critical`, `atomic` or `ordered` — so the clause is only written
once the body has been checked to hold no such construct.

A region with this clause must contain no nested **OMPRegion** that
synchronizes its iterations, which is what the `parallel_canonicalize` pass
checks before writing it. Every region that pass produces carries the
clause, whichever of the three constructs the region was written as, and no
other region does: it is therefore also what tells the passes below that a
region is a parallel loop they may choose a lowering for, rather than a
construct only the OpenMP pass lowers.

## Examples

```clojure
(OMPIndependent)
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md),
[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md),
[omp_clause](omp_clauses.md)
