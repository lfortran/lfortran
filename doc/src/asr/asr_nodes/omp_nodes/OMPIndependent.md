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

Fortran's `do concurrent` carries the assertion in the language, so a
`do concurrent` loop becomes an **OMPRegion** with this clause. An OpenMP
`parallel do` does not assert it — its iterations may synchronize with each
other through `critical`, `atomic` or `ordered` — so it does not carry the
clause unless something proves the iterations independent.

A region with this clause must contain no nested **OMPRegion** that
synchronizes its iterations.

## Examples

```clojure
(OMPIndependent)
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md),
[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md),
[omp_clause](omp_clauses.md)
