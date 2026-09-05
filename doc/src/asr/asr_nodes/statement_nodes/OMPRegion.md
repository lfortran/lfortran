# OMPRegion

An OpenMP region: a directive, its clauses and the code it applies to.

## Declaration

### Syntax

```text
OMPRegion(omp_region_type region, omp_clause* clauses, stmt* body, exec_target exec_target)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `region` | which directive this is; see [omp_region_type](../omp_nodes/omp_region_type.md). |
| `clauses` | the clauses of the directive; see [omp_clause](../omp_nodes/omp_clauses.md). |
| `body` | the statements the directive applies to. |
| `exec_target` | which lowering runs the region; see [exec_target](../enum_nodes/exec_target.md). |

### Return values

None.

## Description

One node represents every OpenMP construct. `region` says which directive it
is, `clauses` carries what was written after it, and `body` is the structured
block it applies to. Regions nest exactly as the directives do, so
`!$omp parallel` containing `!$omp do` is an **OMPRegion** of type `Parallel`
whose body holds an **OMPRegion** of type `Do`.

A directive that applies to no block, such as `barrier` or `taskwait`, is an
**OMPRegion** with an empty `body`.

One shape of this node is a contract the lowerings of a parallel loop read:
the canonical parallel loop the `parallel_canonicalize` pass produces. It is a
single region of type `ParallelDo`, carrying
[OMPIndependent](../omp_nodes/OMPIndependent.md), whose clauses are the data
environment of the whole construct and whose body is one perfectly nested
**DoLoop** nest, as deep as `collapse` says. No `target`, `teams` or
`distribute` wrapper survives it; a region that was one carries
[OMPTargetRequested](../omp_nodes/OMPTargetRequested.md) instead. Every
parallel loop reaches the offload pass and the OpenMP pass in this one shape,
whether it was written as `do concurrent`, as an `!$omp target` region or as
an `!$omp parallel do`.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[omp_region_type](../omp_nodes/omp_region_type.md), [omp_clause](../omp_nodes/omp_clauses.md), [DoConcurrentLoop](DoConcurrentLoop.md)
