# OMPRegion

An OpenMP region: a directive, its clauses and the code it applies to.

## Declaration

### Syntax

```text
OMPRegion(omp_region_type region, omp_clause* clauses, stmt* body)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `region` | which directive this is; see [omp_region_type](../omp_nodes/omp_region_type.md). |
| `clauses` | the clauses of the directive; see [omp_clause](../omp_nodes/omp_clauses.md). |
| `body` | the statements the directive applies to. |

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

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[omp_region_type](../omp_nodes/omp_region_type.md), [omp_clause](../omp_nodes/omp_clauses.md), [DoConcurrentLoop](DoConcurrentLoop.md)
