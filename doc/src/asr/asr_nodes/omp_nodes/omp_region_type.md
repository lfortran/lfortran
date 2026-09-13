# omp_region_type

Which OpenMP directive an [OMPRegion](../statement_nodes/OMPRegion.md) is.

## Declaration

### Syntax

```text
omp_region_type
    = Parallel
    | Do
    | ParallelDo
    | Sections
    | Section
    | ParallelSections
    | Critical
    | Atomic
    | Barrier
    | Single
    | Master
    | Task
    | Taskwait
    | Taskloop
    | Simd
    | Teams
    | Distribute
    | TeamsDistribute
    | DistributeParallelDo
    | Target
    | TargetData
```

### Values

| Value | Meaning |
|----------|-------------|
| `Parallel` | `parallel`: run the body on a team of threads. |
| `Do` | `do`: share the iterations of the loop in the body among the threads of the enclosing team. |
| `ParallelDo` | `parallel do`: the two above combined. |
| `Sections` | `sections`: a set of blocks, each run once. |
| `Section` | `section`: one block of a `sections` construct. |
| `ParallelSections` | `parallel sections`. |
| `Critical` | `critical`: at most one thread at a time runs the body. |
| `Atomic` | `atomic`: the update in the body happens atomically. |
| `Barrier` | `barrier`: wait for every thread of the team. The body is empty. |
| `Single` | `single`: one thread runs the body; the others wait. |
| `Master` | `master`: only the master thread runs the body, and the others do not wait. |
| `Task` | `task`: package the body as a task to be run later. |
| `Taskwait` | `taskwait`: wait for the tasks created so far. The body is empty. |
| `Taskloop` | `taskloop`: turn the iterations of the loop into tasks. |
| `Simd` | `simd`: run the iterations with vector instructions. |
| `Teams` | `teams`: create a league of teams, for a target device. |
| `Distribute` | `distribute`: share the iterations among the teams of a league. |
| `TeamsDistribute` | `teams distribute`. |
| `DistributeParallelDo` | `distribute parallel do`. |
| `Target` | `target`: run the body on a device. |
| `TargetData` | `target data`: map data to a device for the body, without running it there. |

### Return values

None.

## Description

One [OMPRegion](../statement_nodes/OMPRegion.md) node represents every
directive, and this member says which one. Combined directives such as
`parallel do` have their own value rather than being represented as nested
regions, so the region tree matches what was written.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md), [DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md)
