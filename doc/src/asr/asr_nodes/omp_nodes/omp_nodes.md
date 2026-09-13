# ASR OpenMP Nodes

OpenMP is represented by one statement,
[OMPRegion](../statement_nodes/OMPRegion.md), holding a directive, its clauses
and the block it applies to. The directive is an
[omp_region_type](omp_region_type.md) and the clauses are
[omp_clause](omp_clauses.md) values.

Nesting in ASR follows the nesting of the directives: `!$omp parallel`
containing `!$omp do` is an **OMPRegion** whose body holds another
**OMPRegion**.

```{toctree}
---
maxdepth: 1
---
map_type
omp_clauses
omp_region_type
OMPCollapse
OMPDevice
OMPFirstPrivate
OMPIf
OMPIndependent
OMPLastPrivate
OMPMap
OMPNowait
OMPNumTeams
OMPNumThreads
OMPPrivate
OMPReduction
OMPSchedule
OMPShared
OMPTargetRequested
OMPThreadLimit
schedule_type
```
