# omp_clause

The clauses that can appear on an OpenMP directive.

## Declaration

### Syntax

```text
omp_clause
  = OMPPrivate(expr* vars)
  | OMPShared(expr* vars)
  | OMPFirstPrivate(expr* vars)
  | OMPLastPrivate(expr* vars)
  | OMPReduction(reduction_op operator, expr* vars)
  | OMPCollapse(expr count)
  | OMPIf(expr condition)
  | OMPNumThreads(expr num_threads)
  | OMPSchedule(schedule_type kind, expr? chunk_size)
  | OMPNowait()
  | OMPNumTeams(expr num_teams)
  | OMPThreadLimit(expr thread_limit)
  | OMPDevice(expr device)
  | OMPMap(map_type type, expr* vars)
  | OMPIndependent()
  | OMPTargetRequested()
```

### Arguments

None.

### Return values

None.

## Description

The clauses of an [OMPRegion](../statement_nodes/OMPRegion.md) are a list of
these. Each constructor carries exactly what its clause was written with, so
nothing has to be recovered from a list of expressions later.

The data sharing clauses ([OMPPrivate](OMPPrivate.md),
[OMPShared](OMPShared.md), [OMPFirstPrivate](OMPFirstPrivate.md),
[OMPLastPrivate](OMPLastPrivate.md) and [OMPReduction](OMPReduction.md)) say
what each thread sees. The rest control how the region runs
([OMPNumThreads](OMPNumThreads.md), [OMPSchedule](OMPSchedule.md),
[OMPCollapse](OMPCollapse.md), [OMPIf](OMPIf.md), [OMPNowait](OMPNowait.md))
or where ([OMPDevice](OMPDevice.md), [OMPMap](OMPMap.md),
[OMPNumTeams](OMPNumTeams.md), [OMPThreadLimit](OMPThreadLimit.md)).
Two record what the source asserted rather than what it asked to be done:
[OMPIndependent](OMPIndependent.md) says the iterations do not depend on
one another, and [OMPTargetRequested](OMPTargetRequested.md) says a device
was asked for.

A clause that is not written is simply absent from the list; there is no
default clause node.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md), [omp_region_type](omp_region_type.md)
