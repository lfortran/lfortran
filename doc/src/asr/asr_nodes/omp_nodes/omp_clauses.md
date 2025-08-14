# omp_clause

The ``omp_clause`` union represents OpenMP clauses that modify the behavior of an ``OMPRegion`` directive.

## Declaration

### Syntax

```
omp_clause
    = OMPPrivate(expr* vars)
    | OMPShared(expr* vars)
    | OMPFirstPrivate(expr* vars)
    | OMPReduction(reduction_op operator, expr* vars)
    | OMPCollapse(expr count)
    | OMPNumThreads(expr num_threads)
    | OMPSchedule(schedule_type kind, expr? chunk_size)
    | OMPNumTeams(expr num_teams)
    | OMPThreadLimit(expr thread_limit)
    | OMPMap(map_type type, expr* vars)
```

### Arguments


| Clause Type           | Arguments                                         |
|-----------------------|---------------------------------------------------|
| `OMPPrivate`        | `vars`: List of variables to privatize.        |
| `OMPShared`         | `vars`: List of variables to share.            |
| `OMPFirstPrivate`   | `vars`: List of variables to first-privatize.  |
| `OMPReduction`      | `operator`: Reduction operator (e.g., ReduceAdd). `vars`: List of variables for reduction.       |
| `OMPCollapse`       | `count`: Number of loops to collapse.           |
| `OMPNumThreads`     | `num_threads`: Number of threads.               |
| `OMPSchedule`       | `kind`: Schedule type (e.g., Static). `chunk_size`: Optional chunk size expression.   |
| `OMPNumTeams`     | `num_teams`: Number of teams.                   |
| `OMPThreadLimit`    | `thread_limit`: Thread limit per team.          |
| `OMPMap`            | `type`: Mapping type (e.g., ToFrom). `vars`: List of variables to map.              |


### Return values

None.

## Description

``omp_clause`` defines modifiers for OpenMP regions in the Abstract Semantic Representation (ASR). Clauses control aspects like data scoping (e.g., ``OMPPrivate``, ``OMPShared``), reductions (``OMPReduction``), loop scheduling (``OMPSchedule``), team configuration (``OMPNumTeams``), and device data mapping (``OMPMap``). These clauses are attached to ``OMPRegion`` nodes to specify runtime behavior during execution.

## More enums

- ``map_type``: Enumeration for mapping directions in ``OMPMap``.
  - ``To``: Map data to device.
  - ``From``: Map data from device.
  - ``ToFrom``: Map data to and from device.
  - ``Alloc``: Allocate on device.
  - ``Release``: Release from device.
  - ``Delete``: Delete from device.

- ``schedule_type``: Enumeration for loop scheduling in ``OMPSchedule``.
  - ``Static``: Static scheduling.
  - ``Dynamic``: Dynamic scheduling.
  - ``Guided``: Guided scheduling.
  - ``Auto``: Compiler-decided scheduling.
  - ``Runtime``: Runtime-decided scheduling.

## See OpenMp Documentation to understand each of the constructs with examples

- [OpenMP API 6.0 Specification Guide](https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-6-0.pdf)
- [OpenMP API 6.0 Reference Guide](https://www.openmp.org/wp-content/uploads/OpenMP-RefGuide-6.0-OMP60SC24-web.pdf)