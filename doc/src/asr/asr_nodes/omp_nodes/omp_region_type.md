
# omp_region_type

The ``omp_region_type`` enumeration defines the types of OpenMP regions supported in the Abstract Semantic Representation (ASR).

## Declaration

### Syntax

```
omp_region_type = Parallel | Do | ParallelDo | Sections | Section | ParallelSections | Critical | Atomic | Barrier | Single | Master | Task | Taskwait | Taskloop | Simd | Teams | Distribute | TeamsDistribute | DistributeParallelDo | Target | TargetData
```
### Arguments

None.

### Return values

None.

## Description

``omp_region_type`` specifies the category of an OpenMP directive within an ``OMPRegion`` node. Each value corresponds to a specific OpenMP construct, such as parallel execution (``Parallel``), loop distribution (``Distribute``), or device offloading (``Target``). This enumeration enables the ASR to represent diverse OpenMP behaviors, facilitating lowering to appropriate runtime calls.

## Types

An enumeration with the following values:

- ``Parallel``: Parallel region.
- ``Do``: Loop construct.
- ``ParallelDo``: Combined parallel loop.
- ``Sections``: Sections construct.
- ``Section``: Single section within sections.
- ``ParallelSections``: Combined parallel sections.
- ``Critical``: Critical section.
- ``Atomic``: Atomic update.
- ``Barrier``: Synchronization barrier.
- ``Single``: Single execution.
- ``Master``: Master thread execution.
- ``Task``: Task construct.
- ``Taskwait``: Task synchronization.
- ``Taskloop``: Task loop.
- ``Teams``: Teams construct.
- ``Distribute``: Distribute construct.
- ``TeamsDistribute``: Combined teams distribute.
- ``DistributeParallelDo``: Combined distribute parallel loop.
- ``Target``: Target offloading.

## See OpenMp Documentation to understand each of the constructs with examples

- [OpenMP API 6.0 Specification Guide](https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-6-0.pdf)
- [OpenMP API 6.0 Reference Guide](https://www.openmp.org/wp-content/uploads/OpenMP-RefGuide-6.0-OMP60SC24-web.pdf)