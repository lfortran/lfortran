# OMPRegion

The OMPRegion node represents an OpenMP region directive and its enclosed statements.

## Declaration

### Syntax

```
OMPRegion(omp_region_type region, omp_clause* clauses, stmt* body)
```

### Arguments

| Argument name | Argument Description |
|---------------|----------------------|
| `region`      | The type of OpenMP region (e.g., Parallel, Target, Teams). |
| `clauses`     | A list of OpenMP clauses associated with the region. |
| `body`        | A list of statements enclosed within the OpenMP region. |

### Return values

None.

## Description

OMPRegion encapsulates OpenMP directives and their associated constructs in the Abstract Semantic Representation (ASR). It supports various OpenMP region types, such as Parallel, Do, ParallelDo, Sections, Single, Master, Task, Teams, Distribute, Target, and others. Each region can include clauses like private, shared, reduction, map, schedule, num_threads, and num_teams to specify behavior such as data sharing, loop scheduling, or device offloading. The node decouples OpenMP pragmas from general loop constructs, enabling targeted lowering to runtime libraries like libgomp for parallel execution.

## Types

The `region` field uses the `omp_region_type` enumeration, which includes values like Parallel, Target, Teams, and ParallelDo. Clauses are represented by the `omp_clause` union, supporting types such as OMPPrivate, OMPShared, OMPReduction, OMPMap (with map_type like To, From, ToFrom), and OMPSchedule (with schedule_type like Static, Dynamic, Runtime). The body consists of ASR statement nodes.

## Examples

```fortran
program openmp_52
  use omp_lib
  implicit none
  integer, parameter :: N = 100, init=0
  integer :: a(N), i, total
  a = 1  ! Initialize all elements to 1

  !$omp parallel shared(a, total) private(i)
    total = init  ! Initialize total to 0
    !$omp barrier
    
    !$omp do
        do i = 1, N
            !$omp critical
            total = total + a(i)
            !$omp end critical
        end do
    !$omp end do
  !$omp end parallel

  print *, "Total sum:", total
  if (total /= N) error stop "Incorrect sum"
end program openmp_52
```

ASR:

```clojure
(OMPRegion
    Parallel
    [(OMPShared
        [(Var 2 a)
        (Var 2 total)]
    )
    (OMPPrivate
        [(Var 2 i)]
    )]
    [(Assignment
        (Var 2 total)
        (Var 2 init)
        ()
        .false.
    )
    (OMPRegion
        Barrier
        []
        []
    )
    (OMPRegion
        Do
        []
        [(DoLoop
            ()
            ((Var 2 i)
            (IntegerConstant 1 (Integer 4) Decimal)
            (Var 2 n)
            ())
            [(OMPRegion
                Critical
                []
                [(Assignment
                    (Var 2 total)
                    (IntegerBinOp
                        (Var 2 total)
                        Add
                        (ArrayItem
                            (Var 2 a)
                            [(()
                            (Var 2 i)
                            ())]
                            (Integer 4)
                            ColMajor
                            ()
                        )
                        (Integer 4)
                        ()
                    )
                    ()
                    .false.
                )]
            )]
            []
        )]
    )]
)
```
## See Also
