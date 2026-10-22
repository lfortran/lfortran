# OMPOverwrite

The iterations of this region assign every element of an array before they
read it, when a condition holds.

## Declaration

### Syntax

```text
OMPOverwrite(expr array, expr condition)
```

### Arguments

| Argument name | Argument type | Argument description |
|---------------|---------------|----------------------|
| `array` | `expr` | The array, a variable or a component of one. |
| `condition` | `expr` | A logical host expression, evaluated on entry to the region. |

### Return values

None.

## Description

When `condition` holds on entry to a parallel region assigned to the device,
its iterations together assign every element of `array` before any of them
reads one, so what the host holds in `array` need not be copied to the
device. The offload hands this to the launch as the `overwritten` and
`overwritten_if` of a
[GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md).

The offload shows this itself for a loop whose assignment has the loop
variables as subscripts. A loop transformation that rewrites such a loop
into a form the offload cannot read it from, but that runs the same
iterations and assignments, records it from the loop as it was: index-set
splitting does, since the frame it adds runs the original body on indices
it computes. A pass that changes which iterations the region runs drops the
clause, and so does the offload when it runs the region on one thread inside
device code, where there is no launch.

The source never writes it.

## Examples

```clojure
(OMPOverwrite
    (Var 2 d)
    (LogicalBinOp
        (IntegerCompare (Cast (IntegerConstant 1 (Integer 4) Decimal) IntegerToInteger (Integer 8) () ()) Eq (ArrayBound (Var 2 d) (IntegerConstant 1 (Integer 4) Decimal) (Integer 8) LBound ()) (Logical 4) ())
        And
        (IntegerCompare (Cast (Var 2 n) IntegerToInteger (Integer 8) () ()) Eq (ArrayBound (Var 2 d) (IntegerConstant 1 (Integer 4) Decimal) (Integer 8) UBound ()) (Logical 4) ())
        (Logical 4)
        ()
    )
)
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md),
[GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md),
[omp_clause](omp_clauses.md)
