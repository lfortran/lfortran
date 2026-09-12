# exec_target

How one parallel loop is lowered.

## Declaration

### Syntax

```text
exec_target = ExecAuto | ExecSerial | ExecHostThreads | ExecDevice
```

### Values

| Value | Meaning |
|----------|-------------|
| `ExecAuto` | not decided yet. This is what the frontend and the passes that build a loop write. |
| `ExecSerial` | one thread runs the iterations in order, as an ordinary [DoLoop](../statement_nodes/DoLoop.md). |
| `ExecHostThreads` | the iterations are spread over the threads of the host. |
| `ExecDevice` | the iterations become a GPU kernel, launched with a [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md). |

### Return values

None. An enumeration value is not evaluated.

## Description

The execution target is a member of
[OMPRegion](../statement_nodes/OMPRegion.md), so the choice is made per loop
rather than per compilation. A `do concurrent` loop, an `!$omp target` region
and an `!$omp parallel do` are canonicalized into that one node before
anything lowers them, and each region carries the answer to "who runs these
iterations".

The frontend never picks a target: it writes `ExecAuto` and leaves the
decision to the `parallel_dispatch` pass, which reads the command line. Every
lowering below that pass claims only the loops assigned to it, so a loop the
GPU declines can be handed back to the host threads instead of being lost.

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md), [GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [OMPRegion](../statement_nodes/OMPRegion.md), [exec_space](exec_space.md)
