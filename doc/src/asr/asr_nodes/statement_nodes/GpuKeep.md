# GpuKeep

Tells the runtime that the host does not change some arrays until a launch
reads them again.

## Declaration

### Syntax

```text
GpuKeep(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the arrays: whole, contiguous arrays that are locals or dummy arguments of the procedure. |

### Return values

None.

## Description

A runtime with memory of its own copies an array a
[GpuKernelLaunch](GpuKernelLaunch.md) only reads to the device, and keeps
that copy for the launches after it until the host waits
([GpuSync](GpuSync.md) or [GpuWait](GpuWait.md)): only then may the host
change the array. **GpuKeep** comes right before such a wait, and names the
arrays the host does not change from there until a launch reads them or the
next wait: the copies of them stay on the device past this wait. A time loop
whose host reads the result of a reduction at every iteration waits there,
and copies the arrays it reads only once.

The `gpu_launch_sync` pass places it; see
[GPU offloading](../../../gpu_offloading.md).

## Examples

```clojure
(GpuKeep
  :vars [
    (Var
      :v (SymbolRef 2 "a")
    )
  ]
)
```

## See Also

[GpuKernelLaunch](GpuKernelLaunch.md), [GpuSync](GpuSync.md), [GpuWait](GpuWait.md), [GpuDiscard](GpuDiscard.md)
