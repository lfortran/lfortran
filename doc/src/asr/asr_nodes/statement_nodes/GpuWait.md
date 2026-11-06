# GpuWait

Waits for the launched GPU work that reads host memory in place.

## Declaration

### Syntax

```text
GpuWait()
```

### Arguments

None.

### Return values

None.

## Description

**GpuWait** blocks the host until every kernel launched so far that the
runtime handed host memory to read in place has completed, without copying
anything back: what the kernels write stays on the device until the next
[GpuSync](GpuSync.md). After it the host may change or free storage that the
pending launches only read. It returns at once when no launch reads host
memory in place, as when the runtime copies what a kernel only reads.

The `gpu_launch_sync` pass places it before a host statement that may change
or free storage a pending launch reads.

## Examples

```clojure
(GpuWait)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[GpuSync](GpuSync.md), [GpuKernelLaunch](GpuKernelLaunch.md), [GpuDiscard](GpuDiscard.md)
