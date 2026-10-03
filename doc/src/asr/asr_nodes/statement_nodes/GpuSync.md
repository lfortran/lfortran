# GpuSync

Waits for the launched GPU work to finish.

## Declaration

### Syntax

```text
GpuSync()
```

### Arguments

None.

### Return values

None.

## Description

**GpuSync** blocks the host until every kernel launched so far has completed,
so results written by a kernel can be read afterwards.

## Examples

```clojure
(GpuSync)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[GpuKernelLaunch](GpuKernelLaunch.md), [exec_space](../enum_nodes/exec_space.md)
