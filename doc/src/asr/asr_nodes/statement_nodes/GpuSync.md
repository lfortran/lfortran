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
and the host has what they wrote, so results written by a kernel can be read
afterwards. A launch keeps what its kernel writes on the device until then.

## Examples

```clojure
(GpuSync)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/gpu_stmt.asr
:language: clojure
```

## See Also

[GpuKernelLaunch](GpuKernelLaunch.md), [GpuDiscard](GpuDiscard.md), [exec_space](../enum_nodes/exec_space.md)
