# GpuDiscard

Forgets what pending kernel launches write into variables the host frees.

## Declaration

### Syntax

```text
GpuDiscard(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the variables, arrays or derived-type values. |

### Return values

None.

## Description

A [GpuKernelLaunch](GpuKernelLaunch.md) does not wait for its kernel, and
the device keeps what the kernel writes until a [GpuSync](GpuSync.md)
copies it back. **GpuDiscard** comes before the host frees the storage of
`vars` -- at the end of the `block` they are locals of, or before they are
deallocated -- while a launch that writes them may be pending: the values
are not needed, so instead of waiting for them the runtime forgets them, and
copies nothing back into storage that no longer exists.

The `gpu_launch_sync` pass places it; see
[GPU offloading](../../../gpu_offloading.md).

## Examples

```clojure
(GpuDiscard
  :vars [
    (Var
      :v (SymbolRef 2 "a")
    )
  ]
)
```

## See Also

[GpuKernelLaunch](GpuKernelLaunch.md), [GpuSync](GpuSync.md)
