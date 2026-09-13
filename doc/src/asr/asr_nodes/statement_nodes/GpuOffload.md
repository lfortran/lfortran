# GpuOffload

A GPU offload candidate with an explicit CPU alternative.

## Declaration

### Syntax

```text
GpuOffload(symbol kernel, stmt* body, stmt* fallback)
```

`kernel` is the extracted kernel function. `body` contains its launch and
associated transfers; `fallback` retains the original parallel loop.
Neither alternative executes while this node exists.

Shared expression, array and procedure passes lower both alternatives.
OpenMP outlining and region flattening wait until `gpu_kernel_finalize`
selects one: outlining an unchosen alternative must not change the host
variables' storage or introduce unused runtime callbacks.

The finalizer checks the fully lowered kernel, records its verified
`Function.gpu` layout, and replaces the candidate with the selected statement
list. On a permitted decline it removes the unused kernel and keeps the CPU
alternative. This node never reaches host or device code generation.

See [GpuKernelLaunch](GpuKernelLaunch.md) and
[Function](../symbol_nodes/Function.md).
