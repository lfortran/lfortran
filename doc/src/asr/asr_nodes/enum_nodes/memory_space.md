# memory_space

Which memory an array's storage lives in.

## Declaration

### Syntax

```text
memory_space = Global | Shared | Constant | Thread
```

### Values

| Value | Meaning |
|----------|-------------|
| `Global` | device global memory, the buffers the host allocates. This is the default, and it is what every array in host code has. |
| `Shared` | storage shared by the threads of one threadgroup. |
| `Constant` | read only storage. |
| `Thread` | storage private to one thread. |

### Return values

None. An enumeration value is not evaluated.

## Description

The memory space is a member of [Array](../type_nodes/Array.md). It matters
only where the target distinguishes the spaces: Metal qualifies every pointer
as `device`, `threadgroup`, `constant` or `thread`, while CUDA does not
qualify pointers at all.

The `gpu_memory_space` pass assigns the spaces to the arrays of device code.
A procedure called with arrays from different spaces cannot be emitted once,
because the qualifiers are part of its signature, so the pass clones it per
distinct combination of spaces.

The per-thread value is called `Thread` rather than `Private` because the ASR
enumerations share one namespace, and `Private` is already a value of
[access](access.md).

## See Also

[Array](../type_nodes/Array.md), [array_physical_type](array_physical_type.md), [exec_space](exec_space.md)
