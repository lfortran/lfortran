# gpu_argument_kind

The role of an entry in a finalized GPU launch layout.

## Declaration

### Syntax

```text
gpu_argument_kind = GpuArray | GpuStruct | GpuClass | GpuMemberData
    | GpuMemberOffsets | GpuMemberSizes | GpuScalar | GpuArrayExtent
    | GpuPackedOffset
```

## Values

| Value | Meaning |
|-------|---------|
| `GpuArray` | An array data buffer. |
| `GpuStruct` | A nonpolymorphic derived-type buffer. |
| `GpuClass` | A polymorphic argument marshalled as its declared type's data. |
| `GpuMemberData` | Flattened data for an allocatable array component. |
| `GpuMemberOffsets` | Per-element offsets into that component data. |
| `GpuMemberSizes` | Per-element component extents. |
| `GpuScalar` | A scalar kernel argument. |
| `GpuArrayExtent` | One array argument dimension. |
| `GpuPackedOffset` | A buffer's byte offset within a packed allocation. |

See [gpu_kernel_layout](../helper_nodes/gpu_kernel_layout.md).
