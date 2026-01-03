# Array Descriptor in LLVM IR  

In Fortran, arrays whose shape cannot be fully determined at compile time are represented at runtime using array descriptors.
An array descriptor encapsulates both a pointer to the underlying data buffer and the metadata required to interpret the array (such as dimensions, strides, and bounds).

---

##  Array Descriptor Struct  

The LLVM IR representation of an array descriptor:  

```llvm
%array = type { i32*, i32, %dimension_descriptor*, i1, i32 }
```

## Field Descriptors

| Field Name | Type | Description |
|------------|------|-------------|
| `data_pointer`     | `i32*` | Pointer to the beginning of the raw array data |
|   `base_offset`  | `i32` | Offset from the data_pointer to the logical first element of the array |
| `dimension_descriptor` | `%dimension_descriptor*` | Pointer to a contiguous block of `N` dimension descriptors structs, where N is the array rank|
| `is_pointer` | `i1` | Indicates whether this is a Fortran pointer array |
| `rank` | `i32` | The number of dimensions in the array (Rank)|

## Dimension Descriptor Struct
Each dimension of the array has an associated dimension descriptor:
```llvm
%dimension_descriptor = type { i32, i32, i32 }
```

## Field Descriptors

| Field Name | Type | Description |
|------------|------|-------------|
| `stride` | `i32` | The offset between consecutive values along this dimension |
| `lower_bound` | `i32` | The Lower bound of the dimension |
| `size` | `i32` | The extent of the dimension (number of elements along this axis) |

