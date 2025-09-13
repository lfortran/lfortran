# Array Descriptor in LLVM IR  

In Fortran, arrays whose **shape is not fully known at compile time** are represented using **array descriptors**.  
These descriptors contain both a pointer to the raw data and metadata such as bounds, strides, and rank.  

---

##  Array Descriptor Struct  

The LLVM IR representation of an array descriptor:  

```llvm
%array = type { i32*, i32, %dimension_descriptor*, i1, i32 }
```

## Field Descriptors

| Field Name | Type | Description |
|------------|------|-------------|
| `data_pointer`     | `i32*` | Pointer to the first element of the array |
|     | `i32` | Offset to the first element of the array |
| `dimension_descriptor` | `%dimension_descriptor*` | Pointer to an array of dimension descriptors |
| `is_pointer` | `i1` | Indicates if the array is a pointer array |
| `rank` | `i32` | Number of dimensions in the array |

## Dimension Descriptor Struct

```llvm
%dimension_descriptor = type { i32, i32, i32 }
```

## Field Descriptors

| Field Name | Type | Description |
|------------|------|-------------|
| `lower_bound` | `i32` | Lower bound of the dimension |
| `upper_bound` | `i32` | Upper bound of the dimension |
| `stride` | `i32` | Stride of the dimension |

