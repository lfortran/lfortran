# String Physical Type

### Types 

```ASDL
string_physical_type = PointerString | DescriptorString
```

### Description

- **PointerString :** 
	- It's the normal C `char*`. The memory for it gets allocated by the runtime `lfortran_str_copy()`.
	- PointerString can't be an `allocatable` unless it's [casted](../expression_nodes/StringPhysicalCast.md) from `descriptorString` to `pointerString` so it remains to be identified as an allocatable variable in the whole code base, like avoiding some semantic errors when you use the string with some intrinsic function that requires the string to be an allocatable.
	
- **DescriptorString :** 
	- It's an LLVM struct to hold information about data, size and capacity.
	- It's represented in LLVM IR as : `{char*, int64, int64}`. The benefit from having `size` and `capacity` is that it gives us the ability to have dynamic string that's not computationally expensive, just like `std::vector` in c++. So the key points are:
		- `size` avoids calling `strlen` when we want to know the size of a string. 
		- `capacity` gives us the flexibility of extending the string without losing performance by doubling the memory location every time we run out of allocated memory space.
	- DescriptorString must always be an allocatable.
### See Also

[stringPhysicalCast](../expression_nodes/StringPhysicalCast.md)
 
