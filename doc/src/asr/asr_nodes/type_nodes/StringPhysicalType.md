# String Physical Type

## Types 

```ASDL
string_physical_type = PointerString | DescriptorString
```

## Description

- **PointerString :** 
	- It's the normal C `char*`. The memory for it gets allocated by the runtime `lfortran_str_copy()`.
	- PointerString can't be an `allocatable` unless it's an allocatable-array or string [casted](../expression_nodes/StringPhysicalCast.md) from `descriptorString` to `pointerString` so it remains to be identified as an allocatable variable in the whole code base, like avoiding some semantic errors when you use the string with some intrinsic function that requires the string to be an allocatable.
	
- **DescriptorString :** 
	- It's an LLVM struct to hold information about data, size and capacity.
	- It's represented in LLVM IR as : `{char*, int64, int64}`. The benefit from having `size` and `capacity` is that it gives us the ability to have dynamic string that's not computationally expensive, just like `std::string` (`std::vector`) in c++. So the key points are:
		- `size` avoids calling `strlen` when we want to know the size of a string. 
		- `capacity` gives us the flexibility of extending the string without losing performance by doubling the memory location every time we run out of allocated memory space.
	- DescriptorString must always be an allocatable.

## Usage 

- **DescriptorString**

	DescriptorString is used only with allocatable strings.
```Fortran
character(:) , allocatable:: chr
```

```Clojure
(Variable
	2
	chr
	[]
	Local
	()
	()
	Default
	(Allocatable
		(Character 1 -2 () DescriptorString) ; Notice the physical type.
	)
	()
	Source
	Public
	Required
	.false.
)
```


``` Fortran
character(5) :: chr
character(:),allocatable :: chr_RHS
chr_RHS = chr ! Cast RHS from PointerString --> DescriptorString
```

``` Clojure
(Assignment
	(Var 2 chr_rhs)
	(StringPhysicalCast
		(Var 2 chr)
		PointerString
		DescriptorString
		(Allocatable
			(Character 1 5 () DescriptorString) ; Notice Physical type
		)
		()
	)
	()
)
```


- **PointerString**
	It could be used with allocatable strings and non-allocatable strings.
   - non-allocatable + pointerString --> fixed-size string and literal string 

```Fortran
character(5) :: chr 
chr = "Hello"
```

``` Clojure
chr:
	(Variable
		2
		chr
		[]
		Local
		()
		()
		Default
		(Character 1 5 () PointerString) ; Notice The Physical Type.
		()
		Source
		Public
		Required
		.false.
	)
(Assignment
	(Var 2 chr)
	(StringConstant
		"Hello"
		(Character 1 5 () PointerString) ; Notice The Physical Type
	)
	()
)
```
 
 - allocatable + pointerString --> When string casted from descriptorString to pointerString.
 
 ```Fortran
character(5) :: chr
character(:), allocatable :: chr_RHS
chr = chr_RHS ! Cast RHS from DescriptorString to PointerString
```

``` Clojure
(Assignment
	(Var 2 chr)
	(StringPhysicalCast
		(Var 2 chr_rhs)
		DescriptorString
		PointerString
		(Allocatable
			(Character 1 -2 () PointerString) ; Notice alloctable pointerString.
		)
		()
	)
	()
)
```

### See Also

[stringPhysicalCast](../expression_nodes/StringPhysicalCast.md)
 
