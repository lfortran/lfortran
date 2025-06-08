# String Physical Cast
Cast string physical type from [`DescriptorString`](../type_nodes/StringPhysicalType.md) to [`PointerString`](../type_nodes/StringPhysicalType.md) and vice versa
## Declaration

### Syntax

```ASDL
StringPhysicalCast(expr arg, 
	string_physical_type old, 
	string_physical_type new, 
	ttype type,
	expr? value)
```

### Arguments

| Argument Name | Argument Description                                                                                                        |
| ------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `arg`         | String expression argument                                                                                                  |
| `old`         | Current physical string type of the string expression `arg`. <br>**Possible values** : `DescriptorString`, `PointerString`. |
| `new`         | The target physical string type to cast to.<br>**Possible values** : `DescriptorString`, `PointerString`.                   |
| `type`        | Type of expression (It's always a string with physical type equivalent to `new` StringPhysicalType)                         |
| `value`       | The type of the expression. It is always a string type corresponding to the new StringPhysicalType.                         |

### Return values

The return value is a string expression in the _new_ physical type. It's a message to the back end to know which physical type to use on this string expression.
## Description

**StringPhysicalCast** casts the physical type from [`DescriptorString`](../type_nodes/StringPhysicalType.md) to [`PointerString`](../type_nodes/StringPhysicalType.md) and vice versa. It dispatches a message to the backend to cast the physical type of the passed string expression from one type to another, so we can do some runtime string operations like **assignment**, **slicing**, **printing** and **comparing** with no issues when using the *DescriptorString* `{char* data, int64 size, int64 capacity}` and also to keep using it and utilizing the size and capacity members when needed.

**The intention behind casting from [`DescriptorString`](../type_nodes/StringPhysicalType.md) to [`PointerString`](../type_nodes/StringPhysicalType.md) :**
- The intention is to tell the back end to operate on pointerString `char*`instead of the whole descriptorString llvm struct`{char* data, int64 size, int64 capacity}` 
- This happens by fetching `char* data` from the LLVM descriptor 
- **NOTE** : This kind of casting is widely used in the code base as most of the runtime operations use `char*` and don't need to utilize `size` and `capacity` member of the LLVM 

**The intention behind casting from [`PointerString`](../type_nodes/StringPhysicalType.md) to [`DescriptorString`](../type_nodes/StringPhysicalType.md):**\
- The intention is to tell the back end to operate on the whole descriptorString llvm struct`{char* data, int64 size, int64 capacity}` instead of pointerString `char*` 
- This happens by creating an LLVM struct (which represents the descriptorString) called `casted_string_ptr_to_desc` then we just make its internal `char*` point to the passed pointerString arg, and then we fill `size` and `capacity` with `-1` value to be an indication later for any function that utilizes these 2 values - telling it that this string is casted and it's originally a pointer (fixed memory location) and don't extend it.  
- **NOTE** : This isn't widely used in the code base unless we're dealing with some LHS-RHS kinda of operations on strings, that's where we make the two sides have equal physical string types *like function-argument passing*. apart from that, it makes our life easier when implementing LHS-RHS string expression visitors just be knowing a small fact that the internals of this LHS-RHS string expression are of descriptorString physical type.
## Types

Only accepts strings.
## Examples

``` Fortran
program main
	character(:),allocatable :: char_dynamic
	character(20):: char_fixed = "Hello World"
	char_dynamic = char_fixed
	char_fixed = char_dynamic
	print *, char_dynamic
	allocate(character(3) :: char_dynamic)
end program main
```

ASR:  
```Clojure
(Assignment
	 ; here we store char_fixed content into char_dynamic
	 ; So We convert the physical type of rhs to the physical type of lhs by          ; creating a StringPhysicalCast node.

		(Var 2 char_dynamic)
		(StringPhysicalCast  
			(Var 2 char_fixed)
			PointerString
			DescriptorString
			(Allocatable
				(Character 1 20 () DescriptorString)
			)
			()
		)
		()
	)
	(Assignment
		;; Here happens the opposite
		;; here we store char_dynamic content into char_fixed
		;; To create a clean assignment in the back end we had to convert
		;; the RHS side to the physical type of the LHS
		(Var 2 char_fixed)
		(StringPhysicalCast
			(Var 2 char_dynamic)
			DescriptorString
			PointerString
			(Allocatable
				(Character 1 -2 () PointerString)
			)
			()
		)
		()
	)
	(Print
		(StringFormat
			()
			;; StringFormat uses a runtime function to do handle the formatting
			;; which means it operates on char* and doesn't utilized size or
			;; capacity. This forces us to convert to pointerString.
			[(StringPhysicalCast
				(Var 2 char_dynamic)
				DescriptorString
				PointerString
				(Allocatable
					(Character 1 -2 () PointerString)
				)
				()
			)]
			FormatFortran
			(Character -1 0 () PointerString)
			()
		)
	)
	;; allocate utilizes size and capacity, so we don't cast here. 
	(Allocate
		[((Var 2 char_dynamic)
		[]
		(IntegerConstant 3 (Integer 4) Decimal)
		())]
		()
		()
		()
	)

```

## See Also

[`PointerString`](../type_nodes/../type_nodes/StringPhysicalType.md), [`DescriptorString`](../type_nodes/../type_nodes/StringPhysicalType.md) 