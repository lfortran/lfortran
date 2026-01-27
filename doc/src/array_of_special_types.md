
# 1. Arrays of normal types
Normal Types : Integers, Floats, Complex, Compound-Types (C Structs)

These are arrays of types that doesn't require further care beyond allocating the consecutive memory,
No special handling required, No additional information maintainted.

### Example #1  : (DescriptorArray (integer 4))
`{ i32*, i32, %dimension_descriptor*, i1, i32 }`

We just allocate the required memory => (4bytes * size),
and store in first element of that descriptor. 


# 2. Arrays of special types
Special Types (so far) : Class, DescriptorStrings

These are arrays that looks like array of normal type but behave differently.
For types (class, descriptorString), We only allocate the structure of these types once 
and insert it into array normally, then we allocate consecutive memory for the actual useful type.

### Example #1  : (DescriptorArray (class)) -- not unlimited polymorphic
`Array => { Class*, i32, %dimension_descriptor*, i1, i32 }`

`Class => {Vtable*, underlying_struct*}`

**Allocation:** We just allocate single **Class** once the array variable (llvm SSA) is initiated and just insert there to hold the class info later.
Once allocation happens, We allocate consecutive memory for the underlying struct (array_size * struct_alloc_size), and
store it into array's class `underlying_struct*`.

**Fetching An Element:** Fetch the struct by indexing on array's-class-2nd-member, and create a class view (SSA) to hold the struct along with the corresponding VTable. 

### Example #2  : (DescriptorArray (descriptorString))
`Array => { descriptorString*, i32, %dimension_descriptor*, i1, i32 }`

`descriptorString => {i8*, i64}`

**Allocation:** We just allocate single **descriptorString** once the array variable (llvm SSA) is initiated and just insert there to hold the string acutal memory and length later.
If length is fixed we set it once variable initiated, otherwise we just set length on allocation.
Once allocation happens, We allocate consectuive memory for the strings (array_size * length), and
insert it into array's descriptorString `i8*` + Set length.

**Fetching An Element:** Fetch the string by indexing on array's-descriptorString-1st-member, and create a string view (SSA) to hold the string along with the length.
