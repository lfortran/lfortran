# LLVMFinalize Class
## Brief
- Finalizes symbol-table's variables.
- It's used right after inserting return or end block of an ASR construct (program, function, block, etc.)
## How To Use ?
- Create an instance by using the exposed constructor.
- Call the only exposed member function `finalize_symtab()` with the desired symbol table to finalize.
- Use it after declaring the return block OR the end block of the construct you're freeing the symbol table of.
- Notice it doesn't work with module's symbol table.
## Design Flow
Global variables aren't finalized; They live till program ends anyway so we don't care much about them.
The Class has a main finalizer function `finalize()` that either dispatches to `finalize_allocatable()` or `finalize_type()`
#### Main Finalizers
- `finalize_allocatable()` :
    It finalizes the type first by calling `finalize_type()` that consequently dispatches to the actual finalizer function based on the type of couse. Next, It frees the allocated memory (due to the nature of the `allocatable` attribute for sure). 

    **Example**: `type(t), allocatable :: n`
    - Calls `finalize()` -> `finalize_allocatable()` -> `finalize_type()` -> `free_allocatable()`

- `finalize_type()`
    This disptaches to one of following :

    `finalize_scalars()`, `finalize_struct()`, `finalize_string()`,`finalize_array()`,
    `finalize_list()`, `finalize_set()` ,`finalize_dict()` ,`finalize_tuple()`, `finalize_union()`

    Each one of those is responsible of finalizing the type it's named after.

    **Example**: `type(t), allocatable :: n`

    `finalize_struct()` once works on that `t` type, it will loop on each and every variable (struct member) and free it using `finalize()`. 

## Examples

#### `type(t), alloatable :: t_arr(:)`
- `finalize()`             --> Dispatches to `finalize_allocatable()`
- `finalize_allocatable()` --> Calls `finalize_type()`
- `finalize_type()`        --> Calls `finalize_array()`
- `finalize_array()`       --> Finalizes internals elements and other info (if any is heap allocated). Let's see how.
- `finalize_array()`       --> Calls utility `finalize_array_data()` to free array's elements as different array types require different handling, they don't go and just call `finalize` on the type of array.
- `finalize_array_data()` --> Loops on the array size to get each and every struct `t` .
- `finalize_array_data()` --> Calls `finalize_struct()` on each element.  
- `finalize_struct()`     --> Loops on each and every element attempting to finalize each memeber within the struct by its own finalizer based on the struct member type using `finalize_type()`
- `finalize_array()`      --> Calls `free_array_ptr_to_consecutive_data()` this is responsible of freeing consecutively allocated memory of the array.
- `finalize_allocatable()`--> Calls `free_allocatable_ptr()` which should have the logic if allocatable arrays need a finalization (They physical array type).