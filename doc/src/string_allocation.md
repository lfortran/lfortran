# String Allocation In LFortran

### Note : This taking in consideration the LLVM backend only.
***
### General Rules :
- Allocation with (length = `0`) is fine, and string is considered **allocated** even when the (length = `0`).
- We make sure to follow this statement when allocating strings -> `MAX(alloc_len, 1)`; That makes sure that the preceding point is valid and working properly.
- Allocation with (length < `0`) isn't tolerated in LFortran, It raises error on both compile-time and run-time.
- Copying RHS string into LHS string isn't only limited for the data content, but for the allocated-or-not state as well; Meaning that some assignments might act as `deallocate()` if RHS isn't allocated.

## Explicit Allocation

**Example** : 
```fortran
 allocate(character(10)  :: STR)   ! 1
 allocate(character(0)   :: STR)   ! 2
 allocate(character(-10) :: STR)   ! 3
 allocate(character(N)   :: STR)   ! 4
```
1 - Allocates `str` with compile-time value `10`, hence we evaluate `len = max(10, 1)` then we end up inserting this call `malloc(10)`.

2 - Allocates `str` with compile-time value `0`, hence we evaluate `len = max(0, 1)` we end up inserting this call `malloc(1)`.

3 - Allocates `str` with compile-time value `-10`, hence we raise an error.

4 - Allocates `str` with run-time value `N`, hence we insert call `lfortran_string_malloc(N)`


## Runtime Function `lfortran_string_malloc()`

#### It makes sure to do the same things we do when the length is compile-time value.
- Raises runtime error when `length < 0`.
- Makes sure to set length at minimum of length `1` --> `malloc(MAX(length, 1))`.

## Copying 
#### Copying from string into another requires automatic allocation or reallocation (by default).
#### We Copy the allocated-or-not state from RHS; An unallocated RHS makes LHS unallocated as well.

**Example** (realloc):
```fortran
character(:), allocatable :: STR
character(:), allocatable :: STR_2
STR = STR_2        ! 1
STR = "HelloWorld" ! 2
```

1 - Tries to realloc `STR`, but as RHS is not allocated we end up setting LHS to the not-allocated state, similar to`(deallocate(STR))`.

2 - Calls `realloc` at runtime with length of minimum 1 --> `realloc(lhs_data_ptr, MAX(rhs_len, 1))`


**Example** (malloc):

```fortran
character(10), allocatable :: STR
character(:) , allocatable :: STR_2
STR = "HelloWorld" ! 1
STR = STR_2        ! 2
```
1 - Calls `malloc` at runtime with length of minimum 1 --> `malloc(MAX(lhs_len, 1))`.

2 - Tries to copy from RHS (the memory for LHS is already set no need for call to `malloc()`) into `STR`, but as RHS is not allocated we end up setting LHS to the not-allocated state `(deallocate(STR))`.
