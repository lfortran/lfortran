# String Allocation In LFortran

### Note : This taking in consideration the LLVM backend only.
***
### General Rules :
- Allocation with (length = `0`) is fine, and string is considered **allocated** even when the (length = `0`).
- We make sure to follow this statement when allocating strings -> `MAX(alloc_len, 1)`; That makes sure that the preceding point is valid and working properly.
- Allocation with (length < `0`) isn't tolerated in LFortran, It raises error on both compile-time and run-time.

## Explicit Allocation

**Example** : 
```fortran
 allocate(character(10)  :: STR)   ! 1
 allocate(character(0)   :: STR)   ! 2
 allocate(character(-10) :: STR)   ! 3
 allocate(character(N)   :: STR)   ! 4
```
1 - Allocates `str` with compile-time value `10`, hence we evaluate `len = max(10, 1)` then we end up inserting this call `malloc(10)`.

2 - Allocates `str` with compile-time value `0`, hence we evaluate `len = max(0, 1)` at compile-time, we end up inserting this call at runtime `malloc(1)`.

3 - Allocates `str` with compile-time value `-10`, hence we raise an error at compile-time.

4 - Allocates `str` with run-time value `N`, hence we insert call `lfortran_string_malloc(N)`


## Runtime Function `lfortran_string_malloc()`

#### It makes sure to do the same things we do when the length is compile-time value.
- Raises runtime error when `length < 0`.
- Makes sure to set length at minimum of length `1` --> `malloc(MAX(length, 1))`, To handle case of `allocate(character(0) :: str)` properly.

## Copying 
#### Copying from string into another requires automatic allocation or reallocation (reallocation is on by default).

**Example** (realloc):
```fortran
character(:), allocatable :: STR
character(:), allocatable :: STR_2
STR = "HelloWorld" ! 1
STR = "BYE"        ! 2
STR = STR_2        ! 3
```

1 - Allocates memory equal to `rhs_len` --> `malloc(MAX(1, rhs_len))`
2 - Calls `realloc` at runtime with length of minimum 1 --> `realloc(lhs_data_ptr, MAX(rhs_len, 1))`
3 - Raises error as it copies from unallocated string.



**Example** (malloc):

```fortran
character(10), allocatable :: STR
character(:) , allocatable :: STR_2
STR = "HelloWorld" ! 1
STR = "BYE"        ! 2
STR = STR_2        ! 3
```
1 - Calls `malloc` at runtime with length of minimum 1 --> `malloc(MAX(lhs_len, 1))`.

2 - Copies from RHS into LHS + pads LHS. No need for `malloc()` or `realloc` calls as the LHS is already allocated and its length is fixed.

3 - Raises error as it copies from unallocated string.
