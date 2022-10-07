# Allocate

Allocate function, a **statement (stmt)** node.

## Declaration

### Syntax

```fortran
template <typename T> T* allocate(size_t n=1)
```

### Arguments

Input argument of `size_t`, say `n` to get the size to be allocated on heap
memory.

### Return values

Pointer to the starting location of the allocated memory by `allocate()` call.

## Description

**allocate** the destination buffer with `path = (char*)malloc(length + 1);`

Allocates `n` elements of `type T`, returns the pointer `T*` to the first
element.

## Utility Functions/Method and Types

None.

Internally uses `alloc()` C++  method call.

## Types

Only accepts integer whole number value, that can be allocated on available
heap memory.

```c
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```
## Examples

Following example code allocates a memory block of size 2:

```fortran
program allocate_mem
implicit none
    real, allocatable :: a(:)
    integer :: n, ierr
    n = 2
    allocate(a(n), stat=ierr)
    a = 3
    print *, a
    deallocate(a)
end program t01
```

## See Also
