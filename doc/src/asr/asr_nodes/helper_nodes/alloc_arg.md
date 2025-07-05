# alloc_arg

An **allocation argument** used inside the `Allocate` and `ReAlloc` statement.

## Declaration

### Syntax

```fortran
alloc_arg(expr a, dimension* dims, expr? len_expr, symbol? sym_subclass, ttype? type)
```

### Arguments

Input arguments of `a`, `dims`, optional `len_expr`, optional `sym_subclass`, and optional `type`.

`a` contains the variable to be allocated.  
`dims` contains a list of dimension bounds in the form of tuples `(lower, upper)` representing shape.  
`len_expr` is an optional expression specifying length, used for character and derived types.  
`sym_subclass` is an optional symbol representing subclass type for polymorphic allocation.  
`type` is an optional type that specifies the target type of allocation.

### Return values

None.

## Description

**alloc_arg** specifies one element in the `Allocate` statement. It defines what variable to allocate, with what dimensions, type, subclass, and optional length parameters. It is used only as part of the `Allocate` and `ReAlloc` node.

## Types

The variable `a` must be allocatable.  
Dimensions must be valid integer expressions.  
Optional fields may be used for polymorphic or character allocations.

## Examples

Following example code allocates a memory block of size 3:

```fortran
program allocate_mem
real, allocatable :: a(:)
allocate(a(3))
end program
```

ASR (portion corresponding to `alloc_arg`):

```
((Var 2 a)
[((IntegerConstant 1 (Integer 4) Decimal)
(IntegerConstant 3 (Integer 4) Decimal))]
()
()
())
```

```fortran
type :: base
    integer :: x
end type

type, extends(base) :: derived
    real :: y
end type

class(base), allocatable :: obj

allocate(derived :: obj)
```

```
((Var 2 obj)
[]
()
2 derived
(StructType
    []
    []
    .true.
    2 derived
))
```


## See Also

[Allocate](#allocate)
