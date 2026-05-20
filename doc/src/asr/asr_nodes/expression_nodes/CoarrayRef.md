````markdown
# CoarrayRef

Coarray reference with image selectors.

## Declaration

### Syntax

```
CoarrayRef(expr var, array_index* coindices, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`var` | base variable or array reference expression (e.g., `x` or `x(i,j)`) |
|`coindices` | coarray image selectors from `[...]` notation; array of `array_index` |
|`type` | the result type of the coarray reference (same as element type of `var`) |
|`value` | optional compile-time constant value (usually `nullptr`) |

### Return values

The return value is the expression representing the coarray reference.

## Description

**CoarrayRef** represents a reference to a coarray element with explicit image selectors.
In Fortran, coarrays enable single-sided communication and data access across multiple images
(processes/instances). The `CoarrayRef` node captures both the base data reference and the
image indices that specify which images the data is being accessed from.

The `coindices` field preserves the full list of coarray subscripts from the source code,
enabling backends to:
- Validate that the number of coindices matches the variable's corank
- Generate appropriate runtime calls (e.g., MPI for distributed computing)
- Implement single-image or multi-image execution strategies

## Semantic Properties

- The base variable (`var`) must have `corank > 0` (be a coarray)
- Number of `coindices` must equal the variable's `corank`
- In single-image mode, coindices are typically ignored and the reference resolves to `var`
- In multi-image mode, coindices determine which image(s) are accessed

## Types

Accepts any expression that is a coarray (variable, struct member, or array reference).

## Examples

### Single Coindex

```fortran
integer :: x[*]
x[1] = 5  ! Access element on image 1
```

ASR (simplified):
```
CoarrayRef(
    var = Var(x),
    coindices = [array_index(left=IntegerConstant(1), right=nullptr, step=nullptr)],
    type = Integer(4),
    value = nullptr
)
```

### Array Coarray with Multiple Coindices

```fortran
integer :: y(10)[*,*]  ! 2D array with corank 2
y(2)[3,4] = 42  ! Access y(2) on image at position (3,4)
```

ASR (simplified):
```
CoarrayRef(
    var = ArrayItem(
        v = Var(y),
        args = [array_index(left=IntegerConstant(2))],
        type = Integer(4)
    ),
    coindices = [
        array_index(left=IntegerConstant(3)),
        array_index(left=IntegerConstant(4))
    ],
    type = Integer(4),
    value = nullptr
)
```

### Struct Member Coarray

```fortran
type :: mytype
    integer :: data[*]
end type
type(mytype) :: obj
obj%data[2] = 10
```

ASR (simplified):
```
CoarrayRef(
    var = StructInstanceMember(v=Var(obj), m=data, type=Integer(4)),
    coindices = [array_index(left=IntegerConstant(2))],
    type = Integer(4),
    value = nullptr
)
```

## Backend Lowering

### Single-Image Mode (Current Default)
In single-image execution, coindices are semantically validated but typically ignored during code generation,
as all accesses are local to the current image.

### Multi-Image Mode (Future)
Backends will use coindices to generate remote memory access operations via:
- MPI (distributed computing)
- Shared memory access (OpenMP, UPC-like)
- Custom CAF (Coarray Fortran) runtime

## Related Nodes

- `Variable` - tracks `corank` field to validate coindex count
- `ArrayItem` - for indexing into array elements
- `ArraySection` - for array slices
- `Var` - for simple variable references

````
