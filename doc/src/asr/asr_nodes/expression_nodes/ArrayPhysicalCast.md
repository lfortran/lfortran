# Array Physical Cast
Casts array from one physical type to another.

## Declaration

### Syntax

```ASDL
ArrayPhysicalCast(expr arg, 
    array_physical_type old, 
    array_physical_type new, 
    ttype type,
    expr? value)
```

> array_physical_type = DescriptorArray | PointerArray | UnboundedPointerArray | FixedSizeArray | StringArraySinglePointer | NumPyArray | ISODescriptorArray | SIMDArray

### Arguments

| Argument Name | Argument Description                                                                                                        |
| ------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `arg`         | Array expression argument                                                                                                  |
| `old`         | Current physical array type of the array expression `arg`. <br>**Possible values** : `PointerArray`, `DescriptorArray`, etc |
| `new`         | The target physical array type to cast to.<br>**Possible values** : `PointerArray`, `DescriptorArray`, etc                   |
| `type`        | Type of expression (It's always an array with physical type equivalent to `new` ArrayPhysicalType)                         |
| `value`       | The type of the expression. It is always an array type corresponding to the new ArrayPhysicalType.                         |

### Return values

The return value is an array expression in the _new_ physical type. It's a message to the back end to know which physical type to use on this array expression.

## Description

**ArrayPhysicalCast** casts the physical type from one array type to another. It dispatches a message to the backend to cast the physical type of the passed array expression from one type to another, so we can do some runtime array operations like **assignment**, **slicing**, **printing** and **comparing** with no issues.

### Physical Types

**PointerArray** : The size is not known at compile time, but we know an
expression for it at runtime. This type is used for `bind(c)`; represented by
just a pointer, the compiler knows expressions dimensions at compile time. We
know the expression for all dimensions. The expression can contain runtime
parameters. It is represented by just a pointer. The compiler knows the
dimensions at compile time, but they are not stored at runtime explicitly. You
can call size(A), the compiler will determine the size expression at compile
time (using runtime variables).

Any PointerArray can be cast to a DescriptorArray and DescriptorArray can in
general be used without any additional code/logic in all places where
PointerArray is used.

The other direction is more complicated: It is possible to cast a
DescriptorArray to a PointerArray, but sometimes a temporary copy is needed
unless the DescriptorArray was contiguous. Also in order to obtain expressions
for the dimensions, one often requires to create a temporary and then use
ArraySize on the temporary as dimension expressions for the PointerArray.

```fortran
subroutine f(n, m, A) bind(c)
integer, intent(in) :: n, m
real, intent(in) :: A(n, m)
end subroutine
```

```fortran
subroutine f(n, m, A)
integer, intent(in) :: n, m
real, intent(in) :: A(n, 2*m)
end subroutine
```

Note: this is currently PointerArray, but it could also be FixedSizeArray.
```fortran
subroutine f(A)
real, intent(in) :: A(10, 20)
end subroutine
```

LHS is PointerArray (can also be FixedSizeArray), RHS is FixedSizeArray
```fortran
real :: x(3), a, b, c
x = [a, b, c] ! LHS is PointerArray/FixedSizeArray, RHS is FixedSizeArray
ArrayConstructor
```

**FixedSizeArray**: The size is known at compile time. The array elements are
either runtime (expressions) or compile time.

Any FixedSizeArray can be cast to PointerArray, and PointerArray can in general
be used in all places where FixedSizeArray is used (but not vice versa). The
advantage of FixedSizeArray is that the size is known at compile time and it is
enforced at the type level, so if both PointerArray adn FixedSizeArray can be
used, we generally prefer to use FixedSizeArray. By knowing the size at compile
time, the backend has the option to represent the array as a struct of a known
number of elements, and even pass this struct by value. The PointerArray on the
other hand can naturally only be passed as a pointer, because the size is not
known at compile time.

We enforce FixedSizeArray over PointerArray in the following
cases:

* Both ArrayConstant and ArrayConstructor should always be FixedSizeArray.

In other cases we currently allow both. Maybe in the future we can restrict
things more.

Examples of what can be a FixedSizeArray type:

```fortran
program main
real :: A(10, 20)
end program
```

```fortran
subroutine f(A)
real, intent(in) :: A(10, 20)
end subroutine
```

```fortran
real :: x(3), a, b, c
x = [1, 2, 3]  ! LHS is FixedSizeArray, RHS is FixedSizeArray (ArrayConstant)
x = [a, b, c]  ! LHS is FixedSizeArray, RHS is FixedSizeArray (ArrayConstructor)
```

In the above currently only the ArrayConstant/Constructor is enforced, the
other cases can also be a PointerArray, and usually it is.

**DescriptorArray**: Array is represented by an array descriptor (struct that contains the pointer to data, dimensions, strides, etc.)

```fortran
subroutine f(A)
real, intent(in) :: A(:,:)
end subroutine
```

```fortran
subroutine f()
real, pointer :: A(:, :)
end subroutine
```

```fortran
real, allocatable :: x(:)
real :: a, b, c
allocate(x(3))
x = [a, b, c]  ! LHS is DescriptorArray, RHS is FixedSizeArray (ArrayConstructor)
x = [1, 2, 3]  ! LHS is DescriptorArray, RHS is FixedSizeArray (ArrayConstant)
```

**UnboundedPointerArray**: Used for `A(*)`. Represented by just a pointer. The compiler does *not* know the size of the array at compile time, and there is no information about it at runtime. So you cannot call size(A). The `A(*)` usage deprecated, one should use either `A(:)` or `A(n)`. The compiler can check this in Debug mode by passing the expression for the dimension as an argument (the NAG compiler does this, for example).

```fortran
subroutine f(n, A)
integer, intent(in) :: n
real, intent(in) :: A(n, *)
! print *, size(A) ! Not allowed, does not compile
end subroutine
```

**CharacterArraySinglePointer**

```
subroutine f(s)
character(len=*), intent(in) :: s
end subroutine
```

**NumPyArray**: This is using NumPy array descriptor, mostly for LPython, but can be used by any compiler (LFortran can use this in the future for `bind(python)` functions). It is used for easy interoperability with NumPy. An array with this physical type can be accepted from Python and no conversion is needed, the descriptor is used as is.

**ISODescriptorArray**: This is Fortran 2018 ISO array descriptor, as described in the standard. Used for C interoperability. It is for being able to pass `DescriptorArray` arrays (`A(:)`) into C. The `DescriptorArray` is LCompilers specific, while `ISODescriptorArray` is standardized, so we must cast one to the other to make it work.

```fortran
subroutine f(A) bind(c)
real, intent(in) :: A(:,:)
end subroutine
```

**SIMDArray**: Used for arrays that are represented by a SIMD vector in the hardware, or maps directly into a vector register. LFortran provides a `simd` attribute (extension), or one can also use a pragma.
```fortran
subroutine f()
real, simd :: A(64)
end subroutine
```
