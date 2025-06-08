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

> array_physical_type = DescriptorArray | PointerToDataArray | UnboundedPointerToDataArray | FixedSizeArray | StringArraySinglePointer | NumPyArray | ISODescriptorArray | SIMDArray

### Arguments

| Argument Name | Argument Description                                                                                                        |
| ------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `arg`         | Array expression argument                                                                                                  |
| `old`         | Current physical array type of the array expression `arg`. <br>**Possible values** : `PointerToDataArray`, `DescriptorArray`, etc |
| `new`         | The target physical array type to cast to.<br>**Possible values** : `PointerToDataArray`, `DescriptorArray`, etc                   |
| `type`        | Type of expression (It's always an array with physical type equivalent to `new` ArrayPhysicalType)                         |
| `value`       | The type of the expression. It is always an array type corresponding to the new ArrayPhysicalType.                         |

### Return values

The return value is an array expression in the _new_ physical type. It's a message to the back end to know which physical type to use on this array expression.

## Description

**ArrayPhysicalCast** casts the physical type from one array type to another. It dispatches a message to the backend to cast the physical type of the passed array expression from one type to another, so we can do some runtime array operations like **assignment**, **slicing**, **printing** and **comparing** with no issues.

### Physical Types

**PointerToDataArray** : This type is used for `bind(c)`; represented by just a pointer, the compiler knows expressions dimensions at compile time. We know the expression for all dimensions. The expression can contain runtime parameters. It is represented by just a pointer. The compiler knows the dimensions at compile time, but they are not stored at runtime explicitly. You can call size(A), the compiler will determine the size expression at compile time (using runtime variables).

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

```fortran
subroutine f(A)
real, intent(in) :: A(10, 20)
end subroutine
```

**FixedSizeArray**: An array that is fully known at compile time: both size and elements. ArrayConstant. In LLVM backend we can store such an array as a variable with known size (say 10). (This type might in principle be possible to use for `A(10, 20)`, but currently we don't. It cannot be used for `A(n, m)`.). It seems we cannot change elements at runtime, they are all constant.

```fortran
program main
real :: A(10, 20)
end program
```

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

**UnboundedPointerToDataArray**: Used for `A(*)`. Represented by just a pointer. The compiler does *not* know the size of the array at compile time, and there is no information about it at runtime. So you cannot call size(A). The `A(*)` usage deprecated, one should use either `A(:)` or `A(n)`. The compiler can check this in Debug mode by passing the expression for the dimension as an argument (the NAG compiler does this, for example).

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
