---
layout: book
title: asin
permalink: intrinsics/mathematics/trigonometric
---

## asin(x)

Trigonometric archsine function.

## Declaration

### Syntax

```fortran
result = asin(x)
  elemental TYPE(kind=KIND) function asin(x)
  TYPE(kind=KIND) :: x
```
### Arguments

`x` the input value, can be real or complex; less than or equal to 1.

### Return values

The returned value has the kind of the input value and TYPE may be real
or complex.

## Description

**asin(x)** computes the arcsine of the argument **x**.

The arcsine is the inverse function of the sine function. It is commonly used in
trigonometry to find the angle when the lengths of the hypotenuse and the
opposite side of a right triangle are known.

## Examples

```fortran
program intrinsics\_asin
implicit none
integer, parameter :: arg_x = kind(0.0)
real(arg_x) :: x1
real :: ret_val_x
real :: arg_x
ret_val_x = asin(0.84147098)
print *, ret_val_x
ret_val_x = asin(x1)
print *, ret_val_x
end program
```

**Result**:

```
0.99999
0.99999
```
## See Also

[acos](acos.md), [atan](atan.md).

