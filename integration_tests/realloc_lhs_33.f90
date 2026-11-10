program realloc_lhs_33
! Intrinsic assignment to an allocatable array from a whole array of another
! kind allocates the variable with the lower bounds of that whole array: the
! kind conversion is part of the assignment (F2018 10.2.1.3), not of the
! expression
implicit none
type :: t
    real(8), allocatable :: a(:)
end type
real(8), allocatable :: src(:)
real(4), allocatable :: dst(:)
integer(8), allocatable :: isrc(:)
integer(4), allocatable :: idst(:)
integer(8) :: b(5:6)
integer(8), allocatable :: m8(:,:)
integer(4), allocatable :: m4(:,:)
type(t) :: s
integer :: bi(5:6)
real(4) :: r4(5:6)
complex(4) :: z4(5:6)
real(8), allocatable :: e(:), d8(:)

allocate(src(-2:1))
src = [1d0, 2d0, 3d0, 4d0]
dst = src
if (lbound(dst, 1) /= -2) error stop
if (ubound(dst, 1) /= 1) error stop
if (any(abs(dst - [1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop

! An allocated variable of another shape is reallocated with the bounds of
! the whole array too
deallocate(dst)
allocate(dst(3))
dst = src
if (lbound(dst, 1) /= -2) error stop
if (ubound(dst, 1) /= 1) error stop
if (abs(dst(1) - 4.0) > 1e-6) error stop

! A converted array expression still has lower bounds of 1
deallocate(dst)
dst = src + 1d0
if (lbound(dst, 1) /= 1) error stop
if (ubound(dst, 1) /= 4) error stop
if (abs(dst(4) - 5.0) > 1e-6) error stop

allocate(isrc(-2:1))
isrc = [10_8, 20_8, 30_8, 40_8]
idst = isrc
if (lbound(idst, 1) /= -2) error stop
if (ubound(idst, 1) /= 1) error stop
if (any(idst /= [10, 20, 30, 40])) error stop
deallocate(idst)

b = [7_8, 8_8]
idst = b
if (lbound(idst, 1) /= 5) error stop
if (ubound(idst, 1) /= 6) error stop
if (any(idst /= [7, 8])) error stop

! A conversion to another type keeps the bounds of the whole array as well
deallocate(dst)
dst = idst
if (lbound(dst, 1) /= 5) error stop
if (ubound(dst, 1) /= 6) error stop
if (abs(dst(6) - 8.0) > 1e-6) error stop

allocate(m8(0:1, -1:0))
m8 = reshape([1_8, 2_8, 3_8, 4_8], [2, 2])
m4 = m8
if (any(lbound(m4) /= [0, -1])) error stop
if (any(ubound(m4) /= [1, 0])) error stop
if (m4(1, 0) /= 4) error stop

allocate(s%a(-1:1))
s%a = [5d0, 6d0, 7d0]
deallocate(dst)
dst = s%a
if (lbound(dst, 1) /= -1) error stop
if (ubound(dst, 1) /= 1) error stop
if (abs(dst(-1) - 5.0) > 1e-6) error stop

! An explicit conversion is a function reference, whose result has lower
! bounds of 1 (F2018 16.9.109), as has the result of real(a, kind)
bi = [1, 2]
r4 = [1.0, 2.0]
z4 = [(1.0, 0.0), (2.0, 0.0)]
e = dble(bi)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
if (abs(e(2) - 2d0) > 1d-12) error stop
deallocate(e)
e = dfloat(bi)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
deallocate(e)
e = dble(b)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
deallocate(e)
e = dble(r4)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
deallocate(e)
e = dble(z4)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
if (abs(e(2) - 2d0) > 1d-12) error stop
deallocate(e)
e = real(bi, 8)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 2) error stop
deallocate(dst)
dst = float(bi)
if (lbound(dst, 1) /= 1 .or. ubound(dst, 1) /= 2) error stop
if (abs(dst(2) - 2.0) > 1e-6) error stop
deallocate(dst)
dst = dble(bi)
if (lbound(dst, 1) /= 1 .or. ubound(dst, 1) /= 2) error stop
deallocate(dst)
dst = real(b)
if (lbound(dst, 1) /= 1 .or. ubound(dst, 1) /= 2) error stop
deallocate(idst)
idst = int(b, 4)
if (lbound(idst, 1) /= 1 .or. ubound(idst, 1) /= 2) error stop
if (idst(2) /= 8) error stop
allocate(d8(-2:1))
d8 = [1d0, 2d0, 3d0, 4d0]
deallocate(e)
e = dble(d8)
if (lbound(e, 1) /= 1 .or. ubound(e, 1) /= 4) error stop
if (abs(e(4) - 4d0) > 1d-12) error stop

! A function result has lower bounds of 1, also when the function allocates
! it with others and it is converted to the kind of the variable
deallocate(dst)
dst = f8(src)
if (lbound(dst, 1) /= 1 .or. ubound(dst, 1) /= 4) error stop
if (abs(dst(4) - 4.0) > 1e-6) error stop

print *, "ok"

contains

function f8(x) result(r)
real(8), intent(in) :: x(:)
real(8), allocatable :: r(:)
allocate(r(7:6 + size(x)))
r = x
end function

end program
