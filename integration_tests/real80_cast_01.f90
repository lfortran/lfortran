program real80_cast_01
! Constant folding of conversions to and from real(10)
implicit none
integer, parameter :: n = 3
integer(8), parameter :: big = 9007199254740993_8
real(10), parameter :: x = n * 2.0_10
real(10), parameter :: y = 2.5d0
real(10), parameter :: z = 1.5
real(10), parameter :: w = big
integer, parameter :: i = 7.75_10
complex(8), parameter :: c = 2.5_10
real(10) :: a, b, e
real(8) :: d
integer :: j

if (x /= 6) error stop
if (y /= 2.5_10) error stop
if (z /= 1.5_10) error stop
if (w /= 9007199254740993.0_10) error stop
if (i /= 7) error stop
if (c /= (2.5d0, 0.0d0)) error stop

a = 1
if (a /= 1.0_10) error stop
b = 0.5
if (b /= 0.5_10) error stop
e = big
if (e /= 9007199254740993.0_10) error stop
d = 2.5_10
if (d /= 2.5d0) error stop
j = 7.75_10
if (j /= 7) error stop
print *, x, y, z, w, i, c
end program
