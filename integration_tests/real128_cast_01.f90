program real128_cast_01
! Constant folding of implicit conversions between real(16) and real(4)/real(8)
use, intrinsic :: iso_fortran_env, only: real128
implicit none
real(real128), parameter :: y = 0.1d0
real(real128), parameter :: z = 1.5
real(real128), parameter :: s = 1.5_real128 + 2.0
real(8), parameter :: d = 1.0_real128 / 3.0_real128
real(4), parameter :: f = 2.5_real128
real(real128) :: a
real(8) :: e

if (y /= real(0.1d0, real128)) error stop 1
if (z /= 1.5_real128) error stop 2
if (s /= 3.5_real128) error stop 3
if (d /= 1.0d0 / 3.0d0) error stop 4
if (f /= 2.5) error stop 5
if (real(y, 8) * 1.0_real128 /= y) error stop 6

a = 0.5
if (a /= 0.5_real128) error stop 7
a = 2.5d0
if (a /= 2.5_real128) error stop 8
e = 1.0_real128 / 4.0_real128
if (e /= 0.25d0) error stop 9
print *, y, z, s, d, f
end program
