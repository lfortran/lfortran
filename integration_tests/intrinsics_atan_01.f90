program intrinsics_atan_01
! Fortran 2008: atan(y, x) is the same as atan2(y, x)
use, intrinsic :: iso_fortran_env, only: real32, real64, real128
implicit none
real(real32), parameter :: ps = atan(1.0_real32, -1.0_real32)
real(real64), parameter :: pd = 4 * atan(1.0_real64, 1.0_real64)
real(real64), parameter :: pk = atan(y=-2.0_real64, x=-2.0_real64)
real(real64), parameter :: pa(3) = atan([1.0_real64, -1.0_real64, 0.0_real64], -1.0_real64)
real(real32) :: ys, xs, as(3), bs(3), cs(3)
real(real64) :: y, x
real(real128) :: yq, xq
ys = 1.0; xs = -1.0
y = -2.0_real64; x = -2.0_real64
yq = 1; xq = -1
as = [1.0, -1.0, 0.5]; bs = [-1.0, 2.0, -3.0]
if (atan(ys, xs) /= atan2(ys, xs)) error stop 1
if (atan(y, x) /= atan2(y, x)) error stop 2
if (atan(yq, xq) /= atan2(yq, xq)) error stop 3
if (atan(y=y, x=x) /= atan2(y, x)) error stop 4
if (atan(x=x, y=y) /= atan2(y, x)) error stop 5
if (atan(y, x=x) /= atan2(y, x)) error stop 6
if (any(atan(as, bs) /= atan2(as, bs))) error stop 7
cs = atan(as, -1.0)
if (any(cs /= atan2(as, -1.0))) error stop 8
if (abs(ps - atan2(1.0_real32, -1.0_real32)) > 1e-6_real32) error stop 9
if (abs(pd - 4 * atan(1.0_real64)) > 1e-15_real64) error stop 10
if (abs(pk - atan2(-2.0_real64, -2.0_real64)) > 1e-15_real64) error stop 11
if (abs(pa(1) - atan2(1.0_real64, -1.0_real64)) > 1e-15_real64) error stop 12
if (abs(pa(2) - atan2(-1.0_real64, -1.0_real64)) > 1e-15_real64) error stop 13
if (abs(pa(3) - atan2(0.0_real64, -1.0_real64)) > 1e-15_real64) error stop 14
! The one-argument form is unchanged
if (abs(atan(1.0_real64) - pd / 4) > 1e-15_real64) error stop 15
if (abs(atan(x=1.0_real64) - pd / 4) > 1e-15_real64) error stop 16
print *, ps, pd, pk, atan(y, x), atan(yq, xq)
end program
