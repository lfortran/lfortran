program intrinsics_atan_02
! Fortran 2023: atand(y, x) and atanpi(y, x) are atan2d(y, x) and atan2pi(y, x)
implicit none
real(8), parameter :: tol = 1d-12
real(8), parameter :: pd = atand(1d0, -1d0)
real(8), parameter :: pp = atanpi(y=-1d0, x=-1d0)
real(4), parameter :: ps = atand(1.0, 1.0)
real(16), parameter :: qd = atand(-1.0_16, -1.0_16)
real(16), parameter :: qp = atanpi(y=1.0_16, x=-1.0_16)
real(8) :: y, x, a(3), b(3)
real(4) :: ys, xs
y = 1; x = -1
ys = 1; xs = 1
a = [1d0, -1d0, 0d0]; b = [1d0, 1d0, -1d0]
if (abs(pd - 135d0) > tol) error stop 1
if (abs(pp + 0.75d0) > tol) error stop 2
if (abs(ps - 45.0) > 1e-5) error stop 3
if (abs(atand(y, x) - 135d0) > tol) error stop 4
if (abs(atanpi(y, x) - 0.75d0) > tol) error stop 5
if (abs(atand(y=y, x=x) - atan2d(y, x)) > tol) error stop 6
if (abs(atand(x=x, y=y) - atan2d(y, x)) > tol) error stop 7
if (abs(atanpi(y=y, x=x) - atan2pi(y, x)) > tol) error stop 8
if (abs(atanpi(y, x=x) - atan2pi(y, x)) > tol) error stop 9
if (abs(atand(ys, xs) - 45.0) > 1e-5) error stop 10
if (abs(atanpi(ys, xs) - 0.25) > 1e-6) error stop 11
if (any(abs(atand(a, b) - [45d0, -45d0, 180d0]) > tol)) error stop 12
if (any(abs(atanpi(a, b) - [0.25d0, -0.25d0, 1d0]) > tol)) error stop 13
if (abs(qd + 135.0_16) > 1e-30_16) error stop 16
if (abs(qp - 0.75_16) > 1e-30_16) error stop 17
! The one-argument forms are unchanged
if (abs(atand(1d0) - 45d0) > tol) error stop 14
if (abs(atanpi(1d0) - 0.25d0) > tol) error stop 15
print *, pd, pp, ps, atand(y, x), atanpi(y, x)
end program
