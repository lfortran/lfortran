program intrinsics_atan_03
! atan(y, x) of real(16) constants in parameters, in every quadrant
use, intrinsic :: iso_fortran_env, only: real128
implicit none
real(real128), parameter :: pi = 3.14159265358979323846264338327950288_real128
real(real128), parameter :: tol = 1e-30_real128
real(real128), parameter :: q1 = atan(1.0_real128, 1.0_real128)
real(real128), parameter :: q2 = atan(1.0_real128, -1.0_real128)
real(real128), parameter :: q3 = atan(y=-1.0_real128, x=-1.0_real128)
real(real128), parameter :: q4 = atan(x=1.0_real128, y=-1.0_real128)
real(real128), parameter :: ax(3) = atan([0.0_real128, 1.0_real128, -2.0_real128], &
    [-1.0_real128, 0.0_real128, 0.0_real128])
if (abs(q1 - pi / 4) > tol) error stop 1
if (abs(q2 - 3 * pi / 4) > tol) error stop 2
if (abs(q3 + 3 * pi / 4) > tol) error stop 3
if (abs(q4 + pi / 4) > tol) error stop 4
if (abs(ax(1) - pi) > tol) error stop 5
if (abs(ax(2) - pi / 2) > tol) error stop 6
if (abs(ax(3) + pi / 2) > tol) error stop 7
print *, q1, q2, q3, q4, ax
end program
