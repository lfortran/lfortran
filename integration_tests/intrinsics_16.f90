program intrinsics_16
use iso_fortran_env, only: sp=>real32, dp=>real64
real(sp) :: r32
real(dp) :: r64
complex(sp) :: c32
complex(dp) :: c64
real(sp), parameter :: x1 = aimag((5._sp, 7._sp))
real(dp), parameter :: x2 = aimag((8.2_dp, 7.12_dp))
real(sp), parameter :: ar1(3) = aimag([(5._sp, 7._sp), (8.2_sp, 7.12_sp), (1.2_sp, 3.4_sp)])
real(dp), parameter :: ar2(3) = aimag([(5._dp, 7._dp), (8.2_dp, 7.12_dp), (1.2_dp, 3.4_dp)])
complex(sp) :: arr1(3) = [(5._sp, 7._sp), (8.2_sp, 7.12_sp), (1.2_sp, 3.4_sp)]
complex(dp) :: arr2(3) = [(5._dp, 7._dp), (8.2_dp, 7.12_dp), (1.2_dp, 3.4_dp)]
c32 = (5._sp, 7._sp); c64 = (5._dp, 7._dp)

print *, kind(aimag(arr2))
if (kind(aimag(arr2)) /= 8) error stop

print *, x1
if (abs(x1 - 7.0_sp) > 1e-5) error stop

print *, x2
if (abs(x2 - 7.12000000000000011e+00_dp) > 1e-10) error stop

print *, ar1
if (any(ar1 - [7.0_sp, 7.11999989e+00_sp, 3.40000010e+00_sp] > 1e-6)) error stop

print *, ar2
if (any(ar2 - [7.00000000000000000e+00_dp, 7.12000000000000011e+00_dp, 3.39999999999999991e+00_dp] > 1e-10)) error stop

r32 = aimag(c32)
print *, r32
if (abs(r32 - 7) > 1e-5) error stop

r64 = aimag(c64)
print *, r64
if (abs(r64-7) > 1e-10) error stop

r32 = imag(c32)
print *, r32
if (abs(r32-7) > 1e-5) error stop

r64 = dimag(c64)
print *, r64
if (abs(r64-7) > 1e-10) error stop

print *, aimag(arr1)
if (any(aimag(arr1) - [7.0_sp, 7.11999989e+00_sp, 3.40000010e+00_sp] > 1e-6)) error stop

print *, aimag(arr2) ! Does not work yet #4542
if (any(aimag(arr2) - [7.00000000000000000e+00_dp, 7.12000000000000011e+00_dp, 3.39999999999999991e+00_dp] > 1e-10)) error stop

end program
