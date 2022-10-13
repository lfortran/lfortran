program intrinsics_16
use iso_fortran_env, only: sp=>real32, dp=>real64
real(sp) :: r32
real(dp) :: r64
complex(sp) :: c32
complex(dp) :: c64
c32 = (5._sp, 7._sp); c64 = (5._dp, 7._dp)

r32 = aimag(c32)
print *, r32
if (abs(r32-7) > 1e-5) error stop

r64 = aimag(c64)
print *, r64
if (abs(r64-7) > 1e-10) error stop

r32 = imag(c32)
print *, r32
if (abs(r32-7) > 1e-5) error stop

r64 = dimag(c64)
print *, r64
if (abs(r64-7) > 1e-10) error stop

end program
