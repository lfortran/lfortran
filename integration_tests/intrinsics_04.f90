program intrinsics_04
use iso_fortran_env, only: dp=>real64, sp=>real32
real(dp) :: x
complex(dp) :: z

x = 1.5_dp
x = tan(x)
print *, x
if (abs(x - 14.101419947171721_dp) > 1e-10_dp) error stop
if (abs(tan(1.5_sp) - 14.1014204_sp) > 1e-6) error stop
if (abs(tan(tan(1.5_sp) + tan(x+tan(x))) - 2.254825) > 1e-5) error stop

z = (1.5_dp, 3.5_dp)
z = tan(z)
print *, z
if (abs(real(z,dp) - 2.57834890405532317E-004_dp) > 1e-10_dp) error stop
if (abs(real(tan((1.5_sp, 3.5_sp)), sp) - 2.57834879E-004_sp) > 1e-10_dp) error stop
!if (abs(aimag(z) - 1.0018071108086137_dp) > 1e-10_dp) error stop

end program intrinsics_04
