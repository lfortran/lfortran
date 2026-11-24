program lean_names_01
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none
integer :: include, prefix, end, end_, i
real(dp) :: using, z
include = 1; prefix = 2; end = 3; end_ = 4
do i = 1, 3
    end = end + i
end do
if (include /= 1 .or. prefix /= 2 .or. end /= 9 .or. end_ /= 4) error stop 1
z = -0.0_dp
using = sign(1.0_dp, z)
if (using /= -1.0_dp) error stop 2
using = sign(2.0_dp, -0.0_dp)
if (using /= -2.0_dp) error stop 3
using = sign(2.0_dp, 0.0_dp)
if (using /= 2.0_dp) error stop 4
print *, include, prefix, end, end_, using
end program
