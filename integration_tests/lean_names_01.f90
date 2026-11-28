module m_shadow
implicit none
contains
    integer function helper(x)
    integer, intent(in) :: x
    helper = x + 1
    end function

    integer function call_with_shadow(m_shadow)
    integer, intent(in) :: m_shadow
    call_with_shadow = helper(m_shadow)
    end function
end module

program lean_names_01
use, intrinsic :: iso_fortran_env, only: dp => real64
use m_shadow
implicit none
integer :: include, prefix, end, end_, i, repeat, hiding, max, pure
real(dp) :: using, z
include = 1; prefix = 2; end = 3; end_ = 4; repeat = 5; hiding = 6
max = 10; pure = 1
do i = 1, 3
    end = end + i
    max = max + i
end do
if (pure == 1) then
end if
if (include /= 1 .or. prefix /= 2 .or. end /= 9 .or. end_ /= 4) error stop 1
if (repeat /= 5 .or. hiding /= 6 .or. max /= 16 .or. i /= 4) error stop 5
if (call_with_shadow(10) /= 11) error stop 6
z = -0.0_dp
using = sign(1.0_dp, z)
if (using /= -1.0_dp) error stop 2
using = sign(2.0_dp, -0.0_dp)
if (using /= -2.0_dp) error stop 3
using = sign(2.0_dp, 0.0_dp)
if (using /= 2.0_dp) error stop 4
print *, include, prefix, end, end_, using
end program
