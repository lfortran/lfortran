module operator_overloading_40_mod
implicit none
integer :: ncalls = 0

type :: u
    integer :: w = 0
end type

interface assignment(=)
    module procedure asg
end interface

contains

subroutine asg(a, b)
    type(u), intent(out) :: a
    integer, intent(in) :: b
    a%w = b
end subroutine asg

integer function idx()
    ncalls = ncalls + 1
    idx = 2
end function idx

end module operator_overloading_40_mod

program operator_overloading_40
use operator_overloading_40_mod
implicit none
type(u) :: arr(3)

arr(idx()) = 7

if (arr(1)%w /= 0) error stop
if (arr(2)%w /= 7) error stop
if (arr(3)%w /= 0) error stop
if (ncalls /= 1) error stop

print *, "PASS"
end program operator_overloading_40
