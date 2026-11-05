module operator_overloading_57_m
implicit none
type :: t
    integer :: i
contains
    procedure, pass(y) :: op_s, op_1d
    generic :: operator(.op.) => op_s, op_1d
end type
interface operator(.op.)
    module procedure op_r
end interface
contains
integer function op_r(x, y)
    real, intent(in) :: x, y
    op_r = 0
end function
elemental integer function op_s(x, y)
    integer, intent(in) :: x
    class(t), intent(in) :: y
    op_s = x + y%i
end function
function op_1d(x, y) result(r)
    integer, intent(in) :: x(:)
    class(t), intent(in) :: y
    integer :: r(size(x))
    r = 100*(x + y%i)
end function
end module

! F2018 15.5.6: op_1d is used; GFortran picks the first specific that fits.
program operator_overloading_57
use operator_overloading_57_m
implicit none
type(t) :: b
integer :: r(2), k
b%i = 1
r = [1, 2] .op. b
print *, r
if (any(r /= [200, 300])) error stop
k = 5 .op. b
print *, k
if (k /= 6) error stop
end program
