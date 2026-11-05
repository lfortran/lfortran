module operator_overloading_54_m
implicit none
type :: v
    integer :: i
contains
    procedure :: add_1d
    generic :: operator(+) => add_1d
end type
contains
function add_1d(a, b) result(r)
    class(v), intent(in) :: a
    integer, intent(in) :: b(:)
    integer :: r(size(b))
    r = a%i + b
end function
end module

program operator_overloading_54
use operator_overloading_54_m
implicit none
type(v) :: q
integer :: r(3)
q%i = 10
r = q + [1, 2, 3]
print *, r
if (any(r /= [11, 12, 13])) error stop
end program
