module operator_overloading_55_m
implicit none
type :: v
    integer :: i
end type
interface operator(+)
    module procedure add_s, add_1d
end interface
interface operator(==)
    module procedure eq_s, eq_1d
end interface
interface operator(.and.)
    module procedure and_s, and_1d
end interface
contains
elemental type(v) function add_s(a, b)
    type(v), intent(in) :: a, b
    add_s%i = a%i + b%i
end function
function add_1d(a, b) result(r)
    type(v), intent(in) :: a(:), b(:)
    type(v) :: r(size(a))
    r%i = 100*(a%i + b%i)
end function
elemental logical function eq_s(a, b)
    type(v), intent(in) :: a, b
    eq_s = a%i == b%i
end function
function eq_1d(a, b) result(r)
    type(v), intent(in) :: a(:), b(:)
    logical :: r(size(a))
    r = .false.
end function
elemental integer function and_s(a, b)
    type(v), intent(in) :: a, b
    and_s = a%i + b%i
end function
function and_1d(a, b) result(r)
    type(v), intent(in) :: a(:), b(:)
    integer :: r(size(a))
    r = 100*(a%i + b%i)
end function
end module

program operator_overloading_55
use operator_overloading_55_m
implicit none
type(v) :: q(2), s(2)
logical :: l(2)
integer :: k(2)
q%i = [1, 2]
s = q + q
print *, s%i
if (any(s%i /= [200, 400])) error stop
l = q == q
print *, l
if (any(l)) error stop
k = q .and. q
print *, k
if (any(k /= [200, 400])) error stop
end program
