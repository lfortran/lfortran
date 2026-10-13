module operator_overloading_42_mod
implicit none
type :: r_t
    logical :: passed = .false.
    integer :: x = 0
end type
interface operator(.and.)
    module procedure and_op
end interface
interface operator(+)
    module procedure add
end interface
contains
elemental function and_op(lhs, rhs) result(res)
    type(r_t), intent(in) :: lhs, rhs
    type(r_t) :: res
    res%passed = lhs%passed .and. rhs%passed
end function
elemental function add(lhs, rhs) result(res)
    type(r_t), intent(in) :: lhs, rhs
    type(r_t) :: res
    res%x = lhs%x + rhs%x
end function
elemental function twice(a) result(res)
    type(r_t), intent(in) :: a
    type(r_t) :: res
    res%x = 2 * a%x
end function
elemental function thrice_x(a) result(res)
    type(r_t), intent(in) :: a
    integer :: res
    res = 3 * a%x
end function
integer function count_passed(v)
    type(r_t), intent(in) :: v(:)
    count_passed = count(v%passed)
end function
end module

program operator_overloading_42
! An elemental function that overloads an operator, referenced with an
! array operand, gives an array of the shape of that operand.
use operator_overloading_42_mod
implicit none
type(r_t) :: a(3), b(3), c(3), s
integer :: ai(3)
a%passed = [.true., .true., .false.]
b%passed = [.true., .false., .true.]
s%passed = .true.
a%x = [1, 2, 3]
b%x = 10
s%x = 100

c = a .and. b
print *, c%passed
if (any(c%passed .neqv. [.true., .false., .false.])) error stop
c = s .and. b
if (any(c%passed .neqv. [.true., .false., .true.])) error stop
c = a .and. s
if (any(c%passed .neqv. [.true., .true., .false.])) error stop
if (size(a .and. b) /= 3) error stop
if (size(s .and. b) /= 3) error stop
if (count_passed(a .and. b) /= 1) error stop
if (count_passed(s .and. b) /= 2) error stop

c = a + b
if (any(c%x /= [11, 12, 13])) error stop
c = a + s
if (any(c%x /= [101, 102, 103])) error stop
c = s + a
if (any(c%x /= [101, 102, 103])) error stop
c = twice(a + b)
print *, c%x
if (any(c%x /= [22, 24, 26])) error stop
ai = thrice_x(a + b)
if (any(ai /= [33, 36, 39])) error stop
if (size(s + a) /= 3) error stop
end program
