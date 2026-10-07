module overloaded_unary_array_01_m
implicit none
type :: v
    real :: x = 0
end type
interface operator(-)
    module procedure neg
end interface
contains
elemental type(v) function neg(a)
    type(v), intent(in) :: a
    neg%x = -a%x
end function
end module

program p
use overloaded_unary_array_01_m
implicit none
type(v) :: q(3), qq(3)
q%x = 2
qq = -q
print *, qq%x
if (any(qq%x /= -2)) error stop
end program