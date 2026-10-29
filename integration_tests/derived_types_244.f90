module derived_types_244_m
implicit none
type t
    integer :: x(3)
end type
contains
subroutine assign(a, i, c)
    type(t), intent(in) :: a(:)
    integer, target, intent(inout) :: i, c(:)
    ! TARGET permits i to alias c(1) (F2018 15.5.2.13).
    c = a(i)%x + 1
end subroutine
end module

program derived_types_244
use derived_types_244_m
implicit none
type(t) :: a(3)
integer, target :: b(3)
a(1)%x = [2, 3, 4]
a(2)%x = [10, 11, 12]
a(3)%x = [20, 30, 40]
b = [1, 0, 0]
call assign(a, b(1), b)
print *, b
if (any(b /= [3, 4, 5])) error stop
end program
