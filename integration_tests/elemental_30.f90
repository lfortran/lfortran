module elemental_30_m
implicit none
type :: t
    real :: v = 0
contains
    procedure :: add
end type
contains
elemental subroutine add(self, y)
    class(t), intent(inout) :: self
    real, intent(in) :: y
    self%v = self%v + y
end subroutine
elemental subroutine s(x, y)
    real, intent(inout) :: x
    real, intent(in), optional :: y
    if (present(y)) x = x + y
end subroutine
subroutine g(a, b)
    real, intent(inout) :: a(:)
    real, intent(in), optional :: b(:)
    call s(a, b)
end subroutine
end module

program elemental_30
use elemental_30_m
implicit none
type(t), allocatable :: a(:)
real, allocatable :: b(:), c(:)
integer :: n
n = 3
allocate(a(n), b(n), c(n))
b = [1, 2, 3]
call a%add(b)
if (any(a%v /= [1, 2, 3])) error stop
call a(2:n)%add(b(1:n-1))
if (any(a%v /= [1, 3, 5])) error stop
c = 1
call g(c, b)
if (any(c /= [2, 3, 4])) error stop
print *, a%v, c
end program
