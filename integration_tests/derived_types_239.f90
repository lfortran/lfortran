! Whole array components as operands of elementwise operations, read in
! place, when an allocatable target is reallocated to the shape of the
! value (#16215)
module derived_types_239_m
implicit none

type :: inner
    real, allocatable :: x(:)
end type

type :: t
    real, allocatable :: x(:), y(:)
    type(inner) :: in
end type

contains

subroutine add_to_other(a)
    type(t), intent(inout) :: a
    a%x = a%y + a%y
end subroutine

subroutine add_to_self(a)
    type(t), intent(inout) :: a
    a%x = a%x + a%y
end subroutine

subroutine add_to_nested(a)
    type(t), intent(inout) :: a
    a%in%x = -a%y
end subroutine

subroutine add_to_new(a, c)
    type(t), intent(in) :: a
    real, allocatable, intent(inout) :: c(:)
    c = a%x * a%y
end subroutine

subroutine add_to_element(arr)
    type(t), intent(inout) :: arr(:)
    arr(1)%x = arr(2)%x * 2
end subroutine

end module

program derived_types_239
use derived_types_239_m
implicit none
type(t) :: a, arr(2)
real, allocatable :: c(:)

allocate(a%x(2), a%in%x(5))
a%x = [1., 2.]
a%y = [1., 2., 3., 4.]
call add_to_other(a)
if (size(a%x) /= 4) error stop 1
if (any(a%x /= [2., 4., 6., 8.])) error stop 2
call add_to_self(a)
if (any(a%x /= [3., 6., 9., 12.])) error stop 3
call add_to_nested(a)
if (size(a%in%x) /= 4) error stop 4
if (any(a%in%x /= [-1., -2., -3., -4.])) error stop 5
call add_to_new(a, c)
if (size(c) /= 4) error stop 6
if (any(c /= [3., 12., 27., 48.])) error stop 7

arr(1)%x = [1.]
arr(2)%x = [1., 2., 3.]
call add_to_element(arr)
if (size(arr(1)%x) /= 3) error stop 8
if (any(arr(1)%x /= [2., 4., 6.])) error stop 9
print *, "ok"
end program
