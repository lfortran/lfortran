! The same component of two different variables, read in place when an
! allocatable target is reallocated to the shape of the value, and the same
! component of one variable, still copied (#16215)
module derived_types_243_m
implicit none
type :: t
    real, allocatable :: x(:)
end type
type(t) :: g
contains

subroutine whole(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%x = a%x
end subroutine

subroutine section(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%x = a%x(2:3) + 1
end subroutine

subroutine to_module(c)
    type(t), intent(in) :: c
    g%x = c%x * 2
end subroutine

subroutine elements(u, v, i, j)
    type(t), intent(in) :: u(:)
    type(t), intent(inout) :: v(:)
    integer, intent(in) :: i, j
    v(i)%x = u(j)%x(1:2)
end subroutine

subroutine same_object(c)
    type(t), intent(inout) :: c
    c%x = c%x(2:3)
end subroutine

subroutine same_array(v, i, j)
    type(t), intent(inout) :: v(:)
    integer, intent(in) :: i, j
    v(i)%x = v(j)%x(3:1:-1) + 1
end subroutine

end module

program derived_types_243
use derived_types_243_m
implicit none
type(t) :: a, c, u(2), v(2)

allocate(a%x(3), c%x(1))
a%x = [1., 2., 3.]
c%x = 0
call whole(a, c)
if (size(c%x) /= 3) error stop 1
if (any(c%x /= [1., 2., 3.])) error stop 2
call section(a, c)
if (size(c%x) /= 2) error stop 3
if (any(c%x /= [3., 4.])) error stop 4

call to_module(a)
if (size(g%x) /= 3) error stop 5
if (any(g%x /= [2., 4., 6.])) error stop 6

allocate(u(1)%x(3), u(2)%x(3), v(1)%x(3), v(2)%x(5))
u(1)%x = [1., 2., 3.]
u(2)%x = [4., 5., 6.]
call elements(u, v, 2, 1)
if (size(v(2)%x) /= 2) error stop 7
if (any(v(2)%x /= [1., 2.])) error stop 8

c%x = [1., 2., 3.]
call same_object(c)
if (size(c%x) /= 2) error stop 9
if (any(c%x /= [2., 3.])) error stop 10

v(1)%x = [1., 2., 3.]
call same_array(v, 1, 1)
if (size(v(1)%x) /= 3) error stop 11
if (any(v(1)%x /= [4., 3., 2.])) error stop 12
v(2)%x = 0
call same_array(v, 2, 1)
if (size(v(2)%x) /= 3) error stop 13
if (any(v(2)%x /= [3., 4., 5.])) error stop 14

print *, "ok"
end program
