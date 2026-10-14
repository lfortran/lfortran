! `zz%s(3)%u(1)` selects element `u(1)` of component `s(3)` of every element
! of the array `zz`, so the reference is array-valued and must not be marked
! in bounds, even though its subscripts are constants within the bounds.
program array_item_in_bounds_02
implicit none
type :: t
    real :: u(4)
end type
type :: v
    type(t) :: s(3)
end type
type(v) :: zz(2)
integer :: i
do i = 1, 3
    zz(1)%s(i)%u = i
    zz(2)%s(i)%u = 10*i
end do
if (any(abs(zz%s(3)%u(1) - [3., 30.]) > 0)) error stop
print *, zz%s(3)%u(1)
end program
