! A polymorphic allocatable array of an extended dynamic type passed to an
! intent(inout) or intent(out) array dummy of its declared type, with the LHS
! reallocated on assignment: copying the dummy back must keep the actual's
! allocation and dynamic type and only update the parent part.
program class_173
implicit none
type :: t
    integer :: y
end type
type, extends(t) :: t2
    integer :: z
end type
class(t), allocatable :: cw(:)
integer :: i
allocate(t2 :: cw(3))
do i = 1, 3
    cw(i)%y = i
end do
select type (cw)
type is (t2)
    cw%z = 7
end select
call set_second(cw)
call clear(cw)
if (size(cw) /= 3) error stop
if (any(cw%y /= -1)) error stop
select type (cw)
type is (t2)
    if (any(cw%z /= 7)) error stop
class default
    error stop
end select
contains
subroutine set_second(x)
    type(t), intent(inout) :: x(:)
    if (x(2)%y /= 2) error stop
    x(2)%y = 42
end subroutine
subroutine clear(x)
    type(t), intent(out) :: x(:)
    x%y = -1
end subroutine
end program
