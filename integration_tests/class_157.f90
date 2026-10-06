program class_157
implicit none
type :: t
    integer :: y
end type
type, extends(t) :: t2
    integer :: z
end type
class(t), allocatable :: cw(:)
integer :: i
allocate(t2 :: cw(4))
do i = 1, 4
    cw(i)%y = 10*i
end do
select type (cw)
type is (t2)
    cw%z = 7
end select
call check(cw, [10, 20, 30, 40])
call check(cw(2:3), [20, 30])
call check(cw(1:4:2), [10, 30])
call set_second(cw)
call clear(cw(3:4))
if (any(cw%y /= [10, 42, -1, -1])) error stop
select type (cw)
type is (t2)
    if (any(cw%z /= 7)) error stop
class default
    error stop
end select
contains
subroutine check(x, expected)
    type(t), intent(in) :: x(:)
    integer, intent(in) :: expected(:)
    integer :: i
    if (size(x) /= size(expected)) error stop
    do i = 1, size(x)
        if (x(i)%y /= expected(i)) error stop
    end do
end subroutine
subroutine set_second(x)
    type(t), intent(inout) :: x(:)
    x(2)%y = 42
end subroutine
subroutine clear(x)
    type(t), intent(out) :: x(:)
    x%y = -1
end subroutine
end program
