program class_156
implicit none
type :: t
    integer :: y
end type
class(t), allocatable :: cw(:)
integer :: i
allocate(cw(4))
do i = 1, 4
    cw(i)%y = i
end do
call check(cw, [1, 2, 3, 4])
call set_second(cw)
if (cw(2)%y /= 42) error stop
call check(cw, [1, 42, 3, 4])
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
end program
