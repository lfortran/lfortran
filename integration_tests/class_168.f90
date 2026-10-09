program class_168
implicit none
type :: t
    integer :: y
end type
class(t), allocatable :: cw(:)
class(t), pointer :: cp(:)
integer :: i
allocate(cw(4))
do i = 1, 4
    cw(i)%y = i
end do
call check(cw(2:3), [2, 3])
allocate(cp(3))
do i = 1, 3
    cp(i)%y = 10*i
end do
call check(cp, [10, 20, 30])
deallocate(cp)
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
end program
