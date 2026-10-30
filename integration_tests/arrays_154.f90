program arrays_154
implicit none
integer :: a(-2:0), b(3)
integer(8) :: c(2, 3)
a = values()
if (a(-2) /= 10 .or. a(-1) /= 20 .or. a(0) /= 30) error stop 1
call check(a)
if (lbound(a, 1) /= -2 .or. ubound(a, 1) /= 0) error stop 2
if (lbound(values(), 1) /= 1 .or. ubound(values(), 1) /= 3) error stop 3
b = values() + a
if (b(1) /= 20 .or. b(2) /= 40 .or. b(3) /= 60) error stop 4
c = matrix()
if (c(1, 1) /= 11 .or. c(1, 3) /= 13) error stop 5
if (c(2, 1) /= 21 .or. c(2, 3) /= 23) error stop 6
call check_matrix(c)
contains
function values() result(r)
integer :: r(-1:1)
r = [10, 20, 30]
end function
function matrix() result(r)
integer(8) :: r(-1:0, 0:2)
integer :: i, j
do j = 0, 2
    do i = -1, 0
        r(i, j) = 10_8 * (i + 2) + j + 1
    end do
end do
end function
subroutine check(x)
integer, intent(in) :: x(-1:1)
if (x(-1) /= 10 .or. x(0) /= 20 .or. x(1) /= 30) error stop 7
end subroutine
subroutine check_matrix(x)
integer(8), intent(in) :: x(0:1, -2:0)
if (x(0, -2) /= 11 .or. x(0, 0) /= 13) error stop 8
if (x(1, -2) /= 21 .or. x(1, 0) /= 23) error stop 9
end subroutine
end program
