! Extent of an elemental call and of an array element designator with vector
! subscripts: only the vector subscripts are dimensions of the result.
module elemental_27_m
implicit none
contains
    elemental function f(x) result(r)
        real, intent(in) :: x
        real :: r
        r = 2 * x
    end function

    subroutine chk(x, idx)
        real, intent(in) :: x(:,:)
        integer, intent(in) :: idx(:)
        real :: y(2)
        if (size(f(x(idx, 2))) /= 2) error stop 1
        if (size(f(x(idx, idx))) /= 4) error stop 2
        y = f(x(idx, 2))
        if (any(y /= [12.0, 8.0])) error stop 3
    end subroutine
end module

program elemental_27
use elemental_27_m, only: chk
implicit none
real :: x(3,3)
integer :: a(4,5), v(3), w(2), i
x = reshape([(real(i), i = 1, 9)], [3,3])
call chk(x, [3, 1])
v = [4, 1, 2]
w = [5, 3]
if (size(a(2, v), 1) /= 3) error stop 4
if (size(a(v, 3), 1) /= 3) error stop 5
if (size(a(v, w), 1) /= 3) error stop 6
if (size(a(v, w), 2) /= 2) error stop 7
if (size(a(v, w)) /= 6) error stop 8
if (any(shape(a(v, w)) /= [3, 2])) error stop 9
print *, "ok"
end program
