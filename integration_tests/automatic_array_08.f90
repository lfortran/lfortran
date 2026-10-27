! SHAPE of an explicit-shape array, or of an expression with its shape, is
! its shape on entry, also after a variable of its bounds is redefined.
module automatic_array_08_m
implicit none
integer :: g = 3
contains

subroutine s(lo, n, m, a, b)
    integer :: lo, n, m
    real :: a(lo:n, m), b(n)
    real :: w(lo:n, m)
    integer :: sh(2)
    integer(8) :: sh8(2)
    n = 100
    m = 50
    lo = -7
    sh = shape(a)
    if (any(sh /= [2, 3])) error stop 1
    sh8 = shape(a, kind=8)
    if (any(sh8 /= [2_8, 3_8])) error stop 2
    if (any(shape(w) /= [2, 3])) error stop 3
    if (any(shape(b) /= [3])) error stop 4
    if (size(a) /= 6 .or. size(b) /= 3) error stop 5
    w = reshape([1., 2., 3., 4., 5., 6.], shape(a))
    if (w(3, 3) /= 6) error stop 6
    if (any(shape(a + 1) /= [2, 3])) error stop 7
    if (any(shape(sin(b)) /= [3])) error stop 8
    block
        real :: d(g)
        g = 9
        if (any(shape(d) /= [3])) error stop 9
    end block
end subroutine

subroutine inout_bound(a, n)
    integer, intent(inout) :: n
    real, intent(inout) :: a(n)
    n = 1
    if (size(a) /= 3) error stop 10
    if (any(shape(a) /= [3])) error stop 11
end subroutine

function f(n) result(r)
    integer :: n
    real :: r(n)
    r = 1
    n = 4
    if (any(shape(r) /= [2])) error stop 12
end function

end module

program automatic_array_08
use automatic_array_08_m
implicit none
integer :: lo, n, m, k
real :: x(2, 3), y(5)
lo = 2
n = 3
m = 3
call s(lo, n, m, x, y)
n = 3
call inout_bound(y, n)
k = 2
if (any(shape(f(k)) /= [2])) error stop 13
print *, "ok"
end program
