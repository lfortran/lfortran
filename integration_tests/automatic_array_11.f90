module automatic_array_11_m
implicit none
contains
    ! A procedure that a DO CONCURRENT loop calls in device code: an
    ! automatic array whose upper bound is below its lower bound is empty.
    pure real function work(n, v)
        integer, intent(in) :: n
        real, intent(in) :: v
        real :: w(n - 1)
        w = v
        work = 100 * size(w) + sum(w)
    end function
end module

program automatic_array_11
use automatic_array_11_m
implicit none
real :: r(6)
integer :: i, k(6)
k = [4, 3, 1, 0, -2, 2]
do concurrent (i = 1:6)
    r(i) = work(k(i), real(i))
end do
if (any(abs(r - [303., 204., 0., 0., 0., 106.]) > 1e-6)) error stop 1
print *, "ok"
end program
