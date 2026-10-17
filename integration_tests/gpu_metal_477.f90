module gpu_metal_477_m
implicit none
contains
    pure subroutine work(a, n, r)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        real, intent(out) :: r
        real :: w(n)
        w = a + 1
        r = sum(w)
    end subroutine
    pure real function outer(a, n)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        call work(a, n, outer)
    end function
end module

program gpu_metal_477
use gpu_metal_477_m
implicit none
integer, parameter :: n = 4, m = 256
real :: y(n), x(m, n), r
integer :: j, k
do j = 1, n
    x(:, j) = j
end do
! Inlining turns the callee's automatic array into a fixed-size local.
do concurrent (j = 1:n)
    call work(x(:, j), 256, r)
    y(j) = r
end do
do j = 1, n
    if (y(j) /= m * (j + 1)) error stop 1
end do
do concurrent (j = 1:n)
    y(j) = outer(x(:, j), 256)
end do
do j = 1, n
    if (y(j) /= m * (j + 1)) error stop 2
end do
print *, "ok"
end program
