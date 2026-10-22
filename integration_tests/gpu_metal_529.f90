! A boundary stencil in a procedure that is called both from the host and
! from a parallel loop on the device. Index-set splitting leaves it alone:
! called from the device it runs on one thread anyway, and its loop must
! still run there.
module gpu_metal_529_m
implicit none
contains

    pure real function diff(a, i, n) result(r)
        integer, intent(in) :: i, n
        real, intent(in) :: a(:)
        if (i == 1) then
            r = a(2) - a(1)
        else if (i == n) then
            r = a(n) - a(n - 1)
        else
            r = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end function

    pure subroutine column(a, d, n)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        real, intent(out) :: d(n)
        integer :: i
        do concurrent (i = 1:n)
            d(i) = diff(a, i, n)
        end do
    end subroutine

end module

program gpu_metal_529
use gpu_metal_529_m, only: column
implicit none
integer, parameter :: n = 40, m = 8
real :: a(n, m), d(n, m), x(n), y(n)
integer :: i, j
do j = 1, m
    do i = 1, n
        a(i, j) = real(i * i + j)
    end do
end do
d = -100
x = a(:, 1)
y = -100
call column(x, y, n)
do concurrent (j = 1:m)
    call column(a(:, j), d(:, j), n)
end do
do j = 1, m
    if (d(1, j) /= 3) error stop 1
    if (d(n, j) /= real(2 * n - 1)) error stop 2
    do i = 2, n - 1
        if (d(i, j) /= real(2 * i)) error stop 3
    end do
end do
if (y(1) /= 3 .or. y(5) /= 10) error stop 4
print *, "ok"
end program
