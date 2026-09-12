program gpu_kernel_source_02
! Named constants referenced inside a do concurrent body.
implicit none
integer, parameter :: n = 12
real, parameter :: a = 2.5
integer, parameter :: tbl(3) = [7, 8, 9]
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i)
    y(i) = 0.0
    y_expected(i) = a * real(i) + real(tbl(mod(i - 1, 3) + 1))
end do

do concurrent (i = 1:n)
    y(i) = a * x(i) + real(tbl(mod(i - 1, 3) + 1))
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1e-3) error stop
end do

print *, "PASSED"
end program
