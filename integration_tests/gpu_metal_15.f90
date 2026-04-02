program gpu_metal_15
! Element-wise power and exponentiation
implicit none
integer, parameter :: n = 10000
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i) * 0.001
    y_expected(i) = x(i) ** 2 + exp(-x(i))
end do

do concurrent (i = 1:n)
    y(i) = x(i) ** 2 + exp(-x(i))
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-4) error stop
end do

print *, "PASSED"
end program
